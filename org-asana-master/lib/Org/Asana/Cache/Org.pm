package Org::Asana::Cache::Org;

use Moose;

has runpidfilename => (is=>'ro',isa=>'Str',default=>"/tmp/build-cache-org.pid");
has cachefilename => (is=>'rw', isa=>'Str', lazy=>1, default=>sub{shift->oa->dir . "/cache/org.yaml"});

with 'Org::Asana::Cache';
use Org::Asana::Task;
use Org::Parser;
use Class::Load ':all';
use Tie::IxHash;

sub orgfiles {
	my $self = shift;

	opendir (WORKSPACES, $self->oa->dir."/workspaces");
	my @workspaces = grep { $_ ne "." and $_ ne ".." and -d $self->oa->dir."/workspaces/$_" } readdir(WORKSPACES);
	closedir WORKSPACES;

	my @orgfiles;
	foreach my $workspace (map { $self->oa->dir . "/workspaces/$_" } @workspaces) {
		$self->oa->verbose("**** looking at %s", $workspace);
		opendir WORKSPACE, $workspace;
		my @user_files = (grep { -f }
						  map  { "$workspace/$_" }
						  grep { /\.org$/ and ! /^\./ and ! /^#/ }
						  readdir(WORKSPACE));
		close WORKSPACE;
		push @orgfiles, @user_files;
	}
	return @orgfiles;
}

sub is_expired {
	my $self = shift;
	# let's just take a look at the files, shall we?

	# if any of the orgfiles on disk have a modification time later than the scan_time, we are in an expired condition.

	my %modtimes = map { $_ => (stat($_))[9] } $self->orgfiles;

	my $toreturn = 0;
	for my $changed_file (grep { $self->scan_time < $modtimes{$_} } keys %modtimes) {
		$self->oa->verbose("*** org file has changed since the last scan_time: %s", $changed_file);
		$toreturn = 1;
	}
#	$self->oa->verbose("*** all known org files (%s) are younger than the last scan_time (%s)", join (",", $self->orgfiles),
#					   scalar(localtime($self->scan_time))) if $toreturn == 0;
	
	return $toreturn;
}

sub BUILD {
	my $self = shift;
	$self->reload_fromfile;
}

sub rebuild_as_needed {
	my $self = shift;
	if (not $self->is_loaded
		or
		$self->is_expired) {
		$self->contents({});
		$self->build;
	}
	else {
		$self->is_usable(1);
	}
}

sub build_cache {
	my $self = shift;
	$self->oa->verbose("**** Building Org Cache...");

	my $objcount=0;

	foreach my $orgfile ($self->orgfiles) {
		$self->oa->verbose("**** parsing org file %s", $orgfile);

		# all of these are the relevant IDs
		my $_workspace;
		my $_project = "ORPHAN";
		my $_section;
		my $_task;
		my $_subtask;
		my $_story;
		my $_tag;
		my $_user;

		my $orgp = Org::Parser->new();
		my $doc = $orgp->parse_file($orgfile);

		$_user      = $doc->properties->{"asana_user_id"};
		$_workspace = $doc->properties->{"asana_ID"};

		use YAML;
		use WWW::Asana::Workspace;
		use WWW::Asana::Project;
		use WWW::Asana::Task;
		use WWW::Asana::User;
		use WWW::Asana::Story;
		$self->oa->verbose("document properties: %s", $_) for (split /\n/, Dump($doc->properties));

		my $workspace = WWW::Asana::Workspace->new(name => $doc->properties->{"asana_workspace_name"},
												   id   => $_workspace);
		$self->learn(workspace => $_workspace => $workspace);

		my $user = WWW::Asana::User->new(name  => $doc->properties->{"asana_name"},
										 email => $doc->properties->{"asana_email"},
										 id    => $doc->properties->{"asana_user_id"});
		$self->learn(user => $_user => $user);

		$doc->walk(sub {
			my ($el) = @_;
			return unless $el->isa('Org::Element::Headline');
			return unless $el->tags and grep { ($_ eq "workspace" or
												$_ eq "project" or
												$_ eq "section" or
												$_ eq "task" or
												$_ eq "subtask" or
												$_ eq "story") } @{$el->tags};

			$objcount++;

			$_project   = $el->get_property("asana_ID") if (grep { $_ eq "project"   } @{$el->tags});
			$_section   = $el->get_property("asana_ID") if (grep { $_ eq "section"   } @{$el->tags});
			$_task      = $el->get_property("asana_ID") if (grep { $_ eq "task"      } @{$el->tags});
			$_subtask   = $el->get_property("asana_ID") if (grep { $_ eq "subtask"   } @{$el->tags});
			$_story     = $el->get_property("asana_ID") if (grep { $_ eq "story"     } @{$el->tags});
			$_tag       = $el->get_property("asana_ID") if (grep { $_ eq "tag"       } @{$el->tags});

			# we kind of do a NewFromResponse, only it's a NewFromOrg

			# XXX: the org cache is the only place to maintain the two-timestamp log, and the hashes, for both org and asana versions.
			# XXX: what if we want to have headings in org that are not meaningful to asana?

			if (grep {$_ eq "project"} @{$el->tags}) {
				(my $notes = $el->children_as_string) =~ s/^:PROPERTIES:.*?:END:\n+//s;
				$self->oa->verbose("creating new WWW::Asana::Project");
				my $project = WWW::Asana::Project->new(
					name => $el->title->as_string,
					notes => $notes,
					(map { $el->get_property("asana_".uc$_)?($_=>map{ DateTime::Format::ISO8601->parse_datetime($_)} $el->get_property("asana_".uc$_)):()} qw(created_at modified_at)),
					(map { $el->get_property("asana_".uc$_)?($_ => $el->get_property("asana_" . uc $_)) : () } qw(id archived)),
					);

					$self->learn(workspace => $_workspace => project => $_project => $project);
			}
			# when parsing, we shall treat section (priority) headings as tasks.
			if (grep {$_ eq "task" or $_ eq "section" or $_ eq "subtask"} @{$el->tags}) {

				(my $notes = $el->children_as_string) =~ s/^:PROPERTIES:.*?:END:\n+//s; # wonder if this will accidentally capture a subtask or a story.

				$self->oa->verbose("creating new Org::Asana::Task");
				my $task = Org::Asana::Task->new(
					name     => $el->title->as_string,
					notes => $notes,
					workspace => $workspace,
					(map { $el->get_property("asana_" . uc $_) ? ($_ => $el->get_property("asana_" . uc $_)) : () } qw(id assignee_status completed )),
					(map { $el->get_property("asana_".uc$_)?($_=>[map{ WWW::Asana::User   ->new(id=>$_)} split ' ', $el->get_property("asana_".uc$_)]):()} qw(followers)),
					(map { $el->get_property("asana_".uc$_)?($_=>[map{ WWW::Asana::Project->new(id=>$_)} split ' ', $el->get_property("asana_".uc$_)]):()} qw(projects)),
					(map { $el->get_property("asana_".uc$_)?($_=>[map{ WWW::Asana::Task   ->new(id=>$_)} split ' ', $el->get_property("asana_".uc$_)]):()} qw(subtasks)),
					(map { $el->get_property("asana_".uc$_)?($_=> map{ WWW::Asana::User   ->new(id=>$_)} $el->get_property("asana_".uc$_)):()} qw(assignee)),
					(map { $el->get_property("asana_".uc$_)?($_=> map{ WWW::Asana::Task   ->new(id=>$_)} $el->get_property("asana_".uc$_)):()} qw(parent)),
					(map { $el->get_property("asana_".uc$_)?($_=> map{ DateTime::Format::ISO8601->parse_datetime($_)} $el->get_property("asana_".uc$_)):()} qw(created_at completed_at modified_at due_on)),
					$el->get_drawer("PROPERTIES") ? (properties => $el->get_drawer("PROPERTIES")->properties ) : (),
					);

				$self->learn(workspace => $_workspace => task => $task->id => $task);
			}
			if (grep {$_ eq "story"} @{$el->tags}) {
				my $target = $el->get_property("asana_TARGET");
				my ($target_type) = $target =~ s/(\D)//;
				my $target_obj = "WWW::Asana::$target_type"->new(id=>$target);
				my $story = WWW::Asana::Story->new(
					text => $el->title->as_string,
					map { $el->get_property("asana_" . uc $_) ? ($_ => $el->get_property("asana_" . uc $_)) : () }
					qw(id type source created_by created_at),
					target => $target_obj,
					);
				$self->learn(workspace => $_workspace => task => $target => story => $story->id => $story);
			}
				   });
	}

	$self->oa->verbose("**** Org Cache Build complete. Read %d objects", $objcount);
}

1;
# Local Variables:
# eval: (rename-buffer "Cache::Org")
# End:
