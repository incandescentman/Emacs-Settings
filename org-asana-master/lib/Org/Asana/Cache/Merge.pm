package Org::Asana::Cache::Merge;

use Moose;

has cachefilename => (is=>'rw', isa=>'Str', lazy=>1, default=>sub{shift->oa->dir . "/cache/merge.yaml"});

with 'Org::Asana::Cache';

use feature qw(say);
use Tie::IxHash;
use Org::Asana::Cache::Merge::Element;
use DateTime;

sub BUILD {
	my $self = shift;
	$self->reload_fromfile;
	$self->build_cache;
}

sub build_cache {
	my $self = shift;

	my %contents; tie(%contents, "Tie::IxHash"); $self->contents(\%contents);
	$self->walk_asana;
	$self->walk_org;
	$self->resolve;
}

sub walk_asana {
	my $self = shift;
	$self->oa->cache_asana->walk( sub {
		my ($ca, $obj, $path) = (shift, shift, shift);
		$self->oa->verbose("walk_asana: %s %s %s", $path, ref($obj), $obj->can("name") ? $obj->name : $obj->can("text") ? $obj->text : "NO-NAME");
		if ($self->retrieve($path)) { $self->retrieve($path)->asana($obj); }
		else { $self->learn($path => Org::Asana::Cache::Merge::Element->new(oa=>$self->oa, path=>$path, asana => $obj)); }
								  });
}

sub walk_org {
	my $self = shift;
	$self->oa->cache_org->walk( sub {
		my ($co, $obj, $path) = (shift, shift, shift);
		$self->oa->verbose("walk_org: %s %s %s", $path, ref($obj), $obj->can("name") ? $obj->name : $obj->can("text") ? $obj->text : "NO-NAME");
		if ($self->retrieve($path)) { $self->retrieve($path)->org($obj); }
		else { $self->learn($path => Org::Asana::Cache::Merge::Element->new(oa=>$self->oa, path=>$path, org => $obj)); }
								  });
}

sub resolve {
	my $self = shift;
	$self->oa->verbose("resolving merge");
	$self->walk(sub { my ($cm, $obj, $path) = (shift, shift, shift);
					  $obj->resolve(); });
}

sub other_things_to_save {
	my $self = shift;
	return (to_asana_time => $self->to_asana_time,
			to_org_time   => $self->to_org_time,
			output_time   => $self->output_time);
}

sub other_things_to_load {
	my $self=shift;
	my $cache = shift;
	for my $thing (qw(to_asana_time to_org_time output_time)) {
		$self->$thing($cache->{$thing});
	}
}

has output_time => (is=>'rw', isa=>'DateTime', predicate=>"has_output_time");

has to_asana_time => (is=>'rw', isa=>'DateTime');

sub to_asana {
	my $self = shift;
	$self->oa->verbose("pushing changes to asana");
	$self->output_time(DateTime->now);
	$self->to_asana_time(DateTime->now);
	$self->walk(sub { my ($cm, $obj, $path) = (shift, shift, shift);
					  return if $obj->resolution !~ /asana/;
					  $obj->to_asana(); });
}

has to_org_time => (is=>'rw', isa=>'DateTime');

sub to_org {
	my $self = shift;
	$self->oa->verbose("dumping to org");

	# ~/org-asana/workspaces/My Workspace Name/MyUserName.org
	# organize by workspace (directory)
	# then by user          (file)
	# then by project       (org)
	# then by task          (org)
	# then by story         (org)

	# return if $self->org_file_is_being_edited;

	foreach my $workspace ($self->workspaces) {
		my $dir = $self->oa->dir."/workspaces/".$workspace->name;
		-d $dir or make_path($dir); use File::Path qw(make_path);
		my $workspace_id = $workspace->preferred->id;

		$self->oa->verbose("CM: outputting workspace %s", $workspace_id);

		foreach my $user ($self->users()) {
			my @outtxt;
			my %tasks_printed;
			my $user = $user->preferred;

#			$self->oa->verbose("CM: got user %s", $user->id);

			# maybe move this to the reload subroutine.
			my %project2task;
			foreach my $wtask ($self->tasks($workspace_id)) {
#				$self->oa->verbose("CM: considering task %s", $wtask->preferred->id);
				if ($wtask->preferred->can("projects") and not $wtask->preferred->has_projects) { push @{$project2task{"NO PROJECT"}}, $wtask; next }
				for my $project (@{$wtask->preferred->projects || []}) {
#					$self->oa->verbose("CM: considering task %s project %s", $wtask->preferred->id, $project->id);
					if ($wtask->preferred->assignee and $user->id != $wtask->preferred->assignee->id) {
#						$self->oa->verbose("... task %s not assigned to current user %s", $wtask->preferred->id, $user->id);
						next;
					}
					push @{$project2task{$project->id}}, $wtask;
				}
			}

#			$self->oa->verbose("CM: outputting user %s", $user->id);

			foreach my $project ($self->projects($workspace_id)) {

#				$self->oa->verbose("CM: outputting project %s with %d tasks", $project->preferred->id, scalar @{$project2task{$project->preferred->id}||[]});
				
				# the project's ->tasks method goes to the cloud.
				# next if not @{$project->preferred->tasks};

				next if not @{$project2task{$project->preferred->id}||[]};
				push @outtxt, $project->for_org("**");

#				$self->oa->verbose("CM: outputting project %s's tasks", $project->preferred->id);

				foreach my $task (@{$project2task{$project->preferred->id}||[]}) {
					$self->oa->verbose("--- project %s has task %s", $project->preferred->id, $task->preferred->id);
					if ($task->preferred->has_assignee and $task->preferred->assignee->id != $user->id) { next }
					push @outtxt, $task->for_org("***");
				}
			}

			next if not @outtxt;

			open OUTFILE, ">", "$dir/".$user->name.".org";
			print OUTFILE "-*- mode: org; mode: auto-revert -*-\n";
			print OUTFILE "#+TITLE: " . $user->name, "\n";
			print OUTFILE map { "$_\n" } (file_properties(asana_workspace_name => $workspace->name,
														  asana_ID => $workspace_id,
														  asana_user_id => $user->id,
														  asana_email => $user->email,
														  asana_name  => $user->name),
										  @outtxt);
			close OUTFILE;

		}
	}

	$self->output_time(DateTime->now);
	$self->to_org_time(DateTime->now);
}


sub file_properties {
	my %props = @_;
	my @out;
	while (my ($k, $v) = each %props) {
		push @out, join (" ", "#+PROPERTY:", $k => qq("$v"));
	}
	return @out;
}

1;
# Local Variables:
# eval: (rename-buffer "Cache::Merge")
# End:
