package Org::Asana::Cache::Merge::Element;

# contains org and asana versions of the same element
# knows how to resolve conflicts
# knows how to output the element to org
# knows how to output the element to asana

use Moose;
has preferred => (is=>'rw', lazy_build => 1);
has asana => (is=>'rw');
has org   => (is=>'rw');
has resolution => (is=>'rw', isa=>'Str');
has preferred => (is=>'rw');
has oa    => (is=>'ro', required=>1);
has path  => (is=>'rw');

has ref => (is=>'rw', lazy_build => 1);

sub _build_ref {
	my $self = shift;
	my $obj_preferred = $self->preferred;
	my $obj_asana = $self->asana;
	my $obj_org   = $self->org;
	return ($obj_preferred ? ref($obj_preferred) :
			$obj_asana     ? ref($obj_asana) :
			                 ref($obj_org));
}

has name => (is=>'rw', lazy_build=>1);

sub _build_name {
	my $self = shift;
	my $obj_preferred = $self->preferred;
	my $obj_asana = $self->asana;
	my $obj_org   = $self->org;
	return ($obj_asana && $obj_asana->can("name") ? $obj_asana->name :
			$obj_asana && $obj_asana->can("text") ? $obj_asana->text :
			$obj_org   && $obj_org  ->can("name") ? $obj_org  ->name :
			$obj_org   && $obj_org  ->can("text") ? $obj_org  ->text :
			$self->path);
}

# XXX: refactor this, so that instead of C:M manually resolve-> ing everything, just have resolution be a lazy_build, and turn the ->resolve into _build_resolution
sub resolve {
	my $self = shift;
	my $obj_asana = $self->asana;
	my $obj_org   = $self->org;

	# choose a preferred version.
	# XXX: add the two-timestamp logic in here.

	if    ($obj_asana and not $obj_org)   {
		$self->oa->verbose("change: %s asana exists; org doesn't exist. %s",			$self->path, $self->name);
		$self->resolution("create in org"); $self->preferred($self->asana);
	}

	elsif ($obj_org   and not $obj_asana) {
		$self->oa->verbose("change: %s org exists; asana doesn't exist. %s",			$self->path, $self->name);
		if ($obj_org->can("confirm_create_asana") and
			$obj_org->confirm_create_asana
			) {
			$self->resolution("create in asana"); $self->preferred($self->org);
			
		} else {
			$self->resolution("request confirmation in org"); $self->preferred($self->org);
		}
	}

	elsif (not $obj_asana->can("modified_at") or not $obj_org->can("modified_at")) { 
		$self->oa->verbose("change: %s no modtime to compare. %s",                      $self->path, $self->name);
		$self->resolution("noop"); $self->preferred($self->org);
	}

	# XXX: detect conflicts.

	elsif (($obj_asana->modified_at||$obj_asana->created_at) >  ($obj_org->modified_at||$obj_org->created_at)) {
		$self->oa->verbose("change: %s asana is newer. %s", 							$self->path, $self->name);
		$self->resolution("update org"); $self->preferred($self->asana);
	}

	elsif (($obj_asana->modified_at||$obj_asana->created_at) <  ($obj_org->modified_at||$obj_org->created_at)) {
		$self->oa->verbose("change: %s org is newer. %s",								$self->path, $self->name);
		$self->resolution("update asana"); $self->preferred($self->org);
	}

	elsif (($obj_asana->modified_at||$obj_asana->created_at) == ($obj_org->modified_at||$obj_org->created_at)) {
		$self->oa->verbose("change: %s no change. %s",									$self->path, $self->name);
		$self->resolution("noop"); $self->preferred($self->org);

	}
	else { $self->oa->verbose("change: ERROR -- how did we get here?") }
}




my %stars = (workspace => "*",
			 user      => "**",
			 project   => "***",
			 task      => "****",
	);

sub for_org {
	my $self = shift;
	my $prefix = shift;

	my $preferred = $self->preferred;

	my @outtxt;
	my ($lcref) = lc($self->ref) =~ /::(\w+)$/;

	if ($lcref eq "task" and $preferred->name =~ /:$/) {
		$lcref = "section";
	}

	my @org_tags = $lcref;

	if ($lcref eq "task" and 
		$preferred->can("assignee") and
		not $preferred->has_assignee) { push @org_tags, "unassigned" }

	$prefix = $stars{$lcref} if not $prefix;

	# XXX: rearrange the paths so that subtasks fall under the parent path
	if ($preferred->can("parent") and $preferred->has_parent) { $prefix = "*$prefix" }

	my @headline = $prefix;
	if ($lcref eq "task" and
		$self->preferred->can("completed") and
		not grep ($_ eq "unassigned", @org_tags)
		) { push @headline, ($self->preferred->completed ? "DONE" : "TODO") }
	push @headline, $self->name;

	my $org_tags = join ":", @org_tags;

	push @outtxt, join(" ", @headline) . "\t\t\t\t\t\t:$org_tags:";

# line 120 "created_at"
	$self->oa->verbose("$lcref: doing created_at");

	my %properties;
	$properties{asana_ID} = $self->preferred->id;
	$properties{asana_CREATED_AT} = $self->preferred->created_at."" if $self->preferred->has_created_at;

# line 127 "modified_at"
	$self->oa->verbose("$lcref: doing modified_at");

	$properties{asana_MODIFIED_AT} = $self->preferred->modified_at."" if $self->preferred->has_modified_at;

# line 132 "other_properties"

	$self->oa->verbose("$lcref: doing other_properties");

	push @outtxt, format_properties({ %properties, $self->other_properties });

# line 138 "notes"

	$self->oa->verbose("$lcref: doing notes");

	if ($self->preferred->can("notes")) {
		push @outtxt, $self->preferred->notes; # XXX; escape any * or - found in notes
	}

# line 146 stories

	if ($self->preferred->can("stories") and $self->preferred->has_stories) {
		$self->oa->verbose("$lcref: doing stories");
		push @outtxt, map { $_->for_org("*$prefix") } @{$self->preferred->stories};
	}


	return @outtxt;
}

sub other_properties {
	my $self = shift;
	my %properties;
	my $preferred = $self->preferred or return;
	
	$properties{asana_COMPLETED} = $preferred->completed . "" if $preferred->can("completed");
	$properties{asana_PARENT}    = $preferred->parent->id     if $preferred->can("parent") && $preferred->has_parent;
	$properties{asana_ASSIGNEE}  = $preferred->assignee->id if $preferred->can("assignee") && $preferred->has_assignee;
	$properties{asana_ASSIGNEE_NAME}  = $preferred->assignee->name if $preferred->can("assignee") && $preferred->has_assignee;
	$properties{asana_ASSIGNEE_STATUS} = $preferred->assignee_status if $preferred->can("assignee_status");
	$properties{asana_DUE_ON} = $preferred->due_on_value if $preferred->can("due_on_value") and $preferred->has_due_on;
	$properties{asana_PROJECTS} = join " ", map { $_->id } @{ $preferred->projects } if ($preferred->can("projects") and @{ $preferred->projects });
	$properties{asana_URL} = sprintf ("https://app.asana.com/0/%s/%s", $self->oa->asana_me->id, $preferred->id); # XXX: don't know how well this will work.
	return %properties;
}

sub format_properties {
	my $properties = shift;
	my @outtxt;
	push @outtxt, ":PROPERTIES:";
	push @outtxt, ":$_: $properties->{$_}" for keys %$properties;
	push @outtxt, ":END:";
	return @outtxt;
}







sub to_asana {
	my $self = shift;
	$self->oa->verbose("updating asana: %s", $self->path);
}

1;
# Local Variables:
# eval: (rename-buffer "Merge::Element")
# End:
