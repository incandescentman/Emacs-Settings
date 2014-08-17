package Org::Asana::Sync;

use Moose;
has oa => (is=>'ro', isa=>'Org::Asana', required=>1);



sub asana2org {
	my $self = shift;
	my $cache_asana = $self->oa->cache_asana;

#	$cache_asana->walk_contents(
#				my %properties;
#				$properties{asana_ASSIGNEE}        = $task->assignee->id if $task->assignee;
#				$properties{asana_ASSIGNEE_STATUS} = $task->assignee_status;
#				$properties{asana_CREATED_AT}      = $task->created_at."" if $task->has_created_at;
#				$properties{asana_COMPLETED}       = $task->completed . "";
#				$properties{asana_COMPLETED_AT}    = $task->completed_at."" if $task->has_completed_at;
#				$properties{asana_DUE_ON}          = $task->due_on_value if $task->has_due_on;
#				$properties{asana_FOLLOWERS}       = join " ", map { $_->id } @{ $task->followers } if @{ $task->followers };
#				$properties{asana_MODIFIED_AT}     = $task->modified_at."" if $task->has_modified_at;
#				$properties{asana_PROJECTS}        = join " ", map { $_->id } @{ $task->projects  } if @{ $task->projects };
#				$properties{asana_PARENT}          = $task->parent->id if $task->parent and $task->parent->id;
#				$contents{workspaces}{$workspace->id}->{tasks}{$task->id}->{properties} = \%properties;
#
}

1;
