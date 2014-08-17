package Org::Asana::Cache::Asana;

use Moose;

has runpidfilename => (is=>'ro',isa=>'Str',default=>"/tmp/build-cache-asana.pid");
has cachefilename => (is=>'rw', isa=>'Str', lazy=>1, default=>sub{shift->oa->dir . "/cache/asana.yaml"});
has fast_rebuild_every => (is=>'rw', isa=>'Num',default=>1800);
has slow_rebuild_every => (is=>'rw', isa=>'Num',default=>86400);

has fast_or_slow => (is=>'rw', isa=>'Str',default=>"fast");

with 'Org::Asana::Cache';
use WWW::Asana;
use feature qw(say);
use DateTime;
use Tie::IxHash;

sub BUILD {
	my $self = shift;
	$self->reload_fromfile;
}

sub rebuild_as_needed {
	my $self=shift;
	if (not $self->is_loaded
		or
		($self->scan_time + $self->slow_rebuild_every < time)) {
		$self->oa->verbose("*** %s needs a slow rebuild (%d+%d < %d; scan_time=%s)",
						   ref($self), $self->scan_time, $self->slow_rebuild_every, time, scalar(localtime($self->scan_time)));
		$self->fast_or_slow("slow");
		$self->build;
	}
	elsif ($self->scan_time + $self->fast_rebuild_every < time) {
		$self->oa->verbose("*** %s could do with a fast rebuild (%d+%d < %d; scan_time=%s)",
						   ref($self), $self->scan_time, $self->fast_rebuild_every, time, scalar(localtime($self->scan_time)));
		$self->fast_or_slow("fast");
		$self->build;
	}
	else {
		my $next_rebuild = (($self->scan_time + $self->slow_rebuild_every < $self->scan_time + $self->fast_rebuild_every)
							? "slow" : "fast");
		my $next_rebuild_time = (( $self->scan_time + $self->slow_rebuild_every < $self->scan_time + $self->fast_rebuild_every)
								 ? $self->scan_time + $self->slow_rebuild_every
								 : $self->scan_time + $self->fast_rebuild_every);
		$self->oa->verbose("*** %s declines a cache rebuild at this time. the next rebuild will be a %s one in %d seconds",
						   ref($self), $next_rebuild, $next_rebuild_time-time);
		$self->is_usable(1);
	}
}

sub build_cache {
	my $self = shift;
	my $last_save = $self->contents;
	$self->contents({});
	eval { $self->load_core };
	if ($@) {
		$self->oa->verbose("we seem to have died. :-(  %s", $@);
		die;
	}

	$self->attach_peripherals($last_save);

	if    ($self->fast_or_slow eq "fast") { $self->reload_peripheral(since => DateTime->from_epoch(epoch => $self->scan_time)); }
	elsif ($self->fast_or_slow eq "slow") { $self->reload_peripheral(since => DateTime->from_epoch(epoch => 0)); }

	# strip superfluous Client and Response objects
	$self->walk(sub { my $self = shift; my $obj = shift;
					  $self->oa->verbose("trying to strip Client and Response from %s", $obj);
					  if ($obj->can("has_response") and $obj->has_response) { $obj->clear_response; $self->oa->verbose("clearing Response") if 0 }
					  if ($obj->can("has_client")   and $obj->has_client)   { $obj->clear_client;   $self->oa->verbose("clearing Client")   if 0 }
					  if ($obj->can("created_by")   and $obj->has_created_by) { $obj->created_by->clear_client }
				});
}

# load the Users, Workspaces, Projects, and Tasks, but not the Stories and other frilly peripheral bits that are slow to retrieve
sub load_core {
	my $self = shift;

	my $asana = $self->oa->asana_www;
	my $me    = $self->oa->asana_me;

	my @asana_users = @{$asana->users};

	for my $user (@asana_users) {
		$user->reload; # need to retrieve each user individually to get workspaces.
		$self->learn(user => $user->id => $user);
		$self->oa->verbose("learning user %s (%d); workspaces = %s",
						   $user->name, $user->id, join (",", map { $_->name } @{$user->workspaces||[]}));
	}

	foreach my $workspace ( @{$me->workspaces} ) {
		next if ($self->oa->match_workspaces and $workspace->name !~ $self->oa->match_workspaces);

		$self->learn(workspace => $workspace->id => $workspace);
		$self->oa->verbose("learning workspace %s (%d)", $workspace->name, $workspace->id);

		foreach my $tag ( @{$workspace->tags} ) {
			$self->learn(workspace => $workspace->id => tag => $tag->id => $tag);
			$self->oa->verbose("learning tag %s", $tag->name);
		}
		foreach my $project ( @{$workspace->projects} ) {
			$self->learn(workspace => $workspace->id => project => $project->id => $project);
			$self->oa->verbose("learning project %s", $project->name);

			foreach my $task ( @{$project->tasks()} ) { # unassigned tasks don't show up in the workspace->tasks below, so we have to go by project.
				$self->oa->verbose("learning project task %s", $task->name);
				$self->learn(workspace => $workspace->id => task => $task->id => $task);
			}

		}
		foreach my $user ( @asana_users ) {
			next if not grep ($_->id == $workspace->id, @{$user->workspaces});
			$self->oa->verbose("in %s, what tasks are assigned to user %s?", $workspace->name, $user->name);

			foreach my $task ( @{$workspace->tasks($user)} ) {
				$self->oa->verbose("learning workspace task %s", $task->name);
				# this inner loop is where things can get super slow, so let's not do any per-Task retrievals if we can help it.
				# those go into the reload_peripheral bit.

				$self->learn(workspace => $workspace->id => task => $task->id => $task);
			}
		}
	}
	$self->oa->verbose("load_core complete! returning.");
}

# attach stories from the last_save into the newly loaded core.
sub attach_peripherals {
	my $self = shift;
	my $old_core = shift;
	my $new_core = $self->contents;

	foreach my $path (grep { m(/story/\d+) } keys %$old_core) {
		$new_core->{$path} = $old_core->{$path};
	}
}

sub reload_peripheral {
	my $self = shift;
	$self->oa->verbose("reload_peripheral (@_)");
	my %opts = @_;
	my @paths = $self->elements_changed_since($opts{since}||0);
	$self->oa->verbose("reload_peripheral will reload objects %s", join (",",@paths));
	foreach my $path (@paths) { # the object may have come from YAML, so we need to give it a client
		my $obj = $self->contents->{$path};
		if ($obj->has_client) { $self->oa->verbose("object already has client, no need to reassign.") if 0 }
		else {                  $self->oa->verbose("reassigning client to object.");
								$obj->client($self->oa->asana_www);
		}
		if ($obj->can("stories")) { # how else am i supposed to check if a class composes a role? what we really want is to check if it's HasStories.
			$self->oa->verbose("reload_peripheral $path loading stories for %d %s", $obj->id, $obj->name);
			$obj->clear_stories;
			eval {
				foreach my $story (@{$obj->stories}) {
					$self->oa->verbose("reload_peripheral $path learning story %s", $story->text);
					$self->learn($path => story => $story->id => $story);
				}
			};
			if ($@) {
				$self->oa->verbose("reload_peripheral: trapped error while relearning $path. maybe the task has been deleted.");
			}
		}
	}
	$self->oa->verbose("reload_peripheral complete!");
}

sub elements_changed_since {
	my $self = shift;
	my $since = shift;

	my @paths;

	$self->walk(
		sub { # callback
			my ($self, $obj, $path) = (shift, shift, shift);
			if ($obj->can("modified_at")
				and
				$obj->modified_at > $since) {
				$self->oa->verbose("elements_changed_since_last_scan: %s %d (%s) was modified_at=%s",
								   ref($obj), $obj->id, $obj->name, $obj->modified_at);
				push @paths, $path;
			}
		});
	return @paths;
}



1;
# Local Variables:
# eval: (rename-buffer "Cache::Asana")
# End:
