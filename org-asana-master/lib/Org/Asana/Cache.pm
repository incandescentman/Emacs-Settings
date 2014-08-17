package Org::Asana::Cache;


# a Cache object has ->contents
# the contents are a Perl version of both the Org and the Asana data structures.
# the Perl version serves as a translation layer importable and exportable to both Org and Asana.

use Moose::Role;
use YAML qw(LoadFile DumpFile Dump);
use Tie::IxHash;
use Class::Load ':all';

requires 'build_cache';

has oa => (is=>'ro', isa=>'Org::Asana', required=>1);

has is_loaded => (is=>'rw', isa=>'Bool', default=>0);
has is_usable => (is=>'rw', isa=>'Bool', default=>0);

has scan_time => (is=>'rw', isa=>'Num', default=>0);
has scan_start_time => (is=>'rw', isa=>'Num');

sub previous_is_running {
	my $self = shift;

	my $pid = $self->runpidfile_read;

	if ($pid and kill(0,$pid)) {
		$self->oa->verbose("!!! previous cache build is running -- %s contains PID %s", $self->runpidfilename, $pid);
		return 1;
	} elsif ($pid) {
		$self->oa->verbose("!!! previous cache build $pid seems to have died. If that's okay, rm %s", $self->runpidfilename);
		die if $self->oa->sensitive;
	}		
	return 0;
}

sub runpidfile_read {
	my $self = shift;
	return if not -e $self->runpidfilename;
	open FILE, $self->runpidfilename;
	my $pid = <FILE>; chomp $pid;
	close FILE;
	return $pid;
}

sub runpidfile_write {
	my $self = shift;
	open FILE, ">", $self->runpidfilename;
	print FILE $$,"\n";
	close FILE;
	$self->oa->verbose("launched");
}

sub runpidfile_clear {
	my $self = shift;
	unlink ($self->runpidfilename);
}

sub build { # not BUILD. build() actually builds the cache.
	my $self = shift;
	# go launch the build process

	if ($self->previous_is_running) {
		$self->oa->verbose("*** not launching build of (%s) because previous is still running.", ref($self));
		return;
	}
	else { # maybe an earlier build completed?
		$self->oa->verbose("*** forking build of %s", ref($self));
	}		

	if    (fork()) {                          wait; }
	elsif (fork()) {                          exit; }
	else           { $self->oa->verbosity_prefix(sprintf "-child- %s %s: ", ref($self), $$);
					 $self->runpidfile_write;
					 $self->scan_start_time(time);
					 $self->build_cache;
					 $self->scan_time($self->scan_start_time);
					 $self->save_contents_tofile;
					 $self->runpidfile_clear;
					 $self->oa->verbose("build complete. exiting. build took %d seconds.", time - $self->scan_start_time);
					 exit; }
}

requires 'cachefilename';

has contents => (is=>'rw', isa=>'HashRef', default=>sub {{}});

# contents contains:
# - contents: { }
# - scan_time: time()
# - part_or_full: part|full (optional)

sub reload_fromfile {
	my $self = shift;

	if (not -e $self->cachefilename) { $self->is_loaded(0); return; }

	my $cache;
	eval { $cache = LoadFile($self->cachefilename); }; if ($@) { die "!!! error $@ while loading " . $self->cachefilename; }

	if (not defined $cache->{contents}) { $self->oa->verbose("*** loaded %s cache, but it's empty.", ref($self)); return; }
	$self->oa->verbose("*** successfully reloaded cache for %s", ref($self));
	$self->is_loaded(1);
	$self->contents($cache->{contents});
	$self->scan_time($cache->{scan_time});
	$self->other_things_to_load($cache);
}

# latest modified_at time
has modified_at => (is=>'rw', isa=>'Maybe[DateTime]', lazy_build=>1);
sub _build_modified_at {
	my $self = shift;
	my $ts;
	$self->walk(sub {
		my ($cache, $obj, $path) = (shift, shift, shift);
		$obj = $obj->preferred if $obj->can("preferred");
		if ($obj->can("modified_at") and $obj->has_modified_at and (not defined($ts) or $obj->modified_at > $ts)) { $ts = $obj->modified_at }
				});
	return if not $ts;
	$self->oa->verbose("youngest object in %s is %d seconds old", $self, time - $ts->epoch);
	return $ts;
}

sub other_things_to_load { }
sub other_things_to_save { }

sub save_contents_tofile {
	my $self = shift;
	use File::Temp qw(tempfile);
	use File::Basename;
	use File::Path qw(make_path);
	my ($fh, $filename) = tempfile();
	make_path(dirname($self->cachefilename));
	$self->oa->verbose("**** dumping %s to %s via tempfile %s", ref($self), $self->cachefilename, $filename);
	DumpFile($filename, { contents => $self->contents,
						  scan_time => $self->scan_time,
						  $self->other_things_to_save,
			 });
	rename($filename, $self->cachefilename); # atomic rename.
}


sub learn {
	my $self = shift;
	# $self->learn("p","a","t","h" => $object);
	my $object = pop;
	my $path = join "/", @_;
#	$self->oa->verbose("learn(%s) = %s", $path, $object->can("name") ? $object->name : $object->text);
	$self->contents->{$path} = $object;
}

sub retrieve {
	my $self = shift;
	my $path = shift;
#	$self->oa->verbose("retrieve(%s): %s", $path, $self->contents->{path});
	if (not $path) {
		return ();
	}
	return $self->contents->{$path};
}

sub walk {
	my $self = shift;
	my $callback = shift;
	my $contents = $self->contents;

	foreach my $path (keys %$contents) {

#	$self->oa->verbose("walk_contents - $path");
#	$self->oa->verbose("              - $_ = $contents->{$_}") for keys %$contents;

		my $obj = $contents->{$path};
		if (ref($obj) =~ /::/) { load_class(ref($obj)) unless is_class_loaded(ref($obj)); }
#   $self->oa->verbose("walk_contents >> $path >> callbacking object $obj");
		$callback->($self, $obj, $path);

	}
}

sub workspaces {
	my $self = shift;
	return (map  { $self->retrieve($self->tree->{workspace}->{$_}->{path}) }
			keys %{$self->tree->{workspace}} );
}

sub tasks {
	my $self = shift;
	my $workspace_id = shift;
	warn "keys are @{[keys %{$self->tree->{workspace}->{$workspace_id}->{task}}]}\n";
	warn "values are @{[values %{$self->tree->{workspace}->{$workspace_id}->{task}}]}\n";
	warn "value keys are @{[map { keys %$_ } values %{$self->tree->{workspace}->{$workspace_id}->{task}}]}\n";
	warn "paths are @{[map { $self->tree->{workspace}->{$workspace_id}->{task}->{$_}->{path} } keys %{$self->tree->{workspace}->{$workspace_id}->{task}}]}\n";
	return (map  { $self->retrieve($self->tree->{workspace}->{$workspace_id}->{task}->{$_}->{path}) }
			keys %{$self->tree->{workspace}->{$workspace_id}->{task}} );
}

sub projects {
	my $self = shift;
	my $workspace_id = shift;
	return (map  { $self->retrieve($self->tree->{workspace}->{$workspace_id}->{project}->{$_}->{path}) }
			keys %{$self->tree->{workspace}->{$workspace_id}->{project}} );
}

sub tags {
	my $self = shift;
	my $workspace_id = shift;
	return (map  { $self->retrieve($self->tree->{workspace}->{$workspace_id}->{tag}->{$_}->{path}) }
			keys %{$self->tree->{workspace}->{$workspace_id}->{tag}} );
}

sub users {
	my $self = shift;
	return (map  { $self->retrieve($self->tree->{user}->{$_}->{path}) }
			keys %{$self->tree->{user}} );
}


# semantics for path syntax
has tree => (is=>'rw', lazy_build=>1);
sub _build_tree {
	my $self = shift;
	my $tree = {};
	while (my ($path, $obj) = each %{$self->contents}) {
		my @path = split m(/), $path;
		my $node = $tree;
		while (@path) {
			my ($level, $id) = (shift(@path), shift(@path));
			$node = $node->{$level}->{$id} ||= {};
			next if @path;
			$self->oa->verbose("build_tree: %s %s", $level, $path);
			$node->{path} = $path;
		}

		# XXX: relocate subtasks under their parents.

		# $tree->{workspace}->{NNN}                  ->{path} = "workspace/NNN"
		# $tree->{workspace}->{NNN}->{project}->{NNN}->{path} = "workspace/NNN/project/NNN"
		# $tree->{workspace}->{NNN}->{task}   ->{NNN}->{path} = "workspace/NNN/task/NNN"
		# etc with story
	}
#	print STDERR Dump($tree);


	# XXX: rearrange the paths so that subtasks fall under the parent path

	return $tree;
}


1;
# Local Variables:
# eval: (rename-buffer "Asana::Cache")
# End:
