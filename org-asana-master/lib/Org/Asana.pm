package Org::Asana;

# https://github.com/mengwong/org-asana/

use Moose;
use WWW::Asana;
use YAML qw(LoadFile DumpFile);
use Carp;
use File::Path;

has 'verbosity'        => (is=>'rw',isa=>'Num|Str',default=>0);
has 'verbosity_prefix' => (is=>'rw',isa=>'Str',default=>"");
has 'sensitive'        => (is=>'rw',isa=>'Bool',default=>0); # do we die on error? helps with debugging.

sub verbose {
	my $self = shift;
	return if not $self->verbosity;
	no warnings;
	print  STDERR $self->verbosity_prefix;
	printf STDERR @_;
	print  STDERR "\n";
}




sub BUILD {
	my $self = shift;
	$self->initialize_rc;
}





has 'rc'             => (is=>'ro',isa=>'Str',default=>"$ENV{HOME}/.org-asana-rc"); # rcfile in YAML format contains our API key, etc.
has 'dir'            => (is=>'rw',isa=>'Str',default=>"$ENV{HOME}/org-asana");
has 'api_key'        => (is=>'rw',isa=>'Str');

sub initialize_rc {
	my $self = shift;
	if (-e $self->rc) {
		my $rc_config = LoadFile($self->rc);
		for my $config (qw(dir api_key)) { $self->$config($rc_config->{$config}) if $rc_config->{$config}; }
	}
	elsif (-t STDOUT) {
		print "*** .org-asana-rc file not found. Creating.\n";
		print "  - Work dir will default to ~/org-asana/\n";
		print "  - Need Asana API key.\n";
		print "  - Please enter your Asana API key, available at https://app.asana.com/-/account_api\n";
		chomp(my $api_key = <STDIN>);
		if (length($api_key) ne 32) { die "An Asana API key should be 32 characters long.\n"; }
		DumpFile($self->rc, { api_key => $api_key });
		$self->api_key($api_key);
	}
	$self->test_api_key;
}

has 'asana_me' => (is=>'rw', isa=>'WWW::Asana::User');
has 'asana_www' => (is=>'rw', isa=>'WWW::Asana');

sub test_api_key {
	my $self = shift;
	my $asana;
	my $me;
	eval {
		$asana = WWW::Asana->new( api_key => $self->api_key, singleton_instance=>1 );
		$me = $asana->me;
	};
	if ($@ or not $me or not $me->email) {
		die ("Couldn't initialize WWW::Asana. Is your API key correct? (" . $self->api_key .")\n$@\n");
	}
	$self->verbose("  - WWW::Asana works. You are %s (%s)", $me->email, $me->name);
	$self->asana_me($me);
	$self->asana_www($asana);
}





has 'sleep_interval' => (is=>'rw',isa=>'Num',default=>30);
has 'sleep_times'    => (is=>'rw',isa=>'Num',default=>0);

sub sleep {
	my $self = shift;
	$self->verbose("  - sleeping. (%s)", scalar localtime);
	sleep $self->sleep_interval;
	$self->sleep_times($self->sleep_times+1);
}



has 'match_workspaces' => (is=>'rw', isa=>'RegexpRef');



use Org::Asana::Cache::Org;
use Org::Asana::Cache::Asana;
use Org::Asana::Cache::Merge;

sub manage_caches {
	my $self = shift;
	$self->verbose("managing caches.");

	$self->clear_cache_asana; $self->cache_asana->rebuild_as_needed;
	$self->clear_cache_org;   $self->cache_org->rebuild_as_needed;
	$self->clear_cache_merge; $self->cache_merge if ($self->cache_asana->is_usable and $self->cache_org->is_usable);
}

has 'cache_asana' => (is=>'rw', isa=>'Org::Asana::Cache::Asana', lazy_build=>1);
sub _build_cache_asana { Org::Asana::Cache::Asana->new(oa=>shift) }

has 'cache_org' => (is=>'rw', isa=>'Org::Asana::Cache::Org', lazy_build=>1);
sub _build_cache_org { Org::Asana::Cache::Org->new(oa=>shift) }

has 'cache_merge' => (is=>'rw', isa=>'Org::Asana::Cache::Merge', lazy_build=>1);
sub _build_cache_merge { Org::Asana::Cache::Merge->new(oa=>shift) }
# workspace -> project -> task -> story
# workspace -> project -> story
# workspace -> tag


sub changes_detected_since_last_merge {
	my $self = shift;
	if (not $self->cache_merge->has_output_time) { return 1 }
	# have either cache_org or cache_asana changed since our last to_asana_time / to_org_time?
	if ($self->cache_asana->modified_at > $self->cache_merge->output_time
		or
		$self->cache_org->modified_at > $self->cache_merge->output_time) {
		$self->verbose("something has changed since the last merge.");
		return 1;
	} else {
		$self->verbose("nothing has changed since the last merge.");
		return 0;
	}
}

sub sync {
	my $self = shift;
	return if not $self->has_cache_merge;
	$self->verbose("last cache merge output time = %s", $self->cache_merge->output_time);
	return if ($self->cache_merge->output_time and not $self->changes_detected_since_last_merge);
	$self->clear_cache_merge;
	$self->cache_merge->to_asana;
	$self->cache_merge->to_org;
	$self->cache_merge->save_contents_tofile;
}


1;

# Local Variables:
# eval: (rename-buffer "Org::Asana")
# End:

