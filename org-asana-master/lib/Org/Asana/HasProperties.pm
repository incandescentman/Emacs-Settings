package Org::Asana::HasProperties;
use Moose::Role;
has properties => (is=>'rw', isa=>'HashRef', default=>sub{{}});
1;
