package Org::Asana::Task;
use Moose;
extends 'WWW::Asana::Task';
with 'Org::Asana::HasProperties';

sub confirm_create_asana {
	my $self = shift;
	if (@_) { $self->properties->{confirm_create_asana} = shift }
	else { return $self->properties->{confirm_create_asana} }
}

1;
