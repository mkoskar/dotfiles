#! /usr/bin/perl -Tw

# 2005-02-24: Fixed for AD/Exchange 2003 & Unicode characters,
# anders@bsdconsulting.no If you find this script useful, let me know. :-)
#
# 2000/2001: Original version obtained from Andreas Plesner Jacobsen at
# World Online Denmark. Worked for me with Exchange versions prior to Exchange
# 2000.
#
# Use it with mutt by putting in your .muttrc:
# set query_command = "/home/user/bin/mutt-ldap.pl '%s'"
#
# Then you can search for your users by name directly from mutt. Press ^t
# after having typed parts of the name. Remember to edit configuration
# variables below.

use strict;
use Encode qw / encode decode /;
use vars qw { $server $domain $username $password $basedn };

# --- configuration ---
$server = "localhost:1389";
$domain = "mgm";
$username = "";
$password = "";
$basedn = "ou=people";
# --- end configuration ---

#my $search = encode("UTF-8", join(" ", @ARGV));
my $search = join(" ", @ARGV);

if (!$search =~ /[\.\*\w\s]+/) {
    print("Invalid search parameters\n");
    exit 1;
}

use Net::LDAP;

my $ldap = Net::LDAP->new($server) or die "$@";

$ldap->bind("$domain\\$username", password => $password);

my $mesg = $ldap->search(
    base => $basedn,
    filter => "(|(sn=*$search*) (givenname=*$search*) (cn=*$search*) (uid=*$search*) (mail=*$search*))",
    #filter => "(|(cn=*$search*) (rdn=*$search*) (uid=*$search*) (mail=*$search*))",
    attrs => ["mail","cn", "telephonenumber"]
);

$mesg->code && die $mesg->error;

print(scalar($mesg->all_entries), " entries found\n");

foreach my $entry ($mesg->all_entries) {
    my $mail = $entry->get_value("mail");
    if ($mail) {
        #my $name = decode("UTF-8", $entry->get_value("cn"));
        my $name = $entry->get_value("cn");
        my $tel = $entry->get_value("telephonenumber");
        print $mail;
        print "\t";
        print $name;
        print "\t";
        print "mgm LDAP";
        print " - $tel" if $tel;
        print "\n";
    }
}
$ldap->unbind;
