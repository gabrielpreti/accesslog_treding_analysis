use v5.14;
use warnings;
use strict;

my $inputFilePath = $ARGV[0];

open my $inputFile,   "<", $inputFilePath   or die $!;
while ( my $line = <$inputFile> ) {
	my $resultOk = $line =~ m/^(?<ip>.*)\s(?<port>\d+)\s\-\s.*\[(?<day>\d\d)\/(?<month>\w\w\w)\/(?<year>\d\d\d\d)\:(?<hour>\d\d)\:(?<minute>\d\d)\:(?<second>\d\d)\s.*?\]\s\"(?<method>\w+?)\s\/(?<uri>.*?)\sHTTP.*?\"\s(?<return>\d+?)\s.*?\"(?<referrer>.*?)\"\s\"(?<userAgent>.*?)\"\s\"(?<uid>.*)\"\s(?<time>\d*)$/;
	if(!$resultOk){
		next;
	}
	
	my $ip = $+{ip};
	my $port = $+{port};
	my $day = $+{day};
	my $month = $+{month};
	my $year = $+{year};
	my $hour = $+{hour};
	my $minute = $+{minute};
	my $second = $+{second};
	my $method = $+{method};
	my $return = $+{return};
	my $uri = $+{uri};
	my $referrer = $+{referrer};
	my $userAgent = $+{userAgent};
	my $time = $+{time};
	
	if (   ( $uri =~ m/\/stc.*/ )
				|| ( $uri =~ m/.*ws.acesso.intranet\/cryptologin.*/ )
				|| ( $uri =~ m/.*\.(jpg|gif|png)/ )
				|| ( $uri =~ m/.*hc\.html/ )
				|| ( $uri =~ m/.*server-status*/ )
				|| ( $uri =~ m/.*favicon\.ico/ )
				|| ( $uri =~ m/.*robots.txt/ )
				|| ( $uri =~ m/.*checkout\/metrics\/(info|save)\.jhtml/ )
				|| ( $uri =~ m/.*\;.*/ )
				|| ( $uri =~ m/.*\<\/pre\>.*/ )
				|| ( $uri =~ m/.*\\\".*/ ))
			{
				next;
			}
			
	#Essa uri da problema, como tirar?    "pagseguro.uol.com.br/atendimento/perguntas_frequentes/em-quais-casos-a-disputa-e-encerrada-em-favor-do-vendedor.jhtml?q=modera\xe7ao"
	if ( $return =~ m/30\d/ ) {
				next;
	}
	
	if ( $uri =~ m/(.*?)\?.*/ ) {
		$uri = $1;
	}
	
	if ( $uri =~ m/ws.pagseguro.uol.com.br\/v2\/transactions\/.*/ ) {
		$uri = 'ws.pagseguro.uol.com.br/v2/transactions/';
	}
	
	$uri =~ m/(.*?)\//;
	my $domain = $1;
	
	say "\"$ip\"\t\"$port\"\t\"$day/$month/$year:$hour:$minute:$second\"\t\"$method\"\t\"$uri\"\t\"$domain\"\t\"$return\"\t\"$referrer\"\t\"$userAgent\"\t\"$time\"";
	#print $line;
}