# nib_analytics_pilot

## NIB analyse pilot
Analyse-piloten består av script i R utformet for å hente og analysere data fra NIB-basen. Se https://www.openscience.no/nib/pilot for en introduksjon.

## Beskrivelse
Scriptene a) henter data fra NIB (WoS og OpenAlex), b) samkjører data med cris-data og c) kjører utvalgte demoanalyser. 
Scriptene har status som 'pilot' og er ment å illustrere at veien fra data i NIB til analyse på egen pc ikke trenger å være uoverkommelig. Det vil likevel kreve en innsats for å tilpasse script og funksjoner til egne behov. 

## Skjermdumper
(Kommer. Se også https://www.openscience.no/nib/pilot for eksempler).

## Kom i gang
R og RStudio må være installert for å kunne kjøre piloten. Scriptene vil trolig fungere med Positron (som er et alternativt R-ide), men dette er ikke testet. Bruker må også ha en konto til postgresql-basen på NIB-serveren. Hvis man ikke har konto kan man søke om tilgang forutsatt at man er tilnyttet en norsk forskningsinstitusjon. (Les mer på http://www.openscience.no/nib). 

Piloten installeres ved å opprette et prosjekt i R som henter ned data og filer fra gitlab (https://gitlab.sikt.no/openscience-no/nib/nib_analytics_pilot/). Filene kan også lastes ned enkeltvis og tilpasses lokalt.

## Bruk
Etter at prosjektet er lastet ned og installert som lokalt prosjekt i R, så skjer alle operasjoner i RStudio. Det finnes to hoved-script, ett for hver base (WoS og OpenAlex). Disse kjøres enkeltvis, enten linje for linje, eller "sourcet" dvs. at hele scriptet kjøres i en bolk. 

Merk at det må settes en del variable i scriptene før de kjøres. Variabler som brukerid, tidsperiode, institusjonskode og nedslagsfelt i dokumentmengde, er nødvendige delvis for å få kontakt med basen og delvis for å tilpasse kjøringene lokale ønsker og behov. 
(Angivelse av variable er kommentert i scriptene).

## Forfattere og utvikler
Scriptene er utviklet av Lars Wenaas og Jens Aasheim ved Sikt.

## Lisens
CC 4.0

## Prosjekt status
 Prosjektet er aktivt per januar 2026. Scriptene vil bli videreutviklet i dialog med Bibliometriforum.
 (Les mer om bibliometriforum på http://www.openscience.no/nib) 
