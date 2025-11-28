# nib_analytics_pilot

## NIB analyse pilot
Pilot skript i R for å analysere data fra NIB

## Beskrivelse
Scriptene a) henter data fra NIB (WoS og OpenAlex), b) samkjører data med cris-data og c) kjører utvalgte demoanalyser

## Skjermdumper
(Kommer).

## Kom i gang
R og RStudio være installert for å kunne kjøre piloten. Skriptene vil trolig fungere med positron (et annet R-ide), men dette er ikke testet. Bruker må også ha en konto til postgresql-basen på NIB-serveren. Hvis denne mangler, kan man søke om en konto via www.openscience.no. Forutsetning for en konto er at man jobber ved en norsk forskningsinstitusjon. (Les mer på http://www.openscience.no/nib). Installer piloten ved å opprette et prosjekt som henter ned data og filer fra gitlab (https://gitlab.sikt.no/openscience-no/nib/nib_analytics_pilot/)

## Bruk
Når prosjektet er lastet ned og installert som prosjekt i R, kan det åpnes i RStudio. Det finnes to hoved-script, ett for hver base (WoS og OpenAlex). Disse kan kjøres enkeltvis, enten linje for linje, eller "sourcet" dvs. at hele scriptet kjøres i en bolk. Merk at det må settes en del parameterer i scriptene før de kjøres, som å oppgi brukerid, tidsspenn, og nedslagsfelt i dokumentmengde. Parametersetting er kommentert i scriptene.

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Forfattere og utvikler
Scriptene er utviklet av Lars Wenaas og Jens Aasheim ved Sikt.

## Lisens
CC 4.0

## Prosjekt status
Dette er demo-script som er ment å illustrere at veien fra data i NIB  til analyse på egen pc, ikke nødvendigvis trenger å være uoverkommelig. Scriptene vil bli utvidet i dialog med Bibliometriforum.(Les mer om bibliometriforum på http://www.openscience.no/nib) 
