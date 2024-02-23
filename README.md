# NI KONČANO
## TO DO
- error handling vhodnih podatkov
- boljša vizualizacija traku/gui
- graf tabele
- izboljšana interakcija uporabnika (ustvarjanje strojev...)
- definicija turingovega stroja
- več raznolikih(več trakov, nederminističen primer) primerov/testiranje stroja
- dokumentacija
- ...
# Uporaba
V mapi `/data` ustvariš novo mapo z dvema datotekama `tape` in `table`. V datoteki `tape` vpišemo začetne vrednosti traku. Sintaksa za vsak trak je čez dve vrstici, v prvi je indeks(začnejo se z 0), ki označuje kje je začetna pozicija glave. V drugi pa s presledkom ločene vrednosti celic. Če želimo dodati več trakov sledimo enaki sintaksi v sledečih vrsticah. 
```
1
0 1 0 0 0
2
1 0 1 0 0
```
V tem primeru ustvarimo dva trakov, kjer je pri prvem glava na drugi celici in drugem na tretji celici.

Za datoteko `table`, kjer vpišemo pravila za tranzicije v vsako vrstico vpišemo eno pravilo v sledečem vrstnem redu:
`(stanje) (simbol traka) (simbol za pisanje) (smer premika) (naslednje stanje)`
Če uporabljamo več trakov ločimo navodilo za vsak trak (simbol traka/simbol za pisanje/smer premika) z vejico. Za nederministično uporabo pa ločimo navodila z `|`. Za zaključno stanje pa vedno uporabimo `HALT`.
Deterministična enotračna uporaba:
```
A 0 1 R B
A 1 1 R HALT
...
``` 
Deterministična trotračna uporaba:
```
A 0,1,0 1,0,1 R,L,R B
A 0,1,0 1,0,1 R,L,R HALT
...
```
Nederministična enotračna uporaba:
```
A 0 1|0 R|L B
A 1 1 R HALT|C
...
```
Nato poženemo program z ukazom `dune exec turing`. Program nas bo vprašal po imenu stroja, ki ga želimo zagnati. Tu vpišemo ime mape v katero smo shranili navodila. Nato po privzeti vrednosti praznih celic in na koncu še po začetnem stanju. 

https://www.youtube.com/watch?v=gJQTFhkhwPA&ab_channel=EngMicroLectures
https://math.dartmouth.edu/~m29s22/files/406.pdf
https://en.wikipedia.org/wiki/Turing_machine
https://en.wikipedia.org/wiki/Nondeterministic_Turing_machine
https://en.wikipedia.org/wiki/Multitape_Turing_machine
https://en.wikipedia.org/wiki/Multi-track_Turing_machine
https://en.wikipedia.org/wiki/Busy_beaver