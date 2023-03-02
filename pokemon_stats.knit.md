---
title: "Pokemon"
author: "John Cruz"
date: "2023-03-01"
output: pdf_document
url_color: blue
---



## R Markdown

National Pokedex

[Pokemon Stats](https://www.serebii.net/pokemon/nationalpokedex.shtml)

[Pokemon Types](https://m.bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number)

A few kinds of analysis that could be done would be:

to look at which types of Pokémon tend to have the highest base stats
to find the 6 Pokémon with the highest base speed
to observe whether Pokémon with a single type have higher base stats than Pokémon with two types
to observe the frequency of Pokémon by type
to find the 10 rarest Pokémon abilities, i.e. the abilities that the fewest number of Pokémon have access to



```r
library(tidyverse)
library(rvest)
library(xml2)
library(janitor)
```


```r
url <- "https://www.serebii.net/pokemon/nationalpokedex.shtml"

web_table <- read_html(url) 

# use XML to account for <br> with abilities and add '\n'
xml_find_all(web_table, ".//br") |> 
  xml_add_sibling("p", "\n")
xml_find_all(web_table, ".//br") |> 
  xml_remove() 

web_table <- 
  web_table |> 
  html_element('.dextable') |> 
  html_table()

pokemon_stats <- as.data.frame(web_table)
```




```r
# drop null values if Pokemon name is N/A
stats_df <- 
  pokemon_stats |> 
  drop_na(4)

# drop first row (duplicate header) and second column (pic)
stats_df <- stats_df[-1,-2]

# set column headers from first row and clean names
stats_df <- 
  stats_df |> 
  row_to_names(row_number = 1) |> 
  clean_names()

# shift pokemon names, etc to left by 1 column
stats_df[c(2:10)] = stats_df[, c(3:11)]

# split multiple abilities into long format based on created '\n'
stats_df <- 
  stats_df |> 
  separate_longer_delim(abilities, delim = "\n")

# change to pokemon number
stats_df$no <-
  parse_number(stats_df$no)
```




```r
url <- "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number"

web_table <- read_html(url) 

# use XML to account for <br> and replace with '\n'
xml_find_all(web_table, ".//br") |> 
  xml_add_sibling("p", "\n")
xml_find_all(web_table, ".//br") |> 
  xml_remove() 

web_table <- 
  web_table |> 
  html_element('body') |> 
  html_table()

pokemon_types <- as.data.frame(web_table)
```




```r
# drop null values if Pokemon name is N/A
types_df <- 
  pokemon_types |> 
  drop_na(2)

# drop unnecessary columns
types_df <- 
  types_df[, 1:5]

# set column headers from first row and clean names
types_df <- 
  types_df |> 
  row_to_names(row_number = 1) |> 
  clean_names()

# change to pokemon number
types_df$ndex <-
  parse_number(types_df$ndex)

# drop N/A or zero (0) while keeping only distinct pokemon numbers
types_df <- 
  types_df |> 
  drop_na() |> 
  filter(ndex != 0) |> 
  distinct(ndex, .keep_all=TRUE)

# within same pokemon number, replace repeated types with N/A
types_df <- 
  types_df |> 
  mutate(type_2 = if_else(type_2 != type, type_2, NA))
```






```r
types_df |> 
  mutate(type_2 = if_else(type_2 != type, type_2, NA))
```

```
##      ndex           ms                         pokemon     type   type_2
## 1       1                                    Bulbasaur    Grass   Poison
## 2       2                                      Ivysaur    Grass   Poison
## 3       3                                     Venusaur    Grass   Poison
## 4       4                                   Charmander     Fire     <NA>
## 5       5                                   Charmeleon     Fire     <NA>
## 6       6                                    Charizard     Fire   Flying
## 7       7                                     Squirtle    Water     <NA>
## 8       8                                    Wartortle    Water     <NA>
## 9       9                                    Blastoise    Water     <NA>
## 10     10                                     Caterpie      Bug     <NA>
## 11     11                                      Metapod      Bug     <NA>
## 12     12                                   Butterfree      Bug   Flying
## 13     13                                       Weedle      Bug   Poison
## 14     14                                       Kakuna      Bug   Poison
## 15     15                                     Beedrill      Bug   Poison
## 16     16                                       Pidgey   Normal   Flying
## 17     17                                    Pidgeotto   Normal   Flying
## 18     18                                      Pidgeot   Normal   Flying
## 19     19                                      Rattata   Normal     <NA>
## 20     20                                     Raticate   Normal     <NA>
## 21     21                                      Spearow   Normal   Flying
## 22     22                                       Fearow   Normal   Flying
## 23     23                                        Ekans   Poison     <NA>
## 24     24                                        Arbok   Poison     <NA>
## 25     25                                      Pikachu Electric     <NA>
## 26     26                                       Raichu Electric     <NA>
## 27     27                                    Sandshrew   Ground     <NA>
## 28     28                                    Sandslash   Ground     <NA>
## 29     29                                     Nidoran♀   Poison     <NA>
## 30     30                                     Nidorina   Poison     <NA>
## 31     31                                    Nidoqueen   Poison   Ground
## 32     32                                     Nidoran♂   Poison     <NA>
## 33     33                                     Nidorino   Poison     <NA>
## 34     34                                     Nidoking   Poison   Ground
## 35     35                                     Clefairy    Fairy     <NA>
## 36     36                                     Clefable    Fairy     <NA>
## 37     37                                       Vulpix     Fire     <NA>
## 38     38                                    Ninetales     Fire     <NA>
## 39     39                                   Jigglypuff   Normal    Fairy
## 40     40                                   Wigglytuff   Normal    Fairy
## 41     41                                        Zubat   Poison   Flying
## 42     42                                       Golbat   Poison   Flying
## 43     43                                       Oddish    Grass   Poison
## 44     44                                        Gloom    Grass   Poison
## 45     45                                    Vileplume    Grass   Poison
## 46     46                                        Paras      Bug    Grass
## 47     47                                     Parasect      Bug    Grass
## 48     48                                      Venonat      Bug   Poison
## 49     49                                     Venomoth      Bug   Poison
## 50     50                                      Diglett   Ground     <NA>
## 51     51                                      Dugtrio   Ground     <NA>
## 52     52                                       Meowth   Normal     <NA>
## 53     53                                      Persian   Normal     <NA>
## 54     54                                      Psyduck    Water     <NA>
## 55     55                                      Golduck    Water     <NA>
## 56     56                                       Mankey Fighting     <NA>
## 57     57                                     Primeape Fighting     <NA>
## 58     58                                    Growlithe     Fire     <NA>
## 59     59                                     Arcanine     Fire     <NA>
## 60     60                                      Poliwag    Water     <NA>
## 61     61                                    Poliwhirl    Water     <NA>
## 62     62                                    Poliwrath    Water Fighting
## 63     63                                         Abra  Psychic     <NA>
## 64     64                                      Kadabra  Psychic     <NA>
## 65     65                                     Alakazam  Psychic     <NA>
## 66     66                                       Machop Fighting     <NA>
## 67     67                                      Machoke Fighting     <NA>
## 68     68                                      Machamp Fighting     <NA>
## 69     69                                   Bellsprout    Grass   Poison
## 70     70                                   Weepinbell    Grass   Poison
## 71     71                                   Victreebel    Grass   Poison
## 72     72                                    Tentacool    Water   Poison
## 73     73                                   Tentacruel    Water   Poison
## 74     74                                      Geodude     Rock   Ground
## 75     75                                     Graveler     Rock   Ground
## 76     76                                        Golem     Rock   Ground
## 77     77                                       Ponyta     Fire     <NA>
## 78     78                                     Rapidash     Fire     <NA>
## 79     79                                     Slowpoke    Water  Psychic
## 80     80                                      Slowbro    Water  Psychic
## 81     81                                    Magnemite Electric    Steel
## 82     82                                     Magneton Electric    Steel
## 83     83                                   Farfetch'd   Normal   Flying
## 84     84                                        Doduo   Normal   Flying
## 85     85                                       Dodrio   Normal   Flying
## 86     86                                         Seel    Water     <NA>
## 87     87                                      Dewgong    Water      Ice
## 88     88                                       Grimer   Poison     <NA>
## 89     89                                          Muk   Poison     <NA>
## 90     90                                     Shellder    Water     <NA>
## 91     91                                     Cloyster    Water      Ice
## 92     92                                       Gastly    Ghost   Poison
## 93     93                                      Haunter    Ghost   Poison
## 94     94                                       Gengar    Ghost   Poison
## 95     95                                         Onix     Rock   Ground
## 96     96                                      Drowzee  Psychic     <NA>
## 97     97                                        Hypno  Psychic     <NA>
## 98     98                                       Krabby    Water     <NA>
## 99     99                                      Kingler    Water     <NA>
## 100   100                                      Voltorb Electric     <NA>
## 101   101                                    Electrode Electric     <NA>
## 102   102                                    Exeggcute    Grass  Psychic
## 103   103                                    Exeggutor    Grass  Psychic
## 104   104                                       Cubone   Ground     <NA>
## 105   105                                      Marowak   Ground     <NA>
## 106   106                                    Hitmonlee Fighting     <NA>
## 107   107                                   Hitmonchan Fighting     <NA>
## 108   108                                    Lickitung   Normal     <NA>
## 109   109                                      Koffing   Poison     <NA>
## 110   110                                      Weezing   Poison     <NA>
## 111   111                                      Rhyhorn   Ground     Rock
## 112   112                                       Rhydon   Ground     Rock
## 113   113                                      Chansey   Normal     <NA>
## 114   114                                      Tangela    Grass     <NA>
## 115   115                                   Kangaskhan   Normal     <NA>
## 116   116                                       Horsea    Water     <NA>
## 117   117                                       Seadra    Water     <NA>
## 118   118                                      Goldeen    Water     <NA>
## 119   119                                      Seaking    Water     <NA>
## 120   120                                       Staryu    Water     <NA>
## 121   121                                      Starmie    Water  Psychic
## 122   122                                     Mr. Mime  Psychic    Fairy
## 123   123                                      Scyther      Bug   Flying
## 124   124                                         Jynx      Ice  Psychic
## 125   125                                   Electabuzz Electric     <NA>
## 126   126                                       Magmar     Fire     <NA>
## 127   127                                       Pinsir      Bug     <NA>
## 128   128                                       Tauros   Normal     <NA>
## 129   129                                     Magikarp    Water     <NA>
## 130   130                                     Gyarados    Water   Flying
## 131   131                                       Lapras    Water      Ice
## 132   132                                        Ditto   Normal     <NA>
## 133   133                                        Eevee   Normal     <NA>
## 134   134                                     Vaporeon    Water     <NA>
## 135   135                                      Jolteon Electric     <NA>
## 136   136                                      Flareon     Fire     <NA>
## 137   137                                      Porygon   Normal     <NA>
## 138   138                                      Omanyte     Rock    Water
## 139   139                                      Omastar     Rock    Water
## 140   140                                       Kabuto     Rock    Water
## 141   141                                     Kabutops     Rock    Water
## 142   142                                   Aerodactyl     Rock   Flying
## 143   143                                      Snorlax   Normal     <NA>
## 144   144                                     Articuno      Ice   Flying
## 145   145                                       Zapdos Electric   Flying
## 146   146                                      Moltres     Fire   Flying
## 147   147                                      Dratini   Dragon     <NA>
## 148   148                                    Dragonair   Dragon     <NA>
## 149   149                                    Dragonite   Dragon   Flying
## 150   150                                       Mewtwo  Psychic     <NA>
## 151   151                                          Mew  Psychic     <NA>
## 152   152                                    Chikorita    Grass     <NA>
## 153   153                                      Bayleef    Grass     <NA>
## 154   154                                     Meganium    Grass     <NA>
## 155   155                                    Cyndaquil     Fire     <NA>
## 156   156                                      Quilava     Fire     <NA>
## 157   157                                   Typhlosion     Fire     <NA>
## 158   158                                     Totodile    Water     <NA>
## 159   159                                     Croconaw    Water     <NA>
## 160   160                                   Feraligatr    Water     <NA>
## 161   161                                      Sentret   Normal     <NA>
## 162   162                                       Furret   Normal     <NA>
## 163   163                                     Hoothoot   Normal   Flying
## 164   164                                      Noctowl   Normal   Flying
## 165   165                                       Ledyba      Bug   Flying
## 166   166                                       Ledian      Bug   Flying
## 167   167                                     Spinarak      Bug   Poison
## 168   168                                      Ariados      Bug   Poison
## 169   169                                       Crobat   Poison   Flying
## 170   170                                     Chinchou    Water Electric
## 171   171                                      Lanturn    Water Electric
## 172   172                                        Pichu Electric     <NA>
## 173   173                                       Cleffa    Fairy     <NA>
## 174   174                                    Igglybuff   Normal    Fairy
## 175   175                                       Togepi    Fairy     <NA>
## 176   176                                      Togetic    Fairy   Flying
## 177   177                                         Natu  Psychic   Flying
## 178   178                                         Xatu  Psychic   Flying
## 179   179                                       Mareep Electric     <NA>
## 180   180                                      Flaaffy Electric     <NA>
## 181   181                                     Ampharos Electric     <NA>
## 182   182                                    Bellossom    Grass     <NA>
## 183   183                                       Marill    Water    Fairy
## 184   184                                    Azumarill    Water    Fairy
## 185   185                                    Sudowoodo     Rock     <NA>
## 186   186                                     Politoed    Water     <NA>
## 187   187                                       Hoppip    Grass   Flying
## 188   188                                     Skiploom    Grass   Flying
## 189   189                                     Jumpluff    Grass   Flying
## 190   190                                        Aipom   Normal     <NA>
## 191   191                                      Sunkern    Grass     <NA>
## 192   192                                     Sunflora    Grass     <NA>
## 193   193                                        Yanma      Bug   Flying
## 194   194                                       Wooper    Water   Ground
## 195   195                                     Quagsire    Water   Ground
## 196   196                                       Espeon  Psychic     <NA>
## 197   197                                      Umbreon     Dark     <NA>
## 198   198                                      Murkrow     Dark   Flying
## 199   199                                     Slowking    Water  Psychic
## 200   200                                   Misdreavus    Ghost     <NA>
## 201   201                              Unown\nOne form  Psychic     <NA>
## 202   202                                    Wobbuffet  Psychic     <NA>
## 203   203                                    Girafarig   Normal  Psychic
## 204   204                                       Pineco      Bug     <NA>
## 205   205                                   Forretress      Bug    Steel
## 206   206                                    Dunsparce   Normal     <NA>
## 207   207                                       Gligar   Ground   Flying
## 208   208                                      Steelix    Steel   Ground
## 209   209                                     Snubbull    Fairy     <NA>
## 210   210                                     Granbull    Fairy     <NA>
## 211   211                                     Qwilfish    Water   Poison
## 212   212                                       Scizor      Bug    Steel
## 213   213                                      Shuckle      Bug     Rock
## 214   214                                    Heracross      Bug Fighting
## 215   215                                      Sneasel     Dark      Ice
## 216   216                                    Teddiursa   Normal     <NA>
## 217   217                                     Ursaring   Normal     <NA>
## 218   218                                       Slugma     Fire     <NA>
## 219   219                                     Magcargo     Fire     Rock
## 220   220                                       Swinub      Ice   Ground
## 221   221                                    Piloswine      Ice   Ground
## 222   222                                      Corsola    Water     Rock
## 223   223                                     Remoraid    Water     <NA>
## 224   224                                    Octillery    Water     <NA>
## 225   225                                     Delibird      Ice   Flying
## 226   226                                      Mantine    Water   Flying
## 227   227                                     Skarmory    Steel   Flying
## 228   228                                     Houndour     Dark     Fire
## 229   229                                     Houndoom     Dark     Fire
## 230   230                                      Kingdra    Water   Dragon
## 231   231                                       Phanpy   Ground     <NA>
## 232   232                                      Donphan   Ground     <NA>
## 233   233                                     Porygon2   Normal     <NA>
## 234   234                                     Stantler   Normal     <NA>
## 235   235                                     Smeargle   Normal     <NA>
## 236   236                                      Tyrogue Fighting     <NA>
## 237   237                                    Hitmontop Fighting     <NA>
## 238   238                                     Smoochum      Ice  Psychic
## 239   239                                       Elekid Electric     <NA>
## 240   240                                        Magby     Fire     <NA>
## 241   241                                      Miltank   Normal     <NA>
## 242   242                                      Blissey   Normal     <NA>
## 243   243                                       Raikou Electric     <NA>
## 244   244                                        Entei     Fire     <NA>
## 245   245                                      Suicune    Water     <NA>
## 246   246                                     Larvitar     Rock   Ground
## 247   247                                      Pupitar     Rock   Ground
## 248   248                                    Tyranitar     Rock     Dark
## 249   249                                        Lugia  Psychic   Flying
## 250   250                                        Ho-Oh     Fire   Flying
## 251   251                                       Celebi  Psychic    Grass
## 252   252                                      Treecko    Grass     <NA>
## 253   253                                      Grovyle    Grass     <NA>
## 254   254                                     Sceptile    Grass     <NA>
## 255   255                                      Torchic     Fire     <NA>
## 256   256                                    Combusken     Fire Fighting
## 257   257                                     Blaziken     Fire Fighting
## 258   258                                       Mudkip    Water     <NA>
## 259   259                                    Marshtomp    Water   Ground
## 260   260                                     Swampert    Water   Ground
## 261   261                                    Poochyena     Dark     <NA>
## 262   262                                    Mightyena     Dark     <NA>
## 263   263                                    Zigzagoon   Normal     <NA>
## 264   264                                      Linoone   Normal     <NA>
## 265   265                                      Wurmple      Bug     <NA>
## 266   266                                      Silcoon      Bug     <NA>
## 267   267                                    Beautifly      Bug   Flying
## 268   268                                      Cascoon      Bug     <NA>
## 269   269                                       Dustox      Bug   Poison
## 270   270                                        Lotad    Water    Grass
## 271   271                                       Lombre    Water    Grass
## 272   272                                     Ludicolo    Water    Grass
## 273   273                                       Seedot    Grass     <NA>
## 274   274                                      Nuzleaf    Grass     Dark
## 275   275                                      Shiftry    Grass     Dark
## 276   276                                      Taillow   Normal   Flying
## 277   277                                      Swellow   Normal   Flying
## 278   278                                      Wingull    Water   Flying
## 279   279                                     Pelipper    Water   Flying
## 280   280                                        Ralts  Psychic    Fairy
## 281   281                                       Kirlia  Psychic    Fairy
## 282   282                                    Gardevoir  Psychic    Fairy
## 283   283                                      Surskit      Bug    Water
## 284   284                                   Masquerain      Bug   Flying
## 285   285                                    Shroomish    Grass     <NA>
## 286   286                                      Breloom    Grass Fighting
## 287   287                                      Slakoth   Normal     <NA>
## 288   288                                     Vigoroth   Normal     <NA>
## 289   289                                      Slaking   Normal     <NA>
## 290   290                                      Nincada      Bug   Ground
## 291   291                                      Ninjask      Bug   Flying
## 292   292                                     Shedinja      Bug    Ghost
## 293   293                                      Whismur   Normal     <NA>
## 294   294                                      Loudred   Normal     <NA>
## 295   295                                      Exploud   Normal     <NA>
## 296   296                                     Makuhita Fighting     <NA>
## 297   297                                     Hariyama Fighting     <NA>
## 298   298                                      Azurill   Normal    Fairy
## 299   299                                     Nosepass     Rock     <NA>
## 300   300                                       Skitty   Normal     <NA>
## 301   301                                     Delcatty   Normal     <NA>
## 302   302                                      Sableye     Dark    Ghost
## 303   303                                       Mawile    Steel    Fairy
## 304   304                                         Aron    Steel     Rock
## 305   305                                       Lairon    Steel     Rock
## 306   306                                       Aggron    Steel     Rock
## 307   307                                     Meditite Fighting  Psychic
## 308   308                                     Medicham Fighting  Psychic
## 309   309                                    Electrike Electric     <NA>
## 310   310                                    Manectric Electric     <NA>
## 311   311                                       Plusle Electric     <NA>
## 312   312                                        Minun Electric     <NA>
## 313   313                                      Volbeat      Bug     <NA>
## 314   314                                     Illumise      Bug     <NA>
## 315   315                                      Roselia    Grass   Poison
## 316   316                                       Gulpin   Poison     <NA>
## 317   317                                       Swalot   Poison     <NA>
## 318   318                                     Carvanha    Water     Dark
## 319   319                                     Sharpedo    Water     Dark
## 320   320                                      Wailmer    Water     <NA>
## 321   321                                      Wailord    Water     <NA>
## 322   322                                        Numel     Fire   Ground
## 323   323                                     Camerupt     Fire   Ground
## 324   324                                      Torkoal     Fire     <NA>
## 325   325                                       Spoink  Psychic     <NA>
## 326   326                                      Grumpig  Psychic     <NA>
## 327   327                                       Spinda   Normal     <NA>
## 328   328                                     Trapinch   Ground     <NA>
## 329   329                                      Vibrava   Ground   Dragon
## 330   330                                       Flygon   Ground   Dragon
## 331   331                                       Cacnea    Grass     <NA>
## 332   332                                     Cacturne    Grass     Dark
## 333   333                                       Swablu   Normal   Flying
## 334   334                                      Altaria   Dragon   Flying
## 335   335                                     Zangoose   Normal     <NA>
## 336   336                                      Seviper   Poison     <NA>
## 337   337                                     Lunatone     Rock  Psychic
## 338   338                                      Solrock     Rock  Psychic
## 339   339                                     Barboach    Water   Ground
## 340   340                                     Whiscash    Water   Ground
## 341   341                                     Corphish    Water     <NA>
## 342   342                                    Crawdaunt    Water     Dark
## 343   343                                       Baltoy   Ground  Psychic
## 344   344                                      Claydol   Ground  Psychic
## 345   345                                       Lileep     Rock    Grass
## 346   346                                      Cradily     Rock    Grass
## 347   347                                      Anorith     Rock      Bug
## 348   348                                      Armaldo     Rock      Bug
## 349   349                                       Feebas    Water     <NA>
## 350   350                                      Milotic    Water     <NA>
## 351   351                             Castform\nNormal   Normal     <NA>
## 352   352                                      Kecleon   Normal     <NA>
## 353   353                                      Shuppet    Ghost     <NA>
## 354   354                                      Banette    Ghost     <NA>
## 355   355                                      Duskull    Ghost     <NA>
## 356   356                                     Dusclops    Ghost     <NA>
## 357   357                                      Tropius    Grass   Flying
## 358   358                                     Chimecho  Psychic     <NA>
## 359   359                                        Absol     Dark     <NA>
## 360   360                                       Wynaut  Psychic     <NA>
## 361   361                                      Snorunt      Ice     <NA>
## 362   362                                       Glalie      Ice     <NA>
## 363   363                                       Spheal      Ice    Water
## 364   364                                       Sealeo      Ice    Water
## 365   365                                      Walrein      Ice    Water
## 366   366                                     Clamperl    Water     <NA>
## 367   367                                      Huntail    Water     <NA>
## 368   368                                     Gorebyss    Water     <NA>
## 369   369                                    Relicanth    Water     Rock
## 370   370                                      Luvdisc    Water     <NA>
## 371   371                                        Bagon   Dragon     <NA>
## 372   372                                      Shelgon   Dragon     <NA>
## 373   373                                    Salamence   Dragon   Flying
## 374   374                                       Beldum    Steel  Psychic
## 375   375                                       Metang    Steel  Psychic
## 376   376                                    Metagross    Steel  Psychic
## 377   377                                     Regirock     Rock     <NA>
## 378   378                                       Regice      Ice     <NA>
## 379   379                                    Registeel    Steel     <NA>
## 380   380                                       Latias   Dragon  Psychic
## 381   381                                       Latios   Dragon  Psychic
## 382   382                               Kyogre\nKyogre    Water     <NA>
## 383   383                             Groudon\nGroudon   Ground     <NA>
## 384   384                                     Rayquaza   Dragon   Flying
## 385   385                                      Jirachi    Steel  Psychic
## 386   386                         Deoxys\nNormal Forme  Psychic     <NA>
## 387   387                                      Turtwig    Grass     <NA>
## 388   388                                       Grotle    Grass     <NA>
## 389   389                                     Torterra    Grass   Ground
## 390   390                                     Chimchar     Fire     <NA>
## 391   391                                     Monferno     Fire Fighting
## 392   392                                    Infernape     Fire Fighting
## 393   393                                       Piplup    Water     <NA>
## 394   394                                     Prinplup    Water     <NA>
## 395   395                                     Empoleon    Water    Steel
## 396   396                                       Starly   Normal   Flying
## 397   397                                     Staravia   Normal   Flying
## 398   398                                    Staraptor   Normal   Flying
## 399   399                                       Bidoof   Normal     <NA>
## 400   400                                      Bibarel   Normal    Water
## 401   401                                    Kricketot      Bug     <NA>
## 402   402                                   Kricketune      Bug     <NA>
## 403   403                                        Shinx Electric     <NA>
## 404   404                                        Luxio Electric     <NA>
## 405   405                                       Luxray Electric     <NA>
## 406   406                                        Budew    Grass   Poison
## 407   407                                     Roserade    Grass   Poison
## 408   408                                     Cranidos     Rock     <NA>
## 409   409                                    Rampardos     Rock     <NA>
## 410   410                                     Shieldon     Rock    Steel
## 411   411                                    Bastiodon     Rock    Steel
## 412   412                           Burmy\nPlant Cloak      Bug     <NA>
## 413   413                        Wormadam\nPlant Cloak      Bug    Grass
## 414   414                                       Mothim      Bug   Flying
## 415   415                                       Combee      Bug   Flying
## 416   416                                    Vespiquen      Bug   Flying
## 417   417                                    Pachirisu Electric     <NA>
## 418   418                                       Buizel    Water     <NA>
## 419   419                                     Floatzel    Water     <NA>
## 420   420                                      Cherubi    Grass     <NA>
## 421   421                       Cherrim\nOvercast Form    Grass     <NA>
## 422   422                            Shellos\nWest Sea    Water     <NA>
## 423   423                          Gastrodon\nWest Sea    Water   Ground
## 424   424                                      Ambipom   Normal     <NA>
## 425   425                                     Drifloon    Ghost   Flying
## 426   426                                     Drifblim    Ghost   Flying
## 427   427                                      Buneary   Normal     <NA>
## 428   428                                      Lopunny   Normal     <NA>
## 429   429                                    Mismagius    Ghost     <NA>
## 430   430                                    Honchkrow     Dark   Flying
## 431   431                                      Glameow   Normal     <NA>
## 432   432                                      Purugly   Normal     <NA>
## 433   433                                    Chingling  Psychic     <NA>
## 434   434                                       Stunky   Poison     Dark
## 435   435                                     Skuntank   Poison     Dark
## 436   436                                      Bronzor    Steel  Psychic
## 437   437                                     Bronzong    Steel  Psychic
## 438   438                                       Bonsly     Rock     <NA>
## 439   439                                     Mime Jr.  Psychic    Fairy
## 440   440                                      Happiny   Normal     <NA>
## 441   441                                       Chatot   Normal   Flying
## 442   442                                    Spiritomb    Ghost     Dark
## 443   443                                        Gible   Dragon   Ground
## 444   444                                       Gabite   Dragon   Ground
## 445   445                                     Garchomp   Dragon   Ground
## 446   446                                     Munchlax   Normal     <NA>
## 447   447                                        Riolu Fighting     <NA>
## 448   448                                      Lucario Fighting    Steel
## 449   449                                   Hippopotas   Ground     <NA>
## 450   450                                    Hippowdon   Ground     <NA>
## 451   451                                      Skorupi   Poison      Bug
## 452   452                                      Drapion   Poison     Dark
## 453   453                                     Croagunk   Poison Fighting
## 454   454                                    Toxicroak   Poison Fighting
## 455   455                                    Carnivine    Grass     <NA>
## 456   456                                      Finneon    Water     <NA>
## 457   457                                     Lumineon    Water     <NA>
## 458   458                                      Mantyke    Water   Flying
## 459   459                                       Snover    Grass      Ice
## 460   460                                    Abomasnow    Grass      Ice
## 461   461                                      Weavile     Dark      Ice
## 462   462                                    Magnezone Electric    Steel
## 463   463                                   Lickilicky   Normal     <NA>
## 464   464                                    Rhyperior   Ground     Rock
## 465   465                                    Tangrowth    Grass     <NA>
## 466   466                                   Electivire Electric     <NA>
## 467   467                                    Magmortar     Fire     <NA>
## 468   468                                     Togekiss    Fairy   Flying
## 469   469                                      Yanmega      Bug   Flying
## 470   470                                      Leafeon    Grass     <NA>
## 471   471                                      Glaceon      Ice     <NA>
## 472   472                                      Gliscor   Ground   Flying
## 473   473                                    Mamoswine      Ice   Ground
## 474   474                                    Porygon-Z   Normal     <NA>
## 475   475                                      Gallade  Psychic Fighting
## 476   476                                    Probopass     Rock    Steel
## 477   477                                     Dusknoir    Ghost     <NA>
## 478   478                                     Froslass      Ice    Ghost
## 479   479                                 Rotom\nRotom Electric    Ghost
## 480   480                                         Uxie  Psychic     <NA>
## 481   481                                      Mesprit  Psychic     <NA>
## 482   482                                        Azelf  Psychic     <NA>
## 483   483                                       Dialga    Steel   Dragon
## 484   484                                       Palkia    Water   Dragon
## 485   485                                      Heatran     Fire    Steel
## 486   486                                    Regigigas   Normal     <NA>
## 487   487                      Giratina\nAltered Forme    Ghost   Dragon
## 488   488                                    Cresselia  Psychic     <NA>
## 489   489                                       Phione    Water     <NA>
## 490   490                                      Manaphy    Water     <NA>
## 491   491                                      Darkrai     Dark     <NA>
## 492   492                          Shaymin\nLand Forme    Grass     <NA>
## 493   493                               Arceus\nArceus   Normal     <NA>
## 494   494                                      Victini  Psychic     Fire
## 495   495                                        Snivy    Grass     <NA>
## 496   496                                      Servine    Grass     <NA>
## 497   497                                    Serperior    Grass     <NA>
## 498   498                                        Tepig     Fire     <NA>
## 499   499                                      Pignite     Fire Fighting
## 500   500                                       Emboar     Fire Fighting
## 501   501                                     Oshawott    Water     <NA>
## 502   502                                       Dewott    Water     <NA>
## 503   503                                     Samurott    Water     <NA>
## 504   504                                       Patrat   Normal     <NA>
## 505   505                                      Watchog   Normal     <NA>
## 506   506                                     Lillipup   Normal     <NA>
## 507   507                                      Herdier   Normal     <NA>
## 508   508                                    Stoutland   Normal     <NA>
## 509   509                                     Purrloin     Dark     <NA>
## 510   510                                      Liepard     Dark     <NA>
## 511   511                                      Pansage    Grass     <NA>
## 512   512                                     Simisage    Grass     <NA>
## 513   513                                      Pansear     Fire     <NA>
## 514   514                                     Simisear     Fire     <NA>
## 515   515                                      Panpour    Water     <NA>
## 516   516                                     Simipour    Water     <NA>
## 517   517                                        Munna  Psychic     <NA>
## 518   518                                     Musharna  Psychic     <NA>
## 519   519                                       Pidove   Normal   Flying
## 520   520                                    Tranquill   Normal   Flying
## 521   521                                     Unfezant   Normal   Flying
## 522   522                                      Blitzle Electric     <NA>
## 523   523                                    Zebstrika Electric     <NA>
## 524   524                                   Roggenrola     Rock     <NA>
## 525   525                                      Boldore     Rock     <NA>
## 526   526                                     Gigalith     Rock     <NA>
## 527   527                                       Woobat  Psychic   Flying
## 528   528                                      Swoobat  Psychic   Flying
## 529   529                                      Drilbur   Ground     <NA>
## 530   530                                    Excadrill   Ground    Steel
## 531   531                                       Audino   Normal     <NA>
## 532   532                                      Timburr Fighting     <NA>
## 533   533                                      Gurdurr Fighting     <NA>
## 534   534                                   Conkeldurr Fighting     <NA>
## 535   535                                      Tympole    Water     <NA>
## 536   536                                    Palpitoad    Water   Ground
## 537   537                                   Seismitoad    Water   Ground
## 538   538                                        Throh Fighting     <NA>
## 539   539                                         Sawk Fighting     <NA>
## 540   540                                     Sewaddle      Bug    Grass
## 541   541                                     Swadloon      Bug    Grass
## 542   542                                     Leavanny      Bug    Grass
## 543   543                                     Venipede      Bug   Poison
## 544   544                                   Whirlipede      Bug   Poison
## 545   545                                    Scolipede      Bug   Poison
## 546   546                                     Cottonee    Grass    Fairy
## 547   547                                   Whimsicott    Grass    Fairy
## 548   548                                      Petilil    Grass     <NA>
## 549   549                                    Lilligant    Grass     <NA>
## 550   550                   Basculin\nRed-Striped Form    Water     <NA>
## 551   551                                      Sandile   Ground     Dark
## 552   552                                     Krokorok   Ground     Dark
## 553   553                                   Krookodile   Ground     Dark
## 554   554                                     Darumaka     Fire     <NA>
## 555   555                    Darmanitan\nStandard Mode     Fire     <NA>
## 556   556                                     Maractus    Grass     <NA>
## 557   557                                      Dwebble      Bug     Rock
## 558   558                                      Crustle      Bug     Rock
## 559   559                                      Scraggy     Dark Fighting
## 560   560                                      Scrafty     Dark Fighting
## 561   561                                     Sigilyph  Psychic   Flying
## 562   562                                       Yamask    Ghost     <NA>
## 563   563                                   Cofagrigus    Ghost     <NA>
## 564   564                                     Tirtouga    Water     Rock
## 565   565                                   Carracosta    Water     Rock
## 566   566                                       Archen     Rock   Flying
## 567   567                                     Archeops     Rock   Flying
## 568   568                                     Trubbish   Poison     <NA>
## 569   569                                     Garbodor   Poison     <NA>
## 570   570                                        Zorua     Dark     <NA>
## 571   571                                      Zoroark     Dark     <NA>
## 572   572                                     Minccino   Normal     <NA>
## 573   573                                     Cinccino   Normal     <NA>
## 574   574                                      Gothita  Psychic     <NA>
## 575   575                                    Gothorita  Psychic     <NA>
## 576   576                                   Gothitelle  Psychic     <NA>
## 577   577                                      Solosis  Psychic     <NA>
## 578   578                                      Duosion  Psychic     <NA>
## 579   579                                    Reuniclus  Psychic     <NA>
## 580   580                                     Ducklett    Water   Flying
## 581   581                                       Swanna    Water   Flying
## 582   582                                    Vanillite      Ice     <NA>
## 583   583                                    Vanillish      Ice     <NA>
## 584   584                                    Vanilluxe      Ice     <NA>
## 585   585                        Deerling\nSpring Form   Normal    Grass
## 586   586                        Sawsbuck\nSpring Form   Normal    Grass
## 587   587                                       Emolga Electric   Flying
## 588   588                                   Karrablast      Bug     <NA>
## 589   589                                   Escavalier      Bug    Steel
## 590   590                                      Foongus    Grass   Poison
## 591   591                                    Amoonguss    Grass   Poison
## 592   592                                     Frillish    Water    Ghost
## 593   593                                    Jellicent    Water    Ghost
## 594   594                                    Alomomola    Water     <NA>
## 595   595                                       Joltik      Bug Electric
## 596   596                                   Galvantula      Bug Electric
## 597   597                                    Ferroseed    Grass    Steel
## 598   598                                   Ferrothorn    Grass    Steel
## 599   599                                        Klink    Steel     <NA>
## 600   600                                        Klang    Steel     <NA>
## 601   601                                    Klinklang    Steel     <NA>
## 602   602                                       Tynamo Electric     <NA>
## 603   603                                    Eelektrik Electric     <NA>
## 604   604                                   Eelektross Electric     <NA>
## 605   605                                       Elgyem  Psychic     <NA>
## 606   606                                     Beheeyem  Psychic     <NA>
## 607   607                                      Litwick    Ghost     Fire
## 608   608                                      Lampent    Ghost     Fire
## 609   609                                   Chandelure    Ghost     Fire
## 610   610                                         Axew   Dragon     <NA>
## 611   611                                      Fraxure   Dragon     <NA>
## 612   612                                      Haxorus   Dragon     <NA>
## 613   613                                      Cubchoo      Ice     <NA>
## 614   614                                      Beartic      Ice     <NA>
## 615   615                                    Cryogonal      Ice     <NA>
## 616   616                                      Shelmet      Bug     <NA>
## 617   617                                     Accelgor      Bug     <NA>
## 618   618                                     Stunfisk   Ground Electric
## 619   619                                      Mienfoo Fighting     <NA>
## 620   620                                     Mienshao Fighting     <NA>
## 621   621                                    Druddigon   Dragon     <NA>
## 622   622                                       Golett   Ground    Ghost
## 623   623                                       Golurk   Ground    Ghost
## 624   624                                     Pawniard     Dark    Steel
## 625   625                                      Bisharp     Dark    Steel
## 626   626                                   Bouffalant   Normal     <NA>
## 627   627                                      Rufflet   Normal   Flying
## 628   628                                     Braviary   Normal   Flying
## 629   629                                      Vullaby     Dark   Flying
## 630   630                                    Mandibuzz     Dark   Flying
## 631   631                                      Heatmor     Fire     <NA>
## 632   632                                       Durant      Bug    Steel
## 633   633                                        Deino     Dark   Dragon
## 634   634                                     Zweilous     Dark   Dragon
## 635   635                                    Hydreigon     Dark   Dragon
## 636   636                                     Larvesta      Bug     Fire
## 637   637                                    Volcarona      Bug     Fire
## 638   638                                     Cobalion    Steel Fighting
## 639   639                                    Terrakion     Rock Fighting
## 640   640                                     Virizion    Grass Fighting
## 641   641                    Tornadus\nIncarnate Forme   Flying     <NA>
## 642   642                   Thundurus\nIncarnate Forme Electric   Flying
## 643   643                                     Reshiram   Dragon     Fire
## 644   644                                       Zekrom   Dragon Electric
## 645   645                    Landorus\nIncarnate Forme   Ground   Flying
## 646   646                               Kyurem\nKyurem   Dragon      Ice
## 647   647                        Keldeo\nOrdinary Form    Water Fighting
## 648   648                         Meloetta\nAria Forme   Normal  Psychic
## 649   649                           Genesect\nGenesect      Bug    Steel
## 650   650                                      Chespin    Grass     <NA>
## 651   651                                    Quilladin    Grass     <NA>
## 652   652                                   Chesnaught    Grass Fighting
## 653   653                                     Fennekin     Fire     <NA>
## 654   654                                      Braixen     Fire     <NA>
## 655   655                                      Delphox     Fire  Psychic
## 656   656                                      Froakie    Water     <NA>
## 657   657                                    Frogadier    Water     <NA>
## 658   658                                     Greninja    Water     Dark
## 659   659                                     Bunnelby   Normal     <NA>
## 660   660                                    Diggersby   Normal   Ground
## 661   661                                   Fletchling   Normal   Flying
## 662   662                                  Fletchinder     Fire   Flying
## 663   663                                   Talonflame     Fire   Flying
## 664   664                                   Scatterbug      Bug     <NA>
## 665   665                                       Spewpa      Bug     <NA>
## 666   666                     Vivillon\nMeadow Pattern      Bug   Flying
## 667   667                                       Litleo     Fire   Normal
## 668   668                                       Pyroar     Fire   Normal
## 669   669                          Flabébé\nRed Flower    Fairy     <NA>
## 670   670                          Floette\nRed Flower    Fairy     <NA>
## 671   671                          Florges\nRed Flower    Fairy     <NA>
## 672   672                                       Skiddo    Grass     <NA>
## 673   673                                       Gogoat    Grass     <NA>
## 674   674                                      Pancham Fighting     <NA>
## 675   675                                      Pangoro Fighting     Dark
## 676   676                        Furfrou\nNatural Form   Normal     <NA>
## 677   677                                       Espurr  Psychic     <NA>
## 678   678                               Meowstic\nMale  Psychic     <NA>
## 679   679                                      Honedge    Steel    Ghost
## 680   680                                     Doublade    Steel    Ghost
## 681   681                      Aegislash\nShield Forme    Steel    Ghost
## 682   682                                     Spritzee    Fairy     <NA>
## 683   683                                   Aromatisse    Fairy     <NA>
## 684   684                                      Swirlix    Fairy     <NA>
## 685   685                                     Slurpuff    Fairy     <NA>
## 686   686                                        Inkay     Dark  Psychic
## 687   687                                      Malamar     Dark  Psychic
## 688   688                                      Binacle     Rock    Water
## 689   689                                   Barbaracle     Rock    Water
## 690   690                                       Skrelp   Poison    Water
## 691   691                                     Dragalge   Poison   Dragon
## 692   692                                    Clauncher    Water     <NA>
## 693   693                                    Clawitzer    Water     <NA>
## 694   694                                   Helioptile Electric   Normal
## 695   695                                    Heliolisk Electric   Normal
## 696   696                                       Tyrunt     Rock   Dragon
## 697   697                                    Tyrantrum     Rock   Dragon
## 698   698                                       Amaura     Rock      Ice
## 699   699                                      Aurorus     Rock      Ice
## 700   700                                      Sylveon    Fairy     <NA>
## 701   701                                     Hawlucha Fighting   Flying
## 702   702                                      Dedenne Electric    Fairy
## 703   703                                      Carbink     Rock    Fairy
## 704   704                                        Goomy   Dragon     <NA>
## 705   705                                      Sliggoo   Dragon     <NA>
## 706   706                                       Goodra   Dragon     <NA>
## 707   707                                       Klefki    Steel    Fairy
## 708   708                                     Phantump    Ghost    Grass
## 709   709                                    Trevenant    Ghost    Grass
## 710   710                                    Pumpkaboo    Ghost    Grass
## 711   711                                    Gourgeist    Ghost    Grass
## 712   712                                     Bergmite      Ice     <NA>
## 713   713                                      Avalugg      Ice     <NA>
## 714   714                                       Noibat   Flying   Dragon
## 715   715                                      Noivern   Flying   Dragon
## 716   716                        Xerneas\nNeutral Mode    Fairy     <NA>
## 717   717                                      Yveltal     Dark   Flying
## 718   718                           Zygarde\n50% Forme   Dragon   Ground
## 719   719                                      Diancie     Rock    Fairy
## 720   720                        Hoopa\nHoopa Confined  Psychic    Ghost
## 721   721                                    Volcanion     Fire    Water
## 722   722                                       Rowlet    Grass   Flying
## 723   723                                      Dartrix    Grass   Flying
## 724   724                                    Decidueye    Grass    Ghost
## 725   725                                       Litten     Fire     <NA>
## 726   726                                     Torracat     Fire     <NA>
## 727   727                                   Incineroar     Fire     Dark
## 728   728                                      Popplio    Water     <NA>
## 729   729                                      Brionne    Water     <NA>
## 730   730                                    Primarina    Water    Fairy
## 731   731                                      Pikipek   Normal   Flying
## 732   732                                     Trumbeak   Normal   Flying
## 733   733                                    Toucannon   Normal   Flying
## 734   734                                      Yungoos   Normal     <NA>
## 735   735                                     Gumshoos   Normal     <NA>
## 736   736                                      Grubbin      Bug     <NA>
## 737   737                                    Charjabug      Bug Electric
## 738   738                                     Vikavolt      Bug Electric
## 739   739                                   Crabrawler Fighting     <NA>
## 740   740                                 Crabominable Fighting      Ice
## 741   741                        Oricorio\nBaile Style     Fire   Flying
## 742   742                                     Cutiefly      Bug    Fairy
## 743   743                                     Ribombee      Bug    Fairy
## 744   744                                     Rockruff     Rock     <NA>
## 745   745                        Lycanroc\nMidday Form     Rock     <NA>
## 746   746                        Wishiwashi\nSolo Form    Water     <NA>
## 747   747                                     Mareanie   Poison    Water
## 748   748                                      Toxapex   Poison    Water
## 749   749                                      Mudbray   Ground     <NA>
## 750   750                                     Mudsdale   Ground     <NA>
## 751   751                                     Dewpider    Water      Bug
## 752   752                                    Araquanid    Water      Bug
## 753   753                                     Fomantis    Grass     <NA>
## 754   754                                     Lurantis    Grass     <NA>
## 755   755                                     Morelull    Grass    Fairy
## 756   756                                    Shiinotic    Grass    Fairy
## 757   757                                     Salandit   Poison     Fire
## 758   758                                     Salazzle   Poison     Fire
## 759   759                                      Stufful   Normal Fighting
## 760   760                                       Bewear   Normal Fighting
## 761   761                                    Bounsweet    Grass     <NA>
## 762   762                                      Steenee    Grass     <NA>
## 763   763                                     Tsareena    Grass     <NA>
## 764   764                                       Comfey    Fairy     <NA>
## 765   765                                     Oranguru   Normal  Psychic
## 766   766                                    Passimian Fighting     <NA>
## 767   767                                       Wimpod      Bug    Water
## 768   768                                    Golisopod      Bug    Water
## 769   769                                    Sandygast    Ghost   Ground
## 770   770                                    Palossand    Ghost   Ground
## 771   771                                    Pyukumuku    Water     <NA>
## 772   772                                   Type: Null   Normal     <NA>
## 773   773                       Silvally\nType: Normal   Normal     <NA>
## 774   774                          Minior\nMeteor Form     Rock   Flying
## 775   775                                       Komala   Normal     <NA>
## 776   776                                   Turtonator     Fire   Dragon
## 777   777                                   Togedemaru Electric    Steel
## 778   778                      Mimikyu\nDisguised Form    Ghost    Fairy
## 779   779                                      Bruxish    Water  Psychic
## 780   780                                       Drampa   Normal   Dragon
## 781   781                                     Dhelmise    Ghost    Grass
## 782   782                                     Jangmo-o   Dragon     <NA>
## 783   783                                     Hakamo-o   Dragon Fighting
## 784   784                                      Kommo-o   Dragon Fighting
## 785   785                                    Tapu Koko Electric    Fairy
## 786   786                                    Tapu Lele  Psychic    Fairy
## 787   787                                    Tapu Bulu    Grass    Fairy
## 788   788                                    Tapu Fini    Water    Fairy
## 789   789                                       Cosmog  Psychic     <NA>
## 790   790                                      Cosmoem  Psychic     <NA>
## 791   791                                     Solgaleo  Psychic    Steel
## 792   792                                       Lunala  Psychic    Ghost
## 793   793                                     Nihilego     Rock   Poison
## 794   794                                     Buzzwole      Bug Fighting
## 795   795                                    Pheromosa      Bug Fighting
## 796   796                                    Xurkitree Electric     <NA>
## 797   797                                   Celesteela    Steel   Flying
## 798   798                                      Kartana    Grass    Steel
## 799   799                                     Guzzlord     Dark   Dragon
## 800   800                                     Necrozma  Psychic     <NA>
## 801   801                                     Magearna    Steel    Fairy
## 802   802                                    Marshadow Fighting    Ghost
## 803   803                                      Poipole   Poison     <NA>
## 804   804                                    Naganadel   Poison   Dragon
## 805   805                                    Stakataka     Rock    Steel
## 806   806                                  Blacephalon     Fire    Ghost
## 807   807                                      Zeraora Electric     <NA>
## 808   808                                       Meltan    Steel     <NA>
## 809   809                                     Melmetal    Steel     <NA>
## 810   810                                      Grookey    Grass     <NA>
## 811   811                                     Thwackey    Grass     <NA>
## 812   812                                    Rillaboom    Grass     <NA>
## 813   813                                    Scorbunny     Fire     <NA>
## 814   814                                       Raboot     Fire     <NA>
## 815   815                                    Cinderace     Fire     <NA>
## 816   816                                       Sobble    Water     <NA>
## 817   817                                     Drizzile    Water     <NA>
## 818   818                                     Inteleon    Water     <NA>
## 819   819                                      Skwovet   Normal     <NA>
## 820   820                                     Greedent   Normal     <NA>
## 821   821                                     Rookidee   Flying     <NA>
## 822   822                                  Corvisquire   Flying     <NA>
## 823   823                                  Corviknight   Flying    Steel
## 824   824                                      Blipbug      Bug     <NA>
## 825   825                                      Dottler      Bug  Psychic
## 826   826                                     Orbeetle      Bug  Psychic
## 827   827                                       Nickit     Dark     <NA>
## 828   828                                      Thievul     Dark     <NA>
## 829   829                                   Gossifleur    Grass     <NA>
## 830   830                                     Eldegoss    Grass     <NA>
## 831   831                                       Wooloo   Normal     <NA>
## 832   832                                      Dubwool   Normal     <NA>
## 833   833                                      Chewtle    Water     <NA>
## 834   834                                      Drednaw    Water     Rock
## 835   835                                       Yamper Electric     <NA>
## 836   836                                      Boltund Electric     <NA>
## 837   837                                     Rolycoly     Rock     <NA>
## 838   838                                       Carkol     Rock     Fire
## 839   839                                    Coalossal     Rock     Fire
## 840   840                                       Applin    Grass   Dragon
## 841   841                                      Flapple    Grass   Dragon
## 842   842                                     Appletun    Grass   Dragon
## 843   843                                    Silicobra   Ground     <NA>
## 844   844                                   Sandaconda   Ground     <NA>
## 845   845                                    Cramorant   Flying    Water
## 846   846                                     Arrokuda    Water     <NA>
## 847   847                                  Barraskewda    Water     <NA>
## 848   848                                        Toxel Electric   Poison
## 849   849                       Toxtricity\nAmped Form Electric   Poison
## 850   850                                   Sizzlipede     Fire      Bug
## 851   851                                  Centiskorch     Fire      Bug
## 852   852                                    Clobbopus Fighting     <NA>
## 853   853                                    Grapploct Fighting     <NA>
## 854   854                                     Sinistea    Ghost     <NA>
## 855   855                                  Polteageist    Ghost     <NA>
## 856   856                                      Hatenna  Psychic     <NA>
## 857   857                                      Hattrem  Psychic     <NA>
## 858   858                                    Hatterene  Psychic    Fairy
## 859   859                                     Impidimp     Dark    Fairy
## 860   860                                      Morgrem     Dark    Fairy
## 861   861                                   Grimmsnarl     Dark    Fairy
## 862   862                                    Obstagoon     Dark   Normal
## 863   863                                   Perrserker    Steel     <NA>
## 864   864                                      Cursola    Ghost     <NA>
## 865   865                                   Sirfetch'd Fighting     <NA>
## 866   866                                     Mr. Rime      Ice  Psychic
## 867   867                                    Runerigus   Ground    Ghost
## 868   868                                      Milcery    Fairy     <NA>
## 869   869                      Alcremie\nVanilla Cream    Fairy     <NA>
## 870   870                                      Falinks Fighting     <NA>
## 871   871                                   Pincurchin Electric     <NA>
## 872   872                                         Snom      Ice      Bug
## 873   873                                     Frosmoth      Ice      Bug
## 874   874                                  Stonjourner     Rock     <NA>
## 875   875                             Eiscue\nIce Face      Ice     <NA>
## 876   876                               Indeedee\nMale  Psychic   Normal
## 877   877                     Morpeko\nFull Belly Mode Electric     Dark
## 878   878                                       Cufant    Steel     <NA>
## 879   879                                   Copperajah    Steel     <NA>
## 880   880                                    Dracozolt Electric   Dragon
## 881   881                                    Arctozolt Electric      Ice
## 882   882                                    Dracovish    Water   Dragon
## 883   883                                    Arctovish    Water      Ice
## 884   884                                    Duraludon    Steel   Dragon
## 885   885                                       Dreepy   Dragon    Ghost
## 886   886                                     Drakloak   Dragon    Ghost
## 887   887                                    Dragapult   Dragon    Ghost
## 888   888                 Zacian\nHero of Many Battles    Fairy     <NA>
## 889   889              Zamazenta\nHero of Many Battles Fighting     <NA>
## 890   890                                    Eternatus   Poison   Dragon
## 891   891                                        Kubfu Fighting     <NA>
## 892   892                 Urshifu\nSingle Strike Style Fighting     Dark
## 893   893                                       Zarude     Dark    Grass
## 894   894                                    Regieleki Electric     <NA>
## 895   895                                    Regidrago   Dragon     <NA>
## 896   896                                    Glastrier      Ice     <NA>
## 897   897                                    Spectrier    Ghost     <NA>
## 898   898                                      Calyrex  Psychic    Grass
## 899   899                                      Wyrdeer   Normal  Psychic
## 900   900                                      Kleavor      Bug     Rock
## 901   901                                     Ursaluna   Ground   Normal
## 902   902                            Basculegion\nMale    Water    Ghost
## 903   903                                     Sneasler Fighting   Poison
## 904   904                                     Overqwil     Dark   Poison
## 905   905                    Enamorus\nIncarnate Forme    Fairy   Flying
## 906   906                                   Sprigatito    Grass     <NA>
## 907   907                                    Floragato    Grass     <NA>
## 908   908                                  Meowscarada    Grass     Dark
## 909   909                                      Fuecoco     Fire     <NA>
## 910   910                                     Crocalor     Fire     <NA>
## 911   911                                   Skeledirge     Fire    Ghost
## 912   912                                       Quaxly    Water     <NA>
## 913   913                                     Quaxwell    Water     <NA>
## 914   914                                    Quaquaval    Water Fighting
## 915   915                                      Lechonk   Normal     <NA>
## 916   916                                   Oinkologne   Normal     <NA>
## 917   917                                   Tarountula      Bug     <NA>
## 918   918                                      Spidops      Bug     <NA>
## 919   919                                       Nymble      Bug     <NA>
## 920   920                                        Lokix      Bug     Dark
## 921   921                                        Pawmi Electric     <NA>
## 922   922                                        Pawmo Electric Fighting
## 923   923                                       Pawmot Electric Fighting
## 924   924                                    Tandemaus   Normal     <NA>
## 925   925                                     Maushold   Normal     <NA>
## 926   926                                      Fidough    Fairy     <NA>
## 927   927                                     Dachsbun    Fairy     <NA>
## 928   928                                       Smoliv    Grass   Normal
## 929   929                                       Dolliv    Grass   Normal
## 930   930                                     Arboliva    Grass   Normal
## 931   931                                 Squawkabilly   Normal   Flying
## 932   932                                        Nacli     Rock     <NA>
## 933   933                                    Naclstack     Rock     <NA>
## 934   934                                    Garganacl     Rock     <NA>
## 935   935                                    Charcadet     Fire     <NA>
## 936   936                                    Armarouge     Fire  Psychic
## 937   937                                    Ceruledge     Fire    Ghost
## 938   938                                      Tadbulb Electric     <NA>
## 939   939                                    Bellibolt Electric     <NA>
## 940   940                                      Wattrel Electric   Flying
## 941   941                                  Kilowattrel Electric   Flying
## 942   942                                     Maschiff     Dark     <NA>
## 943   943                                   Mabosstiff     Dark     <NA>
## 944   944                                     Shroodle   Poison   Normal
## 945   945                                     Grafaiai   Poison   Normal
## 946   946                                     Bramblin    Grass    Ghost
## 947   947                                 Brambleghast    Grass    Ghost
## 948   948                                    Toedscool   Ground    Grass
## 949   949                                   Toedscruel   Ground    Grass
## 950   950                                        Klawf     Rock     <NA>
## 951   951                                     Capsakid    Grass     <NA>
## 952   952                                   Scovillain    Grass     Fire
## 953   953                                       Rellor      Bug     <NA>
## 954   954                                       Rabsca      Bug  Psychic
## 955   955                                      Flittle  Psychic     <NA>
## 956   956                                     Espathra  Psychic     <NA>
## 957   957                                    Tinkatink    Fairy    Steel
## 958   958                                    Tinkatuff    Fairy    Steel
## 959   959                                     Tinkaton    Fairy    Steel
## 960   960                                      Wiglett    Water     <NA>
## 961   961                                      Wugtrio    Water     <NA>
## 962   962                                   Bombirdier   Flying     Dark
## 963   963                                      Finizen    Water     <NA>
## 964   964                           Palafin\nZero Form    Water     <NA>
## 965   965                                       Varoom    Steel   Poison
## 966   966                                    Revavroom    Steel   Poison
## 967   967                                     Cyclizar   Dragon   Normal
## 968   968                                     Orthworm    Steel     <NA>
## 969   969                                      Glimmet     Rock   Poison
## 970   970                                     Glimmora     Rock   Poison
## 971   971                                     Greavard    Ghost     <NA>
## 972   972                                   Houndstone    Ghost     <NA>
## 973   973                                      Flamigo   Flying Fighting
## 974   974                                     Cetoddle      Ice     <NA>
## 975   975                                      Cetitan      Ice     <NA>
## 976   976                                       Veluza    Water  Psychic
## 977   977                                      Dondozo    Water     <NA>
## 978   978                        Tatsugiri\nCurly Form   Dragon    Water
## 979   979                                   Annihilape Fighting    Ghost
## 980   980                                     Clodsire   Poison   Ground
## 981   981                                    Farigiraf   Normal  Psychic
## 982   982                                  Dudunsparce   Normal     <NA>
## 983   983                                    Kingambit     Dark    Steel
## 984   984                                   Great Tusk   Ground Fighting
## 985   985                                  Scream Tail    Fairy  Psychic
## 986   986                                 Brute Bonnet    Grass     Dark
## 987   987                                 Flutter Mane    Ghost    Fairy
## 988   988                                 Slither Wing      Bug Fighting
## 989   989                                 Sandy Shocks Electric   Ground
## 990   990                                  Iron Treads   Ground    Steel
## 991   991                                  Iron Bundle      Ice    Water
## 992   992                                   Iron Hands Fighting Electric
## 993   993                                 Iron Jugulis     Dark   Flying
## 994   994                                    Iron Moth     Fire   Poison
## 995   995                                  Iron Thorns     Rock Electric
## 996   996                                     Frigibax   Dragon      Ice
## 997   997                                     Arctibax   Dragon      Ice
## 998   998                                   Baxcalibur   Dragon      Ice
## 999   999                       Gimmighoul\nChest Form    Ghost     <NA>
## 1000 1000                                    Gholdengo    Steel    Ghost
## 1001 1001                                     Wo-Chien     Dark    Grass
## 1002 1002                                    Chien-Pao     Dark      Ice
## 1003 1003                                      Ting-Lu     Dark   Ground
## 1004 1004                                       Chi-Yu     Dark     Fire
## 1005 1005                                 Roaring Moon   Dragon     Dark
## 1006 1006                                 Iron Valiant    Fairy Fighting
## 1007 1007                                     Koraidon Fighting   Dragon
## 1008 1008                                     Miraidon Electric   Dragon
## 1009 1009 Walking Wake                    Walking Wake    Water   Dragon
## 1010 1010  Iron Leaves                     Iron Leaves    Grass  Psychic
```
