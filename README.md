Project-1
================
Damon D’Ambrosio
2023-06-21

## Requirements

Packages required to run the functions are as follows:  
- `tidyverse`: All manner of good things for manipulating and analyzing
data  
- `httr`: Connecting to the API  
- `jsonlite`: Getting usable information from the API

## API Interaction Custom Functions

### `pokemon.lookup`

The first custom function we have is `pokemon.lookup`. This function
receives an input, either in the form of a Pokemon’s name or ID number,
and returns a nice, neat, one-row tibble displaying the desired
information for that individual Pokemon. PokeApi does not require a key,
so queries are accomplished by simply pasting the input query to the end
of the necessary hyperlink to look up the information. Name queries on
PokeApi are case-sensitive and lowercase only, so some language is
provided in the function to allow the user to enter the Pokemon name
however they see fit. By default, PokeApi stores Pokemon height and
weight in decimeters and hectograms, respectively. I have a hard enough
time conceptualizing metric units, and these particular units do not
match those used in any of the Pokemon-related media I remember from my
youth. Similarly, they do not match the units used in Pokemon-related
media made for consumption outside of the United States. As such, an
optional `unit` argument exists, which will convert the height and
weight into imperial units (inches and pounds) or metric units (meters
and kilograms). `pokemon.lookup` is a nice standalone function, but will
also serve as a helper function to generate larger reports.

``` r
pokemon.lookup <- function(x, unit = NULL, ...){
  poke.query <- paste0("https://pokeapi.co/api/v2/pokemon/", str_to_lower(x))
  
  poke.get <- GET(poke.query)
  
  poke.char <- fromJSON(rawToChar(poke.get$content))
  
  name <- str_to_title(poke.char$name)
  id.number <- poke.char$id
  type1 <- poke.char$types$type$name[1]
  type2 <- poke.char$types$type$name[2]
  height <- poke.char$height
  weight <- poke.char$weight
  hp <- poke.char$stats$base_stat[1]
  attack <- poke.char$stats$base_stat[2]
  defense <- poke.char$stats$base_stat[3]
  special.attack <- poke.char$stats$base_stat[4]
  special.defense <- poke.char$stats$base_stat[5]
  speed <- poke.char$stats$base_stat[6]
  
  if("imperial" %in% unit){
    height <- round((height * 3.93700787))
    weight <- round((weight * 0.2204622622))
  }
  
  if("metric" %in% unit){
    height <- round((height/10), 1)
    weight <- round((weight/10), 1)
  }

  poke.table <- tibble(name = name,
                       id.number = id.number,
                       type1 = type1,
                       type2 = type2,
                       height = height,
                       weight = weight,
                       hp = hp,
                       attack = attack,
                       special.attack = special.attack,
                       special.defense = special.defense,
                       speed = speed
                )
return(poke.table)
}
```

Lets test it out. My favorite Pokemon as a kid was Rhydon, so let’s make
a query for Rhydon, using imperial units:

``` r
pokemon.lookup("rhydon", unit = "imperial")
```

    ## # A tibble: 1 × 11
    ##   name   id.number type1  type2 height weight    hp attack special.attack special.defense speed
    ##   <chr>      <int> <chr>  <chr>  <dbl>  <dbl> <int>  <int>          <int>           <int> <int>
    ## 1 Rhydon       112 ground rock      75    265   105    130             45              45    40

Now lets demonstrate how it functions with using a numeric Pokemon ID
and metric units. How about one of the 300 first Pokemon, selected
randomly?

``` r
pokemon.lookup(sample(1:300,1), unit = "metric")
```

    ## # A tibble: 1 × 11
    ##   name    id.number type1 type2 height weight    hp attack special.attack special.defense speed
    ##   <chr>       <int> <chr> <chr>  <dbl>  <dbl> <int>  <int>          <int>           <int> <int>
    ## 1 Shuckle       213 bug   rock     0.6   20.5    20     10             10             230     5

The ultimate plan is to use `lapply` on `pokemon.lookup` to generate
large reports for analysis. However, there is some functionality I would
like to add. Namely, I do not have Pokemon ID numbers memorized. I do
remember Pokemon names, however. As such, it would be nice if I could
simply request all Pokemon between “Pokemon X” and “Pokemon Y” using
names, and have the function handle looking up the ID, constructing the
vector, then feeding it to `lapply`. The next custom function deals with
that.

## `pokemon.vector`

`pokemon.vector` allows one to provide the names of two Pokemon (via a
character vector with length = 2), and returns a vector of the ID
numbers between said Pokemon. This vector can then be fed to
`pokemon.lookup` via `lapply` to generate a report of multiple Pokemon.

``` r
pokemon.vector <- function(x, ...){
  
  if(is.numeric(x)){
    stop("Expecting two Pokemon names, not numbers!")
  }
  
  if(length(x) > 2){
    stop("Expecting exactly two Pokemon names!")
  }
  
  poke.1 <- str_to_lower(x[1])
  poke.2 <- str_to_lower(x[2])
  
  poke.1.query <- paste0("https://pokeapi.co/api/v2/pokemon/", poke.1)
  poke.1.get <- GET(poke.1.query)
  poke.1.char <- fromJSON(rawToChar(poke.1.get$content))
  poke.1.id <- poke.1.char$id
  
  poke.2.query <- paste0("https://pokeapi.co/api/v2/pokemon/", poke.2)
  poke.2.get <- GET(poke.2.query)
  poke.2.char <- fromJSON(rawToChar(poke.2.get$content))
  poke.2.id <- poke.2.char$id
  
  poke.vec <- seq(poke.1.id, poke.2.id)
  return(poke.vec)
}
```

Now, a quick demonstration of `pokemon.vector` in action. Let’s get a
numeric vector of all Pokemon IDs of the Generation III Pokemon.
“Treecko” is the first Generation III Pokemon, and “Jirachi” is the last
Generation III Pokemon:

``` r
gen3.list <- c("Treecko", "Jirachi")
pokemon.vector(gen3.list)
```

    ##   [1] 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286
    ##  [36] 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321
    ##  [71] 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356
    ## [106] 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385

One last helper function before we wrap things up…

## `gen1.correction`

The Pokemon games have changed a lot from since I was a kid. Some of
these changes were to correct “balance” issues. The two major changes
that come to mind are as follows:

- New Pokemon “types” have been added since Generation I to correct
  imbalances in the strengths/weaknesses of the various Pokemon. “Steel”
  and “Dark” types were added in Generation II, and at some point in
  time, “Fairy” was also added as a type. Seven Generation I Pokemon
  retroactively had their types changed as a result. I would like to do
  an exploratory data analysis on the Pokemon as they existed in
  Generation I, so would need to correct these Pokemon to their “old”
  typing.

- As can be seen earlier in this vignette, Pokemon have a “special
  attack” and “special defense” stat. In Generation I, these were
  combined into a single stat called “special.” From what I can tell,
  the original “Special” stat does not exist in PokeApi. However, it can
  exist within R as a simple numeric vector with 151 values. The
  `gen1.correction` function will store and provide that information.

``` r
gen1.correction <- function(x, type.change = TRUE, add.special = TRUE, delete.sp.att.def = FALSE, ...){

y <- x %>% filter(id.number <= 151)
z <- x %>% filter(id.number > 151)

  if(isTRUE(type.change)){
    y$type1[y$type1 == "fairy"] <- "normal" #Corrects Clefairy and Clefable to Gen I typing
    y$type2[y$type2 == "fairy"] <- NA       #Corrects Jigglypuff, Wigglytuff, and Mr-Mime to Gen I typing
    y$type2[y$type2 == "steel"] <- NA       #Corrects Magnemite and Magneton to Gen I typing
  }
  
  if(isTRUE(add.special)){
  
  #Special stat values sourced from bulbapedia.bulbagarden.net
  special.vector <- c(65, 80, 100, 50, 65, 85, 50, 65, 85, 20, 25, 80, 20, 25, 45, 35, 50, 70, 25, 50, 31, 61, 40, 65, 50, 90, 30, 55, 40, 55,   75, 40, 55, 75, 60, 85, 65, 100, 25, 50, 40, 75, 75, 85, 100, 55, 80, 40, 90, 45, 70, 40, 65, 50, 80, 35, 60, 50, 80, 40, 50, 70, 105, 120,   135, 35, 50, 65, 70, 85, 100, 100, 120, 30, 45, 55, 65, 80, 40, 80, 95, 120, 58, 35, 60, 70, 95, 40, 65, 45, 85, 100, 115, 130, 30, 90, 115,   25, 50, 55, 80, 60, 125, 40, 50, 35, 35, 60, 60, 85, 30, 45, 105, 100, 40, 70, 95, 50, 80, 70, 100, 100, 55, 95, 85, 85, 55, 70, 20, 100,     95, 48, 65, 110, 110, 110, 75, 90, 115, 45, 70, 60, 65, 125, 125, 125, 50, 70, 100, 154, 100)

  y <- y %>% mutate(gen1.special = special.vector[y$id.number])
  }

new.report <- bind_rows(y,z)

  if(isTRUE(delete.sp.att.def)){
    if(max(new.report$id.number > 151)){
      warning("Pokemon that did not exist in Generation I are in this report. Are you sure you wanted to delete special attack and special defense?")
    }
  new.report <- new.report %>% select(-c(special.attack, special.defense))
  }
return(new.report)
}
```

`gen1.correction` will check that Pokemon belong to Generation I by
making sure their ID number is 151 or lower. It will then change types
if needed, and append the “Special” stat to that Pokemon’s record. The
user can delete “Special Attack” and “Special Defense” if they wish to
have a “pure” Generation I dataset. If Pokemon that did not exist in
Generation I is included in the provided data set and the user specifies
dropping “Special Attack” and “Special Defense,” a warning message will
appear, as the resultant data set will have no “special” data associated
with such records.

Here’s a quick example. Let’s build a simple data set containing two
Pokemon: Magnemite, a Generation I Pokemon who would need a type
correction to remove its secondary “Steel” typing, and Scizor, a
Generation II Pokemon who does not need a type correction, but would be
inadvertently “corrected” if the function did not work as intended.
We’ll also specify the funciton to delete Special Attack and Special
Defense to see the warning message:

``` r
a <- pokemon.lookup("Magnemite")
b <- pokemon.lookup("Scizor")
c <- bind_rows(a,b)

gen1.correction(c, delete.sp.att.def = TRUE)
```

    ## # A tibble: 2 × 10
    ##   name      id.number type1    type2 height weight    hp attack speed gen1.special
    ##   <chr>         <int> <chr>    <chr>  <int>  <int> <int>  <int> <int>        <dbl>
    ## 1 Magnemite        81 electric <NA>       3     60    25     35    45           95
    ## 2 Scizor          212 bug      steel     18   1180    70    130    65           NA

As we can see, Magnemite had its secondary Steel type assigned to NA,
while Scizor retained its secondary Steel type. A warning message was
printed since a Pokemon outside of Generation I was in the report while
Special Attack and Special Defense were deleted. Finally, Scizor does
not have a `gen1.special` value.

## `pokemon.batch.report`

The final step is to put all of these functions into a nice wrapper,
generate some data, and start an exploratory analysis.

``` r
pokemon.batch.report <- function(x, gen1.correct = FALSE, ...){
  if(!is.numeric(x)){
    if(length(x) > 1){
    x <- pokemon.vector(x)
    }
  }
  
  x <- lapply(x, pokemon.lookup, ...)
  
  batch.lookup <- bind_rows(x)
  
  if(isTRUE(gen1.correct)){
    batch.lookup <- gen1.correction(batch.lookup, ... )
  }
  
  return(batch.lookup)
  
}
```

## Data Exploration

A narrative showing a data exploration activity will go here. I have a
preliminary idea of what I would like to do, depending on the
information available in the API.
