Project 1
================
Damon D’Ambrosio
2023-06-18

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
provided in the function to allow the user to enter the pokemon name
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
pokemon.lookup <- function(id, unit = NULL){
  poke.query <- paste0("https://pokeapi.co/api/v2/pokemon/", str_to_lower(id))
  
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
    ##   name   id.number type1  type2 height weight    hp attack special.attack
    ##   <chr>      <int> <chr>  <chr>  <dbl>  <dbl> <int>  <int>          <int>
    ## 1 Rhydon       112 ground rock      75    265   105    130             45
    ## # ℹ 2 more variables: special.defense <int>, speed <int>

Now lets demonstrate how it functions with using a numeric Pokemon ID
and metric units. How about Pokemon 219?

``` r
pokemon.lookup(219, unit = "metric")
```

    ## # A tibble: 1 × 11
    ##   name     id.number type1 type2 height weight    hp attack special.attack
    ##   <chr>        <int> <chr> <chr>  <dbl>  <dbl> <int>  <int>          <int>
    ## 1 Magcargo       219 fire  rock     0.8     55    60     50             90
    ## # ℹ 2 more variables: special.defense <int>, speed <int>

## Data Exploration

A narrative showing a data exploration activity will go here. I have a
preliminary idea of what I would like to do, depending on the
information