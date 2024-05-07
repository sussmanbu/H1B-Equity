standardize_country_names <- function(countries) {
  countries %>%
    tolower() %>%
    str_replace_all("china - mainland", "china") %>%
    str_replace_all("congo, democratic republic of the", "democratic replublic of congo") %>%
    str_replace_all("congo, republic of the", "republic of congo") %>%
    str_replace_all("congo (brazzaville)", "republic of congo") %>%
    str_replace_all("congo (kinshasa)", "democratic republic of congo") %>%
    str_replace_all("hong kong s.a.r.", "hong kong") %>%
    str_replace_all("cape verde", "cabo verde") %>%
    str_replace_all("hong kong sar", "hong kong") %>%
    str_replace_all("islamic republic of iran", "iran") %>%
    str_replace_all("kyrgyz republic", "kyrgyzstan") %>%
    str_replace_all("lao p.d.r.", "laos") %>%
    str_replace_all("macao sar", "macau") %>%
    str_replace_all("north macedonia", "macedonia") %>%
    str_replace_all("russian federation", "russia") %>%
    str_replace_all("korea, republic of", "south korea") %>%
    str_replace_all("taiwan province of china", "taiwan") %>%
    str_replace_all("united republic of tanzania", "tanzania") %>%
    str_replace_all("türkiye, republic of", "turkey") %>%
    str_replace_all("united kingdom", "great britain") %>%
    str_replace_all("bolivarian republic of venezuela", "venezuela") %>%
    str_replace_all("congo, dem. rep. of the", "democratic republic of congo") %>%
    str_replace_all("slovak republic", "slovakia") %>%
    str_replace_all("st lucia", "saint lucia") %>%
    str_replace_all("st vincent", "saint vincent and the grenadines") %>%
    str_replace_all("sri lanka", "sri lanka") %>%
    str_replace_all("bahamas, the", "bahamas") %>%
    str_replace_all("gambia, the", "gambia") %>%
    str_replace_all("myanmar \\(myanmar\\)", "myanmar") %>%
    str_replace_all("united states of america", "united states") %>%
    str_replace_all("china, people's republic of", "china") %>%
    str_replace_all("cote d'ivoire", "côte d'ivoire" )
}