# wildlife-tracker-dev
Dev build of a wildlife/bear tracking app

URL used to populate WARP map (https://warp.wildsafebc.com/warp/):
https://warp.wildsafebc.com/warp/service/map_read?sort=encounter_date+desc&filter=(location_approximate+%3D+0+OR+location_approximate+%3D+1+OR+encounter_media_reference+%3D+1)+AND+(encounter_date+%3E%3D+CAST(%272019-06-10+10%3A52%3A28%27+AS+DATETIME)+AND+encounter_date+%3C%3D+CAST(%272019-06-17+10%3A52%3A28%27+AS+DATETIME))&record=%7B%7D&_=1560793941886

But this also works.
https://warp.wildsafebc.com/warp/service/map_read?sort=encounter_date+desc&_=1560793941886