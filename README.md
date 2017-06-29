# shinyApps

You can access these apps directly on the web
* [catapult](https://durrande.shinyapps.io/catapult/)
* [GP playground](https://durrande.shinyapps.io/gp_playground/)

If you want to run them locally, you'll need *R* with the *shiny* package.

```R
library(shiny)
runGitHub("shinyApps",username="NicolasDurrande",subdir="GP_playground")
runGitHub("shinyApps",username="NicolasDurrande",subdir="catapult")
```

Have fun, any feedback is more than welcome!
