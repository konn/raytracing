# raytracing

Following [Ray Tracing in One Weekend][one week] in Haskell.

[one week]: https://raytracing.github.io/books/RayTracingInOneWeekend.html

Goals:

- We use massiv for image representation
  * At early stage, however, we don't resort to massiv-io's fancy features for image formats for the better understanding of what going on under the hood.
  * Eventually, we will switch to utilise `massiv-io`'s functionality to make things easy.
- OTOH, we use `linear` package, as we assume that we understand easy linear algebra enough ;-)
