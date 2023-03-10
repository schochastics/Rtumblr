% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blogs.R
\name{get_blog_avatar}
\alias{get_blog_avatar}
\title{Retrieve a Blog Avatar
You can get a blog's avatar in 9 different sizes}
\usage{
get_blog_avatar(blog, size = 64)
}
\arguments{
\item{blog}{name of the blog}

\item{size}{Integer. The size of the avatar (square, one value for both length and width). Must be one of the values:  16, 24, 30, 40, 48, 64, 96, 128, 512}
}
\value{
png of avatar
}
\description{
Retrieve a Blog Avatar
You can get a blog's avatar in 9 different sizes
}
\examples{
\dontrun{
avatar <- get_blog_avatar("schochastics")
png::writePNG("avatar_schochastics.png")
}
}
