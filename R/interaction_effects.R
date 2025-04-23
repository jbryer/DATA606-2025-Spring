baseline <- read.csv('course_data/baselinevars.csv')

z <- baseline$bods
x <- baseline$boasev
y <- baseline$bpa

library(plot3D)
library(ggplot2)


lm(bods ~ boasev + bpa, data = baseline) |> summary()

lm(bods ~ boasev * bpa, data = baseline) |> summary()


ggplot2::theme_set(ggplot2::theme_minimal())

scatterplot3d::scatterplot3d(
	x = baseline$boasev,
	y = baseline$bpa,
	z = baseline$bods,
	xlab = "Anxiety Severity",
	ylab = "Positive Affect",
	zlab = "Depression Severity"
)

grid.lines = 21
fit.dep1b <- lm(bods ~ boasev * bpa, data = baseline)
x.pred <- seq(min(baseline$boasev), max(baseline$boasev), length.out = grid.lines)
y.pred <- seq(min(baseline$bpa), max(baseline$bpa), length.out = grid.lines)
xy <- expand.grid(boasev = x.pred, bpa = y.pred)
z.pred <- matrix(predict(fit.dep1b, newdata = xy),
				 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit.dep1b)

plot3D::scatter3D(
	x = baseline$boasev,
	y = baseline$bpa,
	z = baseline$bods,
	pch = 21, cex = .6, cex.lab = .7, bty = "b2",
	theta = 25,  phi = -2,
	ticktype = "detailed",
	zlim = c(-4, 21.5),
	xlab = "Anxiety Severity",
	ylab = "Positive Affect",
	zlab = "Depression Severity",
	surf = list(x = x.pred, y = y.pred, z = z.pred,
				facets = NA,
				col="grey75"),
	colkey = F,
	col = "steelblue4",
	main = "",
	plot = T
)


baseline$affect <- cut(
	x = baseline$bpa,
	# breaks = quantile(baseline$bpa, probs = c(0, 0.3, 0.6, 1)),
	# labels = c('Low Affect', 'Medium Affect', 'High Affect'),
	breaks = quantile(baseline$bpa, probs = seq(0, 1, 0.25)),
	labels = c('Low Affect', 'Medium-Low Affect', 'Medium-High', 'High Affect'),
	include.lowest = TRUE)

baseline$affect |> table(useNA = 'ifany')

ggplot(baseline, aes(x = boasev, y = bods, color = affect)) +
	geom_point(alpha = 0.4) +
	geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
	facet_grid(~ affect, margins = TRUE) +
	theme(legend.position = 'none')

# ggplot(baseline, aes(x = boasev, y = bods, color = affect)) +
# 	geom_point(alpha = 0.4) +
# 	geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
# 	facet_wrap(~ affect, nrow = 1) +
# 	theme(legend.position = 'none')

high_affect <- baseline |> dplyr::filter(bpa >= 40)
low_affect <-  baseline |> dplyr::filter(bpa <= 20)

x_limits <- range(baseline$boasev)
y_limits <- range(baseline$bods)

p_high <- ggplot(high_affect, aes(x = boasev, y = bods)) +
	geom_point() +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
	xlim(x_limits) + ylim(y_limits) +
	ggtitle("High Positive Affect")

p_low <- ggplot(low_affect, aes(x = boasev, y = bods)) +
	geom_point() +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
	xlim(x_limits) + ylim(y_limits) +
	ggtitle("Low Positive Affect")

cowplot::plot_grid(p_high, p_low, nrow = 1)
