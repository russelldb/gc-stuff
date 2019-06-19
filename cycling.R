# with thanks to https://www.gribble.org/
dtl <- function(loss ) {
    1 - (loss / 100)
}

mph_to_ms <- function(mph) {
    mph * 0.44704
}

drag <- function(cda, rho, mph) {
    0.5 * cda * rho * (mph_to_ms(mph) ^ 2)
}

rr <- function(wkg, gpct, crr) {
    9.8067 * cos(atan(gpct/100)) * wkg * crr
}

gforce <- function(wkg, gpct) {
    9.8067 * sin(atan(gpct/100)) * wkg
}

rforces <- function(Weight, Grade, Crr, Cda, Rho, MPH) {
    gforce(Weight, Grade) + rr(Weight, Grade, Crr) + drag(Cda, Rho, MPH)
}

resist <- function(Weight, Grade, Crr, Cda, Rho, MPH, loss=0) {
    (dtl(loss) ^ -1) * rforces(Weight, Grade, Crr, Cda, Rho, MPH) * mph_to_ms(MPH)
}

# https://stagescycling.com/us/news/what-it-means-watts-to-kjs-to-kcals/
kj <- function(Watts, Seconds) {
    (Watts * Seconds) / 1000
}

watts <- function(Kj, Seconds) {
    (Kj * 1000) / Seconds
}

seconds <- function(Hours=0, Minutes=0, Seconds=0) {
    (Hours * 3600) + (Minutes * 60) + Seconds
}

cals <- function(Kj) {
    (Kj / 4.186) / .22
}

kj_from_cals <- function(Cals) {
    (Cals * 4.186) * .22
}

hp <- function(Watts) {
    Watts / 746
}
