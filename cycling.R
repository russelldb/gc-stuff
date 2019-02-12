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


