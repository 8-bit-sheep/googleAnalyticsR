## get request with sampling

## if sampled, get samplingSpaceSizes / samplesReadCounts rounded up is number of batches needed.

## divide up the call into batches using date, hour and minute.
## end_date - start_date is number of days

## divide number of days by batches

## make new requests with date filter

## if number of days = 1, split call using hours

## if number of hours = 1, split call using minutes

## if number of minutes = 1, can't help you. Fetch what you can.


## if response unsampled, keep

## if repoonse still sampled, feed back into function

## output response

## rbind with other responses
