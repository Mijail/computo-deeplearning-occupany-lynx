Resize_good <- function (size, method = "crop", pad_mode = "reflection", resamples = list(2, 0)) {
  args <- list(size = as.integer(size), method = method, pad_mode = pad_mode, 
               resamples = as.list(as.integer(unlist(resamples))))
  do.call(vision()$all$Resize, args)
}



path = "pix/test/"
train = "train"
valid = "valid"
valid_pct = NULL
seed = NULL
vocab = NULL
item_tfms = NULL
batch_tfms = NULL
bs = 64
val_bs = NULL
shuffle_train = TRUE
device = NULL
size = NULL


ImageDataLoaders_from_folder_good <- function (path, train = "train", valid = "valid", valid_pct = NULL, 
          seed = NULL, vocab = NULL, item_tfms = NULL, batch_tfms = NULL, 
          bs = 64, val_bs = NULL, shuffle_train = TRUE, device = NULL, 
          size = NULL, ...) 
{
  args <- list(path = path, train = train, valid = valid, valid_pct = valid_pct, 
               seed = seed, vocab = vocab, item_tfms = item_tfms, batch_tfms = batch_tfms, 
               bs = as.integer(bs), val_bs = val_bs, shuffle_train = shuffle_train, 
               device = device, size = size, ...)
  if (!is.null(args$batch_tfms)) {
    args$batch_tfms <- unlist(args$batch_tfms)
  }
  if (!is.null(args$size)) {
    args$size = as.integer(args$size)
  }
  if (!is.null(args$val_bs)) {
    args$val_bs <- as.integer(args$val_bs)
  }
  do.call(vision()$all$ImageDataLoaders$from_folder, args)
}





