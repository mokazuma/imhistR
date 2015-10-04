#' Provide image HSB(HSV) color space histogram
#'
#' Analysis an image from .bmp, .jpg(.jpeg) and .png file. Draw Hue, Saturation, and Brightness(Value) color space histogram of pixel in ggplot2. HSB color space computed from RGB value.
#' Calculate mean, sd, skewness, and kurtsis for each histogram.
#'
#' @param input Set file, folder or url for image analysis corresponding to mode parameter.
#' @param mode Select a mode in all four modes. Modes are "file"(default), "url", "folder", and "scraping".
#' @param output Set the name of output histogram and data. Each mode have default output name.
#' @param hist Whether histogram draw or not. Dafult is draw.
#' However, you should set FALSE when you want fast computation for images of digital camera and smartphone.
#' Rendering of ggplot2 is so long for these large pixels image.
#' @param resize This argument is important to process many image histogram fastly.
#' If you set resize=1/4 or 1/8, the speed of drawing histogram is dramatically up although output values are approximation.
#' Resize value indicate image compression ratio. Resize is recommended when you use folder mode and want to get many histogram.
#' @param endoff If you want to get rid of image borders extreme value (white or black frame), you set this parameter TRUE.
#' @param textsize Font size of histogram caption. Dafult setting is textsize=16.
#'
#' @return image histogram and thier descriptive stastics (HSB color space). Folder and scraping mode provide a pdf file.
#' Range of all values are 0-1.
#'
#' @export
#'
#' @examples
#' # Simple use is only set input an image file name. Japanese file name is accepted.
#' originaidir <- getwd()  # get current dir
#' setwd(system.file("extdata", package="imhistR"))  # set analysis dir
#' hsbhist("Newlogo.png", endoff=TRUE)
#' setwd(originaidir)      # set original dir
#'
#' # hsbhist("yourfile.jpg")  # you can use like this after set image dir.
#'
#'
#' # Url mode needs to input image URL.
#' # Only URL tail ".bmp", ".jpg" or ".png" can analyze.
#' url <- "http://www.ess.ic.kanagawa-it.ac.jp/std_img/colorimage/Mandrill.jpg"
#' hsbhist(input=url, mode="url", output="Mandrill")
#'
#'
#' # If you have an image folder in your PC, easily analyze all images by using folder mode.
#' # Althogh the type of these images are limited ".bmp", ".jpg" or ".png", three type files in a folder can analyze by one command.
#' # Histogram is provided by a pdf file.
#' hsbhist(input=setwd(system.file("extdata", package="imhistR")), mode="folder", output="Rlogo", endoff=TRUE)
#'
#' # When you analyze your picture folder, many time may have need.
#' # Resize give fast drawing of histogram. Compressed method is kernel.
#' hsbhist(input="folder name of iphone picture", mode="folder", resize=1/4)
#'
#'
#' # Web scraping from google image search is conducted by scraping mode. Twenty images were automatically downloaded and analyzed.
#' # So many scraping should avoid in order to conform web manner.
#' # If you already scraping image by other function of this package, you should use folder mode.
#' # Histogram is provided by a pdf file.
#' url <- "url from google image search of xxx"  # This package does not provide the way to scraping other web pages
#' hsbhist(input=url, mode="scraping")
#'
#'

hsbhist <- function(input, mode="file", output=input, hist=TRUE,
                    resize=FALSE, endoff=FALSE, textsize=16) {
  ##### set print
  if(hist==TRUE && (mode=="folder" || mode=="scraping")) {
    if(!(is.element(input, output))) {
      cpdf <- paste0(output,".pdf")
    } else {
      if(mode=="folder")    cpdf <- "dir.pdf"
      if(mode=="scraping")  cpdf <- "webfile.pdf"
    }
    if(Sys.info()[1]!="Windows")  Cairo::CairoFonts(regular="Meiryo:style=Medium")
    Cairo::CairoPDF(cpdf, paper="a4r", width=11.69, height=8.27)
  }
  tryCatch(
    { ##### set input filetype
      if(grepl(".bmp",  tolower(input), fixed=TRUE))   filetype <- ".bmp"
      if(grepl(".jpg",  tolower(input), fixed=TRUE))   filetype <- ".jpg"
      if(grepl(".jpeg", tolower(input), fixed=TRUE))   filetype <- ".jpeg"
      if(grepl(".png",  tolower(input), fixed=TRUE))   filetype <- ".png"
      ##### set mode: file, url, dir, scraping
      current <- getwd()
      if(mode=="file") {
        datfil <- input; filNum <- 1
      } else if(mode=="url") {
        if(is.element(input, output))   output <- "webfile"
        downloader::download(input, paste0(current, "/", output, filetype), mode="wb")
        datfil <- paste0(output, filetype); filNum <- 1
      } else if(mode=="folder") {
        datfil <- c(dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".bmp"),
                    dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".jpg"),
                    dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".jpeg"),
                    dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".png") )
        filNum <- length(datfil)
      } else if(mode=="scraping") {
        if(is.element(input, output))  output <- "webfile"
        filetype <- ".jpg"
        if (!file.exists(paste0(current, "/", mode)))  dir.create(mode)     # folder check
        cat(paste0(mode, " folder in ", current, "\n"))  # return message
        webinf <- rvest::html(input)
        imgnod <- rvest::html_nodes(webinf, "img")
        nodtext <- rvest::html_attrs(imgnod)
        for(i in 1:length(nodtext)) {
          downloader::download(as.character(nodtext[[i]][2]),
                               sprintf("%s/%s/%s_%02d%s", current, "scraping", output, i, filetype), 
                               mode="wb")
          Sys.sleep(1)
        }
        datfil <- dir(path=paste0(current, "/", mode), pattern=filetype)
        filNum <- length(list.files(path = paste0(current, "/", mode), pattern = ".jpg"))
      }

      ##### main loop
      savedat <- matrix(0, nrow = filNum, ncol = 12)
      if(mode=="file" && !(is.element(input, output)))  {
        title <- paste0(output, filetype)
      } else if(mode=="folder") {
        title <- gsub(paste0(input,"/"), "", datfil)
      } else {
        title <- datfil
      }
      for(f in 1:filNum) {
        ##### read dat
        if(mode=="folder" || mode=="scraping") {
          cat(paste0("Processing ", title[f], "(", f, "/", filNum, ")", "...\n"))
        }
        if(mode=="scraping")  setwd(paste0(current, "/scraping"))
        img <- readbitmap::read.bitmap(datfil[f])  # read file
        if(mode=="url")       file.remove(datfil[f])

        ##### thumbnail rescale (under 90000 pixel)
        dim1 <- dim(img)[1]; dim2 <- dim(img)[2]
        if(dim1>300 && dim2>300) {
          while(dim1 > 200)  dim1 <- dim1 / 2
          while(dim2 > 200)  dim2 <- dim2 / 2
          resizep <- 1 / min(dim(img)[1] / dim1, dim(img)[2] / dim2)
          redp   <- mmand::rescale(img[,,1], resizep, mmand::mnKernel())
          greenp <- mmand::rescale(img[,,2], resizep, mmand::mnKernel())
          bluep  <- mmand::rescale(img[,,3], resizep, mmand::mnKernel())
          imgp <- array(0, dim = c(dim(redp)[1], dim(redp)[2], 3))
          imgp[,,1] <- redp; imgp[,,2] <- greenp; imgp[,,3] <- bluep;
          imgp <- (imgp - min(imgp)) / (max(imgp) - min(imgp))
        } else {
          imgp <- img
        }

        ##### HSV calculate
        dat <- array(0, c(nrow(img), ncol(img), 3))
        if(nrow(img) < ncol(img)) {
          for(i in 1:ncol(img))   dat[,i,] <- t(rgb2hsv(t(img[,i,1:3]), maxColorValue=1))
        } else {
          for(i in 1:nrow(img))   dat[i,,] <- t(rgb2hsv(t(img[i,,1:3]), maxColorValue=1))
        }
        val <- c("Hue", "Saturation", "Brightness")
        if(resize) {
          dat2 <- array(0, dim=c(dim(dat)[1]*resize, dim(dat)[2]*resize, 3))
          dat2[,,1] <- mmand::rescale(dat[,,1], resize, mmand::mnKernel())
          dat2[,,2] <- mmand::rescale(dat[,,2], resize, mmand::mnKernel())
          dat2[,,3] <- mmand::rescale(dat[,,3], resize, mmand::mnKernel())
          dat <- dat2
        }
        ##### color settings
        s <- 1; v <- .8; leg <- c(0, .5, 1)
        #pals <- c("red","yellow","green","cyan","blue","magenta", "red")
        pals <- c(hsv(0, s, v), hsv(1/6, s, v), hsv(2*(1/6), s, v), hsv(3*(1/6), s, v),
                  hsv(4*(1/6), s, v), hsv(5*(1/6), s, v), hsv(1, s, v))

        ##### print settings
        if(mode=="scraping")  setwd(current)
        ##### plot loop
        p <- list(); imgall <- NULL; maxval <- NULL
        for(i in 1:3) {
          ###### stastics
          imgdat <- tidyr::gather(data.frame(dat[,,i]), pixel, value)
          if(endoff)  imgdat <- dplyr::filter(imgdat, dplyr::between(value, 0.01, 0.99))
          imgval <- na.omit(dplyr::select(imgdat, value))
          imgsta <- c(as.numeric(unlist(dplyr::summarise(imgval, mean(value), sd(value)))),
                      e1071::skewness(imgval$value, type=2), e1071::kurtosis(imgval$value, type=2))
          imgall <- c(imgall, imgsta)
          if(i==1) {
            dist <- dplyr::summarise(dplyr::group_by(dplyr::mutate(
              imgdat, range = cut(value, breaks=seq(0, 1, 1/70))), range), v=n())
            maxdist <- as.numeric(dist$range[which.max(dist$v)])
            maxhue <- ((1/70 * maxdist) + (1/70 * (maxdist-1))) / 2
            if(is.na(maxhue))  maxhue <- 0
          }
          ##### histogram
          if(hist==TRUE) {
            g <- ggplot2::ggplot(imgdat, ggplot2::aes(x=value, fill=..x..)) +
              ggplot2::stat_bin(binwidth = 1/70) +
              ggplot2::ylab("Number of pixels") + ggplot2::xlab(val[i]) +
              ggplot2::xlim(0, 1) + ggplot2::theme_bw(base_size=textsize) +
              ggplot2::theme(legend.position="bottom", legend.title=ggplot2::element_blank(),
                             plot.title=ggplot2::element_text(size=textsize-2)) +
              ggplot2::ggtitle(paste0("Mean = ",round(imgsta[1],2), ", SD = ",round(imgsta[2],2), "   ",
                                      "\nSkew = ",round(imgsta[3],2), ", Kurt = ",round(imgsta[4],2)))
            if     (i==1)  g <- g + ggplot2::scale_fill_gradientn(colours=pals, breaks=leg, labels=leg, limits=c(0, 1))
            else if(i==2)  g <- g + ggplot2::scale_fill_gradient(low=hsv(maxhue, 0, v), high=hsv(maxhue, 1, v), breaks=leg, labels=leg)
            else           g <- g + ggplot2::scale_fill_gradient(low=hsv(maxhue, 1, 0), high=hsv(maxhue, 1, 1), breaks=leg, labels=leg)
            if((dim(dat)[1]*dim(dat)[2]) > 1e+06) {
              g <- g + ggplot2::scale_y_continuous(labels=scales::scientific_format())
            } else {
              g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
            }
            p <- c(p, list(g))
          }
        }
        savedat[f,] <- imgall
        if(hist==TRUE) {
          ##### multiple plot
          grid::grid.newpage()
          grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 3, heights = grid::unit(c(1, 9), "null"))))
          print(p[[1]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p[[2]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
          print(p[[3]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 3))
          if(Sys.info()[1]=="Windows") {
            windowsFonts(MEI = windowsFont("Meiryo"))
            grid::grid.text(title[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                            gp=grid::gpar(fontsize=2*textsize, fontfamily="MEI"))
          } else {
            grid::grid.text(title[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                            gp=grid::gpar(fontsize=2*textsize, fontfamily="Meiryo"))
          }
          grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=1, just=c('centre','top')))
          grid::grid.draw(grid::rasterGrob(imgp, interpolate=TRUE))
        }
      }
      colnames(savedat) <- c("Mean_Hue", "SD_Hue", "Skew_Hue", "Kurt_Hue",
                             "Mean_Saturation", "SD_Saturation", "Skew_Saturation", "Kurt_Saturation",
                             "Mean_Brightness", "SD_Brightness", "Skew_Brightness", "Kurt_Brightness")
      row.names(savedat) <- title
      if(hist==TRUE && (mode=="folder" || mode=="scraping")) {
        dev.off()
        if(!(is.element(input, output))) {
          cat(paste0(output, ".pdf in ", current, "\n"))  # return message
        } else {
          if(mode=="folder")    cat(paste0("dir.pdf in ", current, "\n"))  # return message
          if(mode=="scraping")  cat(paste0("webfile.pdf in ", current, "\n"))
        }
      }
      return(savedat)
    },
    ##### error processing
    error = function(e) {
      message("!! Error has occurred. Please change some parameters")
      message(e)
      if(hist==TRUE && (mode=="folder" || mode=="scraping"))  dev.off()
    },
    silent=TRUE)
}

