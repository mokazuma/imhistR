#' Provide image Luminance & RGB histogram
#'
#' Analysis an image from .bmp, .jpg(.jpeg) and .png file. Draw luminance, read, blue, and green histogram of pixels in ggplot2.
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
#' @return image histogram and thier descriptive stastics (Luminance & RGB). Folder and scraping mode provide a pdf file.
#' Range of all values are 0-1. Mathematical formula of luminance is 0.298912*red + 0.586611*green + 0.114478*blue.
#'
#' @export
#'
#' @examples
#' # Simple use is only set input an image file name. Japanese file name is accepted.
#' originaidir <- getwd()  # get current dir
#' setwd(system.file("extdata", package="imhistR"))  # set analysis dir
#' lrgbhist("Newlogo.png", endoff=TRUE)
#' setwd(originaidir)      # set original dir
#'
#' # lrgbhist("yourfile.jpg")  # you can use like this after set image dir.
#'
#'
#' # Url mode needs to input image URL.
#' # Only URL tail ".bmp", ".jpg" or ".png" can analyze.
#' url <- "https://www.r-project.org/Rlogo.png"
#' lrgbhist(input=url, mode="url", output="Rlogo", endoff=TRUE)
#'
#' # If you have an image folder in your PC, easily analyze all images by using folder mode.
#' # Althogh the type of these images are limited ".bmp", ".jpg" or ".png", three type files in a folder can analyze by one command.
#' # Histogram is provided by a pdf file.
#' lrgbhist(input=setwd(system.file("extdata", package="imhistR")), mode="folder", output="Rlogo", endoff=TRUE)
#'
#' # When you analyze your picture folder, many time may have need.
#' # Resize give fast drawing of histogram. Compressed method is kernel.
#' lrgbhist(input="folder name of iphone picture", mode="folder", resize=1/4)
#'
#'
#' # Web scraping from google image search is conducted by scraping mode. Twenty images were automatically downloaded and analyzed.
#' # So many scraping should avoid in order to conform web manner.
#' # Histogram is provided by a pdf file.
#' url <- "url from google image search of xxx"  # This package does not provide the way to scraping other web pages
#' lrgbhist(input=url, mode="scraping")
#'
#'

lrgbhist <- function(input, mode="file", output=input, hist=TRUE,
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
                               sprintf("%s/%s/%s_%02d%s", current,"scraping", output, i, filetype),
                               mode="wb")
          Sys.sleep(1)
        }
        datfil <- dir(path=paste0(current, "/", mode), pattern=filetype)
        filNum <- length(list.files(path = paste0(current, "/", mode), pattern = ".jpg"))
      }

      ##### main loop
      savedat <- matrix(0, nrow = filNum, ncol = 16)
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

        ##### Luminance & RGB calculation
        if(resize) {
          red   <- mmand::rescale(img[,,1], resize, mmand::mnKernel())
          green <- mmand::rescale(img[,,2], resize, mmand::mnKernel())
          blue  <- mmand::rescale(img[,,3], resize, mmand::mnKernel())
        } else {
          red <- img[,,1]; green <- img[,,2]; blue  <- img[,,3]
        }
        luminance <- .298912 * red + .586611 * green + .114478 * blue
        dat <- list(luminance, red, green, blue)
        ##### color settings
        val <- c("Luminance", "Red", "Green", "Blue")
        hcol <- c("black", "white", "black", "red", "black", "green", "black", "blue")

        ##### print settings
        if(mode=="scraping")  setwd(current)
        ##### plot loop
        p <- list(); maxval <- NULL; imgall <- NULL
        for(i in 1:4) {
          ###### stastics
          imgdat <- tidyr::gather(data.frame(dat[i]), pixel, value)
          if(endoff)  imgdat <- dplyr::filter(imgdat, dplyr::between(value, 0.01, 0.99))
          imgval <- na.omit(dplyr::select(imgdat, value))
          imgsta <- c(as.numeric(unlist(dplyr::summarise(imgval, mean(value), sd(value)))),
                      e1071::skewness(imgval$value, type=2), e1071::kurtosis(imgval$value, type=2))
          imgall <- c(imgall, imgsta)
          ##### histogram
          if(hist==TRUE) {
            g <- ggplot2::ggplot(imgdat, ggplot2::aes(x=value, fill=..x..)) +
              ggplot2::stat_bin(binwidth = 1/255) +
              ggplot2::ylab("Number of pixels") + ggplot2::xlab(val[i]) + ggplot2::xlim(0, 1) +
              ggplot2::theme_bw(base_size=textsize) +
              ggplot2::theme(legend.position="", plot.title=ggplot2::element_text(size=textsize-2)) +
              ggplot2::scale_fill_gradient(low=hcol[2*i-1], high=hcol[2*i]) +
              ggplot2::ggtitle(paste0("Mean = ",round(imgsta[1],2), ", SD = ",round(imgsta[2],2),
                                      ", Skew = ",round(imgsta[3],2), ", Kurt = ",round(imgsta[4],2)))
            if((dim(red)[1]*dim(red)[2]) > 1e+06) {
              g <- g + ggplot2::scale_y_continuous(labels=scales::scientific_format())
            } else {
              g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
            }
            p <- c(p, list(g))
          }
          #maxval <- c(maxval, ggplot2::ggplot_build(g)$panel$ranges[[1]]$y.range[2])
        }
        savedat[f,] <- imgall
        ##### multiple plot
        if(hist==TRUE) {
          grid::grid.newpage()
          grid::pushViewport(
            grid::viewport(layout = grid::grid.layout(3, 4, heights=grid::unit(c(1, 7, 7), "null"))))
          print(p[[1]], vp = grid::viewport(layout.pos.row=2, layout.pos.col=1:2))
          print(p[[2]], vp = grid::viewport(layout.pos.row=2, layout.pos.col=3:4))
          print(p[[3]], vp = grid::viewport(layout.pos.row=3, layout.pos.col=1:2))
          print(p[[4]], vp = grid::viewport(layout.pos.row=3, layout.pos.col=3:4))
          if(Sys.info()[1]=="Windows") {
            windowsFonts(MEI = windowsFont("Meiryo"))
            grid::grid.text(title[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                            gp=grid::gpar(fontsize=2*textsize, fontfamily="MEI"))
          } else {
            grid::grid.text(title[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                            gp=grid::gpar(fontsize=2*textsize, fontfamily="Meiryo"))
          }
          grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=1, just=c('centre','top')))
          grid::grid.draw(grid::rasterGrob(imgp, interpolate=FALSE))
        }
      }
      ##### save plot and stastical dat
      colnames(savedat) <- c("Mean_Luminance", "SD_Luminance", "Skew_Luminance", "Kurt_Luminance",
                             "Mean_Red", "SD_Red", "Skew_Red", "Kurt_Red",
                             "Mean_Green", "SD_Green", "Skew_Green", "Kurt_Green",
                             "Mean_Blue", "SD_Blue", "Skew_Blue", "Kurt_Blue")
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
