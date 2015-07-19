#' Provide image Luminance & RGB histgram
#'
#' Analysis an image from .jpg or .png file. Draw luminance, read, blue, and green histgram of pixel in ggplot2.
#' Calculate mean, sd, skewness, and kurtsis for each histgram.
#'
#' @param input Set file, folder or url for image analysis corresponding to mode parameter.
#' @param mode Select a mode in all four modes. Modes are "file"(default), "url", "folder", and "scraping".
#' @param hist Whether histgram draw or not. Dafult is draw. However, you should set FALSE when you want fast computation for images of digital camera and smartphone. Rendering of ggplot2 is so long for these large pixels image.
#' Also this parameter is used for output name when you use url, folder or scraping mode (Default output name is "histgram").
#' @param resize This argument is important to process many image histgram fastly. If you set resize=1/4, the speed of drawing histgram is dramatically up although output values are approximation.
#' Resize is recommended when you use folder mode and want to get many histgram.
#' @param endoff If you want to get include of image borders extreme value (white or black frame), you set this parameter FALSE.
#'
#' @return image histgram and thier descriptive stastics (Luminance & RGB). Folder and scraping mode provide a pdf file.
#' Range of all values are 0-1. Mathematical formula of luminance is 0.298912*red + 0.586611*green + 0.114478*blue.
#'
#' @export
#'
#' @examples
#' # Simple use is only set input a image file name. Japanese file name is accepted.
#' originaidir <- getwd()  # get current dir
#' setwd(system.file("img", package="imhistR"))  # set analysis dir
#' lrgbhist("Newlogo.png")
#' setwd(originaidir)  # set original dir
#'
#' # lrgbhist("yourfile.jpg")  # you can simply use like this.
#'
#'
#' # Url mode needs to input image URL.
#' # Only URL tail ".jpg" or ".png" can analyze.
#' url <- "http://www.r-project.org/Rlogo.png"
#' lrgbhist(input=url, mode="url", hist="Rlogo")
#'
#'
#' # If you have a image folder in your PC, easily analyze all images by using folder mode.
#' # Althogh the type of these images is limited ".jpg" or ".png", both type files in a folder can analyze one command.
#' lrgbhist(input=setwd(system.file("img", package="imhistR")), mode="folder", hist="Rlogo")
#'
#' # Resize give fast drawing of histgram. Compressed method is kernel.
#' lrgbhist(input="folder name of iphone picture", mode="folder", hist="iphoneImg", resize=1/4)
#'
#'
#' # Web scraping from google image search is conducted by scraping mode. Twenty images were automatically downloaded and analyzed.
#' # So many scraping should avoid in order to conform web manner.
#' url <- "url from google image search of xxx"  # This package does not provide the way to scraping other web pages
#' lrgbhist(input=url, mode="scraping", hist="xxx")
#'
#'

lrgbhist <- function(input, mode="file", hist="histgram", resize=FALSE, endoff=TRUE) {
  ##### set print
  if((hist!=FALSE && mode=="folder") || (hist!=FALSE && mode=="scraping")) {
    Cairo::CairoPDF(paste0(hist,".pdf"), paper="a4r", width=11.69, height=8.27)
  }
  tryCatch(
    { ##### set input type
      if(grepl(".jpg",  input, fixed = TRUE))   type <- ".jpg"
      if(grepl(".jpeg", input, fixed = TRUE))   type <- ".jpeg"
      if(grepl(".png",  input, fixed = TRUE))   type <- ".png"
      ##### set mode: file, url, dir, scraping
      current <- getwd()
      if(mode=="file") {
        datfil <- input; filNum <- 1
      } else if(mode=="folder") {
        datfil <- c(list.files(path=input, pattern=".jpg"), list.files(path=input, pattern=".png"),
                    list.files(path=input, pattern=".jpeg") )
        filNum <- length(datfil)
      } else if(mode=="url") {
        downloader::download(input, paste0(current, "/", hist, type), mode="wb")
        datfil <- paste0(hist, type); filNum <- 1
      } else if(mode=="scraping") {
        type <- ".jpg"
        if (!file.exists(paste0(current, "/", mode)))  dir.create(mode)     # folder check
        cat(paste0(mode, "folder in ", current, "\n"))  # return message
        webinf <- rvest::html(input)
        imgnod <- rvest::html_nodes(webinf, "img")
        nodtext <- rvest::html_attrs(imgnod)
        for(i in 1:length(nodtext)) {
          downloader::download(as.character(nodtext[[i]][2]),
                               sprintf("%s/%s/%s_%02d%s", current, mode, hist, i, type),
                               mode="wb")
          Sys.sleep(3)
        }
        datfil <- list.files(path=paste0(current, "/", mode), pattern=type)
        filNum <- length(nodtext)
      }

      ##### main loop
      savedat <- matrix(0, nrow = filNum, ncol = 16)
      for(f in 1:filNum) {
        if(mode=="folder" || mode=="scraping") {
          cat(paste0("Processing ", datfil[f], "(", f, "/", filNum, ")", "...\n"))
        }
        ##### read dat
        if(mode=="scraping")  setwd(paste0(current, "/scraping"))
        if(mode=="folder") {
          if(grepl(".jpg", datfil[f], fixed = TRUE))   type <- ".jpg"
          if(grepl(".jpeg", datfil[f], fixed = TRUE))  type <- ".jpeg"
          if(grepl(".png", datfil[f], fixed = TRUE))   type <- ".png"
        }
        if(type==".jpg" || type==".jpeg")   img <- jpeg::readJPEG(datfil[f])
        if(type==".png")                    img <- png::readPNG(datfil[f])
        if(mode=="url")  file.remove(datfil[f])
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

        ##### luminance & RGB calculate
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
          if(endoff) {
            imgval <- imgdat$value[(0.01 <imgdat$value) & (imgdat$value < 0.99)]
          } else {
            imgval <- imgdat$value
          }
          imgsta <- c(mean(imgval), sd(imgval), e1071::skewness(imgval, type=2), e1071::kurtosis(imgval, type=2))
          imgall <- c(imgall, imgsta); imgval <- data.frame(imgval)
          ##### histgram
          if(hist!=FALSE) {
            g <- ggplot2::ggplot(imgval, ggplot2::aes(x=imgval, fill=..x..)) +
              ggplot2::stat_bin(binwidth = 1/255) +
              ggplot2::ylab("Number of pixels") + ggplot2::xlab(val[i]) + ggplot2::xlim(0, 1) +
              ggplot2::theme_bw(base_size=16) +
              ggplot2::theme(legend.position="", plot.title=ggplot2::element_text(size=14)) +
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
        if(hist!=FALSE) {
          grid::grid.newpage()
          grid::pushViewport(
            grid::viewport(layout = grid::grid.layout(3, 4, heights=grid::unit(c(1, 7, 7), "null"))))
          print(p[[1]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1:2))
          print(p[[2]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 3:4))
          print(p[[3]], vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1:2))
          print(p[[4]], vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 3:4))
          grid::grid.text(datfil[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:4),
                          gp=grid::gpar(fontsize=35, fontfamily = "Meiryo"))
          grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=1, just=c('centre','top')))
          grid::grid.draw(grid::rasterGrob(imgp, interpolate=FALSE))
        }
      }
      ##### save plot and stastical dat
      colnames(savedat) <- c("Mean_Luminance", "SD_Luminance", "Skew_Luminance", "Kurt_Luminance",
                             "Mean_Red", "SD_Red", "Skew_Red", "Kurt_Red",
                             "Mean_Green", "SD_Green", "Skew_Green", "Kurt_Green",
                             "Mean_Blue", "SD_Blue", "Skew_Blue", "Kurt_Blue")
      row.names(savedat) <- datfil
      if((hist!=FALSE && mode=="folder") || (hist!=FALSE && mode=="scraping")) {
        dev.off()
        cat(paste0(hist, ".pdf in ", current, "\n"))  # return message and dat
      }
      return(savedat)
    },
    ##### error processing
    error = function(e) {
      message("!! Error has occurred. Please change some parameters")
      message(e)
      if(hist!=FALSE && mode=="folder" || mode=="scraping")  dev.off()
    },
    silent=TRUE)
}
