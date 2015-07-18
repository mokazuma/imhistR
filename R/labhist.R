#' Provide image Lab color space histgram
#'
#' Analysis an image from .jpg or .png file. Draw L or a or b color space histgram of pixel.
#' Calculate mean, sd, skewness, and kurtsis for each histgram.
#'
#' @param input Set file, folder or url for analysis corresponding to mode parameter.
#' @param mode Select a mode in all four modes. Modes are "file"(default), "url", "folder", and "scraping".
#' @param hist Whether histgram draw or not. Dafult is draw. However, you should set FALSE when you want fast computation.
#' Also this parameter is used for output name when you use folder or scraping mode (Default output name is "histgram").
#' @param resize file
#'
#' @return image histgram and thier descriptive stastics (Lab color space)
#'
#' @export
#'
#' @examples
#' # Simple use is only set input image file.
#' labhist(system.file("img", "newlogo.png", package="imhistR"))
#' # labhist("img.png")  # you can use like this.
#'
#' # Url mode needs to input image URL.
#' # Only URL tail ".jpg" or ".png" can analysis.
#' url <- "http://www.r-project.org/Rlogo.png"
#' labhist(url, mode="url")
#'
#'

labhist <- function(input, mode="file", hist="histgram", resize=FALSE) {
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
        cat(paste0(hist, type, " in ", current, "\n"))  # return message
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
      savedat <- matrix(0, nrow = filNum, ncol = 12)
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
        ##### thumbnail rescale (under 9000 pixel)
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

        ##### LAB calculate
        if(resize) {
          img2 <- array(0, dim = c(dim(img)[1]*resize, dim(img)[2]*resize, 3))
          img2[,,1] <- mmand::rescale(img[,,1], resize, mmand::mnKernel())
          img2[,,2] <- mmand::rescale(img[,,2], resize, mmand::mnKernel())
          img2[,,3] <- mmand::rescale(img[,,3], resize, mmand::mnKernel())
          img <- img2
        }
        dat <- array(0, c(nrow(img), ncol(img), 3))
        if(nrow(img) < ncol(img)) {
          for(i in 1:ncol(img))   dat[,i,] <- patchPlot::RGB2Lab(img[,i,1:3])
        } else {
          for(i in 1:nrow(img))   dat[i,,] <- patchPlot::RGB2Lab(img[i,,1:3])
        }
        ##### color settings
        val <- c("L* dimension", "A* dimension", "B* dimension")
        hcol <- c("black", "white", "green", "magenta", "blue", "yellow")
        xrange <- c(100, 220, 220)
        leg <- data.frame(c(0, 50, 100), c(-110, 0, 110), c(-110, 0, 110))

        ##### print settings
        if(mode=="scraping")  setwd(current)
        ##### plot loop
        p <- list(); imgall <- NULL; maxval <- NULL;
        for(i in 1:3) {
          ###### stastics
          imgdat <- tidyr::gather(data.frame(dat[,,i]), pixel, value)
          if(i==1)  imgdat <- dplyr::filter(imgdat, dplyr::between(value, 1, 99))
          if(i==2)  imgdat <- dplyr::filter(imgdat, dplyr::between(value, -109, 109))
          if(i==3)  imgdat <- dplyr::filter(imgdat, dplyr::between(value, -109, 109))
          imgsta <- c(as.numeric(unlist(dplyr::summarise(imgdat, mean(value), sd(value)))),
                      e1071::skewness(imgdat$value, type=2), e1071::kurtosis(imgdat$value, type=2))
          imgall <- c(imgall, imgsta)
          ##### histgram
          if(hist!=FALSE) {
            g <- ggplot2::ggplot(imgdat, ggplot2::aes(x=value, fill=..x..)) +
              ggplot2::stat_bin(binwidth = xrange[i]/255) +
              ggplot2::ylab("Number of pixels") + ggplot2::xlab(val[i]) +
              ggplot2::theme_bw(base_size=16) +
              ggplot2::theme(legend.position="bottom", legend.title=ggplot2::element_blank(),
                             plot.title=ggplot2::element_text(size=14)) +
              ggplot2::scale_fill_gradient(low=hcol[2*i-1], high=hcol[2*i],
                                           breaks=leg[,i], labels=leg[,i], space="Lab") +
              ggplot2::ggtitle(paste0("Mean = ",round(imgsta[1],2), ", SD = ",round(imgsta[2],2), "  ",
                                      "\nSkew = ",round(imgsta[3],2), ", Kurt = ",round(imgsta[4],2)))
            if((dim(dat)[1]*dim(dat)[2]) > 1e+06) {
              g <- g + ggplot2::scale_y_continuous(labels=scales::scientific_format())
            } else {
              g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
            }
            p <- c(p, list(g))
          }
        }
        savedat[f,] <- imgall
        if(hist!=FALSE) {
          ##### multiple plot
          grid::grid.newpage()
          grid::pushViewport(
            grid::viewport(layout = grid::grid.layout(2, 3, heights = grid::unit(c(1, 9), "null"))))
          print(p[[1]] + ggplot2::xlim(0, 100),    vp=grid::viewport(layout.pos.row=2, layout.pos.col=1))
          print(p[[2]] + ggplot2::xlim(-110, 110), vp=grid::viewport(layout.pos.row=2, layout.pos.col=2))
          print(p[[3]] + ggplot2::xlim(-110, 110), vp=grid::viewport(layout.pos.row=2, layout.pos.col=3))
          grid::grid.text(datfil[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                          gp=grid::gpar(fontsize=35))
          grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=1, just=c('centre','top')))
          grid::grid.draw(grid::rasterGrob(imgp, interpolate=TRUE))
        }
      }
      ### print end
      colnames(savedat) <- c("Mean_L*dim.", "SD_L*dim.", "Skew_L*dim.", "Kurt_L*dim.",
                             "Mean_A*dim.", "SD_A*dim.", "Skew_A*dim.", "Kurt_A*dim.",
                             "Mean_B*dim.", "SD_B*dim.", "Skew_B*dim.", "Kurt_B*dim.")
      row.names(savedat) <- datfil
      if((hist!=FALSE && mode=="folder") || (hist!=FALSE && mode=="scraping")) {
        dev.off()
        cat(paste0(hist, ".pdf in ", current, "\n"))  # return message and dat
      }
      return(savedat)
    },
    ##### error processing
    error = function(e) {
      message("Error has occurred. Please change some parameters")
      message(e)
      if(hist!=FALSE && mode=="folder" || mode=="scraping")  dev.off()
    },
    silent=TRUE)
}
