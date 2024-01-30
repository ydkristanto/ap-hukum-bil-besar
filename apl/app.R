# Paket ----
library(shiny)
library(tidyverse)

# UI ----
ui <- navbarPage(
  title = "Hukum Bilangan Besar",
  tabPanel(
    "Eksplorasi",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          ## Input statistik sampel ----
          sliderInput("n_sampel", "Ukuran sampel:",
                      min = 1, max = 1000,
                      value = 10),
          sliderInput("k_sampel", "Banyak sampel:",
                      min = 1, max = 5,
                      value = 1, step = 1)
        ),
        wellPanel(
          ## Input parameter populasi ----
          selectInput("dist_pop", "Distribusi populasi:",
                      choices = c("Normal" = "normal",
                                  "Binomial" = "binom",
                                  "Seragam" = "seragam",
                                  "Gamma" = "gamma"),
                      selected = "normal"),
          conditionalPanel("input.dist_pop == 'normal'",
                           sliderInput("rerata_pop", "Rerata:",
                                       min = 40, max = 60, value = 50,
                                       step = 1),
                           sliderInput("sd_pop", "Simpangan baku:",
                                       min = 1, max = 20, value = 10,
                                       step = 1)),
          conditionalPanel("input.dist_pop == 'binom'",
                           sliderInput("ukuran_pop", "Ukuran eksperimen:",
                                       min = 1, max = 20, value = 10,
                                       step = 1),
                           sliderInput("peluang_pop", "Peluang sukses:",
                                       min = 0, max = 1, value = .5)),
          conditionalPanel("input.dist_pop == 'seragam'",
                           sliderInput("minmaks_pop", "Minimum dan maksimum:",
                                       min = 30, max = 70, value = c(40, 60),
                                       step = 1)),
          conditionalPanel("input.dist_pop == 'gamma'",
                           sliderInput("bentuk_pop", "Bentuk:",
                                       min = .5, max = 5, value = 1.5,
                                       step = .5),
                           sliderInput("skala_pop", "Skala:",
                                       min = 1, max = 10, value = 5,
                                       step = .25))
        )
      ),
      mainPanel(
        tabsetPanel(
          ## Tab tren rerata ----
          tabPanel("Tren",
            br(),
            plotOutput("plot_tren"),
            textOutput("teks_tren"),
            br()
          ),
          ## Tab distribusi sampel ----
          tabPanel("Distribusi Sampel",
            br(),
            plotOutput("plot_dist"),
            textOutput("teks_dist"),
            br()
          )
        )
      )
    )
  ),
  tabPanel("Informasi",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 div(h4("Deskripsi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Aplikasi Shiny ini mendemonstrasikan Hukum Bilangan Besar."))
               ),
               wellPanel(
                 div(h4("Kode sumber",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-selang-kepercayaan", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-selang-kepercayaan/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-selang-kepercayaan/pulls", target = "_blank"), "di repositori tersebut."))
               ),
               wellPanel(
                 div(h4("Lisensi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Lisensi MIT"),
                     p("Copyright (c) 2024 Yosep Dwi Kristanto"))
               )
             ),
             mainPanel(
               div(h3("Aplikasi Shiny Selang Kepercayaan")),
               div(p("Aplikasi interaktif ini dimaksudkan untuk mendemonstrasikan selang-selang kepercayaan proporsi dan rerata yang mencakup parameter populasinya. Beberapa ide penting statistik yang ditunjukkan oleh aplikasi ini antara lain sebagai berikut."), align = "justify"),
               div(tags$ul(tags$li("Semakin besar tingkat kepercayaannya, semakin besar persentase selang kepercayaan yang mencakup parameter populasinya."),
                           tags$li("Semakin besar tingkat kepercayaannya, pias galatnya juga semakin besar. Hal ini ditunjukkan dengan semakin panjangnya selang kepercayaannya."),
                           tags$li("Untuk rerata, ketika ukuran sampelnya kecil dan populasinya tidak berdistribusi normal, hubungan antara tingkat kepercayaan dan persentase pencakupan semakin tidak menentu.")), align = "justify"),
               hr(),
               div(p("Aplikasi interaktif ini dikembangkan dengan menggunakan bahasa pemrogram", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah."), align = "justify"),
               div(p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta."), align = "justify"),
               width = 6)
           )
    
  )
)

# Peladen ----
server <- function(input, output) {
  seed <- as.numeric(Sys.Date())
  ## Membuat set sampel ----
  membuat_set_sampel <- function(n, k, dist = "normal",
                                 eks, pel, mean, sd, min, maks,
                                 bentuk, skala) {
    set.seed(seed)
    if (dist == "normal") {
      set_sampel <- tibble(
        id_sampel = rep(1:k, each = n),
        observasi = rep(1:n, k),
        nilai = c(replicate(k, round(rnorm(n, mean = mean, sd = sd), 2)))
      )
    } else if (dist == "binom") {
      set_sampel <- tibble(
        id_sampel = rep(1:k, each = n),
        observasi = rep(1:n, k),
        nilai = c(replicate(k, rbinom(n, size = eks, prob = pel)))
      )
    } else if (dist == "seragam") {
      set_sampel <- tibble(
        id_sampel = rep(1:k, each = n),
        observasi = rep(1:n, k),
        nilai = c(replicate(k, round(runif(n, min = min, max = maks), 2)))
      )
    } else if (dist == "gamma") {
      set_sampel <- tibble(
        id_sampel = rep(1:k, each = n),
        observasi = rep(1:n, k),
        nilai = c(replicate(k, round(rgamma(n, shape = bentuk,
                                            scale = skala), 2)))
      )
    } else {
      set_sampel <- tibble(
        id_sampel = rep(1:k, each = n),
        observasi = rep(1:n, k),
        nilai = c(replicate(k, round(rnorm(n), 2)))
      )
    }
    
    stat_sampel <- set_sampel %>% 
      group_by(id_sampel) %>% 
      mutate(id_sampel = factor(id_sampel), 
             rerata = cumsum(nilai) / seq_along(nilai))
    
    return(stat_sampel)
  }
  
  rep_membuat_set_sampel <- repeatable(membuat_set_sampel)
  
  stat_sampel <- reactive({
    data_stat_sampel <- membuat_set_sampel(n = input$n_sampel,
                                           k = input$k_sampel,
                                           dist = input$dist_pop,
                                           eks = input$ukuran_pop,
                                           pel = input$peluang_pop,
                                           mean = input$rerata_pop,
                                           sd = input$sd_pop,
                                           min = input$minmaks_pop[1],
                                           maks = input$minmaks_pop[2],
                                           bentuk = input$bentuk_pop,
                                           skala = input$skala_pop)
    
    return(data_stat_sampel)
  })
  
  ## Plot tren rerata ----
  output$plot_tren <- renderPlot({
    data_stat_sampel <- stat_sampel()
    
    dist <- input$dist_pop
    if (dist == "normal") {
      rerata_riil <- input$rerata_pop
    } else if (dist == "binom") {
      rerata_riil <- input$ukuran_pop * input$peluang_pop
    } else if (dist == "seragam") {
      rerata_riil <- (input$minmaks_pop[1] + input$minmaks_pop[2]) / 2
    } else if (dist == "gamma") {
      rerata_riil <- input$bentuk_pop * input$skala_pop
    }
    
    data_stat_sampel %>% 
      ggplot(aes(x = observasi, y = rerata,
                 group = id_sampel, color = id_sampel)) +
      geom_hline(yintercept = rerata_riil,
                 linewidth = 1,
                 linetype = "dashed") +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "ID Sampel") +
      theme_bw(base_size = 16) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom") +
      labs(title = "Tren Rerata Ketika Ukuran Sampel Naik",
           x = "Observasi", y = "Rerata")
  })
  
  ## Teks tren rerata ----
  output$teks_tren <- renderText({
    n <- input$n_sampel
    k <- input$k_sampel
    
    data_stat_sampel <- stat_sampel()
    ringkasan_stat <- data_stat_sampel %>% 
      group_by(id_sampel) %>% 
      summarise(rerata = round(mean(nilai), 2))
    vektor_rerata <- ringkasan_stat$rerata
    
    if (k == 1) {
      paste0("Gambar 1: Tren rerata sebuah sampel ketika ukurannya bertambah besar, dari 1 sampai ", n, ". Rerata terakhir nilai-nilai dalam sampel tersebut kurang lebih adalah ", vektor_rerata[1], ".")
    } else if (k == 2) {
      paste0("Gambar 1: Tren rerata dua sampel ketika ukurannya bertambah besar, dari 1 sampai ", n, ". Rerata terakhir sampel-sampel tersebut secara berturut-turut kurang lebih sama dengan ", vektor_rerata[1], " dan ", vektor_rerata[2], ".")
    } else if (k == 3) {
      paste0("Gambar 1: Tren rerata tiga sampel ketika ukurannya bertambah besar, dari 1 sampai ", n, ". Rerata terakhir sampel-sampel tersebut secara berturut-turut kurang lebih sama dengan ", vektor_rerata[1], ", ", vektor_rerata[2], ", dan ", vektor_rerata[3], ".")
    } else if (k == 4) {
      paste0("Gambar 1: Tren rerata empat sampel ketika ukurannya bertambah besar, dari 1 sampai ", n, ". Rerata terakhir sampel-sampel tersebut secara berturut-turut kurang lebih sama dengan ", vektor_rerata[1], ", ", vektor_rerata[2], ", ", vektor_rerata[3], ", dan ", vektor_rerata[4], ".")
    } else if (k == 5) {
      paste0("Gambar 1: Tren rerata lima sampel ketika ukurannya bertambah besar, dari 1 sampai ", n, ". Rerata terakhir sampel-sampel tersebut secara berturut-turut kurang lebih sama dengan ", vektor_rerata[1], ", ", vektor_rerata[2], ", ", vektor_rerata[3], ", ", vektor_rerata[4], ", dan ", vektor_rerata[5], ".")
    } else {
      "Gambar 1: Tren rerata sampel ketika ukurannya bertambah besar."
    }
    
  })
  
  
  ## Plot distribusi sampel ----
  output$plot_dist <- renderPlot({
    data_stat_sampel <- stat_sampel()
    
    k <- input$k_sampel
    if (k == 1) {
      data_stat_sampel %>% 
        ggplot(aes(x = nilai)) +
        geom_histogram(aes(y = after_stat(density)),
                       color = "white", fill = "#1b9e77") +
        geom_density(linewidth = 1.5) +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom",
              plot.title = element_text(face = "bold"),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        labs(title = paste0("Distribusi Data Sampel"),
             x = "Nilai")
    } else {
      data_stat_sampel %>% 
        ggplot(aes(x = nilai)) +
        geom_histogram(aes(y = after_stat(density),
                           fill = id_sampel),
                       color = "white") +
        geom_density(linewidth = 1.5, show.legend = FALSE) +
        facet_grid(id_sampel ~ .) +
        theme_bw(base_size = 16) +
        scale_fill_brewer(palette = "Dark2",
                          name = "ID Sampel") +
        theme(legend.position = "bottom",
              plot.title = element_text(face = "bold"),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        labs(title = paste0("Distribusi Data ", k, " Sampel"),
             x = "Nilai")
    }
  })
  
  ## Teks distribusi sampel ----
  output$teks_dist <- renderText({
    n <- input$n_sampel
    k <- input$k_sampel
    
    data_stat_sampel <- stat_sampel()
    ringkasan_stat <- data_stat_sampel %>% 
      group_by(id_sampel) %>% 
      summarise(rerata = round(mean(nilai), 2))
    vektor_rerata <- ringkasan_stat$rerata
    
    if (k == 1) {
      paste0("Gambar 2: Distribusi data sebuah sampel yang berukuran ", n, ". Rerata sampel tersebut kurang lebih ", vektor_rerata[1], ".")
    } else if (k == 2) {
      paste0("Gambar 2: Distribusi data dua sampel yang masing-masing ukurannya ", n, ". Rerata kedua sampel tersebut secara berturut-turut kurang lebih ", vektor_rerata[1], " dan ", vektor_rerata[2], ".")
    } else if (k == 3) {
      paste0("Gambar 2: Distribusi data tiga sampel yang berukuran ", n, ". Rerata ketiga sampel tersebut secara berturut-turut kurang lebih ", vektor_rerata[1], ", ", vektor_rerata[2], ", dan ", vektor_rerata[3], ".")
    } else if (k == 4) {
      paste0("Gambar 2: Distribusi data empat sampel yang masing-masing ukurannya ", n, ". Rerata keempat sampel tersebut secara berturut-turut kurang lebih ", vektor_rerata[1], ", ", vektor_rerata[2], ", ", vektor_rerata[3], ", dan ", vektor_rerata[4], ".")
    } else if (k == 5) {
      paste0("Gambar 2: Distribusi data lima sampel yang masing-masing berukuran ", n, ". Rerata kelima sampel tersebut secara berturut-turut kurang lebih sama dengan ", vektor_rerata[1], ", ", vektor_rerata[2], ", ", vektor_rerata[3], ", ", vektor_rerata[4], ", dan ", vektor_rerata[5], ".")
    } else {
      paste0("Gambar 1: Distribusi sampel yang berukuran ", n, ".")
    }
  })
  
}


# Aplikasi Shiny
shinyApp(ui, server)