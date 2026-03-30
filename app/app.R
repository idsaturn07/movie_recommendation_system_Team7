library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(tm)

source("utils/hybrid_model.R")

model  <- build_model()
movies <- model$movies

#safe genres_list
if (!"genres_list" %in% colnames(movies)) {
  if ("genres_clean" %in% colnames(movies)) {
    movies$genres_list <- strsplit(as.character(movies$genres_clean), "\\s+")
  } else {
    movies$genres_list <- vector("list", nrow(movies))
  }
}

# ensure final_score / popularity exist 
if (!"final_score"  %in% colnames(movies)) movies$final_score  <- movies$vote_average
if (!"popularity"   %in% colnames(movies)) movies$popularity   <- movies$vote_count
if (!"vote_average" %in% colnames(movies)) movies$vote_average <- 0
if (!"vote_count"   %in% colnames(movies)) movies$vote_count   <- 0
if (!"overview"     %in% colnames(movies)) movies$overview     <- ""

TMDB_KEY <- "YOUR_API_KEY_HERE"

# TMDB helpers
tmdb_search <- function(title) {
  tryCatch({
    url <- paste0("https://api.themoviedb.org/3/search/movie?api_key=",
                  TMDB_KEY, "&query=", URLencode(title))
    response <- GET(url, config(ssl_verifypeer = FALSE))
    res <- fromJSON(rawToChar(response$content), flatten = TRUE)
    if (!is.null(res$results) && nrow(res$results) > 0) res$results[1, ] else NULL
  }, error = function(e) NULL)
}

get_poster <- function(title) {
  r <- tmdb_search(title)
  if (!is.null(r) && !is.na(r$poster_path))
    return(paste0("https://image.tmdb.org/t/p/w300", r$poster_path))
  "https://placehold.co/160x240/141414/e50914?text=No+Image"
}

get_backdrop <- function(title) {
  r <- tmdb_search(title)
  if (!is.null(r) && !is.na(r$backdrop_path))
    return(paste0("https://image.tmdb.org/t/p/w1280", r$backdrop_path))
  NULL
}

# Rating-based recommendations
get_rating_recs <- function(ratings_list, model, n = 10) {
  if (length(ratings_list) == 0) return(NULL)
  mv  <- model$movies
  tfn <- model$tfidf_norm          # rows = movies
  liked <- names(ratings_list[ratings_list >= 4])
  if (length(liked) == 0) liked <- names(ratings_list)
  
  sv <- rep(0, nrow(mv))
  for (m in liked) {
    idx <- which(tolower(mv$title) == tolower(m))
    if (length(idx) > 0) {
      # correct: dot-product of every movie against the liked movie's vector
      sv <- sv + (ratings_list[[m]] / 5) *
        as.numeric(as.matrix(tfn) %*% as.numeric(tfn[idx[1], ]))
    }
  }
  # exclude already-rated movies
  sv[which(tolower(mv$title) %in% tolower(names(ratings_list)))] <- -Inf
  mv$title[order(sv, decreasing = TRUE)[seq_len(n)]]
}

#Star colour helper
sc <- function(i, rating) {
  if (!is.null(rating) && !is.na(rating) && i <= rating)
    "color:#e50914;" else "color:rgba(255,255,255,0.25);"
}

# Card builder (returns raw HTML string)
nf_card <- function(title, poster, score = NULL, rating = NULL) {
  # safe single-quote escaping for inline JS
  js_title  <- gsub("\\\\", "\\\\\\\\", title)   # escape backslashes first
  js_title  <- gsub("'",    "\\\\'",    js_title) # then single quotes
  ht        <- htmltools::htmlEscape(title)
  
  score_html <- if (!is.null(score) && !is.na(score))
    paste0('<div class="card-score">&#11088; ', round(score, 1), '</div>') else ""
  
  stars <- paste0(
    '<div class="star-row" data-movie="', ht, '">',
    paste0(sapply(1:5, function(i) paste0(
      '<span class="nf-star" style="', sc(i, rating), '" ',
      'onclick="event.stopPropagation();cmRate(\'', js_title, '\',', i, ')">&#9733;</span>'
    )), collapse = ""),
    '</div>'
  )
  
  paste0(
    '<div class="nf-card" onclick="openMovie(\'', js_title, '\')">',
    '<div class="nf-card-img">',
    '<img src="', poster, '" loading="lazy" alt="', ht, '"/>',
    '<div class="nf-card-hover">',
    '<div class="hover-play">&#9654;</div>',
    '<div class="hover-title">', ht, '</div>',
    score_html,
    '</div>',
    '</div>',
    '<div class="nf-card-foot">',
    '<div class="card-title">', ht, '</div>',
    stars,
    '</div>',
    '</div>'
  )
}

#Genre row helper (server-side)
genre_movies <- function(genre_kw, n = 14) {
  movies %>%
    filter(sapply(genres_list, function(g) genre_kw %in% g)) %>%
    arrange(desc(final_score)) %>%
    slice(seq_len(n))
}

# UI
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$title("CineMatch"),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?family=",
                            "Bebas+Neue&family=DM+Sans:wght@300;400;600;700",
                            "&display=swap")),
    tags$script(HTML(paste0(
      "var CM_TITLES = ",
      jsonlite::toJSON(movies$title, auto_unbox = FALSE), ";"))),
    tags$script(src = "app.js"),
    tags$style(HTML('
      *,*::before,*::after{box-sizing:border-box;margin:0;padding:0;}
      .container-fluid{padding:0!important;margin:0!important;}
      .shiny-output-error{display:none!important;}

      html,body{
        background:#141414!important;
        color:#e5e5e5;
        font-family:"DM Sans",sans-serif;
        min-height:100vh;
        overflow-x:hidden;
      }

      /* NAVBAR */
      #nf-nav{
        position:fixed;top:0;left:0;right:0;z-index:9000;
        height:68px;padding:0 60px;
        display:flex;align-items:center;justify-content:space-between;
        background:linear-gradient(180deg,rgba(0,0,0,0.9) 0%,transparent 100%);
        transition:background 0.4s;
      }
      #nf-nav.scrolled{background:rgba(20,20,20,0.98)!important;}
      .nf-logo{
        font-family:"Bebas Neue",sans-serif;
        font-size:2.2rem;letter-spacing:4px;
        color:#e50914;cursor:pointer;user-select:none;
        text-shadow:0 0 24px rgba(229,9,20,0.45);
      }
      .nf-logo span{color:#e5e5e5;}
      .nav-right{display:flex;align-items:center;gap:16px;}

      /* SEARCH */
      #nf-search-wrap{position:relative;width:320px;}
      #cm-inp{
        width:100%;
        background:rgba(0,0,0,0.7);
        border:1.5px solid rgba(255,255,255,0.2);
        border-radius:4px;
        padding:9px 34px 9px 36px;
        color:#fff!important;
        -webkit-text-fill-color:#fff!important;
        font-size:0.875rem;
        font-family:"DM Sans",sans-serif;outline:none;
        transition:border-color 0.2s,background 0.2s;
      }
      #cm-inp:focus{
        border-color:rgba(255,255,255,0.65);
        background:rgba(0,0,0,0.9);
      }
      #cm-inp::placeholder{color:rgba(255,255,255,0.4)!important;}
      .cm-si{
        position:absolute;left:11px;top:50%;
        transform:translateY(-50%);
        color:rgba(255,255,255,0.45);font-size:13px;pointer-events:none;
      }
      #cm-clr{
        position:absolute;right:10px;top:50%;
        transform:translateY(-50%);
        background:none;border:none;color:rgba(255,255,255,0.45);
        font-size:0.9rem;cursor:pointer;display:none;padding:0;
      }
      #cm-clr:hover{color:#fff;}
      #cm-drop{
        position:absolute;top:calc(100% + 8px);left:0;width:100%;
        background:#1a1a1a;border:1px solid #2e2e2e;border-radius:5px;
        max-height:320px;overflow-y:auto;z-index:99999;display:none;
        box-shadow:0 16px 48px rgba(0,0,0,0.9);
      }
      #cm-drop::-webkit-scrollbar{width:4px;}
      #cm-drop::-webkit-scrollbar-thumb{background:#e50914;border-radius:4px;}
      .drop-label{
        padding:7px 14px 3px;font-size:0.63rem;letter-spacing:3px;
        text-transform:uppercase;color:#555;border-bottom:1px solid #222;
      }
      .drop-item{
        padding:10px 14px;font-size:0.875rem;color:#ccc;
        cursor:pointer;border-bottom:1px solid #1f1f1f;
        display:flex;align-items:center;gap:8px;
        transition:background 0.1s;
      }
      .drop-item:hover,.drop-item.focused{background:#e50914;color:#fff;}
      .drop-item:hover b,.drop-item.focused b{color:#fff;}
      .drop-item b{color:#e50914;}
      .drop-empty{padding:12px 14px;color:#555;font-size:0.875rem;}

      /* HERO */
      #nf-hero{
        position:relative;
        height:88vh;min-height:520px;
        display:flex;align-items:flex-end;
        padding:0 60px 90px;
        overflow:hidden;background:#0a0a0a;
      }
      #hero-bg{
        position:absolute;inset:0;
        background-size:cover;background-position:center 20%;
        filter:brightness(0.42);
        transition:opacity 0.6s;
      }
      #hero-grad{
        position:absolute;inset:0;
        background:linear-gradient(
          0deg,
          rgba(20,20,20,1) 0%,
          rgba(20,20,20,0.55) 40%,
          transparent 75%
        );
      }
      .hero-content{position:relative;z-index:2;max-width:540px;}
      .hero-badge{
        display:inline-block;background:#e50914;color:#fff;
        font-size:0.68rem;font-weight:700;letter-spacing:3px;
        text-transform:uppercase;padding:3px 10px;
        border-radius:3px;margin-bottom:12px;
      }
      .hero-title{
        font-family:"Bebas Neue",sans-serif;
        font-size:clamp(2.6rem,5.5vw,4.8rem);
        letter-spacing:2px;line-height:1;
        color:#fff;margin-bottom:12px;
        text-shadow:2px 2px 20px rgba(0,0,0,0.7);
      }
      .hero-meta{
        display:flex;gap:14px;align-items:center;
        margin-bottom:12px;font-size:0.875rem;
      }
      .hero-score{color:#46d369;font-weight:700;}
      .hero-genre{color:#b3b3b3;}
      .hero-desc{
        font-size:0.925rem;line-height:1.7;color:#b3b3b3;
        margin-bottom:20px;
        display:-webkit-box;-webkit-line-clamp:3;
        -webkit-box-orient:vertical;overflow:hidden;
      }
      .hero-btns{display:flex;gap:12px;}
      .hero-btn{
        display:flex;align-items:center;gap:8px;
        padding:10px 26px;border-radius:4px;
        font-size:0.95rem;font-weight:600;cursor:pointer;border:none;
        transition:opacity 0.15s,transform 0.1s;
        font-family:"DM Sans",sans-serif;
      }
      .hero-btn:hover{opacity:0.82;transform:scale(1.02);}
      .btn-play{background:#fff;color:#141414;}
      .btn-info{background:rgba(109,109,110,0.65);color:#fff;}

      /* SECTIONS */
      #nf-content{padding-top:0;}
      .nf-section{padding:0 60px 38px;}
      .nf-section-title{
        font-size:1.2rem;font-weight:600;color:#e5e5e5;
        margin-bottom:12px;letter-spacing:0.3px;
        display:flex;align-items:center;gap:10px;
      }

      /* HORIZONTAL ROW */
      .nf-row{
        display:flex!important;
        flex-direction:row!important;
        flex-wrap:nowrap!important;
        gap:8px;
        overflow-x:auto;
        overflow-y:visible;
        padding:10px 0 18px;
        scroll-behavior:smooth;
        -webkit-overflow-scrolling:touch;
        scrollbar-width:none;
      }
      .nf-row::-webkit-scrollbar{display:none;}

      /* CARD */
      .nf-card{
        flex:0 0 160px;width:160px;min-width:160px;
        cursor:pointer;
        transition:transform 0.25s,z-index 0s 0.25s;
        position:relative;z-index:1;
        border-radius:4px;
      }
      .nf-card:hover{
        transform:scale(1.22);z-index:100;
        transition:transform 0.25s,z-index 0s;
      }
      .nf-card-img{
        position:relative;width:160px;height:240px;
        border-radius:4px;overflow:hidden;background:#1f1f1f;
      }
      .nf-card-img img{
        width:100%;height:100%;object-fit:cover;display:block;
      }
      .nf-card-hover{
        position:absolute;inset:0;
        background:linear-gradient(180deg,transparent 30%,rgba(0,0,0,0.95));
        opacity:0;transition:opacity 0.2s;
        display:flex;flex-direction:column;
        justify-content:flex-end;padding:10px;
      }
      .nf-card:hover .nf-card-hover{opacity:1;}
      .hover-play{font-size:1.7rem;color:#fff;margin-bottom:4px;}
      .hover-title{font-size:0.76rem;font-weight:600;color:#fff;line-height:1.3;margin-bottom:2px;}
      .card-score{font-size:0.68rem;color:#46d369;margin-top:2px;}
      .nf-card-foot{padding:5px 2px 0;width:160px;}
      .card-title{
        font-size:0.74rem;color:#b3b3b3;
        white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
        margin-bottom:3px;
      }
      .star-row{display:flex;gap:1px;}
      .nf-star{
        font-size:0.82rem;cursor:pointer;
        transition:color 0.12s,transform 0.1s;
      }
      .nf-star:hover{transform:scale(1.3);color:#e50914!important;}

      /* SEARCH PANEL */
      .search-panel{
        background:linear-gradient(180deg,#1c1c1c,#141414);
        border-bottom:1px solid #222;
        padding:28px 60px;
        animation:fadeIn 0.3s ease;
      }
      @keyframes fadeIn{
        from{opacity:0;transform:translateY(-8px)}
        to{opacity:1;transform:none}
      }
      .search-top{display:flex;gap:22px;align-items:flex-start;margin-bottom:24px;}
      .search-poster{width:110px;border-radius:5px;flex-shrink:0;box-shadow:0 8px 28px rgba(0,0,0,0.7);}
      .search-info{flex:1;}
      .search-title{
        font-family:"Bebas Neue",sans-serif;
        font-size:2rem;letter-spacing:2px;color:#fff;margin-bottom:4px;
      }
      .search-genre{font-size:0.7rem;color:#e50914;letter-spacing:2px;text-transform:uppercase;margin-bottom:5px;}
      .search-score{color:#46d369;font-size:0.875rem;margin-bottom:8px;}
      .search-sub{font-size:0.78rem;color:#555;margin-bottom:10px;}
      .rec-label{font-size:1rem;font-weight:600;color:#e5e5e5;margin-bottom:10px;}

      /* RATINGS BOX */
      .ratings-box{
        background:#1a1a1a;border:1px solid #222;
        border-radius:5px;padding:16px;margin:0 60px 28px;
      }
      .ratings-box h4{
        font-size:0.7rem;font-weight:700;letter-spacing:3px;
        text-transform:uppercase;color:#e50914;margin-bottom:10px;
      }
      .rated-row{
        display:flex;justify-content:space-between;
        font-size:0.8rem;color:#b3b3b3;
        padding:5px 8px;background:#141414;
        border-radius:3px;margin-bottom:4px;
      }
      .rated-val{color:#e50914;}

      /* MODAL */
      #nf-modal{
        display:none;position:fixed;inset:0;
        background:rgba(0,0,0,0.88);z-index:99000;
        overflow-y:auto;padding:36px 20px;
      }
      #nf-modal.open{
        display:flex!important;
        justify-content:center;align-items:flex-start;
      }
      .modal-box{
        background:#181818;border-radius:7px;
        width:100%;max-width:880px;overflow:hidden;
        position:relative;border:1px solid #282828;
        animation:mUp 0.25s ease;margin:auto;
      }
      @keyframes mUp{from{transform:translateY(28px);opacity:0}to{transform:translateY(0);opacity:1}}
      .modal-close{
        position:absolute;top:13px;right:13px;
        background:rgba(0,0,0,0.8);border:none;color:#fff;
        font-size:1rem;width:32px;height:32px;border-radius:50%;
        cursor:pointer;display:flex;align-items:center;justify-content:center;
        z-index:20;transition:background 0.15s;
      }
      .modal-close:hover{background:#e50914;}
      .modal-bd{
        width:100%;height:260px;overflow:hidden;
        background:#0a0a0a;position:relative;
      }
      .modal-bd img{
        width:100%;height:260px;object-fit:cover;
        object-position:center 25%;filter:brightness(0.48);display:block;
      }
      .modal-bd-fade{
        position:absolute;bottom:0;left:0;right:0;height:110px;
        background:linear-gradient(transparent,#181818);
      }
      .modal-body{padding:0 26px 26px;background:#181818;}
      .modal-top{
        display:flex;gap:18px;align-items:flex-end;
        margin-top:-55px;position:relative;z-index:2;margin-bottom:14px;
      }
      .modal-poster{width:90px;border-radius:5px;flex-shrink:0;box-shadow:0 6px 24px rgba(0,0,0,0.8);}
      .modal-title{font-family:"Bebas Neue",sans-serif;font-size:1.9rem;letter-spacing:2px;color:#fff;line-height:1;margin-bottom:3px;}
      .modal-genre{font-size:0.68rem;color:#e50914;letter-spacing:2px;text-transform:uppercase;margin-bottom:3px;}
      .modal-score{font-size:0.8rem;color:#46d369;}
      .modal-overview{font-size:0.875rem;color:#bbb;line-height:1.78;margin:12px 0 14px;}
      .modal-star-row{display:flex;gap:3px;}
      .modal-star{
        font-size:1.25rem;cursor:pointer;
        color:rgba(255,255,255,0.25);
        transition:color 0.12s,transform 0.1s;
      }
      .modal-star:hover{color:#e50914!important;transform:scale(1.2);}
      .modal-rec-label{
        font-size:0.95rem;font-weight:600;color:#fff;
        margin:16px 0 10px;padding-top:12px;border-top:1px solid #222;
      }
      .modal-rec-row{
        display:flex!important;flex-direction:row!important;
        flex-wrap:nowrap!important;gap:8px;
        overflow-x:auto;padding-bottom:4px;
        scrollbar-width:none;
      }
      .modal-rec-row::-webkit-scrollbar{display:none;}

      .divider{height:1px;margin:0 60px 8px;background:linear-gradient(90deg,transparent,#2a2a2a,transparent);}
    '))
  ),
  
  # MODAL
  tags$div(id = "nf-modal",
           tags$div(class = "modal-box",
                    tags$button(class = "modal-close", onclick = "closeModal()",
                                HTML("&#x2715;")),
                    uiOutput("modal_content")
           )
  ),
  
  # NAVBAR
  tags$div(id = "nf-nav",
           tags$div(class = "nf-logo", onclick = "cmClear()",
                    "CINE", tags$span("MATCH")),
           tags$div(class = "nav-right",
                    tags$div(id = "nf-search-wrap",
                             tags$span(class = "cm-si", HTML("&#128269;")),
                             tags$input(id = "cm-inp", type = "text",
                                        placeholder = "Search movies...",
                                        autocomplete = "off",
                                        oninput = "cmFilter(this.value)"),
                             tags$button(id = "cm-clr", onclick = "cmClear()",
                                         HTML("&#x2715;")),
                             tags$div(id = "cm-drop")
                    )
           )
  ),
  
  # HERO
  tags$div(id = "nf-hero",
           tags$div(id = "hero-bg"),
           tags$div(id = "hero-grad"),
           tags$div(class = "hero-content",
                    tags$div(class = "hero-badge", HTML("&#127917; Featured Today")),
                    uiOutput("hero_content")
           )
  ),
  
  # CONTENT
  tags$div(id = "nf-content",
           uiOutput("search_panel"),
           uiOutput("rating_recs_section"),
           uiOutput("ratings_box_ui"),
           
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#128293; Top Rated")),
                    tags$div(class = "nf-row", uiOutput("row_top_rated"))
           ),
           tags$div(class = "divider"),
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#127775; Most Popular")),
                    tags$div(class = "nf-row", uiOutput("row_popular"))
           ),
           tags$div(class = "divider"),
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#128248; Action & Adventure")),
                    tags$div(class = "nf-row", uiOutput("row_action"))
           ),
           tags$div(class = "divider"),
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#128561; Horror & Thriller")),
                    tags$div(class = "nf-row", uiOutput("row_horror"))
           ),
           tags$div(class = "divider"),
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#128514; Comedy")),
                    tags$div(class = "nf-row", uiOutput("row_comedy"))
           ),
           tags$div(class = "divider"),
           tags$div(class = "nf-section",
                    tags$div(class = "nf-section-title", HTML("&#10084; Romance & Drama")),
                    tags$div(class = "nf-row", uiOutput("row_romance"))
           ),
           tags$div(style = "height:50px;")
  )
)

# SERVER
server <- function(input, output, session) {
  
  picked     <- reactiveVal(NULL)
  recs       <- reactiveVal(NULL)
  modal_film <- reactiveVal(NULL)
  ratings    <- reactiveValues(data = list())
  rat_recs   <- reactiveVal(NULL)
  
  #Hero movie (computed once)
  hero_movie <- movies %>%
    filter(vote_count > 500) %>%
    arrange(desc(final_score)) %>%
    slice(1)
  
  # Send hero background ONCE after session flushes
  session$onFlushed(function() {
    bg <- get_backdrop(hero_movie$title[1])
    if (!is.null(bg))
      session$sendCustomMessage("setHeroBg", list(url = bg))
  }, once = TRUE)
  
  output$hero_content <- renderUI({
    m        <- hero_movie$title[1]
    score    <- hero_movie$vote_average[1]
    genre    <- if ("genres_clean" %in% names(hero_movie)) hero_movie$genres_clean[1] else ""
    overview <- if (!is.na(hero_movie$overview[1])) hero_movie$overview[1] else ""
    js_m     <- gsub("'", "\\'", gsub("\\\\", "\\\\\\\\", m))
    
    tagList(
      tags$div(class = "hero-title", m),
      tags$div(class = "hero-meta",
               tags$span(class = "hero-score", paste0("⭐ ", round(score, 1))),
               tags$span(class = "hero-genre", genre)
      ),
      tags$div(class = "hero-desc", overview),
      tags$div(class = "hero-btns",
               tags$button(class = "hero-btn btn-play",
                           onclick = paste0("openMovie('", js_m, "')"),
                           HTML("&#9654; Play")),
               tags$button(class = "hero-btn btn-info",
                           onclick = paste0("openMovie('", js_m, "')"),
                           HTML("&#9432; More Info"))
      )
    )
  })
  
  # Search / clear
  observeEvent(input$clear_search, {
    picked(NULL)
    recs(NULL)
  })
  
  observeEvent(input$picked_movie, {
    film <- input$picked_movie
    if (is.null(film) || nchar(trimws(film)) == 0) {
      picked(NULL); recs(NULL); return()
    }
    picked(film)
    withProgress(message = paste("Finding matches for", film), value = 0.6,
                 recs(get_recommendations(film, model, n = 10))
    )
  })
  
  observeEvent(input$modal_movie, {
    req(input$modal_movie)
    modal_film(input$modal_movie)
  })
  
  # Modal
  output$modal_content <- renderUI({
    req(modal_film())
    film <- modal_film()
    mrow <- movies %>% filter(tolower(title) == tolower(film))
    if (nrow(mrow) == 0)
      return(tags$div(style = "padding:30px;color:#666;", "Movie not found."))
    
    poster   <- get_poster(film)
    backdrop <- get_backdrop(film)
    score    <- mrow$vote_average[1]
    genre    <- if ("genres_clean" %in% names(mrow)) mrow$genres_clean[1] else ""
    overview <- if (!is.na(mrow$overview[1]) && nchar(mrow$overview[1]) > 0)
      mrow$overview[1] else "No description available."
    rating   <- ratings$data[[film]]
    mrecs    <- get_recommendations(film, model, n = 8)
    
    tagList(
      tags$div(class = "modal-bd",
               if (!is.null(backdrop))
                 tags$img(src = backdrop, alt = film)
               else
                 tags$div(style = "width:100%;height:260px;background:#0a0a0a;"),
               tags$div(class = "modal-bd-fade")
      ),
      tags$div(class = "modal-body",
               tags$div(class = "modal-top",
                        tags$img(src = poster, class = "modal-poster", alt = film),
                        tags$div(
                          tags$div(class = "modal-title", film),
                          if (nchar(as.character(genre)) > 0)
                            tags$div(class = "modal-genre", genre),
                          if (!is.na(score))
                            tags$div(class = "modal-score",
                                     paste0("⭐ ", round(score, 1), " / 10"))
                        )
               ),
               tags$p(class = "modal-overview", overview),
               # modal star row — uses same data-movie attr so cmRate() updates it
               tags$div(class = "modal-star-row", `data-movie` = film,
                        lapply(1:5, function(i) {
                          js_film <- gsub("'", "\\'", gsub("\\\\", "\\\\\\\\", film))
                          tags$span(class = "modal-star",
                                    style = sc(i, rating),
                                    onclick = paste0("event.stopPropagation();cmRate('",
                                                     js_film, "',", i, ")"),
                                    "★")
                        })
               ),
               if (!is.null(mrecs) && length(mrecs) > 0)
                 tagList(
                   tags$div(class = "modal-rec-label", "MORE LIKE THIS"),
                   tags$div(class = "modal-rec-row",
                            lapply(mrecs, function(m) {
                              p  <- get_poster(m)
                              sv <- movies$vote_average[tolower(movies$title) == tolower(m)][1]
                              HTML(nf_card(m, p, sv, ratings$data[[m]]))
                            })
                   )
                 )
      )
    )
  })
  
  # Search panel
  output$search_panel <- renderUI({
    req(picked())
    film   <- picked()
    poster <- get_poster(film)
    mrow   <- movies %>% filter(tolower(title) == tolower(film))
    score  <- if (nrow(mrow) > 0) mrow$vote_average[1] else NULL
    genre  <- if (nrow(mrow) > 0 && "genres_clean" %in% names(mrow))
      mrow$genres_clean[1] else ""
    rating <- ratings$data[[film]]
    
    tagList(
      tags$div(class = "search-panel",
               tags$div(class = "search-top",
                        tags$img(src = poster, class = "search-poster", alt = film),
                        tags$div(class = "search-info",
                                 tags$div(class = "search-title", film),
                                 if (nchar(as.character(genre)) > 0)
                                   tags$div(class = "search-genre", genre),
                                 if (!is.null(score) && !is.na(score))
                                   tags$div(class = "search-score",
                                            paste0("⭐ ", round(score, 1), " / 10")),
                                 tags$div(class = "search-sub",
                                          "Rate this movie to personalise your recommendations"),
                                 tags$div(class = "star-row", `data-movie` = film,
                                          lapply(1:5, function(i) {
                                            js_film <- gsub("'", "\\'", gsub("\\\\", "\\\\\\\\", film))
                                            tags$span(class = "nf-star",
                                                      style = paste0("font-size:1.05rem;cursor:pointer;", sc(i, rating)),
                                                      onclick = paste0("cmRate('", js_film, "',", i, ")"),
                                                      "★")
                                          })
                                 )
                        )
               ),
               if (!is.null(recs()) && length(recs()) > 0)
                 tagList(
                   tags$div(class = "rec-label", "Recommended For You"),
                   tags$div(class = "nf-row",
                            lapply(recs(), function(m) {
                              p  <- get_poster(m)
                              sv <- movies$vote_average[tolower(movies$title) == tolower(m)][1]
                              HTML(nf_card(m, p, sv, ratings$data[[m]]))
                            })
                   )
                 )
      )
    )
  })
  
  # Ratings
  observeEvent(input$rated_movie, {
    info <- input$rated_movie
    ratings$data[[info$title]] <- info$rating
    rat_recs(get_rating_recs(ratings$data, model, n = 10))
  })
  
  output$rating_recs_section <- renderUI({
    req(length(ratings$data) > 0, !is.null(rat_recs()))
    tags$div(class = "nf-section",
             tags$div(class = "nf-section-title", HTML("&#128161; Because You Rated")),
             tags$div(class = "nf-row",
                      lapply(rat_recs(), function(m) {
                        p  <- get_poster(m)
                        sv <- movies$vote_average[tolower(movies$title) == tolower(m)][1]
                        HTML(nf_card(m, p, sv))
                      })
             )
    )
  })
  
  output$ratings_box_ui <- renderUI({
    req(length(ratings$data) > 0)
    tags$div(class = "ratings-box",
             tags$h4("Your Ratings"),
             lapply(names(ratings$data), function(m) {
               r <- ratings$data[[m]]
               tags$div(class = "rated-row",
                        tags$span(m),
                        tags$span(class = "rated-val", paste(rep("★", r), collapse = "")))
             })
    )
  })
  
  # Genre rows
  make_row <- function(gm) {
    lapply(gm$title, function(m)
      HTML(nf_card(m, get_poster(m),
                   movies$vote_average[movies$title == m][1])))
  }
  
  output$row_top_rated <- renderUI({
    top <- movies %>% filter(vote_count > 500) %>%
      arrange(desc(vote_average)) %>% slice(1:14)
    make_row(top)
  })
  
  output$row_popular <- renderUI({
    pop <- movies %>% arrange(desc(popularity)) %>% slice(1:14)
    make_row(pop)
  })
  
  output$row_action  <- renderUI({ make_row(genre_movies("action"))  })
  output$row_horror  <- renderUI({ make_row(genre_movies("horror"))  })
  output$row_comedy  <- renderUI({ make_row(genre_movies("comedy"))  })
  output$row_romance <- renderUI({ make_row(genre_movies("romance")) })
}

shinyApp(ui, server)
