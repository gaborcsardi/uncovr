re_exclude <- function(pkg) {
  c(
    paste0(
      "(?i)", # these are case insensitive
      c(
        "[.]o$",
        "[.]so$",
        "[.]dll$",

        "(^|/)\\.DS_Store$", # by macOS finder
        "^\\.RData$", # .RData at /
        "~$",
        "\\.bak$",
        "\\.swp$", # backup files
        "(^|/)\\.#[^/]*$",
        "(^|/)#[^/]*#$", # more backup files (Emacs)

        "^config\\.(cache|log|status)$", # leftover by autoconf
        "(^|/)autom4te\\.cache$",

        "^src/.*\\.d$",
        "^src/Makedeps$", # INSTALL leftover on Windows

        "^inst/doc/Rplots\\.(ps|pdf)$" # Sweave leftover
      )
    ),

    "(^|/)\\._[^/]*$", # macOS resource forks

    paste0(
      # hidden files
      "(^|/)\\.",
      c(
        "Renviron",
        "Rprofile",
        "Rproj.user",
        "Rhistory",
        "Rapp.history",
        "tex",
        "log",
        "aux",
        "pdf",
        "png",
        "backups",
        "cvsignore",
        "cproject",
        "directory",
        "dropbox",
        "exrc",
        "gdb.history",
        "gitattributes",
        "github",
        "gitignore",
        "gitmodules",
        "hgignore",
        "hgtags",
        "htaccess",
        "latex2html-init",
        "project",
        "seed",
        "settings",
        "tm_properties"
      ),
      "$"
    ),

    paste0(
      "(^|/)",
      pkg,
      "_[0-9.-]+",
      "\\.(tar\\.gz|tar|tar\\.bz2|tar\\.xz|tgz|zip)",
      "$"
    )
  )
}

re_exclude_dir <- function(pkg) {
  c(
    "^revdep$", # revdepcheck
    paste0(
      # VC
      "(^|/)",
      c(
        "CVS",
        ".svn",
        ".arch-ids",
        ".bzr",
        ".git",
        ".hg",
        "_darcs",
        ".metadata"
      ),
      "$"
    ),

    "(^|/)[^/]*[Oo]ld$",
    "(^|/)[^/]*\\.Rcheck",

    "^src.*/\\.deps$"
  )
}
