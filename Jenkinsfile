pipeline {
  agent {
    docker {
      image 'rocker/tidyverse:3.5.0'
    }
  }
  stages {
    stage('Install/Update Dependency Pkgs') {
      steps {
        sh '''
        Rscript -e \'devtools::install_deps()\'
        '''
      }
    }
    stage('Set R CMD args') {
      steps {
        sh '''
        args="--no-site-file --no-environ --no-save --no-restore --quiet"
        '''
      }
    }
    stage('R CMD build') {
      steps {
        sh '''
        R $args CMD build . --no-resave-data --no-manual
        '''
      }
    }
    stage('R CMD check') {
      steps {
        sh '''
        R $args CMD check  *.tar.gz --as-cran --timings --no-manual
        '''
      }
    }
  }
}
