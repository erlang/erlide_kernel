#!groovy

pipeline {
	agent any
	options {
		disableConcurrentBuilds()
		timestamps()
		skipDefaultCheckout()
		buildDiscarder(logRotator(numToKeepStr: '10'))
	}
	stages {
		stage('Checkout') {
			steps{
				retry(3) {
					timeout(time: 30, unit: 'SECONDS') {
						script {
							checkout()
						}
					}
				}
			}
		}

		stage('Compile') {
			steps{
				script {
					compile()
					analyze1()
				}
			}
		}

		stage('Test') {
			steps{
				script {
					test()
					analyze2()
				}
			}
		}

		stage('Eclipse') {
			steps{
				script {
					buildEclipse()
                    publishEclipse()
				}
			}
		}

		stage('Server') {
			steps{
				script {
                    buildServer()
                    publishServer()
				}
			}
		}
	}
	//post {
		//always {
			//deleteDir()
		//}
	//}


}

///////////////////////////////////

def checkout() {
    deleteDir()
    if(env.BRANCH_NAME != null) { // multi branch
        checkout scm
        git_branch = env.BRANCH_NAME
    } else {
        git url: 'git@github.com:vladdu/erlide_kernel.git', branch: 'pu'
        sh 'git symbolic-ref --short HEAD > GIT_BRANCH'
        git_branch=readFile('GIT_BRANCH').trim()
    }
    sh('git rev-parse HEAD > GIT_COMMIT')
    git_commit=readFile('GIT_COMMIT')
    short_commit=git_commit.take(6)

    currentBuild.setDescription("${git_branch} - ${short_commit}")
}

def compile() {
    sh "chmod u+x build"
    sh "./build"
}

def test() {
    sh "chmod u+x build"
    sh "./build test"
}

def analyze1() {
    step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: true, canRunOnFailed: true,
        consoleParsers: [[parserName: 'Erlang Compiler (erlc)'], [parserName: 'Maven']],
        excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])
    step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '**/_build/**/*.*', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.erl,**/*.hrl', unHealthy: ''])
}

def analyze2() {
    step([$class: 'AnalysisPublisher', canComputeNew: false, healthy: '', unHealthy: ''])
    step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: '**/TEST*.xml'])
	//step([$class: 'JacocoPublisher', exclusionPattern: '', sourcePattern: '**/src/'])
    // we need Cobertura...
    // publishHTML([
    //     allowMissing: false,
    //     alwaysLinkToLastBuild: false,
    //     keepAll: true,
    //     reportDir: '',
    //     reportFiles:
    //     '''common/_build/test/index.html
    //     ''',
    //     reportName: 'Coverage Report'
    //     ])
}

def buildEclipse() {
    sh "cd eclipse && ./build && cd .."
    step([$class: 'ArtifactArchiver', artifacts: "eclipse/org.erlide.kernel.site-*.zip", fingerprint: true])
}

def buildServer() {
    sh "cd server && ./build && cd .."
    step([$class: 'ArtifactArchiver', artifacts: "server/erlide_ide", fingerprint: true])
}

@NonCPS
def getVersion(String archive) {
    def m = (archive =~ /org.erlide.kernel[-_]([0-9]+\.[0-9]+\.[0-9]+)(\.(.+))?.zip/)
    return m[0]
}

def publishEclipse() {
    def archive = "org.erlide.kernel.site-.zip"
    def isMaster = (git_branch=='master')
    sh "git remote get-url origin > REPO"
    def isMainRepo = readFile('REPO').trim().contains('github.com/erlang/')

    if(!isMaster || !isMainRepo) {
        // only do a github release if on master and in main repo
        return
    }
    // TODO not working for now
    return
    def v = getVersion(archive)
    def vsn = v[1]
    def ts = v[2]
    def vvsn = "v${vsn}"
    // FIXME we can't push to https git url, needs password... Jenkins Github plugin uses https...
    //sh "git push origin :refs/tags/${vvsn}"
    //sh "git fetch --prune origin +refs/tags/*:refs/tags/*"

    sh 'rm -rf GIT_TAG'
    sh 'git describe --exact-match > GIT_TAG || true'
    def git_tag = readFile('GIT_TAG').trim()
    if(git_tag == null || git_tag == '') {
        sh "git tag -a ${vvsn} -m ${vvsn}"
        //sh "git push origin ${vvsn}"
        git_tag = vvsn
    }
    if(git_tag != vvsn) {
        // if there is a tag, but it's not $vvsn, skip publishing
        return
    }

    def draft = true
    def body = ""
    def owner = "erlang"
    def repository = "erlide_kernel"
    def access_token = "${env.GITHUB_TOKEN___}" // the token will be printed in console output...

    sh "rm -rf RELEASE"
    def API_create="{\"tag_name\": \"${vvsn}\",\"name\": \"${vvsn}\",\"body\": \"${body}\",\"draft\": ${draft},\"prerelease\": false}"
    sh "curl -H \"Content-Type:application/json\" --data '${API_create}' https://api.github.com/repos/${owner}/${repository}/releases?access_token=${access_token} > RELEASE"
    def release = readFile('RELEASE').trim()
    def info = getReleaseInfo(release)
    if(info != null) {
        def release_id = info[1]
        sh "curl -X POST --header \"Content-Type:application/edn\" --data-binary @target/${archive} https://uploads.github.com/repos/${owner}/${repository}/releases/${release_id}/assets?access_token=${access_token}\\&name=${archive}"
    }
}

@NonCPS
def getReleaseInfo(String data) {
    def m = (data.replaceAll("\n"," ").trim() =~ /\{[^{]*"id": *([^,]*),.*/)
    if(!m) return null
    return m[0]
}

def publishServer() {
}
