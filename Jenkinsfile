#!groovy

stage 'Checkout'
node {
    wrap([$class: 'TimestamperBuildWrapper']) {
        checkout()
    }
}

stage 'Compile'
node {
    wrap([$class: 'TimestamperBuildWrapper']) {
        compile()
    }
}

//stage 'Tests'
//  runTests()

stage 'Analyze'
node {
    wrap([$class: 'TimestamperBuildWrapper']) {
        analyze()
    }
}

stage 'Archive'
node {
    wrap([$class: 'TimestamperBuildWrapper']) {
        archive = archive()
    }
}

stage 'Publish'
node {
    wrap([$class: 'TimestamperBuildWrapper']) {
        publishRelease(archive)
    }
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

    //currentBuild.setName("${short_commit}__${env.BUILD_NUMBER}")
    currentBuild.setDescription("${git_branch} - ${short_commit}")
}

def compile() {
    wrap([$class: 'Xvfb', displayNameOffset: 100, installationName: 'xvfb', screen: '1024x768x24']) {
        sh "chmod u+x gradlew"
        sh "./gradlew build test assemble"

        if(git_branch=="master") {
            // TODO rename product artifacts
        }
    }
}

def analyze() {
    step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false,
        consoleParsers: [[parserName: 'Erlang Compiler (erlc)']],
        excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])

    step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '**/_build/**/*.*,**/.eunit/**/*.*', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.erl,**/*.hrl', unHealthy: ''])

    step([$class: 'AnalysisPublisher', canComputeNew: false, healthy: '', unHealthy: ''])

    step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: '**/TEST*.xml'])

    // locks

    // jacoco

}

def archive() {
    sh 'rm -rf VSN'
    sh 'cat org.erlide.kernel/META-INF/MANIFEST.MF | grep "Bundle-Version:" | cut -d " " -f 2 > VSN'
    def vsn = readFile('VSN').trim().replace('.qualifier', '')
    def archive = "org.erlide.kernel_${vsn}.zip"
    sh "mv target/org.erlide.kernel.zip ${archive}"
    step([$class: 'ArtifactArchiver', artifacts: archive, fingerprint: true])
    return archive
}

@NonCPS
def getVersion(String archive) {
    def m = (archive =~ /org.erlide.kernel_([0-9]+\.[0-9]+\.[0-9]+)(\.(.+))?.zip/)
    return m[0]
}

def publishRelease(def archive) {
    def isMaster = (git_branch=='master')
    sh "git remote get-url origin > REPO"
    def isMainRepo = readFile('REPO').trim().contains('github.com/erlang/')

    // FIXME we can't push to https git url, needs password... Jenkins Github plugin uses https...
    return

    if(!isMaster || !isMainRepo) {
        // only do a github release if on master and in main repo
        return
    }

    def v = getVersion(archive)
    def vsn = v[1]
    def ts = v[2]
    def vvsn = "v${vsn}"
    sh "git push origin :refs/tags/${vvsn}"
    sh "git fetch --prune origin +refs/tags/*:refs/tags/*"

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
    def access_token = "${env.GITHUB_TOKEN}"

    sh "rm -rf RELEASE"
    def API_create="{\"tag_name\": \"${vvsn}\",\"name\": \"${vvsn}\",\"body\": \"${body}\",\"draft\": ${draft},\"prerelease\": false}"
    sh "curl -H \"Content-Type:application/json\" --data '${API_create}' https://api.github.com/repos/${owner}/${repository}/releases?access_token=${access_token} > RELEASE"
    def release = readFile('RELEASE').trim()
    def info = getReleaseInfo(release)
    def release_id = info[1]
    sh "curl -X POST --header \"Content-Type:application/edn\" --data-binary @target/${archive} https://uploads.github.com/repos/${owner}/${repository}/releases/${release_id}/assets?access_token=${access_token}\\&name=${archive}"
}

@NonCPS
def getReleaseInfo(String data) {
    def m = (data.replaceAll("\n"," ").trim() =~ /\{[^{]*"id": *([^,]*),.*/)
    return m[0]
}

