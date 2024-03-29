QT       += core gui testlib

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++17

# You can make your code fail to compile if it uses deprecated APIs.
# In order to do so, uncomment the following line.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

SOURCES += \
    diffviewer.cpp \
    diffviewertext.cpp \
    diffviewertextbuilder.cpp \
    global.cpp \
    linenumberarea.cpp \
    main.cpp \
    mainwindow.cpp \
    statjsonreader.cpp \
    stats.cpp

HEADERS += \
    diffviewer.h \
    diffviewertext.h \
    diffviewertextbuilder.h \
    global.h \
    linenumberarea.h \
    mainwindow.h \
    statjsonreader.h \
    stats.h

FORMS += \
    mainwindow.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

QMAKE_EXTRA_TARGETS += before_build makefilehook

makefilehook.target = $(MAKEFILE)
makefilehook.depends = .beforebuild

PRE_TARGETDEPS += .beforebuild

before_build.target = .beforebuild
before_build.depends = FORCE
before_build.commands = chcp 1251
