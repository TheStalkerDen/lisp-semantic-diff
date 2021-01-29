#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "diffviewertext.h"
#include <QDir>
#include <QFileDialog>
#include <QProcess>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}


void MainWindow::on_actionloadFiles_triggered()
{
    QStringList lispFiles = QFileDialog::getOpenFileNames(this,"Select lisp files");
    QProcess* backend_process = new QProcess(this);
    QString file = "diff-backend.exe";
    qDebug() << file;
    backend_process->start(file,lispFiles);
    if(!backend_process->waitForFinished()){
        qDebug() << "something is wrong";
    }

    doc1.setTextDescriptionFromJson("res1.json");
    doc2.setTextDescriptionFromJson("res2.json");
    ui->plainTextEdit->appendHtml(doc1.getText());
    ui->plainTextEdit_2->appendHtml(doc2.getText());
}
