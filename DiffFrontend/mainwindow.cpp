#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "diffviewertext.h"
#include <QDir>
#include <QFileDialog>
#include <QJsonValue>
#include <QJsonArray>
#include <QPainter>
#include <QProcess>
#include "stat.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    qDebug() << "Test";
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

    stats = Stat("stats.json");
    fillStatsTree();
    file1 = getJsonDocument("res1.json");
    file2 = getJsonDocument("res2.json");
    analyzeAST(file1,1);
    analyzeAST(file2,2);

    doc1.setTextDescriptionFromJson(QJsonValue(file1.array()));
    doc2.setTextDescriptionFromJson(QJsonValue(file2.array()));
    ui->plainTextEdit->appendHtml(doc1.getText());
    ui->plainTextEdit_2->appendHtml(doc2.getText());
}

void MainWindow::fillStatsTree()
{
    auto& statsTree = ui->treeWidget;
    auto* topLevelItem = new QTreeWidgetItem();
    topLevelItem->setText(0,"all");
    statsTree->addTopLevelItem(topLevelItem);
    auto* defunsStats = new QTreeWidgetItem();
    defunsStats->setText(0,"defuns");
    statsTree->addTopLevelItem(defunsStats);

    auto* no_mods = new QTreeWidgetItem();
    no_mods->setText(0,"no-mods");
    auto& no_mod_ids = stats.getNoMods();
    for(auto& no_mod_id : no_mod_ids){
        auto* no_mod_item = new QTreeWidgetItem();
        no_mod_item->setText(0,no_mod_id);
        no_mods->addChild(no_mod_item);
    }
    defunsStats->addChild(no_mods);

    auto* mods = new QTreeWidgetItem();
    mods->setText(0,"mods");
    auto& mod_ids = stats.getMod();
    for(auto& mod_id : mod_ids){
        auto* mod_item = new QTreeWidgetItem();
        mod_item->setText(0,mod_id);
        mods->addChild(mod_item);
    }
    defunsStats->addChild(mods);

    auto* news  = new QTreeWidgetItem();
    news->setText(0,"news");
    auto& new_ids = stats.getNew();
    for(auto& new_id : new_ids){
        auto* new_item = new QTreeWidgetItem();
        new_item->setText(0,new_id);
        news->addChild(new_item);
    }
    defunsStats->addChild(news);

    auto* dels  = new QTreeWidgetItem();
    dels->setText(0,"dels");
    auto& del_ids = stats.getDel();
    for(auto& del_id : del_ids){
        auto* del_item = new QTreeWidgetItem();
        del_item->setText(0,del_id);
        dels->addChild(del_item);
    }
    defunsStats->addChild(dels);
}

void MainWindow::analyzeAST(QJsonDocument &doc, int num)
{
   const auto& topLevelArray = doc.array();
   for(const auto& obj: topLevelArray){
       QJsonObject topLevelDef = obj.toObject();
       QString tldName = topLevelDef["props"].toObject()["isDefun"].toString();
       if(num == 1){
           nameToObj1[tldName] = topLevelDef;
       }else {
           nameToObj2[tldName] = topLevelDef;
       }
   }
}

QJsonDocument MainWindow::getJsonDocument(QString pathname)
{
    QFile loadFile(pathname);
    if(!loadFile.open(QIODevice::ReadOnly)){
        qWarning("Couldn't open save file.");
        return QJsonDocument();
    }

    QByteArray saveData = loadFile.readAll();

    return QJsonDocument::fromJson(saveData);
}

void MainWindow::on_treeWidget_itemClicked(QTreeWidgetItem *item, int column)
{
    qDebug() << item->text(0);
    QString text = item->text(0);
    if(text == "all"){
        doc1.setTextDescriptionFromJson(QJsonValue(file1.array()));
        doc2.setTextDescriptionFromJson(QJsonValue(file2.array()));
        ui->plainTextEdit->clear();
        ui->plainTextEdit_2->clear();
        ui->plainTextEdit->appendHtml(doc1.getText());
        ui->plainTextEdit_2->appendHtml(doc2.getText());
    } else {
        if(nameToObj1.contains(text)){
            doc1.setTextDescriptionFromJson(QJsonValue(nameToObj1[text]),false);
            ui->plainTextEdit->clear();
            ui->plainTextEdit->appendHtml(doc1.getText());
        }
        if(nameToObj2.contains(text)){
            doc2.setTextDescriptionFromJson(QJsonValue(nameToObj2[text]),false);
            ui->plainTextEdit_2->clear();
            ui->plainTextEdit_2->appendHtml(doc2.getText());
        }
        if(nameToObj1.contains(text) && !nameToObj2.contains(text)){
            ui->plainTextEdit_2->clear();
        }else if (!nameToObj1.contains(text) && nameToObj2.contains(text)) {
            ui->plainTextEdit->clear();
        }
    }
}
