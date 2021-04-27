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
    cleanOldJsonFiles();
    backend_process->start(file,lispFiles);
    if (!backend_process->waitForFinished()) {
        qDebug() << "something is wrong";
    }
    qDebug() << "Backend is ok!";
    if (QFile::exists("lexer-errors-msgs1.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        file1ErrorsMode = ErrorsModeTypes::LexicalErrors;
        errorsMsgs1 = convertJsonArrayToErrorsMsgsMap(getJsonDocument("lexer-errors-msgs1.json").array());
    }
    if (QFile::exists("lexer-errors-msgs2.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        file2ErrorsMode = ErrorsModeTypes::LexicalErrors;
        errorsMsgs2 = convertJsonArrayToErrorsMsgsMap(getJsonDocument("lexer-errors-msgs2.json").array());
    }

    if (QFile::exists("comments1.json")) {
        commentsJsonObj1 = getJsonDocument("comments1.json").object();
    }
    if (QFile::exists("comments2.json")) {
        commentsJsonObj2 = getJsonDocument("comments2.json").object();
    }

    if(viewerMode == ViewerMode::NormalMode){
        stats = Stat("stats.json");
        fillStatsTree();
        synTreeJson1 = getJsonDocument("res1.json");
        synTreeJson2 = getJsonDocument("res2.json");
        analyzeSynTree(synTreeJson1,1);
        analyzeSynTree(synTreeJson2,2);

        doc1.generateHTMLTextFromJson(QJsonValue(synTreeJson1.array()),commentsJsonObj1);
        doc2.generateHTMLTextFromJson(QJsonValue(synTreeJson2.array()),commentsJsonObj2);
        ui->plainTextEdit->appendHtml(doc1.getText());
        ui->plainTextEdit_2->appendHtml(doc2.getText());
    } else if (viewerMode == ViewerMode::ErrorsMode){
        if(QFile::exists("lexems1.json")){
            qDebug("I am here");
            lexemsArrayJson1 = getJsonDocument("lexems1.json").array();
            doc1.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson1,commentsJsonObj1);
            ui->plainTextEdit->appendHtml(doc1.getText());
        }
        if(QFile::exists("lexems2.json")){
            lexemsArrayJson2 = getJsonDocument("lexems2.json").array();
            doc1.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson2,commentsJsonObj2);
            ui->plainTextEdit_2->appendHtml(doc2.getText());
        }
    }
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

void MainWindow::analyzeSynTree(QJsonDocument &doc, int num)
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

void MainWindow::cleanOldJsonFiles()
{
    QFile::remove("lexems1.json");
    QFile::remove("lexems2.json");
    QFile::remove("comments1.json");
    QFile::remove("comments2.json");
    QFile::remove("lexer-errors-msgs1.json");
    QFile::remove("lexer-errors-msgs2.json");
    QFile::remove("res1.json");
    QFile::remove("res2.json");
    QFile::remove("stats.json");
}

ErrorsMsgsMap MainWindow::convertJsonArrayToErrorsMsgsMap(QJsonArray array)
{
    ErrorsMsgsMap errorsMsgsMap;
    for(int i = 0; i < array.size(); i++){
        int errorLexId = array[i].toObject()["errorLexId"].toInt();
        errorsMsgsMap[errorLexId] = array[i].toObject();
        errorsMsgsMap[errorLexId].insert("isSelected",QJsonValue(false));
    }
    return errorsMsgsMap;
}

QJsonDocument MainWindow::getJsonDocument(QString pathname)
{
    QFile loadFile(pathname);
    if(!loadFile.open(QIODevice::ReadOnly)){
        qWarning("Couldn't open file.");
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
        doc1.generateHTMLTextFromJson(QJsonValue(synTreeJson1.array()), commentsJsonObj1);
        doc2.generateHTMLTextFromJson(QJsonValue(synTreeJson2.array()), commentsJsonObj2);
        ui->plainTextEdit->clear();
        ui->plainTextEdit_2->clear();
        ui->plainTextEdit->appendHtml(doc1.getText());
        ui->plainTextEdit_2->appendHtml(doc2.getText());
    } else {
        if(nameToObj1.contains(text)){
            doc1.generateHTMLTextFromJson(QJsonValue(nameToObj1[text]), commentsJsonObj1,false);
            ui->plainTextEdit->clear();
            ui->plainTextEdit->appendHtml(doc1.getText());
        }
        if(nameToObj2.contains(text)){
            doc2.generateHTMLTextFromJson(QJsonValue(nameToObj2[text]), commentsJsonObj2,false);
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
