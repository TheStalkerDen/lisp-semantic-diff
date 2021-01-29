#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "diffviewertext.h"

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

    DiffViewerText doc1;
    DiffViewerText doc2;

private slots:
    void on_actionloadFiles_triggered();


private:
    Ui::MainWindow *ui;
};
#endif // MAINWINDOW_H
