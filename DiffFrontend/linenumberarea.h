#ifndef LINENUMBERAREA_H
#define LINENUMBERAREA_H

#include "diffviewer.h"

#include <QWidget>

class LineNumberArea : public QWidget
{
    Q_OBJECT
public:
    explicit LineNumberArea(DiffViewer *viewer);

    QSize sizeHint() const override;

signals:

private:
    DiffViewer *diff_viewer;


    // QWidget interface
protected:
    void paintEvent(QPaintEvent *event) override;
};

#endif // LINENUMBERAREA_H
