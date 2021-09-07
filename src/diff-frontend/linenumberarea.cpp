#include "linenumberarea.h"



LineNumberArea::LineNumberArea(DiffViewer *viewer)
    :QWidget(viewer),diff_viewer(viewer)
{

}



QSize LineNumberArea::sizeHint() const
{
    return QSize(diff_viewer->lineNumberAreaWidth(),0);
}

void LineNumberArea::paintEvent(QPaintEvent *event)
{
    diff_viewer->lineNumbeerAreaPaintEvent(event);
}
