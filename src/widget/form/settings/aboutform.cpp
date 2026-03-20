/*
    Copyright © 2014-2019 by The Messthon Contributors
    Based on qTox project

    This file is part of Messthon, a Qt-based graphical interface for Tox.

    Messthon is libre software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Messthon is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Messthon.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "aboutform.h"
#include "ui_aboutsettings.h"

#include "src/net/updatecheck.h"
#include "src/persistence/profile.h"
#include "src/persistence/settings.h"
#include "src/widget/style.h"
#include "src/widget/tool/recursivesignalblocker.h"
#include "src/widget/translator.h"

#include <tox/tox.h>

#include <QDebug>
#include <QDesktopServices>
#include <QPushButton>
#include <QTimer>

#include <memory>

// index of UI in the QStackedWidget
enum class updateIndex
{
    available = 0,
    upToDate = 1,
    failed = 2
};

/**
 * @class AboutForm
 *
 * This form contains information about Messthon and libraries versions, external
 * links and licence text. Shows progress during an update.
 */

/**
 * @brief Constructor of AboutForm.
 */
AboutForm::AboutForm(UpdateCheck* updateCheck_, Style& style_)
    : GenericForm(QPixmap(":/img/settings/general.png"), style_)
    , bodyUI(new Ui::AboutSettings)
    , progressTimer(new QTimer(this))
    , updateCheck(updateCheck_)
    , style{style_}
{
    bodyUI->setupUi(this);

#if !UPDATE_CHECK_ENABLED
    bodyUI->updateStack->setVisible(false);
#endif
    bodyUI->unstableVersion->setVisible(false);
#if UPDATE_CHECK_ENABLED
    connect(updateCheck_, &UpdateCheck::versionIsUnstable, this, &AboutForm::onUnstableVersion);
#endif

    // block all child signals during initialization
    const RecursiveSignalBlocker signalBlocker(this);

    replaceVersions();

    if (QString(GIT_VERSION).indexOf(" ") > -1)
        bodyUI->gitVersion->setOpenExternalLinks(false);

    eventsInit();
    Translator::registerHandler(std::bind(&AboutForm::retranslateUi, this), this);
}

/**
 * @brief Update versions and links.
 *
 * Update commit hash if built with git, show author info.
 * It also updates Messthon, toxcore and Qt versions.
 */
void AboutForm::replaceVersions()
{
    QString TOXCORE_VERSION = QString::number(tox_version_major()) + "."
                              + QString::number(tox_version_minor()) + "."
                              + QString::number(tox_version_patch());

    bodyUI->youAreUsing->setText(tr("You are using Messthon version %1.").arg(QString(GIT_DESCRIBE)));

#if UPDATE_CHECK_ENABLED
    if (updateCheck != nullptr) {
        connect(updateCheck, &UpdateCheck::updateAvailable, this, &AboutForm::onUpdateAvailable);
        connect(updateCheck, &UpdateCheck::upToDate, this, &AboutForm::onUpToDate);
        connect(updateCheck, &UpdateCheck::updateCheckFailed, this, &AboutForm::onUpdateCheckFailed);
    } else {
        qWarning() << "AboutForm passed null UpdateCheck!";
    }
#else
    qDebug() << "AboutForm not showing updates, Messthon built without UPDATE_CHECK";
#endif

    QString commitLink = "https://github.com/Kolyadual/messthon/commit/" + QString(GIT_VERSION);
    bodyUI->gitVersion->setText(
        tr("Commit hash: %1").arg(createLink(commitLink, QString(GIT_VERSION))));

    bodyUI->toxCoreVersion->setText(tr("toxcore version: %1").arg(TOXCORE_VERSION));
    bodyUI->qtVersion->setText(tr("Qt version: %1").arg(QT_VERSION_STR));

    // Author info
    QString authorInfo =
        QString("<p>%1</p><p>%2</p>")
            .arg(tr("Original author: %1").arg(createLink("https://github.com/Kolyadual", "Kolyadual")))
            .arg(
                tr("See a full list of %1 at Github",
                   "`%1` is replaced with translation of word `contributors`")
                    .arg(createLink("https://github.com/Kolyadual/messthon/graphs/contributors",
                                    tr("contributors", "Replaces `%1` in `See a full list of…`"))));

    bodyUI->authorInfo->setText(authorInfo);
}

void AboutForm::onUpdateAvailable(QString latestVersion, QUrl link)
{
    std::ignore = latestVersion;
    QObject::disconnect(linkConnection);
    linkConnection = connect(bodyUI->updateAvailableButton, &QPushButton::clicked,
                             [link]() { QDesktopServices::openUrl(link); });
    bodyUI->updateStack->setCurrentIndex(static_cast<int>(updateIndex::available));
}

void AboutForm::onUpToDate()
{
    bodyUI->updateStack->setCurrentIndex(static_cast<int>(updateIndex::upToDate));
}

void AboutForm::onUpdateCheckFailed()
{
    bodyUI->updateStack->setCurrentIndex(static_cast<int>(updateIndex::failed));
}

void AboutForm::reloadTheme()
{
    replaceVersions();
}

void AboutForm::onUnstableVersion()
{
    bodyUI->updateStack->hide();
    bodyUI->unstableVersion->setVisible(true);
}

/**
 * @brief Creates hyperlink with specific style.
 * @param path The URL of the page the link goes to.
 * @param text Text, which will be clickable.
 * @return Hyperlink to paste.
 */
QString AboutForm::createLink(QString path, QString text) const
{
    return QString::fromUtf8(
               "<a href=\"%1\" style=\"text-decoration: underline; color:%2;\">%3</a>")
        .arg(path, style.getColor(Style::ColorPalette::Link).name(), text);
}

AboutForm::~AboutForm()
{
    Translator::unregister(this);
    delete bodyUI;
}

/**
 * @brief Retranslate all elements in the form.
 */
void AboutForm::retranslateUi()
{
    bodyUI->retranslateUi(this);
    replaceVersions();
}
