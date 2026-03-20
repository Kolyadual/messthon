

#include <QString>
#include <QPixmap>

#pragma once

namespace Status
{
    // Status::Status is weird, but Status is a fitting name for both the namespace and enum class..
    enum class Status
    {
        Online = 0,
        Away,
        Busy,
        Offline,
        Blocked,
        Negotiating,
    };

    QString getIconPath(Status status, bool event = false);
    QString getTitle(Status status);
    QString getAssetSuffix(Status status);
    bool isOnline(Status status);
}
