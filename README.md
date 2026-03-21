# Messthon

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub release](https://img.shields.io/github/v/release/Kolyadual/messthon)](https://github.com/Kolyadual/messthon/releases)
[![GitHub issues](https://img.shields.io/github/issues/Kolyadual/messthon)](https://github.com/Kolyadual/messthon/issues)

**Secure Peer-to-Peer Messenger – A New Era of Private Communication**

Messthon is a free, open-source, and completely decentralized instant messenger. It is a powerful continuation of the legendary Tox clients (qTox, uTox, Venom), designed to give you back control of your digital privacy without sacrificing usability.

Built on the proven Tox protocol, Messthon encrypts everything automatically. There are no servers to spy on you, no phone numbers to give away, and no registration—just you and your friends.

---

## Key Features

- **End-to-End Encryption** — Every message, file, and call is automatically encrypted using the robust libsodium library.
- **No Central Servers** — We use a Distributed Hash Table (DHT), just like BitTorrent. The network lives because its users do.
- **Modern Communication** — Enjoy secure group chats, crystal-clear audio calls, and video calls with screensharing.
- **File Transfers** — Send files directly to your contacts, with no size limits or middlemen.
- **Beautiful Interface** — A modern, intuitive user interface that feels right at home on your Linux desktop.
- **Compatible** — You can chat with friends using any other Tox client (qTox, µTox, etc.), ensuring you are never locked into a single app.
- **No Tracking** — No analytics, no telemetry. Your privacy is non-negotiable.
- **Completely Free** — No ads, no subscriptions, no "freemium" features. Just free software.

---

## Screenshots

<img width="542" height="359" alt="Снимок экрана_20260321_001852" src="https://github.com/user-attachments/assets/973dd3ae-c5e9-4899-b324-370e57bb19eb" />


<img width="907" height="579" alt="Снимок экрана_20260321_152515" src="https://github.com/user-attachments/assets/55026292-cfd5-4358-b051-7accebd82b94" />

---

## Getting Started

### Linux (Debian/Ubuntu)
The easiest way to get started is to download the latest release from our [Releases page](https://github.com/Kolyadual/messthon/releases).

For those who prefer to build from source:

```bash
# Clone the repository
git clone https://github.com/Kolyadual/messthon.git
cd messthon

# Install build dependencies
sudo apt update
sudo apt install build-essential cmake git libsodium-dev libopus-dev libvpx-dev \
    qt5-qmake qtbase5-dev qttools5-dev-tools libqt5sql5-sqlite libqt5svg5-dev \
    libavcodec-dev libavdevice-dev libavfilter-dev libavformat-dev libavutil-dev \
    libswscale-dev libopenal-dev libxss-dev libqrencode-dev libgtk2.0-dev

sudo bash systemr.sh

# Create a build directory
mkdir build && cd build

# Configure and build
cmake ..
make -j$(nproc)

# Install
sudo make install

```

# How to Connect with Friends

- **Launch Messthon** using command messthon or with desktop file in your tray
- **Copy your Tox ID** . This is your unique, private address. You'll find it in the profile settings or the main window.
- **Share your Tox ID** with a friend (via a secure channel like a messenger or in person).
- **Add your friend** . Click "Add Friend" and paste their Tox ID. They will receive a friend request.
- **Start chatting** ! Once accepted, you are connected directly and securely.

# Contributing

I believe in the power of community. Whether you're a developer, a designer, or just a passionate user, your contribution can make Messthon better.

- **Report Bugs:** Use our <a href="https://github.com/Kolyadual/messthon/issues">Issues tracker</a>
- **Discuss Ideas:** Start a discussion in the <a href="https://github.com/Kolyadual/messthon/discussions">Discussions tab</a>
- **Code:** We welcome pull requests! Please read our <a href="https://contributing.md/">Contributing Guidelines</a> first

# License

Messthon is free software, licensed under the GNU General Public License v3.0. This means you are free to use, study, share, and improve the software.

# Team & Acknowledgments

- **Lead Developer:** <a href="https://github.com/Kolyadual">Kolyadual</a>
- **Based on the work of:** The <a href="https://github.com/qTox/qTox/graphs/contributors">qTox Project Contributors</a>, the <a href="https://github.com/TokTok">TokTok Team</a> (for the amazing toxcore library), and the entire open-source community.

# Support the Project

If you like Messthon, please give it a star on GitHub! It helps others discover the project and shows us that you care.

https://api.star-history.com/svg?repos=Kolyadual/messthon&type=Date

## Messthon – Privacy is not a feature, it's a right.
