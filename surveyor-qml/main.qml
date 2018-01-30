import QtQuick 2.7
import QtQuick.Layouts 1.3
import QtQuick.Window 2.2
import QtQuick.Controls 2.0

ApplicationWindow {
    visible: true
    id: mainWindow
    title: "surveyor"

    Column {
        width: parent.width
        height: parent.height

        Component.onCompleted: {
            shutdown.connect(function() {Qt.quit();});
            updateStackIndex.connect(function(ix) {
                layout.currentIndex = ix;
                console.log('New index is ', ix);
            });
            console.log('Connected signals');
        }

        Keys.onReturnPressed: {
            event.accepted = true;
            console.log("onReturnPressed" + minibuffer.text);
            runCommand(minibuffer.text);
            minibuffer.text = '';
        }

        StackLayout {
            id: layout
            currentIndex: 0
            width: parent.width
            height: parent.height - minibuffer.height

            Rectangle {
                id: summary
                color: "brown"
                Layout.fillWidth: true
                Layout.fillHeight: true
                Text {
                    text: "Summary"
                }
            }
            Rectangle {
                id: functionList
                color: "red"
                Layout.fillWidth: true
                Layout.fillHeight: true
                MouseArea {
                    anchors.fill: parent
                    onClicked: hello();
                }
            }
            Rectangle {
                id: functionViewer
                color: "blue"
                Layout.fillWidth: true
                Layout.fillHeight: true
                MouseArea {
                    anchors.fill: parent
                    onClicked: world();
                }
            }
            Rectangle {
                id: callGraph
                color: "green"
                Layout.fillWidth: true
                Layout.fillHeight: true
                MouseArea {
                    anchors.fill: parent
                    onClicked: bye();
                }
            }
            Rectangle {
                id: log
                color: 'orange'
                Layout.fillWidth: true
                Layout.fillHeight: true
                Text {
                    text: "Diagnostics"
                }
            }
        }

        TextInput {
            id: minibuffer
            focus: true
            enabled: true
            height: 20
        }
    }
}
