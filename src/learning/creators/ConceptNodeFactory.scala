package learning.creators

import learning.core.{Terminals, TerminalNodeFactory}

class ConceptNodeFactory extends TerminalNodeFactory {
    override val terminalType: Terminals.Value = Terminals.CONCEPT

//    "user" -> "(me|myself|i|us|we|ourselves|ourself|you)",
//    "product / company" -> "(it|the|that|there|they|dropbox|ebay|evernote|mint|todoist|fitbit)",
//    "action" -> "(download|upload|sync|synchronize|open|access|save|store|edit|delete|remove|erase|create|pay|backup|drag|drop|import|export|share|copy|paste|clip)",
//    "object" -> "(note|notes|snippet|video|photo|media|notification|pdf|document|documents|attachment|attachments|lists|list|files|widget|pages|check lists|check list|checklist|tag|tags|storage|notebook|reminder)",
//    "component" -> "(button|panel|toolbar|window|menu|dropdown|sidebar|screen|shortcut|hot key)",
//    "device" -> "(device|devices|phone|tablet|laptop|notebook|desktop|pc|iphone|galaxy s5|nexus 4|tab S10.5)",
//    "platform" -> "(android|ios|lollipop|linux|windows|blackberry|webos|mac)",
//    "bug" -> "(bug|bugs|crash|crashes|error|problem|fix|issue)",
//    "update" -> "(fix|update|updated|updates|changes|changed|new version|bug)"

    override val categories: Seq[String] = Seq(
        "(me|myself|i|us|we|ourselves|ourself|you)",
        "(it|the|that|there|they|dropbox|ebay|evernote|mint|todoist|fitbit)",
        "(download|upload|sync|synchronize|open|access|save|store|edit|delete|remove|erase|create|pay|backup|drag|drop|import|export|share|copy|paste|clip)",
        "(note|notes|snippet|video|photo|media|notification|pdf|document|documents|attachment|attachments|lists|list|files|widget|pages|check lists|check list|checklist|tag|tags|storage|notebook|reminder)",
        "(button|panel|toolbar|window|menu|dropdown|sidebar|screen|shortcut|hot key)",
        "(device|devices|phone|tablet|laptop|notebook|desktop|pc|iphone|galaxy s5|nexus 4|tab S10.5)",
        "(android|ios|lollipop|linux|windows|blackberry|webos|mac)",
        "(bug|bugs|crash|crashes|error|problem|fix|issue)",
        "(fix|update|updated|updates|changes|changed|new version|bug)"
    )
}
