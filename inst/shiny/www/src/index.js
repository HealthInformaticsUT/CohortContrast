// ✅ Listen for Messages from the Parent Window (Shiny)
window.addEventListener("message", function(event) {
    console.log("✅ BPMN Data Received in iframe:", event.data);

    if (!event.data || !event.data.nodes) {
        console.error("❌ ERROR: Received invalid BPMN data.");
        return;
    }

    // ✅ Call BPMN Renderer with Data
    window.renderBPMNFromData(event.data);
});
