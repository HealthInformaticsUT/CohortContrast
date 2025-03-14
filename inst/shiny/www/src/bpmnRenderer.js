window.renderBPMNFromData = function(jsonData) {
    console.log("✅ Checking BPMN data from R...", jsonData); // Debugging log

    if (!jsonData || !jsonData.nodes) {
        console.error("❌ ERROR: BPMN data is missing or undefined.");
        return; // Stop execution if no data is provided
    }

    const bpmnContainer = document.getElementById("bpmn-container");
    if (!bpmnContainer) {
        console.error("❌ ERROR: 'bpmn-container' not found.");
        return;
    }

    const bpmnModeler = new BpmnJS({ container: bpmnContainer });

    bpmnModeler.createDiagram().then(() => {
        const modeling = bpmnModeler.get("modeling");
        const elementFactory = bpmnModeler.get("elementFactory");
        const canvas = bpmnModeler.get("canvas");
        const elementRegistry = bpmnModeler.get("elementRegistry");

        const rootElement = canvas.getRootElement();
        const elementsMap = {};

        console.log("✅ Rendering nodes...");

        jsonData.nodes.forEach((node) => {
            if (!node.id || !node.type) {
                console.warn("⚠️ Skipping invalid node:", node);
                return;
            }

            const shape = elementFactory.createShape({
                type: node.type,
                businessObject: bpmnModeler.get("moddle").create(node.type, {
                    id: node.id,
                    name: node.label,
                }),
            });

            modeling.createShape(shape, { x: node.position_x, y: node.position_y }, rootElement);
            elementsMap[node.id] = shape;
        });

        function getEdgeColor(prevalence) {
            if (!prevalence) return "black"; // Default color for missing values

            // Convert prevalence (0-100) into a gradient from Black (0,0,0) to Dark Red-Orange (180,40,40)
            const redValue = Math.round((prevalence / 100) * 180); // Scale red component from 0 to 180
            const greenValue = Math.round((1 - prevalence / 100) * 40); // Subtle green for warmth
            const blueValue = Math.round((1 - prevalence / 100) * 40); // Subtle blue for contrast

            return `rgb(${redValue}, ${greenValue}, ${blueValue})`; // Return readable dark gradient
        }

        console.log("✅ Rendering edges...");
        jsonData.edges.forEach((edge) => {
            const source = elementsMap[edge.source];
            const target = elementsMap[edge.target];

            if (!source || !target) {
                console.warn("⚠️ Skipping invalid edge:", edge);
                return;
            }

            const connection = modeling.connect(source, target, {
                type: "bpmn:SequenceFlow",
            });

            // ✅ Set Edge Labels
            if (edge.label) {
                modeling.updateProperties(connection, {
                    name: edge.label,
                });
            }

            // ✅ Apply Edge Colors Based on Prevalence
            if (edge.prevalence !== undefined) {
                const edgeColor = getEdgeColor(edge.prevalence);
                modeling.setColor(connection, {
                    stroke: edgeColor,
                });
            }
        });

        console.log("✅ BPMN Diagram Rendered Successfully!");
        canvas.zoom(0.75);

        // ✅ Check if "StartEvent_1" exists before removing
        const startEvent = elementRegistry.get("StartEvent_1");
        if (startEvent) {
            modeling.removeElements([startEvent]);
        } else {
            console.warn("⚠️ 'StartEvent_1' not found, skipping removal.");
        }

        // ✅ Add event listeners for dynamic zooming
        bpmnContainer.addEventListener("wheel", (event) => {
            event.preventDefault();
            const zoomFactor = event.deltaY > 0 ? 0.9 : 1.1;
            canvas.zoom(canvas.zoom() * zoomFactor, {
                x: event.offsetX,
                y: event.offsetY,
            });
        });

        // ✅ Optional: Add buttons for zooming in and out
        const zoomInButton = document.getElementById("zoom-in");
        const zoomOutButton = document.getElementById("zoom-out");

        if (zoomInButton && zoomOutButton) {
            zoomInButton.addEventListener("click", () => {
                canvas.zoom(canvas.zoom() * 1.1);
            });

            zoomOutButton.addEventListener("click", () => {
                canvas.zoom(canvas.zoom() * 0.9);
            });
        }
    });
};

// ✅ Make function available globally for Shiny
window.renderBPMNFromData = renderBPMNFromData;
