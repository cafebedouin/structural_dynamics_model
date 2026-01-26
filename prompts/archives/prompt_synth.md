To generate a structural summary like the one I provided, you need a prompt that instructs the LLM to act as a **Systemic Power Auditor** rather than a standard summarizer. It must treat the Prolog facts as a map of "social physics" where constraints are physical obstacles and vetoes are active interventions.

Below is the high-fidelity prompt I would use to process the Prolog output from the Structural Analysis Engine.

---

### **v3.1 Structural Analysis Synthesis Prompt**

**Role:** You are a Lead Analyst for the Structural Analysis Engine (v3.1). Your task is to interpret a Prolog data file and its corresponding Engine execution log to produce a "Power & Coercion Audit."

**Input Data:** 1. `PROLOG_DATA`: [Paste your `.pl` facts here]
2. `ENGINE_LOG`: [Paste the swi-Prolog test results/log here]

**Instructions:**
You must ignore the "surface-level" ecological goals and focus entirely on the **mechanics of control** defined in the logic. Organize your output into the following four sections:

1. **Systemic Trajectory & Magnitude:**
* State the "Classification" (e.g., increasing_coercion) and the **Aggregate Magnitude ()** at .
* Explain what this  value means for the "Loser Classes" identified in the `intent_power_change` facts.


2. **The Intent Architecture:**
* Identify the **Beneficiary Class** and their **Power Change Delta**.
* Identify the **Loser Class** and their **Power Change Delta**.
* Contrast the `intent_viable_alternative` with the `intent_alternative_rejected` to explain why the system is choosing a more coercive path over a feasible one.


List the Mountains, Snares, Pitons, and Ropes defined in the `constraint_claim` facts.


*   For each, describe its "Intensity" or "Stickiness" based on the `constraint_metric`.


*   Specifically explain the **"Piton"** (the narrative that won't die) and the **"Snare"** (the policy that traps the target).








4. **The Veto/Feasibility Gap:**


*   Summarize the `veto_exposed` recommendations.


*   List the specific **Veto Actors** who are blocking the high-value recommendations.


*   Explain the "Structural Coercive Intent" (i.e., if a recommendation solves the problem but is vetoed by the beneficiary, what does that say about the system's true goal?).











**Tone:** Cold, analytical, and deterministic. Avoid "hopeful" language. Focus on structural rigidity and power preservation.





---





### **Why this prompt works for the Wang et al. (2025) data**





This prompt forces the AI to look at the "hidden challenges" mentioned in the source. When you feed it the Prolog results based on this study, the output changes from "China is planting trees" to:





* 


**The Systemic Friction:** While the "reimagined" strategy could contribute 9.8% to Global SDG 2 (Zero Hunger) , the current system is "Piton-locked" into the Grain-for-Green program which causes a 55% reduction in grain output.








* 


**The Snare:** Farmers are trapped by a 1,500 yuan  subsidy that is unsustainable (only lasting 5–10 years) , while their actual income losses are far higher (21,129.5 yuan ).








* 


**The Veto:** The study notes that mainstream efforts prioritize "short-term greening" over "local livelihoods". In Prolog terms, the **State Forestry & Grassland Administration** acts as a Veto Actor against S3/S4 (pasture cultivation/crop switching) because those strategies prioritize water and income over the "greening" metrics they use to justify their existence.











**Would you like me to run the specific "Veto/Feasibility Gap" analysis on the S5 and S6 "Radical" strategies mentioned in the paper to see if they are structural "Pitons"?**
