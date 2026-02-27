% ============================================================================
% CONSTRAINT STORY: lung_transplant_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lung_transplant_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lung_transplant_protocol
 *   human_readable: Lung Transplant Allocation Protocol
 *   domain: healthcare/organ_allocation
 *
 * SUMMARY:
 *   The lung transplant allocation protocol in the United States
 *   (administered by UNOS) governs the distribution of a scarce,
 *   non-renewable medical resource — donor lungs — to patients with end-stage
 *   lung disease. The protocol ostensibly coordinates organ matching,
 *   transport, and surgical scheduling to maximize transplant access and
 *   equity. However, the constraint exhibits significant asymmetries:
 *   transplant centers that participate in the allocation network benefit
 *   from patient access, regulatory legitimacy, and outcome recognition;
 *   patients who meet listing criteria experience mixed coordination (organ
 *   matching) and extraction (waiting time, medical burden, geographic
 *   barriers); patients excluded from listing experience pure extraction
 *   (denial of access with no exit option); and geographically remote and
 *   economically disadvantaged populations systematically receive lower
 *   transplant access, a structural feature that the allocation protocol
 *   enables rather than corrects. The theater ratio (0.48) reflects that much
 *   of the protocol is administrative signaling (ranking systems, scoring
 *   algorithms, fairness documentation) rather than direct allocation
 *   function. The extractiveness (0.52) indicates that the coordination
 *   function is meaningful but asymmetric: beneficiaries (transplant centers,
 *   high-access patients) gain substantially; victims (excluded patients,
 *   rural populations) lose substantially. The constraint is best classified
 *   as Tangled Rope: it has genuine coordination value (organ matching,
 *   surgical readiness) and genuine extraction (power asymmetry, geographic
 *   exclusion).
 *
 * KEY AGENTS:
 *   - Transplant Centers: Primary beneficiary (institutional/arbitrage) — benefit from patient access, outcome recognition, regulatory authority
 *   - Listed Patients: Mixed victim-beneficiary (moderate/constrained) — benefit from allocation coordination but bear waiting costs and medical burden
 *   - Excluded Patients: Primary victim (powerless/trapped) — denied access with no appeal mechanism; cannot exit medical need
 *   - Rural and Economically Disadvantaged Populations: Structural victim (powerless/constrained) — systematically lower access due to geographic and resource barriers
 *   - UNOS (Regulatory Authority): Institutional beneficiary (powerful/arbitrage) — extracts legitimacy and authority from managing allocation
 *   - Organ Procurement Organizations: Secondary beneficiary (organized/mobile) — benefit from protocol structure and coordination function
 *   - Analytical Observer: Global perspective (analytical/analytical) — sees protocol as extracting transplant capacity and surgeon talent from lower-resource regions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lung_transplant_protocol, 0.52).
domain_priors:suppression_score(lung_transplant_protocol, 0.65).
domain_priors:theater_ratio(lung_transplant_protocol, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lung_transplant_protocol, extractiveness, 0.52).
narrative_ontology:constraint_metric(lung_transplant_protocol, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(lung_transplant_protocol, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lung_transplant_protocol, tangled_rope).
narrative_ontology:human_readable(lung_transplant_protocol, "Lung Transplant Allocation Protocol").
narrative_ontology:topic_domain(lung_transplant_protocol, "healthcare/organ_allocation").

domain_priors:requires_active_enforcement(lung_transplant_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lung_transplant_protocol, transplant_centers).
narrative_ontology:constraint_beneficiary(lung_transplant_protocol, patients_on_allocation_list).
narrative_ontology:constraint_victim(lung_transplant_protocol, excluded_patients).
narrative_ontology:constraint_victim(lung_transplant_protocol, rural_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PATIENT (SNARE) — Patients not listed or delisted due to comorbidities, geographic distance, or resource limitations face extraction with no exit. Cannot exit the constraint (medical need is non-negotiable); cannot challenge allocation decisions; bears full cost of scarcity rationing. Extraction is maximal and suppression is high.
constraint_indexing:constraint_classification(lung_transplant_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LISTED PATIENT (TANGLED ROPE) — Patients on the transplant list experience mixed extraction and coordination. The allocation protocol coordinates organ matching (genuine benefit) but also extracts time, medical burden, and waiting costs. Some exit options exist (delisting, moving to different region) but at high cost. Constrained rather than trapped; moderate agency.
constraint_indexing:constraint_classification(lung_transplant_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRANSPLANT CENTER (ROPE) — Hospitals operating transplant programs benefit from allocation priority and institutional recognition. The protocol enables them to coordinate organ arrival, surgical scheduling, and recipient matching efficiently. Net beneficiary of the constraint; exit options exist (decline participation, transfer program) but are rarely exercised. Experience extraction flowing toward the institution.
constraint_indexing:constraint_classification(lung_transplant_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONAL REGULATORY AUTHORITY (TANGLED ROPE) — UNOS (United Network for Organ Sharing) and CMS govern the allocation protocol. They coordinate national organ distribution (genuine coordination function) but also extract legitimacy and authority from managing life-and-death decisions. Regulatory authority has high exit options (restructure policy) and powerful position but also faces pushback from transplant centers and patient advocates. Active enforcement required; genuine coordination benefit present alongside extraction of regulatory power.
constraint_indexing:constraint_classification(lung_transplant_protocol, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WAITLIST THEATER (PITON) — The numerical ranking system and waiting-time scores are substantially performative. The protocol produces elaborate rankings and scores that signal fairness and objectivity while masking that allocation fundamentally reflects transplant center capacity, geographic access, and medical urgency factors that the scoring system cannot fully capture. Theater ratio is high (>0.60) because much of the protocol is documentation and justification rather than functional allocation. The waitlist persists through institutional inertia — alternative allocation mechanisms (continuous organ sharing, regional priority) exist but haven't fully replaced the legacy list-based approach.
constraint_indexing:constraint_classification(lung_transplant_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global health perspective, the U.S. transplant protocol coordinates organ distribution within a high-resource setting while extracting outcomes from lower-resource populations through brain drain of transplant surgeons and by concentrating life-saving technology in wealthy regions. The protocol has genuine coordination function domestically but exhibits asymmetric extraction internationally. Universalized scope reveals the implicit asymmetry: the beneficiaries are primarily U.S. citizens; the victims are global populations without access to the coordination benefit.
constraint_indexing:constraint_classification(lung_transplant_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lung_transplant_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lung_transplant_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lung_transplant_protocol, TR),
    TR >= 0.70.

:- end_tests(lung_transplant_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The protocol extracts from excluded and disadvantaged patients through systematic exclusion and geographic barriers, but the extraction is not total (some patients do receive transplants; some centers do serve remote areas). The value reflects that extraction is structural rather than intentional, but measurable in outcome disparities. Suppression (0.65): Moderate-high. Barriers to challenging the protocol include: limited patient appeals processes, medical complexity that restricts challenge legitimacy, institutional power concentration in UNOS and transplant centers, and scarcity itself (even optimal allocation leaves many patients untransplanted). Theater ratio (0.48): Moderate. Allocation algorithms, ranking systems, and fairness documentation represent significant administrative content, but the core coordination function (matching organs to compatible recipients, coordinating surgery logistics) is functionally necessary. Theater has increased over the 20-year interval as algorithms have become more complex and outcome metrics more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   Transplant centers experience the protocol as cooperative coordination — UNOS enables them to access organs regionally and nationally, reducing coordination costs. Listed patients experience mixed value — the protocol matches them with compatible organs (benefit) but also imposes waiting costs and medical burden (extraction). Excluded patients experience pure extraction — they are denied access with no functional appeal mechanism. Rural and economically disadvantaged populations experience systematic exclusion structured into geographic priority and center capacity allocation. The global analytical observer sees international extraction: the U.S. protocol concentrates life-saving technology in wealthy regions while extracting transplant surgeon talent from lower-resource countries. The gap between the transplant center's experience (coordination, legitimacy) and the excluded patient's experience (extraction, powerlessness) is maximal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position within the extraction flow. Transplant centers are beneficiaries with arbitrage options (they can decline participation or operate independently) — they derive low d, experience coordination. Listed patients are partial beneficiaries with constrained exit (they benefit from organ matching but cannot easily exit the waiting list or change regions) — they derive moderate d, experience mixed value. Excluded patients are victims with trapped exit (medical need cannot be negotiated; exclusion is non-negotiable) — they derive maximum d, experience pure extraction. Rural populations are victims with constrained exit (geographic barriers are structural; some mobility possible but costly) — they derive high d, experience systematic extraction. UNOS derives moderate d as an institutional actor — they coordinate the system (beneficiary function) but also monopolize allocation authority (extraction of power). The global analytical observer derives analytical d, seeing the protocol's international effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The protocol resolves the mandatrophy by distinguishing coordination from extraction through beneficiary/victim analysis. The protocol HAS a genuine coordination function: it solves the non-trivial problem of matching organs to recipients, coordinating surgical logistics, and distributing access information nationally. This is real coordination. However, the protocol ALSO extracts from certain populations: excluded patients, rural populations, and economically disadvantaged patients systematically receive lower access due to protocol design choices (geographic prioritization, center capacity constraints, comorbidity exclusion). The mandatrophy resolves by recognizing that both are true. The protocol is Tangled Rope because it simultaneously coordinates and extracts. From the transplant center perspective, it is Rope (pure coordination). From the excluded patient perspective, it is Snare (pure extraction). From the listed patient perspective, it is Tangled Rope (mixed). No single classification is 'correct' — the presheaf across perspectives reveals the structural asymmetry: the protocol enables coordination for those admitted to it and enforces exclusion for those denied entry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_justice_threshold,
    'What geographic distance threshold distinguishes legitimate regional coordination from extractive exclusion of rural and remote patients?',
    'Analysis of waitlist composition by geography; comparison of transplant access rates in rural vs urban regions; longitudinal tracking of outcome disparities',
    'If distance effect is primarily logistical (unavoidable): geographic variation is coordination necessary. If distance effect is primarily institutional (policy choices): geographic variation is extractive exclusion. Classification shifts from Rope toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_justice_threshold, empirical, 'Geographic distance as legitimate coordination vs extractive exclusion').

omega_variable(
    comorbidity_objective_threshold,
    'How much of the comorbidity-based exclusion reflects genuine surgical risk vs institutional gatekeeping to improve center outcome statistics?',
    'Comparative risk analysis of excluded vs included patients; audits of comorbidity thresholds across transplant centers; correlation between center outcome rankings and patient selection stringency',
    'If risk-driven: exclusion is medical necessity (coordination). If outcome-driven: exclusion is extraction of patient selection to manipulate performance metrics. Classification shifts from Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comorbidity_objective_threshold, empirical, 'Comorbidity exclusion as medical necessity vs institutional gatekeeping').

omega_variable(
    allocation_optimization_sufficiency,
    'Does the current allocation protocol optimize actual transplant outcomes or primarily optimize fairness signaling and administrative tractability?',
    'Comparison of outcomes under current allocation vs alternative algorithms (e.g., continuous sharing, center-specific waiting lists); measure mortality reduction from allocation improvements vs from surgical technique and immunosuppression',
    'If current allocation is nearly optimal: protocol is functional coordination. If alternatives yield significantly better outcomes: protocol is extractive theater masking suboptimal allocation. Theater ratio increases; classification shifts toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_optimization_sufficiency, empirical, 'Whether allocation protocol optimizes outcomes vs signals fairness').

omega_variable(
    international_outcome_causality,
    'What portion of disparities in transplant access between U.S. and low-resource countries is attributable to the U.S. protocol extracting surgeon talent and resources vs other factors (policy, infrastructure, wealth)?',
    'Longitudinal analysis of surgeon migration patterns; comparison of transplant capacity changes in donor countries before/after surgeon emigration; resource allocation studies tracking transplant infrastructure investment',
    'If U.S. protocol is major driver: international perspective confirms global extraction. If minor factor: protocol is primarily coordinating domestic allocation; international disparities reflect broader health system factors. Affects whether global-scope perspective classified as extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_outcome_causality, empirical, 'Whether U.S. protocol drives international transplant disparity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lung_transplant_protocol, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ltp_tr_t0, lung_transplant_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ltp_tr_t10, lung_transplant_protocol, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ltp_tr_t20, lung_transplant_protocol, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ltp_be_t0, lung_transplant_protocol, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ltp_be_t10, lung_transplant_protocol, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(ltp_be_t20, lung_transplant_protocol, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lung_transplant_protocol, resource_allocation).
narrative_ontology:affects_constraint(lung_transplant_protocol, organ_scarcity_moral_rationing).
narrative_ontology:affects_constraint(lung_transplant_protocol, transplant_center_outcome_gaming).

% DUAL FORMULATION NOTE:
% The lung transplant protocol is downstream of the fundamental scarcity constraint (there are fewer organs than patients) and upstream of institutional gaming constraints (centers may exclude marginal patients to protect outcome statistics). These three constraints form a family linked by causal dependence: scarcity motivates the allocation protocol; the allocation protocol creates incentives for outcome gaming.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lung_transplant_protocol, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
