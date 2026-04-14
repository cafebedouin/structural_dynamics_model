% ============================================================================
% CONSTRAINT STORY: chemical_risk_postponement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chemical_risk_postponement, []).

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
 *   constraint_id: chemical_risk_postponement
 *   human_readable: Chemical Risk Postponement: Regulatory Arbitrage and Burden Shifting
 *   domain: environmental_health/chemical_regulation/public_policy
 *
 * SUMMARY:
 *   Chemical risk postponement is a regulatory mechanism that delays
 *   restrictions on chemicals with documented or suspected hazards,
 *   ostensibly to allow industry time to develop safer alternatives or to
 *   permit continued use pending complete scientific certainty. Structurally,
 *   it operates as an asymmetric burden-shifting device: those bearing
 *   exposure costs (workers, consumers, developing nations) have minimal
 *   agency in postponement decisions, while those benefiting from continued
 *   chemical use (manufacturers, downstream product companies, capital
 *   owners) have substantial lobbying capacity and regulatory influence. The
 *   constraint exhibits simultaneous coordination and extraction functions:
 *   manufacturers genuinely need supply stability and phase-out coordination
 *   (Rope perspective), but the postponement mechanism is sustained through
 *   suppression (information asymmetry, regulatory capture, slow assessment
 *   processes) that keeps exposed populations locked in place. The theater
 *   ratio (0.64) reflects that risk assessments are conducted but rarely
 *   result in restrictions; safety claims are published but often reflect
 *   manufacturer-funded research; phase-out timelines are announced but
 *   repeatedly extended. The constraint's core extraction mechanism is
 *   temporal: decisions made by those with institutional power (executives,
 *   regulators, policy makers) are deferred, but exposure costs are borne by
 *   those without decision-making power (workers, consumers, future
 *   generations). This temporal mismatch between decision-maker cohorts and
 *   bearing-cohorts is the mechanism that sustains suppression at high levels
 *   despite surface coordination logic.
 *
 * KEY AGENTS:
 *   - Chemical manufacturers: Primary beneficiaries (institutional/arbitrage) — capture postponement benefits through continued production and avoided reformulation costs; possess regulatory arbitrage capacity.
 *   - Exposed workers: Primary victims (powerless/trapped) — bear biological costs with zero agency in postponement decisions; economic dependency prevents exit.
 *   - Consumer populations: Primary victims (powerless/trapped) — exposed through product use with information asymmetry about chemical composition and risk status.
 *   - Environmental systems: Victim (structural, no agency) — bear bioaccumulation and persistence costs; no decision-making capacity.
 *   - Future generations: Victim (powerless/temporal displacement) — bear latency-period disease costs without participating in postponement decisions.
 *   - Regulatory agencies: Institutional beneficiary (institutional/constrained) — capacity-limited but also partly captured; benefit from industry pressure to avoid stringent restrictions.
 *   - Environmental advocates: Organized secondary actor (moderate/constrained) — possess some litigation capacity and transparency tools but constrained by resource asymmetry.
 *   - Developing nations: Constrained institutional victim (institutional/constrained) — receive postponed-restriction chemicals as imports; lack domestic assessment capacity.
 *   - Analytical observer: Sees full structure (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable industrial features.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chemical_risk_postponement, 0.58).
domain_priors:suppression_score(chemical_risk_postponement, 0.68).
domain_priors:theater_ratio(chemical_risk_postponement, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chemical_risk_postponement, extractiveness, 0.58).
narrative_ontology:constraint_metric(chemical_risk_postponement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(chemical_risk_postponement, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chemical_risk_postponement, tangled_rope).
narrative_ontology:human_readable(chemical_risk_postponement, "Chemical Risk Postponement: Regulatory Arbitrage and Burden Shifting").
narrative_ontology:topic_domain(chemical_risk_postponement, "environmental_health/chemical_regulation/public_policy").

domain_priors:requires_active_enforcement(chemical_risk_postponement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chemical_risk_postponement, chemical_manufacturers).
narrative_ontology:constraint_beneficiary(chemical_risk_postponement, downstream_product_companies).
narrative_ontology:constraint_beneficiary(chemical_risk_postponement, regulatory_agencies_with_limited_capacity).
narrative_ontology:constraint_victim(chemical_risk_postponement, exposed_workers).
narrative_ontology:constraint_victim(chemical_risk_postponement, consumer_populations).
narrative_ontology:constraint_victim(chemical_risk_postponement, environmental_systems).
narrative_ontology:constraint_victim(chemical_risk_postponement, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED WORKERS (SNARE) — Trapped in manufacturing jobs with direct chemical exposure. Cannot exit without severe economic penalty (relocate, lose specialization, abandon pension/benefits). Bear full biological cost of postponement. Maximum extractiveness from workers' perspective: exposure occurs with zero agency or compensation alternatives available.
constraint_indexing:constraint_classification(chemical_risk_postponement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER POPULATIONS (SNARE) — Trapped in exposure to products containing postponed-risk chemicals. Cannot easily exit consumer markets; exposure is structural to modern life. No transparency about chemical composition or risk status. Suppression operates through information asymmetry and regulatory theater (safety claims with weak evidentiary basis).
constraint_indexing:constraint_classification(chemical_risk_postponement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ADVOCATES AND CIVIL SOCIETY (TANGLED ROPE) — Constrained by resource limitations and information gaps, but organized with some agency. Benefit from increased transparency requirements and litigation capacity (part of the coordination function). Experience significant extraction: campaigns are resource-intensive, litigation against manufacturers is asymmetric, and victories often result in graduated phase-out rather than immediate bans. Moderate power with constrained exit (funding dependency, legal system barriers).
constraint_indexing:constraint_classification(chemical_risk_postponement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHEMICAL MANUFACTURERS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination: postponement allows continued product development, market expansion, and avoided R&D costs for safer alternatives. Possess arbitrage options (regulatory shopping across jurisdictions, reformulation timing, lobbying capacity). View postponement as solving a collective action problem: if all firms must phase out simultaneously, competitive dynamics and innovation costs rise for all. Rope-type coordination that generates significant asymmetric extraction for others.
constraint_indexing:constraint_classification(chemical_risk_postponement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCIES (SCAFFOLD) — See postponement as a temporary coordination mechanism bridging old (unsafe legacy chemicals) and new (safer alternatives). Organized but constrained by political pressure and limited capacity. Sunset logic is implicit: accelerating chemical safety assessments, expanding screening capacity, and strengthening alternatives markets are building pathways to genuine phase-out. Experience extraction through institutional capture (industry influence on timelines) but also benefit from clarity in transitional rules. Scaffold classification reflects belief in sunset mechanism and agency in constructing exit pathway.
constraint_indexing:constraint_classification(chemical_risk_postponement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORK (PITON) — Traditional chemical regulation (burden of proof on regulators, grandfathering of pre-1976 chemicals, slow assessment cycles) persists through institutional inertia despite widespread recognition of failure. The theater is high: risk assessments are conducted but rarely result in restrictions; safety data is submitted by manufacturers (conflict of interest); phase-out timelines are protracted. The framework is degraded — newer systems (precautionary principle, toxicity screening, accelerated assessment) have demonstrated superiority but haven't fully replaced legacy structures. Theater maintains piton classification.
constraint_indexing:constraint_classification(chemical_risk_postponement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DEVELOPING NATIONS (TANGLED ROPE) — Constrained by economic dependence on chemical imports and limited domestic regulatory capacity. Experience postponement as dual extraction: (1) manufactured goods from developed countries contain chemicals already restricted domestically in origin countries (dumping effect), and (2) informal workers in processing/recycling sectors bear exposure costs with no regulatory oversight. Some coordination benefit exists (international trade norms, eventual technology transfer of safer alternatives) but is asymmetrically distributed. Constrained exit options (WTO obligations, trade agreements, capital requirements for alternatives).
constraint_indexing:constraint_classification(chemical_risk_postponement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CONSUMER PRODUCT COMPANIES (TANGLED ROPE) — Powerful but experience constraint differently than raw chemical manufacturers. Benefit from postponement (continued supply security, lower reformulation costs) but also face reputational and litigation risks. Mobile exit options exist (can reformulate, source from safer suppliers, switch jurisdictions) but coordination benefits from postponement remain significant (market stability, competitive parity). Experience moderate extraction relative to benefits. Tangled rope reflects genuine coordination function (supply stability) alongside asymmetric extraction (externalized health costs).
constraint_indexing:constraint_classification(chemical_risk_postponement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, chemical risk postponement might appear as an immutable feature of modern industrial systems: complex supply chains require ingredient standardization, regulatory systems require time for assessment, and complete phase-out of legacy chemicals appears physically impossible given infrastructure lock-in. This perspective risks naturalizing what is actually a contingent institutional arrangement driven by power asymmetries and regulatory capture.
constraint_indexing:constraint_classification(chemical_risk_postponement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chemical_risk_postponement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chemical_risk_postponement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chemical_risk_postponement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chemical_risk_postponement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chemical_risk_postponement, TR),
    TR >= 0.70.

:- end_tests(chemical_risk_postponement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing over time. Initial extractiveness (0.35) reflects genuine coordination challenges when postponement begins — manufacturers do need supply stability, and regulators do need time for assessment. But extractiveness increases to 0.58 because postponement becomes a permanent feature: assessment timelines extend indefinitely (US TSCA assessments take 10+ years despite stated deadlines), chemicals are grandfathered under old regulations indefinitely, and 'precautionary phase-out' becomes 'indefinite use pending perfect certainty.' The trajectory reflects that postponement mechanisms, once created, are used for extraction rather than genuine transition. Suppression (0.68): High and structural. Multiple suppression mechanisms operate simultaneously: (1) Information suppression — chemical composition is claimed as proprietary information; safety data is submitted by manufacturers under confidentiality claims; (2) Regulatory suppression — slow assessment cycles, limited agency funding, high evidentiary thresholds for restrictions; (3) Economic suppression — workers cannot exit without losing livelihood; consumers cannot identify or avoid chemicals; (4) Political suppression — industry lobbying capacity vastly exceeds public health advocacy funding. Theater ratio (0.64): Moderate-high and increasing. Risk assessments are conducted (theater) but rarely result in restrictions (function). Safety determinations are announced (theater) but based on manufacturer-funded research and conservative dose-response assumptions (theater mechanism). Phase-out timelines are published (theater) but repeatedly extended (function deferred). The theater has increased over the interval as assessment processes have become more elaborate while restriction mechanisms have atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Manufacturers experience pure coordination (Rope) — postponement solves the collective action problem of staggered phase-out. Regulatory agencies experience temporary coordination with sunset (Scaffold) — they see assessment processes eventually producing restrictions and alternatives markets maturing. Environmental advocates experience mixed coordination and extraction (Tangled Rope) — they must lobby for restrictions but benefit from information transparency requirements. Exposed workers experience pure extraction (Snare) — they bear biological costs with zero decision-making agency. Consumer populations experience extraction with suppression (Snare) — they are unaware of exposure and cannot exit product markets. Future generations experience extraction with temporal displacement (Snare) — they bear latency-period disease costs from decisions they did not make. The developing nations perspective (Tangled Rope) reveals that postponement in one jurisdiction becomes dumping in others. The legacy regulatory framework perspective (Piton) shows that outdated assessment structures are maintained through institutional inertia despite superior alternatives existing. The analytical observer risks the false summit (Mountain) — naturalizing contingent institutional power imbalances as inherent limits of chemical governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent structural position and power. Manufacturers as beneficiaries with arbitrage options derive low d (~0.08-0.15), producing negative effective extraction (chi < 0). Exposed workers as victims with trapped exit derive very high d (~0.95), producing maximum effective extraction. Consumers as victims with trapped exit derive high d (~0.88-0.95). Environmental advocates as moderate-power organized victims derive moderate d (~0.65-0.75), producing high chi despite moderate power. Regulatory agencies occupy an ambiguous position: they claim constrained exit (Scaffold perspective) but possess institutional arbitrage capacity (ability to stringently assess and restrict). An override is recommended to raise their d from derived ~0.30 (institutional beneficiary with some constraints) to ~0.45-0.50, reflecting their partial capture and their role in sustaining postponement despite alternatives existing. The directionality spread across perspectives (low d for beneficiaries, very high d for workers, high d for consumers) produces the perspectival gap: each agent experiences qualitatively different effective extraction values from the same base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   Chemical risk postponement resolves the mandatrophy by demonstrating that classification varies legitimately across positions while revealing a structural preference for Snare and Tangled Rope over Rope. From the manufacturer perspective, the constraint genuinely solves coordination problems (Rope is accurate). From the worker/consumer perspective, the constraint is pure extraction (Snare is accurate). From the regulatory perspective, the constraint contains both functions with sunset logic (Scaffold is plausible). The mandatrophy is resolved by accepting that all four types (Rope, Snare, Tangled Rope, Scaffold) are structurally accurate from their respective positions while noting that the power asymmetry means the Snare and Tangled Rope perspectives represent the constraint's dominant functional reality. The false summit (Mountain/natural law view) is rejected: postponement is not an inevitable feature of chemical governance but a contingent institutional choice reflecting regulatory capture and power asymmetries. The engine's computed constraint_claim from the analytical perspective should identify this as a high-confidence Tangled Rope with extractiveness trending toward Snare, not a Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_claimed_safety_margins,
    'What proportion of postponement delays reflect genuine scientific uncertainty vs manufactured uncertainty by manufacturers?',
    'Meta-analysis of manufacturer safety claims vs independent academic assessments; comparison of assessment timelines across jurisdictions (EU vs US vs developing nations); tracking of chemicals where postponement ended vs where postponement continued indefinitely.',
    'If genuine uncertainty dominates: postponement is legitimate coordination (Rope from most perspectives). If manufactured uncertainty dominates: postponement is extractive cover story (Snare from worker/consumer perspectives confirmed; Tangled Rope reassessed upward in extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_vs_claimed_safety_margins, empirical, 'Genuine vs manufactured scientific uncertainty in safety claims').

omega_variable(
    substitution_barrier_reality,
    'Are technical substitutes for postponed chemicals genuinely unavailable, or are they available but economically disadvantaged by market structures that favor the postponed chemical?',
    'Patent analysis and technology availability studies; cost-benefit analysis comparing actual reformulation timelines across companies; identification of which companies reformulated vs which lobbied for postponement extension.',
    'If substitutes genuinely unavailable: postponement solves real coordination problem (Scaffold sunset logic is real). If substitutes available but economically disadvantaged: manufacturers are choosing postponement over viable alternatives (Snare extraction confirmed; extractiveness reassessed upward).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_barrier_reality, empirical, 'Availability of technical substitutes vs economic factors preventing substitution').

omega_variable(
    exposure_dose_relationship_contestation,
    'Is the dose-response relationship for postponed chemicals genuinely uncertain, or is uncertainty manufactured to justify continued exposure?',
    'Systematic review of manufacturer-funded vs independent safety studies; comparison of assessment conclusions across regulatory jurisdictions; tracking of chemicals where new evidence later revealed higher risk than ''postponement-justified'' assessments claimed.',
    'If genuinely uncertain: suppression is partly justified by epistemic limitation (Tangled Rope). If manufactured: suppression is cover story for extraction (Snare with high theater ratio).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exposure_dose_relationship_contestation, empirical, 'Genuine vs manufactured uncertainty in dose-response relationships').

omega_variable(
    regulatory_capture_mechanisms,
    'What proportion of postponement delays result from regulatory agency capture by industry vs genuine capacity limitations?',
    'Funding source analysis (industry funding of regulatory agencies); revolving door analysis (regulator careers before/after agency service); comparison of assessment timelines and stringency across jurisdictions with different agency funding structures.',
    'If capture dominates: beneficiary perspective (manufacturers) understates directionality; directionality_override required for regulatory agencies (d should be higher). If capacity dominates: scaffold sunset logic is real but constrained by resource rather than intentional postponement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanisms, empirical, 'Regulatory capture vs capacity constraints as drivers of postponement').

omega_variable(
    intergenerational_harm_asymmetry,
    'How much of the postponement cost falls on individuals who made no decisions and who cannot influence the constraint (future generations, workers not yet born, consumers at time of chemical deployment)?',
    'Lifecycle tracking of chemical exposure cohorts; latency period analysis for chronic diseases; intergenerational exposure modeling; comparison of decision-maker cohorts (executives/regulators making postponement decisions) vs bearing cohorts (workers/consumers experiencing exposure).',
    'If asymmetry is high: suppression metric should be adjusted upward to reflect that exit options don''t exist for non-deciding cohorts. If asymmetry is extreme (future generations have zero agency), this may justify reclassification toward Snare despite moderate theater (extraction mechanism is temporally distributed beyond decision horizon).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_harm_asymmetry, empirical, 'Intergenerational harm asymmetry and decision-maker vs bearing-cohort mismatch').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chemical_risk_postponement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chrisk_tr_t0, chemical_risk_postponement, theater_ratio, 0, 0.48).
narrative_ontology:measurement(chrisk_tr_t10, chemical_risk_postponement, theater_ratio, 10, 0.6).
narrative_ontology:measurement(chrisk_tr_t20, chemical_risk_postponement, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(chrisk_be_t0, chemical_risk_postponement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chrisk_be_t10, chemical_risk_postponement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(chrisk_be_t20, chemical_risk_postponement, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chemical_risk_postponement, enforcement_mechanism).
narrative_ontology:affects_constraint(chemical_risk_postponement, regulatory_capture_in_chemical_safety).
narrative_ontology:affects_constraint(chemical_risk_postponement, information_asymmetry_in_product_disclosure).
narrative_ontology:affects_constraint(chemical_risk_postponement, intergenerational_environmental_harm).
narrative_ontology:affects_constraint(chemical_risk_postponement, worker_occupational_health_suppression).

% DUAL FORMULATION NOTE:
% Chemical risk postponement decomposes into structurally distinct constraints: (1) postponement of specific chemicals (ε varies by chemical; PFOA postponement has ε~0.72 with multi-decade extraction; many pesticides have ε~0.55-0.65), (2) regulatory capacity limitations that enable postponement (ε~0.35; genuine infrastructure problem), (3) regulatory capture that sustains postponement despite capacity (ε~0.62; institutional failure). This story models the generic postponement mechanism (ε=0.58); specific chemical cases should be decomposed into their own stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chemical_risk_postponement, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
