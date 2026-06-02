% ============================================================================
% CONSTRAINT STORY: proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proportionality_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proportionality_reading
 *   human_readable: Proportionality Reading: Coercion Legitimacy Scales with Disease Severity and Transmission Dynamics
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint embodies one reading of a fundamental contest over when
 *   public health authority may legitimately override bodily autonomy. The
 *   proportionality reading holds that coercion justified by disease severity
 *   and transmission dynamics — measles with R0 ~12 and 99% infectivity among
 *   unvaccinated justifies mandates; seasonal flu with R0 ~1-2 and lower
 *   fatality does not. The reading grounds legitimacy in epidemiological
 *   metrics and case-by-case adjudication. It coexists with two sibling
 *   readings: the public-health-primary reading (maximizes disease
 *   suppression regardless of autonomy cost) and the bodily-autonomy-primary
 *   reading (treats medical autonomy as inviolable except under extreme
 *   emergency). The proportionality reading positions itself as the balanced
 *   compromise — restrained, principled, metric-based. However, the
 *   structural data reveals tension: extractiveness (0.52) reflects that the
 *   reading's case-by-case adjudication process is operationally complex and
 *   politically vulnerable to capture. Theater ratio (0.55) indicates that
 *   proportionality hearings involve substantial performative content — risk
 *   metrics are contested, epidemiological projections are speculative, and
 *   outcomes correlate with political factors independent of severity
 *   thresholds. The constraint exhibits tangled-rope structure: genuine
 *   coordination function (legitimate coercion requires scientific
 *   justification) coupled with asymmetric extraction (public health
 *   authority gains gatekeeping power over disease classifications). The
 *   measurement trajectory shows rising extractiveness and theater ratio over
 *   the interval, suggesting gradual decay of the proportionality reading's
 *   functional restraint as authority capture and threshold manipulation
 *   accumulate.
 *
 * KEY AGENTS:
 *   - Population Exposed to Low-Severity Disease Mandate: Primary victim (powerless/trapped) — bears coercion despite proportionality reading's logic that low-severity diseases should not justify mandates; experiences extraction when actual threat profile does not match severity threshold
 *   - Individual Agents with Medical Autonomy Interest: Secondary victim (moderate/constrained) — face cost-benefit trade-off adjudicated by proportionality framework; experience both coordination benefit (disease reduction) and extraction (bodily autonomy loss)
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — gains legitimacy and gatekeeping power through proportionality framework; can claim precision and restraint while maintaining coercion authority
 *   - Epidemiological Science and Disease Classification: Secondary beneficiary (powerful/mobile) — becomes the legitimating apparatus for state coercion; gains influence as arbiter of severity metrics but becomes implicated in enforcement
 *   - Rights-Based Oversight Institutions: Organized agent (organized/constrained) — enforces proportionality constraints via case-by-case adjudication; creates functional sunset logic but faces authority capture risk
 *   - Constitutional Framework: Institutional actor (institutional/arbitrage) — hosts proportionality doctrine as a legitimation mechanism; persists through inertia even after functional restraint degrades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proportionality_reading, 0.52).
domain_priors:suppression_score(proportionality_reading, 0.68).
domain_priors:theater_ratio(proportionality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(proportionality_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(proportionality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proportionality_reading, tangled_rope).
narrative_ontology:human_readable(proportionality_reading, "Proportionality Reading: Coercion Legitimacy Scales with Disease Severity and Transmission Dynamics").
narrative_ontology:topic_domain(proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proportionality_reading, '791bd8dd-77be-4732-a047-27906e35dd2c').
narrative_ontology:cs_created_at('791bd8dd-77be-4732-a047-27906e35dd2c', '').
narrative_ontology:cs_kernel_codification('791bd8dd-77be-4732-a047-27906e35dd2c', formalized).
narrative_ontology:cs_authority_grounding('791bd8dd-77be-4732-a047-27906e35dd2c', extraction).
narrative_ontology:cs_interpretation_layer_present('791bd8dd-77be-4732-a047-27906e35dd2c').
narrative_ontology:cs_kernel_id(proportionality_reading, coercion_legitimacy_boundary).
narrative_ontology:cs_reading_relation('791bd8dd-77be-4732-a047-27906e35dd2c', public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('791bd8dd-77be-4732-a047-27906e35dd2c', bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('791bd8dd-77be-4732-a047-27906e35dd2c', foundational, severity_proportionality_principle).
narrative_ontology:cs_axiom_status(severity_proportionality_principle, holdable).
narrative_ontology:cs_axiom('791bd8dd-77be-4732-a047-27906e35dd2c', foundational, metric_based_adjudication).
narrative_ontology:cs_axiom_status(metric_based_adjudication, holdable).
narrative_ontology:cs_reference_frame('791bd8dd-77be-4732-a047-27906e35dd2c', balanced_restraint_framework).
narrative_ontology:cs_drift_state('791bd8dd-77be-4732-a047-27906e35dd2c', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proportionality_reading, high_severity_disease_protection).
narrative_ontology:constraint_beneficiary(proportionality_reading, public_health_authority).
narrative_ontology:constraint_victim(proportionality_reading, low_severity_disease_populations).
narrative_ontology:constraint_victim(proportionality_reading, medical_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULATION EXPOSED TO LOW-SEVERITY MANDATE (SNARE) — Trapped by legal requirement despite low threat profile. The proportionality reading treats low-R0, low-mortality diseases (seasonal flu, mild variants) as insufficiently severe to justify coercion. Populations mandated under flu restrictions experience extraction without commensurate risk justification. Zero exit options and zero perceived benefit from the constraint. Maximum experienced extraction.
constraint_indexing:constraint_classification(proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL AGENTS WITH AUTONOMY INTEREST (TANGLED ROPE) — Face constrained choice: pay penalty (medical care denial, employment loss, educational exclusion) or comply with mandate. The constraint provides genuine coordination benefit (disease reduction) while simultaneously extracting bodily autonomy. The proportionality reading recognizes this trade but adjudicates it as case-by-case — high-severity diseases justify the extraction, low-severity do not. Moderate power with structured barriers to exit.
constraint_indexing:constraint_classification(proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Benefits from the proportionality framework as a coordination mechanism. Gains institutional legitimacy by grounding coercion in disease severity metrics (R0, mortality, transmission mode). The reading enables the authority to claim precision and principled restraint — 'we coerce only when justified.' High institutional power with exit option (can adjust severity thresholds). Net beneficiary — the proportionality logic institutionalizes public health authority as the arbiter of legitimacy boundaries.
constraint_indexing:constraint_classification(proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EPIDEMIOLOGICAL CLASSIFICATION SYSTEM (TANGLED ROPE) — The proportionality reading depends on disease metrics (R0, case fatality rate, transmission dynamics, mutation rate) to adjudicate coercion legitimacy. This binds the legal/policy system to epidemiological categories, creating a coordination function (legitimate coercion requires scientific justification) while simultaneously extracting authority from scientific discourse (science becomes the legitimating apparatus for state coercion). Scientists gain power as arbiters but also become implicated in enforcement. Mobile at generational scale — new pathogens or new metrics can shift the classification.
constraint_indexing:constraint_classification(proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHTS-BASED OVERSIGHT (SCAFFOLD) — International human rights bodies, constitutional courts, and medical ethics committees enforce proportionality constraints on public health coercion. The scaffold functions via case-by-case adjudication: courts assess whether the severity/transmissibility of a specific pathogen justifies the degree of bodily autonomy restriction proposed. This creates a sunset logic — the coercion remains only as long as the disease meets the severity threshold. Organized agents with constrained leverage but genuine review capacity. Low effective extraction because the oversight mechanism is designed to limit it.
constraint_indexing:constraint_classification(proportionality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL FRAMEWORK INERTIA (PITON) — The proportionality doctrine (coercion justified by disease severity) performs legitimation work without necessarily delivering restraint. Once a proportionality threshold is set, the doctrine can persist through institutional inertia even after the scientific basis shifts or after political capture tilts the adjudication. Theater ratio (0.55) reflects that proportionality hearings often involve procedural theater — risk metrics are contested, comparative analysis is speculative, and the outcome is driven by political rather than epidemiological factors. The doctrine's legitimating power persists even when its restraining function degrades.
constraint_indexing:constraint_classification(proportionality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, proportionality appears as an immutable principle of legitimate governance: coercion must be justified and proportionate to threat. This reading risks naturalizing a specific legal doctrine (proportionality review, originating in 20th-century constitutional law) as a universal principle. The analytical observer's potential mountain classification here reveals the false-summit signature: proportionality is a contested institutional framework, not a natural law. Alternative readings (public-health-primary, bodily-autonomy-primary) foreclose the proportionality reading's logical space within their respective frameworks.
constraint_indexing:constraint_classification(proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proportionality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(proportionality_reading, TR),
    TR >= 0.70.

:- end_tests(proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The proportionality reading creates a coordination function (disease control requires justified coercion) but simultaneously extracts authority from the adjudication process. Public health authorities gain power to define severity thresholds and determine which diseases justify which restrictions. The reading's case-by-case logic creates processing costs that centralize decision-making. However, extraction is not maximal (0.70+) because rights-review institutions provide genuine oversight and metrics-based adjudication creates some transparency. The measurement trajectory (0.35→0.52) shows extractiveness accumulating as authority capture and threshold manipulation grow over time. Suppression (0.68): Moderate-high. Populations classified as low-severity-disease-exposed face legal requirements (vaccine mandates, medical care denial, employment conditions) with limited exit options. The suppression is structural and legal — barriers to exit are formal and enforced. However, suppression is not extreme (0.85+) because some escape hatches exist (medical exemptions, jurisdictional shopping, non-compliance costs). Theater ratio (0.55): Moderate. Proportionality adjudication involves performance — severity metrics are presented as scientific facts even when epidemiologically contested or politically derived. Proportionality hearings follow procedural theater (expert testimony, risk assessment reports) but lack the full incoherence of pure performative constraints. The measurement trajectory (0.42→0.55) suggests increasing theater as the constraint moves from principle to practice — the ideology of proportionality persists even as adjudication becomes decoupled from actual disease severity.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality reading produces maximal perspectival heterogeneity. Populations experiencing low-severity-disease mandates see Snare (extraction without proportionate justification). Individual autonomy-interested agents see Tangled Rope (genuine coordination benefit paired with asymmetric bodily autonomy loss). Public health authorities see Rope (pure coordination: disease suppression requires justified coercion). Epidemiological science sees Tangled Rope (gains authority but becomes implicated in state coercion). Rights-oversight institutions see Scaffold (temporary constraint with sunset logic determined by disease severity). Constitutional framework sees Piton (legitimation mechanism persisting through inertia). The analytical observer risks seeing Mountain (proportionality as natural principle of governance) — a false summit that naturalizes a specific legal doctrine. The perspectival gap reflects the structural ambiguity: is the proportionality reading a functional restraint on legitimate coercion, or a legitimating narrative for authority expansion?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the proportionality constraint. Populations subject to low-severity mandates have d ≈ 0.95 (full targets, no beneficiary role, trapped with no exit) — they experience maximum f(d) extraction modifier. Individual autonomy agents have d ≈ 0.65 (mixed beneficiary/victim: gain disease protection but lose autonomy; constrained exit) — moderate extraction. Public health authorities have d ≈ 0.10 (full beneficiaries: gain gatekeeping power and institutional legitimacy; arbitrage exit options) — negative/minimal effective extraction. Epidemiological science has d ≈ 0.55 (symmetric: gains authority as legitimating apparatus but becomes implicated in enforcement; mobile at generational scale) — moderate extraction. Rights-oversight institutions have d ≈ 0.40 (constrained beneficiaries: gain role as restraining force but constrained by authority power dynamics) — low-to-moderate extraction. Constitutional framework has d ≈ 0.20 (beneficiary: persists through inertia with minimal functional role; arbitrage in selecting which doctrines to activate). The derivation reflects that the proportionality reading is fundamentally asymmetric: it distributes authority concentration (high beneficiary power) against distributed vulnerability (powerless populations subject to threshold disputes).
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading resolves mandatrophy through reference to the kernel context. The coercion_legitimacy_boundary kernel admits three readings with different legitimacy conditions. Proportionality resolves the paradox (why is coercion sometimes legitimate and sometimes not?) by anchoring legitimacy to disease severity metrics. However, the resolution is itself vulnerable to the mandatrophy — the reading's coherence depends on whether the adjudication process actually constrains authority or becomes captured and performative. As theater_ratio rises and extractiveness accumulates, the constraint approaches Piton (legitimation inertia). The mandatrophy persists as a lower-level ambiguity: does the proportionality reading function as restraint or legitimation?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_metric_commensurability,
    'Are epidemiological severity metrics (R0, CFR, transmission mode, mutation rate) commensurable with constitutional rights costs, or do they operate on incommensurable scales?',
    'Formal analysis of metrics across multiple pathogens (SARS-CoV-2 variants, measles, seasonal flu, monkeypox, polio); assessment of whether severity-justification logic produces consistent policy across cases with similar metric profiles.',
    'If commensurable: proportionality reading is coherent (severity determines legitimacy). If incommensurable: the constraint is a tangled rope masquerading as a coordination mechanism — the severity framing conceals asymmetric authority claims about what counts as sufficient threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_metric_commensurability, conceptual, 'Whether epidemiological severity is commensurable with rights costs').

omega_variable(
    reading_foreclosure_structure,
    'Does the proportionality reading logically foreclose the bodily-autonomy-primary reading within a single coherent framework, or do they coexist as competing frameworks?',
    'Identify whether accepting ''coercion legitimacy scales with severity'' requires rejecting ''bodily autonomy is inviolable regardless of severity,'' or whether a framework could coherently hold both (e.g., by partitioning when each applies).',
    'If foreclosure: the proportionality reading and bodily-autonomy reading cannot both be live options in the same constitutional framework — one must be chosen. If coexistence: they represent competing frameworks held by different parties, and the DR classification system treats them as siblings without logical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether proportionality forecloses bodily autonomy reading or coexists with it').

omega_variable(
    authority_capture_risk,
    'Does the proportionality reading''s dependence on public health authority adjudication of severity thresholds create structural vulnerability to authority capture and threshold manipulation?',
    'Historical analysis of proportionality adjudication across cases: (a) do authorities consistently apply severity metrics, or do outcomes correlate with political factors independent of severity? (b) are thresholds revised downward (stricter interpretation of ''sufficient severity'') during political pressure, upward during public resistance? (c) do rights-review institutions meaningfully constrain authority, or do they provide cover for predetermined outcomes?',
    'High capture risk: the extractiveness value (0.52) understates the true extraction under politically captured conditions. Theater ratio (0.55) rises toward 0.75+ as adjudication becomes performative. Low capture risk: the constraint functions as designed (tangled rope with meaningful oversight).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_capture_risk, empirical, 'Risk of authority capture in proportionality adjudication').

omega_variable(
    variant_emergence_severity_recalibration,
    'When new pathogenic variants emerge with different severity/transmission profiles, how does the proportionality reading''s adjudication process handle the recalibration? Does it update thresholds, or does prior coercion persist despite changed threat profile?',
    'Longitudinal case study of SARS-CoV-2 policy: measure whether coercion constraints (vaccine mandates, quarantine requirements, business closures) were relaxed when Omicron variant reduced severity compared to Delta/Alpha. Assess lag time between severity change and policy adjustment.',
    'If thresholds update promptly: scaffold perspective confirmed, sunset logic is real. If coercion persists despite severity reduction: theater ratio rises (policy divorced from severity justification), constraint shifts toward snare. This reveals whether proportionality is functional restraint or legitimating narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(variant_emergence_severity_recalibration, empirical, 'Whether proportionality thresholds adjust when variant severity changes').

omega_variable(
    natural_vs_constructed_legitimacy_kernel,
    'Is the proportionality principle itself (coercion legitimacy scales with threat) a discovered natural law of governance, or a constructed interpretive framework grounded in specific historical/legal traditions?',
    'Comparative institutional analysis across legal systems: do all constitutional orders adopt proportionality review, or is it specific to particular traditions (European, derived from 20th-century German law)? What alternative legitimacy principles for coercion exist in other frameworks (consent-based, utilitarian aggregate benefit, communitarian harmony)?',
    'If natural law: proportionality is the only coherent framework, and the mountain analytical perspective is accurate. If constructed: the proportionality reading is one contingent interpretation among viable alternatives (coexists with, does not foreclose, other readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_legitimacy_kernel, conceptual, 'Whether proportionality is discovered natural law or constructed interpretive framework').

omega_variable(
    committer_reading_anchor,
    'This constraint instantiates the proportionality reading of the coercion_legitimacy_boundary kernel. How do the sibling readings (public_health_primary, bodily_autonomy_primary) structure their own legitimacy claims, and what would shift an agent from this reading to a sibling reading?',
    'Identify which specific contextual factors (pathogen severity metrics, institutional context, political pressure, rights-review outcomes) anchor commitment to the proportionality reading versus shifting commitment to a sibling. What constitutes crossing the threshold?',
    'Maps the committer-axis structure: reveals whether proportionality is the ''default'' reading with siblings as alternatives, or whether all three readings are live simultaneously with context-dependent adoption. Clarifies why the same constraint (public health coercion) generates different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_anchor, conceptual, 'Anchoring and switching conditions between proportionality and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_tr_t0, proportionality_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prop_tr_t5, proportionality_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(prop_tr_t10, proportionality_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(prop_be_t0, proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prop_be_t5, proportionality_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prop_be_t10, proportionality_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel admits three structurally distinct constraint readings. The proportionality_reading (this story) depends on disease severity metrics (R0, CFR, transmission mode) to adjudicate case-by-case. The public_health_primary reading grounds legitimacy in aggregate disease suppression regardless of autonomy cost. The bodily_autonomy_primary reading treats medical autonomy as inviolable except under extreme emergency. Each reading has its own ε value and its own beneficiary/victim structure. All three readings coexist as live institutional frameworks held by different actors, courts, and constitutional traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proportionality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
