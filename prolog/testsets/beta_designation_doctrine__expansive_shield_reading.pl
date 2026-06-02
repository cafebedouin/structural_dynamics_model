% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive Liability Shield (Expansive Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The beta designation doctrine exists as a contested kernel with three
 *   structurally distinct readings. This constraint instantiates the
 *   EXPANSIVE SHIELD READING: beta designation constitutes a comprehensive
 *   liability waiver with indefinite duration that applies across all
 *   software contexts, regardless of actual development stage, defect
 *   severity, or user population vulnerability. This reading interprets the
 *   kernel to shield developers from all statutory liability standards
 *   (merchantability, fitness for purpose, duty to warn, product safety) and
 *   to apply indefinitely regardless of how long the software remains in
 *   'beta' status or how many users depend on it. The doctrine emerged from
 *   legitimate concerns about development velocity and iterative refinement
 *   but has been extended to cover mature, revenue-generating,
 *   privacy-critical platforms that operate in essential digital
 *   infrastructure. The sibling readings — the narrow_warning_reading (beta
 *   applies only when genuine uncertainties are disclosed and development is
 *   active) and severity_carve_out_reading (beta exempts minor defects but
 *   not critical safety/privacy harms) — represent structural alternatives
 *   that constrain the doctrine's extraction mechanism. This expansive
 *   reading maximizes developer extraction by removing temporal and severity
 *   boundaries, making it the harshest formulation for users and the most
 *   beneficial for developers who can maintain indefinite beta status. The
 *   ascending measurements reflect how the doctrine's extractiveness has
 *   increased over time as developers learned to claim beta status regardless
 *   of maturity, and as the legal framework's suppression of alternative
 *   liability standards has hardened through precedent and market
 *   normalization.
 *
 * KEY AGENTS:
 *   - Software Developers & Platform Operators: Primary beneficiaries (institutional/arbitrage) — externalize all defect costs to users; capture efficiency gains from indefinite iteration without liability exposure
 *   - End Users: Primary victims (powerless/trapped) — accept beta designation as condition of software access; bear all defect costs indefinitely with no remedy or exit
 *   - Consumer Protection Authorities: Secondary victims (moderate/constrained) — statutory frameworks degraded by doctrine that preempts liability standards; constrained by market concentration and developer coalition resistance
 *   - Legal/Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains formal authority but exercises it performatively; degraded to piton status
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent legal doctrine as immutable property of software development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.68).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.72).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Shield (Expansive Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '229ef22e-eaf4-41f9-b942-dfe02f30697a').
narrative_ontology:cs_kernel_codification('229ef22e-eaf4-41f9-b942-dfe02f30697a', formalized).
narrative_ontology:cs_authority_grounding('229ef22e-eaf4-41f9-b942-dfe02f30697a', extraction).
narrative_ontology:cs_interpretation_layer_present('229ef22e-eaf4-41f9-b942-dfe02f30697a').
narrative_ontology:cs_reading_relation('229ef22e-eaf4-41f9-b942-dfe02f30697a', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('229ef22e-eaf4-41f9-b942-dfe02f30697a', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('229ef22e-eaf4-41f9-b942-dfe02f30697a', foundational, beta_status_absolute_exemption).
narrative_ontology:cs_axiom_status(beta_status_absolute_exemption, holdable).
narrative_ontology:cs_axiom_grounding('229ef22e-eaf4-41f9-b942-dfe02f30697a', beta_status_absolute_exemption, conventional).
narrative_ontology:cs_axiom('229ef22e-eaf4-41f9-b942-dfe02f30697a', foundational, beta_duration_indefinite).
narrative_ontology:cs_axiom_status(beta_duration_indefinite, holdable).
narrative_ontology:cs_axiom_grounding('229ef22e-eaf4-41f9-b942-dfe02f30697a', beta_duration_indefinite, conventional).
narrative_ontology:cs_reference_frame('229ef22e-eaf4-41f9-b942-dfe02f30697a', absolute_liability_exemption_regime).
narrative_ontology:cs_drift_state('229ef22e-eaf4-41f9-b942-dfe02f30697a', contemporary_regulatory_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('229ef22e-eaf4-41f9-b942-dfe02f30697a', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, end_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, downstream_liability_bearers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_protection_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped by asymmetric acceptance terms. Users cannot negotiate or refuse beta designation; exit requires abandoning the software ecosystem entirely. Bears all defect costs indefinitely. No alternative pathways exist for accessing equivalent functionality. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER PROTECTION AUTHORITY (SNARE) — Constrained by doctrine that beta designation preempts statutory liability standards. Cannot enforce product safety requirements, duty-to-warn obligations, or merchantability standards when developer claims beta status. Defect costs are externalized from developers to users and public health/safety systems. High extraction with constrained exit: authorities could challenge the doctrine but face coordinated developer resistance and entrenched market practice.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOFTWARE DEVELOPER (ROPE) — Experiences the constraint as coordination: beta designation enables fast iteration and user feedback collection without liability exposure. Genuine coordination function (rapid development cycles, user participation in refinement). Net beneficiary with full arbitrage — can move software between beta and non-beta status, can market-test across jurisdictions with different enforcement postures. Extraction asymmetry flows toward this agent.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR / OLIGOPOLY (TANGLED ROPE) — Powerful actor with mobile exit options but also genuine coordination function: platforms do enable developer ecosystem participation and user choice. However, benefits from indefinite beta status to defer liability for platform-level defects (privacy breaches, algorithmic harms, data security). Mixed extraction and coordination: the platform coordinates ecosystem participation while extracting from users who cannot exit without losing access to essential digital infrastructure. Moderate effective extraction but structurally asymmetric.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL/REGULATORY FRAMEWORK (PITON) — Statutory liability standards (merchantability, fitness for purpose, duty to warn) persist in law but are functionally degraded: beta designation has become a ritualistic exemption that operators routinely claim regardless of actual development stage, maturity, or known defect severity. The regulatory framework maintains formal authority to adjudicate defect claims but exercises it performatively — most beta-designated software is never challenged in courts. Theater ratio is high because the legal apparatus exists but produces few enforcements. Piton classification reflects institutional inertia: the doctrine persists because alternatives (developer licensing, mandatory insurance, graduated liability by maturity metrics) haven't fully replaced it, not because the current system functions.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, software development inherently involves iterative refinement and risk — users cannot demand perfection, and developers cannot guarantee zero defects. The beta designation captures an immutable property of software creation: the gap between laboratory performance and real-world deployment. This perspective sees the doctrine as reflecting a natural law of technological systems: imperfection is unavoidable, so users must bear some defect risk. However, the false summit detector will flag this reading: the 'natural law' framing naturalizes what is actually a contingent legal doctrine that benefits identifiable actors (developers, platforms) and can be reformed via alternative liability regimes.
constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beta_designation_doctrine__expansive_shield_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, TR),
    TR >= 0.70.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Developers capture full efficiency gains from rapid iteration (no liability constraint on deployment) while externalizing all defect costs to users. The metric reflects that this reading removes temporal and severity boundaries — developers can maintain indefinite beta status and claim exemption from liability even for critical defects (data breaches, privacy violations, safety failures in safety-critical contexts). The 0.68 value is below the snare floor (0.66) only marginally because some jurisdictions have carved out exceptions for specific harm categories. Suppression (0.72): High. Users face strong barriers to exit (network effects, monopoly control, no functional alternatives for essential software) and strong barriers to seeking remedy (beta designation preempts statutory liability claims, asymmetric contract terms). The doctrine suppresses alternative liability regimes by establishing precedent and market expectation. Theater ratio (0.58): Moderate. The legal framework for product liability persists formally but is functionally degraded by beta exemption doctrine. Courts maintain authority to adjudicate defect claims but rarely do so for beta-designated software. The theatrical element is that statutory frameworks exist and are invoked in litigation, but beta designation preempts them before substantive adjudication occurs. Theater ratio rises over the measurement interval as the doctrine becomes more entrenched and the legal ritual of invoking 'beta status' becomes more routine.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximum perspectival divergence. Developers see Rope (coordination mechanism enabling rapid iteration). End users see Snare (trapped extraction with no exit). Regulatory authorities see Snare (suppressed liability standards with constrained exit). Platform oligopolies see Tangled Rope (mixed coordination and extraction given their power and partial arbitrage options). The legal framework sees Piton (formally maintained but functionally degraded by the doctrine). The civilizational analytical observer risks seeing Mountain (natural law of software development) but the structural data reveals false summit: this is a contingent legal doctrine benefiting identifiable actors, not an immutable property of software creation. The sibling readings would shift the perspectival gap: the narrow_warning_reading would push developers toward constrained_rope (genuine development activity required), and the severity_carve_out_reading would push platforms toward tangled_rope (liability exposure for critical harms limits extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Developer directionality (d ≈ 0.08–0.15): Beneficiary with arbitrage exit options. Can move software between beta and non-beta status, market-test across jurisdictions, exit beta status when convenient. Low d produces negative or minimal f(d), so effective extraction chi from the developer's perspective is low or inverted — they experience the constraint as coordination benefit (rapid iteration capability), not extraction. End user directionality (d ≈ 0.92–0.98): Victim with trapped exit. Network effects and monopoly control over essential digital infrastructure eliminate functional alternatives. Suppression is high because users cannot negotiate terms or exit without abandoning software ecosystem. High d produces maximum f(d) ≈ 1.42, amplifying chi to severe levels. Regulatory authority directionality (d ≈ 0.65–0.75): Victim with constrained exit. Can challenge doctrine but faces coordinated developer resistance, precedent-based entrenchment, and market concentration effects. Moderate-high d produces elevated f(d) ≈ 0.95–1.15, producing moderate-high chi. Platform operator directionality (d ≈ 0.45–0.55): Mixed beneficiary/victim with mobile exit. Benefits from indefinite beta status to defer liability for platform-level harms. Can also exit beta designation when regulatory pressure rises. Moderate d produces moderate f(d) ≈ 0.60–0.80, producing moderate chi — tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exemplifies how a single kernel can instantiate vastly different constraints under different interpretive regimes. The expansive shield reading maximizes extraction (snare dominates from powerless/victim perspectives). The narrow_warning_reading would constrain extraction by requiring active development and honest disclosure (rope or constrained-tangled_rope from more perspectives). The severity_carve_out_reading would create liability exposure for critical harms, reducing developer arbitrage options (tangled_rope from all perspectives). The mandatrophy is resolved by recognizing that the three readings represent different institutional settlements of the same kernel. The expansive reading reflects 2015–2025 market practice where developers claim beta status indefinitely. The narrow_warning_reading reflects 1990s-early-2000s legal doctrine where beta meant genuine developmental uncertainty. The severity_carve_out_reading reflects emerging regulatory responses (GDPR carve-outs for data-handling defects, accessibility liability for disabled users) that constrain beta exemptions. Mandatrophy resolution: the observed state of the doctrine is path-dependent. Which reading prevails depends on which coalition (developers, consumer protection advocates, regulatory authorities, users) successfully institutionalizes its interpretation. The expanding extractiveness measurement trajectory (0.45 → 0.68) reflects that the expansive reading has dominated recent practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_status_semantic_drift,
    'Has ''beta'' retained a precise technical meaning (genuine development stage with known instability) or has it become a ritualistic legal exemption applied to mature, revenue-generating software?',
    'Temporal analysis of beta-designated software: correlation between label duration and actual defect rates, maturity metrics, user base size, and revenue generation. If mature software remains labeled ''beta'' indefinitely despite low change rates and high usage, the term has drifted from technical descriptor to liability shield.',
    'If retained meaning: doctrine may reflect legitimate developer interest in rapid iteration (Rope classification broadens). If drifted: doctrine is pure extraction mechanism divorced from real development stages (Snare classification hardens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beta_status_semantic_drift, empirical, 'Whether beta designation retains technical meaning or becomes ritualistic liability exemption').

omega_variable(
    jurisdiction_carve_out_applicability,
    'Does the expansive shield reading apply uniformly across all jurisdictions, or do some regulatory regimes (EU GDPR, California consumer protection, consumer goods liability statutes) override beta designation for specific harm categories?',
    'Comparative legal analysis: case law and statutory interpretation across jurisdictions; empirical documentation of which defects trigger liability despite beta status (data breaches, algorithmic discrimination, accessibility failures affecting disabled users).',
    'If uniform application: doctrine is truly comprehensive (high extractiveness confirmed). If jurisdiction-specific carve-outs exist: doctrine is weaker than this reading suggests (extractiveness drops to 0.45–0.55; snare weakens toward tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdiction_carve_out_applicability, empirical, 'Whether expansive shield applies uniformly or regulatory carve-outs limit it').

omega_variable(
    consent_voluntariness_ambiguity,
    'Is user acceptance of beta designation genuinely voluntary (users have real alternatives for critical software needs) or illusory (monopoly control or network effects eliminate functional alternatives)?',
    'Market analysis: availability of functional alternatives for dominant beta-designated platforms; actual user exit rates when formal non-beta alternatives emerge; regulatory investigation into whether network effects or platform lock-in render user ''acceptance'' unenforceable under consumer protection doctrine.',
    'If genuinely voluntary: suppression metric should be lower (0.45–0.55), and forced acceptance aspect of snare classification weakens. If illusory: suppression confirmed at 0.72, and snare classification is structurally accurate (trapped exit is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_voluntariness_ambiguity, empirical, 'Whether user acceptance of beta terms is genuinely voluntary or illusory').

omega_variable(
    kernel_reading_ambiguity,
    'Is this expansive shield reading a defensible interpretation of the kernel (beta designation as written), or is it an over-extension that the kernel text itself does not support?',
    'Textual and historical analysis: original statutory language or common-law doctrine origins; legislative intent where available; whether courts have explicitly endorsed or rejected the expansive reading; whether the reading requires pushing statutory language beyond its original scope.',
    'If defensible: this reading remains a live axiom (holdable status). If over-extension: the axiom should be marked overridden or foreclosed by the reading_relations structure. This determines whether coexists_with or forecloses is the correct relation to the narrow_warning_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether expansive shield reading is defensible interpretation of kernel or over-extension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_exp_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(beta_exp_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(beta_exp_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(beta_exp_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(beta_exp_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(beta_exp_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(beta_exp_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(beta_exp_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(beta_exp_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, software_terms_of_service_asymmetry).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, platform_immunity_doctrine).

% DUAL FORMULATION NOTE:
% The beta designation doctrine decomposes into three constraint stories reflecting three readings of a single kernel. The expansive shield reading maximizes extraction (snare); the narrow warning reading constrains it to active development phases (tangled_rope/rope); the severity carve-out reading carves out liability for critical harms (tangled_rope). Each reading has its own ε, beneficiary/victim structure, and temporal trajectory. All three are linked via network.affects_constraints to enable contaminaton analysis: if one reading's authority degrades (e.g., courts reject it), which sibling readings strengthen? This constraint family enables modeling how contingent legal doctrines evolve through competing institutional readings rather than through neutral 'discovery' of correct interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
