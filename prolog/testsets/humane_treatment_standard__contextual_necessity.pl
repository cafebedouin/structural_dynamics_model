% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Humane Treatment Standard: Contextual Necessity Reading
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes baseline humane
 *   treatment protections binding on all parties to armed conflict,
 *   irrespective of legal status or national security justification. The
 *   contextual necessity reading permits security agencies to define 'humane'
 *   and 'necessary' in operational contexts where national security
 *   imperatives are deemed to override baseline protections. This constraint
 *   exhibits acute mandatrophy: humanitarian law as written (absolute
 *   baseline) versus humanitarian law as practiced (contextual baseline). The
 *   reading instantiates one interpretive resolution of a contested kernel —
 *   the humane_treatment_standard — that grants state security authorities
 *   discretion to assess context-specific necessity. The constraint structure
 *   encodes a fundamental legitimacy claim: that the security agency's
 *   judgment about necessity is both epistemically reliable and normatively
 *   authoritative. The measurements show rising extractiveness and theater as
 *   the necessity carve-out diffuses into practice, suggesting the constraint
 *   drifts from mixed coordination (Tangled Rope) toward degraded ritual
 *   (Piton) or pure extraction (Snare) depending on perspective.
 *
 * KEY AGENTS:
 *   - Security Agencies: Primary beneficiary (institutional/arbitrage) — gain discretion to override baseline protections in necessity scenarios; define operational parameters of 'humane'
 *   - Designated High-Value Detainees: Primary victim (powerless/trapped) — no exit capacity; status determined by unilateral security assessment; all protections conditional
 *   - Ordinary Detainee Population: Secondary victim (moderate/constrained) — protected by baseline Article 3 but live in shadow of necessity exception; status contingent on remaining non-essential
 *   - Human Rights Monitoring Mechanisms: Organized observer (organized/constrained) — mandate to verify compliance undermined by classification restrictions and sovereignty exemptions
 *   - Humanitarian Law Regime (State Parties): Institutional signatories (institutional/arbitrage) — maintain formal Common Article 3 commitment while necessity carve-outs erode functional coherence
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing state security discretion as immutable feature of state logic rather than contingent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.58).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Humane Treatment Standard: Contextual Necessity Reading").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '7f384fe9-3367-499e-b1fb-ef232f301909').
narrative_ontology:cs_kernel_codification('7f384fe9-3367-499e-b1fb-ef232f301909', fixed_text).
narrative_ontology:cs_authority_grounding('7f384fe9-3367-499e-b1fb-ef232f301909', extraction).
narrative_ontology:cs_interpretation_layer_present('7f384fe9-3367-499e-b1fb-ef232f301909').
narrative_ontology:cs_reading_relation('7f384fe9-3367-499e-b1fb-ef232f301909', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('7f384fe9-3367-499e-b1fb-ef232f301909', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('7f384fe9-3367-499e-b1fb-ef232f301909', foundational, necessity_overrides_baseline_protections).
narrative_ontology:cs_axiom_status(necessity_overrides_baseline_protections, holdable).
narrative_ontology:cs_axiom_grounding('7f384fe9-3367-499e-b1fb-ef232f301909', necessity_overrides_baseline_protections, instrumental).
narrative_ontology:cs_axiom('7f384fe9-3367-499e-b1fb-ef232f301909', foundational, state_discretion_defines_necessity_threshold).
narrative_ontology:cs_axiom_status(state_discretion_defines_necessity_threshold, holdable).
narrative_ontology:cs_axiom_grounding('7f384fe9-3367-499e-b1fb-ef232f301909', state_discretion_defines_necessity_threshold, deontological).
narrative_ontology:cs_reference_frame('7f384fe9-3367-499e-b1fb-ef232f301909', security_imperative_primacy).
narrative_ontology:cs_drift_state('7f384fe9-3367-499e-b1fb-ef232f301909', contemporary_global_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f384fe9-3367-499e-b1fb-ef232f301909', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_high_value_scenarios).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, humanitarian_law_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESIGNATED HIGH-VALUE DETAINEE (SNARE) — No exit capacity. Security classification itself becomes a mechanism of suppression; the detainee cannot appeal or even know the criteria by which 'necessity' is determined. Experiences maximal extraction: all legal protections become conditional on an assessment they cannot contest.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORDINARY DETAINEE POPULATION (TANGLED ROPE) — Constrained by detention itself but benefits from baseline Common Article 3 protections. Yet live in the shadow of the 'necessity exception' — their status is secure only if they remain non-essential to security interrogations. Mixed extraction: genuine coordination of baseline care coexists with asymmetric conditional withdrawal.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY AGENCY (ROPE) — Net beneficiary. Gains discretion to define 'necessity' and 'humane' in operational contexts; retains international humanitarian law framework as cover while achieving interrogation objectives. Experiences the constraint as enabling coordination: procedural authority to assess context-specific security imperatives.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RIGHTS MONITORING BODIES (TANGLED ROPE) — Organized but constrained by state sovereignty and classification restrictions. Coordinating function: provide independent review of detention conditions. Extraction: their verification capacity is systematically undermined by national security exemptions, rendering their mandate performative. Mixed experience of coordination purpose and extractive capability limits.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN LAW REGIME AS INSTITUTION (PITON) — The Common Article 3 framework persists as an institutional form but its functional coherence has degraded. The 'contextual necessity' carve-out transforms the regime into theater: states maintain the formal commitment while operational practice diverges. Regime actors know the framework is compromised but maintain it through institutional inertia and diplomatic convention.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REALISM VIEW (MOUNTAIN) — From a civilizational perspective, state security imperatives always override individual protections in existential scenarios; humane treatment is structurally dependent on state discretion to define the threat level. This perspective naturalizes the contextual necessity framework as inherent to state logic itself — an immutable feature of how sovereigns operate. However, the engine will detect this as a false summit: the framework is a contingent reading of the humanitarian law kernel, not a natural law.
constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humane_treatment_standard__contextual_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, TR),
    TR >= 0.70.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The contextual necessity reading grants security agencies unilateral discretion to define 'necessary' and override baseline protections for designated detainees. This is asymmetric extraction: the beneficiary (state security apparatus) gains authority over the victim's (detainee's) condition. The extraction is real but partial — the baseline Common Article 3 protections remain nominally in force for ordinary detainees, and the necessity carve-out is theoretically limited to genuine security scenarios. The measurement trajectory (0.35 → 0.58 over 10 periods) reflects the diffusion of necessity-based interrogation practices: early period shows lower extractiveness because necessity invocations are rare and contested; later period shows higher extractiveness as the practice normalizes and security agencies routinize necessity determinations. Suppression (0.68): High. Detainees cannot contest their classification as 'high-value'; cannot access the threat assessment that justifies enhanced interrogation; have no procedural channel to challenge necessity determinations. Classification itself is weaponized as suppression mechanism. The trajectory shows suppression intensifying as interrogation infrastructure develops and protocols harden around necessity scenarios. Theater ratio (0.64): Moderate-high. The public commitment to Article 3 baseline persists as formal doctrine while operational practice diverges. States maintain the humanitarian law framework institutionally — training, policy documents, formal compliance structures — while necessity carve-outs become the functional reality for high-value cases. The performative content increases as institutional theater expands without corresponding functional verification (monitoring mechanisms are systematically excluded from high-value scenarios).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement about the nature of the binding principle. The security agency sees coordination: procedural authority to assess necessity in context. The high-value detainee sees pure extraction: conditionality on an assessment they cannot contest. The monitoring mechanism sees institutional degradation: their verification mandate is rendered performative by sovereignty exemptions. The humanitarian law regime sees itself (from piton perspective) as maintaining form while losing function. The analytical observer risks seeing a natural law — that state security always overrides individual protections in existential scenarios — when the structure is actually a contingent institutional reading. The absolute prohibition reading (sibling constraint) would see only baseline protections with no carve-out; the proportionality reading would permit necessity balancing within a framework of strict limitations and burden-shifting.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) derives from their structural position relative to the constraint. Security agencies benefit from the necessity carve-out (beneficiary status with arbitrage options to invoke security exemptions) — they experience low or negative d, meaning low effective extraction chi. High-value detainees are victims with no exit options (trapped) — they experience high d, maximum f(d), maximum experienced chi. The ordinary detainee population occupies a middle position: they benefit from baseline Article 3 protections but are constrained by the knowledge that necessity could deconditionalize those protections if they were reclassified as high-value — constrained exit options produce moderate d. Monitoring mechanisms are organized but their exit options are severely limited by state sovereignty claims — they cannot exit the framework without abandoning their mandate. The piton perspective derives from theater rather than from high chi — the regime maintains institutional form despite functional degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The contextual necessity reading resolves the mandatrophy by choosing which aspect of the humanitarian law commitment to privilege: the baseline protection norm (absolute prohibition) or the state security imperative (contextual necessity). This is not an empirical question — it is a normative choice about which reading of the kernel is legitimate. The mandatrophy persists as an omega variable: the kernel itself is under-specified in a way that permits multiple readings without logical contradiction. The engine's classification will reveal that the contextual necessity reading produces asymmetric extraction and legitimacy-grounding in discretionary security assessment — it is a Tangled Rope in which security coordination is real but coexists with extraction. The piton classification (from the regime institutional perspective) indicates institutional degradation: the regime maintains form while necessity carve-outs undermine function. The mountain classification (from analytical realism view) is a false summit: naturalizing state security discretion as immutable rather than recognizing it as a contingent reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_threshold_indeterminacy,
    'What constitutes ''national security imperative'' sufficient to override Article 3 baseline protections? Is the threshold defined ex ante by law or ex post by security assessment?',
    'Legal text analysis comparing pre-defined threat scenarios vs operational necessity determinations; empirical audit of decisions classified as necessity-driven interrogations',
    'If ex ante legal definition: constraint approaches Rope (transparent coordination mechanism). If ex post assessment: constraint remains Snare (detainee cannot know criteria; security agency has unilateral discretion). This is the binding parameter for classification stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_threshold_indeterminacy, conceptual, 'Specification of necessity threshold determines classification trajectory').

omega_variable(
    interrogation_efficacy_empiricism,
    'Do enhanced interrogation techniques produce reliable intelligence that materially affects security outcomes, or does the ''necessity'' justification rest on unvalidated assumptions about efficacy?',
    'Declassified interrogation outcomes paired with subsequent intelligence validation; comparative analysis of information obtained through enhanced vs standard techniques; longitudinal tracking of decision quality when enhanced techniques were authorized',
    'If empirically effective: necessity argument gains structural legitimacy (extraction serves a coordination function). If ineffective: necessity framing is purely rhetorical cover for extraction without security justification — constraint collapses toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interrogation_efficacy_empiricism, empirical, 'Whether enhanced interrogation produces materially superior security outcomes').

omega_variable(
    contextual_necessity_vs_absolute_prohibition_kernel_reading,
    'This constraint instantiates ONE reading of the contested kernel: humane_treatment_standard. The contextual_necessity reading permits security-driven discretion to override baseline protections. The sibling readings — absolute_prohibition and proportionality_balancing — foreclose or constrain this discretion differently. Which reading''s axioms are actually operative in contemporary state practice?',
    'Comparative analysis of interrogation policy across signatory states; tracking which jurisdictions invoke necessity exceptions and frequency of invocation; examination of legal challenge outcomes when necessity defenses are mounted',
    'If contextual_necessity prevails in practice: the humanitarian law kernel drifts toward security authority framing. If absolute_prohibition prevails: the constraint collapses toward Rope (simple coordination without extraction carve-out). If proportionality_balancing prevails: the constraint remains Tangled Rope but with tighter constraints on when extraction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_necessity_vs_absolute_prohibition_kernel_reading, conceptual, 'Which kernel reading (contextual necessity vs absolute prohibition vs proportionality) operationally governs state interrogation practice').

omega_variable(
    classification_capture_and_false_positives,
    'Do security classifications assigned to ''high-value detainees'' reflect genuine threat assessment or function primarily as a mechanism to justify enhanced interrogation? What proportion of detainees classified as high-value are later found to pose negligible security risk?',
    'Post-release analysis of detainee threat assessments: rate of false positives in initial threat classifications; comparison of interrogation outcomes when classification was accurate vs when detainees were misclassified; case-level review of detainee reclassification patterns over detention duration',
    'If false-positive rate is high (>30%): the necessity exception operates partially as extraction mechanism targeting innocents — Snare tendency amplified. If low: necessity framing has empirical grounding, supporting Tangled Rope classification. Systematic false positives would be evidence that classification itself is the extraction mechanism, not a prerequisite for it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_capture_and_false_positives, empirical, 'Rate of false positives in security threat classifications').

omega_variable(
    humanitarian_law_regime_coherence_drift,
    'As the contextual necessity reading diffuses across state practice, does the humanitarian law regime''s perceived legitimacy and compliance rate change? Does institutional coherence degrade?',
    'Longitudinal tracking of state adherence to non-necessity-carved-out Article 3 provisions; survey data on civilian and legal professional confidence in humanitarian law regime; analysis of regime institutional strength (dispute resolution capacity, enforcement willingness) before and after necessity carve-out adoption',
    'If regime coherence decays: the Piton classification is correct — the constraint becomes performative institutional theater. If coherence persists: the constraint retains functional coordination value despite the necessity carve-out — remains Tangled Rope. Coherence decay is the diagnostic signal for institutional degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_law_regime_coherence_drift, empirical, 'Whether humanitarian law regime institutional coherence persists or degrades under contextual necessity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_ctx_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(humane_ctx_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.5).
narrative_ontology:measurement(humane_ctx_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(humane_ctx_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(humane_ctx_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(humane_ctx_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(humane_ctx_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(humane_ctx_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(humane_ctx_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, interrogation_practice_divergence).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, monitoring_mechanism_accessibility).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel decomposes into three structurally distinct constraint readings with different ε values: absolute_prohibition (ε~0.25, Mountain or Rope depending on perspective), proportionality_balancing (ε~0.40, Tangled Rope across perspectives), contextual_necessity (ε~0.58, this story — Tangled Rope to Snare depending on perspective). Each reading encodes a different interpretation of the binding principle. They coexist as live positions in contemporary state practice, with different jurisdictions and time periods privileging different readings. All three readings share the same kernel (Common Article 3 humane treatment commitment) but instantiate different structural relationships between state security authority and detainee protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
