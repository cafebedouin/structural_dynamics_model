% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Protection as Coercive Constraint on End-of-Life Autonomy
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The sanctity-of-life reading of the dignified-death kernel posits that
 *   human life possesses intrinsic transcendent value that cannot be validly
 *   overridden by consent or circumstance. Death-seeking is treated as
 *   categorically impermissible — a violation of natural/divine law —
 *   regardless of suffering severity or decision-maker consent. This
 *   constraint produces a structural tension: framed as protection (defending
 *   vulnerable populations from coercion), it operates as coercion (imposing
 *   prolonged suffering against autonomous choice). The measurement
 *   trajectory shows increasing extraction and suppression over the interval
 *   — the constraint has hardened as medical technology extends
 *   life-prolongation capacity beyond what suffering individuals would
 *   choose. Theater ratio remains moderate, indicating genuine institutional
 *   coordination around protection doctrine but with increasing performative
 *   content as the gap between medical capability and patient preference
 *   widens. This reading coexists with autonomy_primary (dignity through
 *   self-determination) and relational_autonomy (dignity through relational
 *   safeguards) as live positions held by different institutional and moral
 *   actors. The sanctity reading dominates legal-institutional frameworks in
 *   many jurisdictions but faces mounting pressure from lived experience of
 *   suffering populations and the existence of functioning alternative models
 *   (autonomy-centered jurisdictions with robust safeguarding).
 *
 * KEY AGENTS:
 *   - Terminally suffering individuals: Primary victims (powerless/trapped) — bear full cost of coercive life-prolongation; no exit from legal prohibition or institutional enforcement
 *   - Severely disabled and chronically ill populations: Secondary victims (powerless/identity_locked) — caught between material prohibition and internalized sanctity framing; structurally mobile but identity-locked
 *   - Family caregivers: Secondary victims (moderate/constrained) — forced to perpetuate suffering; bear emotional and legal burden of non-facilitation
 *   - Medical establishment: Primary beneficiary (institutional/arbitrage) — gains institutional authority over life/death, liability reduction, professional identity clarity
 *   - Institutional moral order & religious community: Abstract beneficiary (institutional/arbitrage) — preserves moral authority and cosmological coherence; benefits from legal instantiation
 *   - Clinicians & healthcare providers: Secondary extractors (moderate/constrained) — forced to enforce the constraint; bear moral burden of continued intervention despite suffering
 *   - Policymakers & legislators: Institutional managers (powerful/mobile) — navigate competing constituencies; benefit from stance-taking and authority allocation
 *   - Analytical observer: Perspective on false summit — risks naturalizing contingent institutional arrangement as transcendent law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.68).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Protection as Coercive Constraint on End-of-Life Autonomy").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'd1cf02d8-9c5b-4060-b46f-720dbe191a4f').
narrative_ontology:cs_kernel_codification('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', formalized).
narrative_ontology:cs_authority_grounding('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', lineage).
narrative_ontology:cs_interpretation_layer_present('d1cf02d8-9c5b-4060-b46f-720dbe191a4f').
narrative_ontology:cs_reading_relation('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', foundational, life_transcendent_intrinsic_value).
narrative_ontology:cs_axiom_status(life_transcendent_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', life_transcendent_intrinsic_value, theological).
narrative_ontology:cs_axiom('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', foundational, death_seeking_categorically_impermissible).
narrative_ontology:cs_axiom_status(death_seeking_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', death_seeking_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', absolute_life_preservation_framework).
narrative_ontology:cs_drift_state('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', contemporary_autonomy_challenge_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d1cf02d8-9c5b-4060-b46f-720dbe191a4f', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, institutional_moral_order).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, medical_establishment).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, suffering_terminally_ill).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, severely_disabled_populations).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, economically_vulnerable_elderly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERMINALLY SUFFERING INDIVIDUAL (SNARE) — Trapped by legal prohibition and institutional enforcement (criminal penalties, medical duty overrides, family pressure). No exit from prolonged suffering except through clandestine means. The constraint's suppression falls entirely on this agent: medical intervention cannot be refused without legal consequence; timing and method are state-controlled. Maximum experienced extraction because the constraint extracts the agent's final self-determination.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY CAREGIVERS (SNARE) — Constrained by simultaneous witnessing of suffering and legal prohibition on facilitation. High cost to exit: bearing the trauma of prolonged death while potentially facing conspiracy charges. Experienced extraction: the constraint forces family to perpetuate suffering against their judgment, transferring emotional and practical burden without agency.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL ESTABLISHMENT (ROPE) — Benefits from preservation mandate: institutional authority over life/death decisions, reduced liability exposure, clear directive alignment with duty preservation. The constraint coordinates the medical profession's self-understanding and legitimacy. Experiences the constraint as functional protection of vulnerable populations — a genuine coordination problem solved. Net beneficiary with substantial institutional arbitrage options.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICYMAKERS & LEGISLATORS (TANGLED ROPE) — Mobile and powerful but constrained by competing moral constituencies and judicial oversight. Genuine coordination function: balancing vulnerable population protection against suffering individual autonomy. Real asymmetric extraction: enables institutional moral authority and career positioning through bioethics stance, while distributing compliance burden on clinicians and patients. Can arbitrage: some jurisdictions shift to autonomy models; policymakers respond.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLINICIANS & HEALTHCARE PROVIDERS (TANGLED ROPE) — Constrained by dual obligation: preserve life (legal mandate) and relieve suffering (medical ethics). Genuine coordination exists (protocols, training, institutional support for difficult cases), but extraction is present: the constraint assigns them moral responsibility for outcomes they cannot control (refusal of death), forcing continued intervention despite suffering. Asymmetric extraction: they bear reputational and emotional burden of enforcement.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DISABLED & CHRONICALLY ILL (SNARE, IDENTITY-LOCKED) — Trapped by both legal prohibition and internalized sanctity framing. Identity fusion: 'my life has intrinsic value' is literally true, but the constraint weaponizes this framing to override their autonomy. Structurally mobile (could seek jurisdictions with different law, could refuse treatment), but identity-locked to the sanctity narrative that makes refusal unthinkable — to exit would require abandoning the framework that constitutes their disability identity. The lock is cognitive/religious (internalized transcendent law), not material.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 7: MORAL & RELIGIOUS COMMUNITY (ROPE) — Coordinates community self-understanding through sanctity principle. Experiences constraint as pure coordination: protects collective moral identity and prevents normalization of death-seeking. Low effective extraction for institutional religion because arbitrage is unlimited — religious communities have agency in doctrine revision and can shift interpretation. The coordination function is genuine from this perspective.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the constraint appears as natural law: the intrinsic dignity of human life is posited as transcendent, unchanging, and beyond human authority to modify. Death-seeking is treated as logically incoherent (life cannot will its own negation) and universally inaccessible to modification. However, the structural data reveals this as a FALSE SUMMIT: identifiable beneficiaries (institutional authority, moral order), measurable extraction (0.58), and suppression (0.68) indicate a human-constructed constraint, not a natural law. The mountain classification naturalizes what is contingent.
constraint_indexing:constraint_classification(dignified_death__sanctity_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dignified_death__sanctity_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dignified_death__sanctity_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from suffering individuals (removal of autonomy, forced prolongation, loss of self-determination) and moderately from clinicians (forced enforcement role). The extraction is not total (0.70+) because some legitimate protection function exists — the constraint does prevent some coercive scenarios. However, the extraction trajectory is rising (0.35 → 0.58) as medical technology extends life-prolongation capacity beyond patient preference, indicating increasing asymmetry between what the constraint prevents (coerced death) and what it imposes (coerced life). Suppression (0.68): High. The constraint operates through legal prohibition, institutional enforcement (medical duty overrides, criminal penalties for facilitation), and internalized moral framing. Alternatives are substantially suppressed: no legal pathway, social stigma against discussing autonomy-based choice, medical professional culture that resists honoring refusal. The rising trajectory (0.52 → 0.68) reflects institutional intensification — enforcement mechanisms have hardened as the legitimacy of autonomy models has grown. Theater ratio (0.55): Moderate. The constraint involves genuine institutional coordination (protocols, training, ethical guidelines) and real protection logic, but also performative content: medical extension of dying without patient consent, ritual continuation of interventions, institutional claims about 'what patients really want' that override stated preferences. The moderate and stable theater ratio indicates the constraint is not primarily performative (that would be piton), but neither is it purely functional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range from snare (powerless/trapped victim) through rope (institutional beneficiary) to false-summit mountain (analytical naturalization). The terminally suffering individual experiences maximal extraction and suppression — the constraint forecloses their final autonomous choice. The medical establishment experiences the constraint as coordination — solving the problem of protecting vulnerable populations and clarifying institutional responsibility. Clinicians experience tangled extraction (genuine safeguarding role + forced enforcement burden). Policymakers experience mobile choice among alternative frameworks. The analytical observer at civilizational scope risks seeing the sanctity principle as transcendent natural law rather than contingent institutional commitment. The perspectival gap is maximal because the power differential is maximal: powerless victims experience snare; institutional actors experience rope or coordination; the observer risks false naturalization. This is precisely the diagnostic pattern that indicates the constraint may be misclassified as mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: powerless suffering individuals have d ≈ 0.95 (full targets), institutional beneficiaries have d ≈ 0.05-0.15 (full beneficiaries with arbitrage), moderate clinicians have d ≈ 0.60-0.70 (asymmetrically extracted). The sigmoid function maps these to effective extractiveness: powerless victims experience χ ≈ 0.82 (base 0.58 × f(0.95) ≈ 1.42), while institutional beneficiaries experience χ ≈ -0.07 (base 0.58 × f(0.05) ≈ -0.12). The perspectival gaps are not measurement error — they reflect real structural differences in how different agents experience the same constraint. Beneficiaries genuinely see coordination; victims genuinely bear extraction. No single d value captures the constraint's full structure; the presheaf over observation positions IS the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE vs FALSE SUMMIT CONTEST: The constraint resolves mandatrophy by instantiating the false-summit mechanism. The analytical observer at civilizational scope naturally tends to classify as mountain — the sanctity principle appears universal, unchanging, and grounded in transcendent law. However, the structural data contradicts the mountain gates: (1) identifiable beneficiaries exist (institutional moral order, medical establishment), (2) measurable extraction occurs (0.58, rising to 0.68 suppression), (3) alternatives are suppressed rather than naturally absent. The false_summit_mountain signature fires: mountain classification with declared beneficiaries triggers FSM evaluation, and the omega variables documenting the sanctity-versus-autonomy contest confirm this is a contested institutional reading, not natural law. The snare classification captures the actual structural relationship: the constraint operates through suppression and extraction mechanisms, not natural impossibility. The mandatrophy is resolved by recognizing that 'natural law' framing naturalizes what is contingent — a powerful institutional commitment, not a transcendent truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_versus_autonomy_kernel,
    'Is the sanctity-of-life principle a transcendent moral law, or a contingent institutional commitment?',
    'Cross-cultural and historical analysis: does sanctity appear universally and unchangingly, or does it vary with institutional power structures and theological traditions? Comparison of sanctity-centered vs autonomy-centered jurisdictions for correlation with institutional authority concentration.',
    'If transcendent: constraint is mountain (natural law). If contingent: constraint is snare (institutional extraction). This omega instantiates the false-summit reading — the sanctity framework is held by powerful institutional actors who benefit from its legal instantiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_versus_autonomy_kernel, conceptual, 'Whether sanctity-of-life is transcendent natural law or contingent institutional commitment').

omega_variable(
    vulnerable_population_coercion,
    'Do sanctity-based protection laws actually prevent coercion of vulnerable populations, or do they create conditions for concealed harm and coercion?',
    'Empirical tracking: post-legalization comparison of reported elder abuse, financial coercion in healthcare, and concealed hastening of death in sanctity-prohibitive vs legalization jurisdictions. Survey of disabled and elderly populations regarding perceived autonomy and coercion pressure.',
    'If protection laws prevent coercion: snare classification is incorrect; constraint should be rope or tangled_rope. If protection laws enable concealed coercion: snare classification confirmed; suppression metric may be understated (0.68 → 0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_coercion, empirical, 'Whether sanctity-protection laws prevent or enable coercion of vulnerable populations').

omega_variable(
    identity_lock_mechanism_in_disabled_populations,
    'For disabled individuals and chronically ill patients, is the barrier to end-of-life choice primarily material (legal prohibition, medical enforcement, economic dependency) or internalized (identity fusion with sanctity narrative)?',
    'Qualitative interviews with disabled individuals in legalization vs prohibition jurisdictions; analysis of refusal language (external pressure vs internal identity claim). Comparison of choice-making patterns pre- and post-legalization for same population cohorts.',
    'If primarily internalized: identity_locked classification is correct; the constraint operates through cognitive capture rather than material force. If primarily material: trapped or constrained classification is more accurate. The distinction determines whether legalization removes the constraint or merely shifts its mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_disabled_populations, empirical, 'Whether identity-lock in disabled populations is cognitive or material').

omega_variable(
    institutional_benefit_decomposition,
    'What proportion of the medical establishment''s beneficiary status derives from legitimate protection (preventing coercion of vulnerable populations) vs illegitimate extraction (institutional control over life/death authority)?',
    'Comparative institutional analysis: jurisdictions with autonomy models retain medical involvement (consultation, safeguarding); the difference is decision authority distribution. Measure extraction as the delta in institutional authority between sanctity and autonomy models.',
    'If legitimate protection dominates: constraint should be tangled_rope (mixed coordination and extraction). If extraction dominates: snare classification confirmed. This affects whether the constraint can be reformed (reduce extraction while preserving protection) or must be replaced (different protection model).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_decomposition, empirical, 'Institutional benefit decomposition: legitimate protection vs extractive authority').

omega_variable(
    transcendent_law_vs_doctrinal_reading,
    'Is the sanctity principle a foundational commitment of all major religious and philosophical traditions, or a specific doctrinal reading that competes with other readings within the same traditions?',
    'Theological and philosophical exegesis: survey Catholic, Protestant, Jewish, Islamic, Buddhist, and secular philosophical literatures for evidence of internal debate over sanctity vs autonomy. Document lineages of interpretation showing how the same traditions have held different positions across time.',
    'If foundational: mountain classification gains support (natural-law appearance strengthened). If doctrinal reading: false-summit omega confirmed; the constraint reflects contested authority rather than transcendent truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transcendent_law_vs_doctrinal_reading, conceptual, 'Sanctity as foundational commitment vs contested doctrinal reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_sanct_theater_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dign_sanct_theater_t25, dignified_death__sanctity_primary, theater_ratio, 25, 0.52).
narrative_ontology:measurement(dign_sanct_theater_t50, dignified_death__sanctity_primary, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(dign_sanct_extract_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dign_sanct_extract_t25, dignified_death__sanctity_primary, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(dign_sanct_extract_t50, dignified_death__sanctity_primary, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_sanct_supp_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(dign_sanct_supp_t25, dignified_death__sanctity_primary, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(dign_sanct_supp_t50, dignified_death__sanctity_primary, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, medical_coercion_institutional_asymmetry).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, vulnerable_population_extraction_healthcare).

% DUAL FORMULATION NOTE:
% The dignified-death kernel decomposes into three constraint stories: sanctity_primary (this file), autonomy_primary, and relational_autonomy. Each story represents a different reading of the same contested kernel (what constitutes dignity at life's end). Each reading has its own ε value, beneficiary/victim structure, and classification. The sanctity reading (ε=0.58, snare) treats life-preservation as primary coordination with protection function. The autonomy reading would treat self-determination as primary with ε in 0.45-0.55 range (tangled_rope). The relational reading would integrate both with distributed decision authority. Network links track how readings influence each other: sanctity sets maximum constraints; autonomy challenges sanctity's outcome-priority; relational attempts procedural mediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__sanctity_primary, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
