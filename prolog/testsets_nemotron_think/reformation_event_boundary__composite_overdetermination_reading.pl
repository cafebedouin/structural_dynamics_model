% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation Event Boundary: Composite Overdetermination Reading
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story models the composite overdetermination reading of
 *   the Reformation event boundary as a commitment-system constraint. The
 *   reading asserts that theological innovation (climb), institutional
 *   collapse (drop), political realignment (swap), and denominational
 *   proliferation (emergence) occurred simultaneously and irreducibly, making
 *   any single-causal-driver or periodization scheme inadequate. The
 *   constraint operates in the space of historiographical interpretation: it
 *   coordinates multiple causal strands (coordination function) but also
 *   marginalizes scholars committed to monocausal narratives (extraction
 *   function). The reading is actively enforced through academic peer review,
 *   grant structures, and curricular norms that treat monocausal accounts as
 *   outdated. Beneficiaries are historians who build careers on complex
 *   causality and interdisciplinary scholars who gain institutional
 *   recognition; victims are confessional historians and monocausal
 *   narrativists whose explanatory frameworks are delegitimized. The claimed
 *   type is tangled_rope because the reading both coordinates (integrates
 *   multiple drivers) and extracts (asymmetrically disadvantages monocausal
 *   positions).
 *
 * KEY AGENTS:
 *   - historians_of_complex_causality: Primary beneficiary (institutional/biographical) — gains professional validation for interdisciplinary work
 *   - interdisciplinary_scholars: Primary beneficiary (organized/biographical) — receives funding and platform access for multi-causal approaches
 *   - monocausal_narrativists: Primary payer (moderate/biographical) — sees their explanatory frameworks marginalized in top journals and curricula
 *   - confessional_historians: Primary payer (organized/biographical) — loses theological coherence as explanatory primacy
 *   - theological_climb_advocates: Excluded (powerful/generational) — would object to demotion of theological innovation to one strand among equals
 *   - political_swap_advocates: Excluded (powerful/generational) — would object to demotion of political realignment to one strand among equals
 *   - academic_institutions: Agenda setter (institutional/generational) — sets methodological standards through hiring, funding, and publication norms
 *   - religious_communities: Observer (organized/generational) — affected by historiographical framing of their origins but not direct participants in academic discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.35).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation Event Boundary: Composite Overdetermination Reading").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '15fde458-490e-4446-8400-dd7119902591').
narrative_ontology:cs_kernel_codification('15fde458-490e-4446-8400-dd7119902591', distributed).
narrative_ontology:cs_authority_grounding('15fde458-490e-4446-8400-dd7119902591', practice).
narrative_ontology:cs_interpretation_layer_present('15fde458-490e-4446-8400-dd7119902591').
narrative_ontology:cs_reading_relation('15fde458-490e-4446-8400-dd7119902591', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('15fde458-490e-4446-8400-dd7119902591', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('15fde458-490e-4446-8400-dd7119902591', foundational, historical_events_are_overdetermined).
narrative_ontology:cs_axiom_status(historical_events_are_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('15fde458-490e-4446-8400-dd7119902591', historical_events_are_overdetermined, instrumental).
narrative_ontology:cs_axiom('15fde458-490e-4446-8400-dd7119902591', secondary, periodization_schemes_are_contingent).
narrative_ontology:cs_axiom_status(periodization_schemes_are_contingent, holdable).
narrative_ontology:cs_axiom_grounding('15fde458-490e-4446-8400-dd7119902591', periodization_schemes_are_contingent, conventional).
narrative_ontology:cs_reference_frame('15fde458-490e-4446-8400-dd7119902591', event_as_given).
narrative_ontology:cs_drift_state('15fde458-490e-4446-8400-dd7119902591', contemporary_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15fde458-490e-4446-8400-dd7119902591', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, historians_of_complex_causality).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_scholars).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, monocausal_narrativists).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_historians).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, overdetermination_thesis).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, parallel_cs_patterns_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build careers on multi-causal frameworks; gain publication in top journals, grant funding, and conference invitations by demonstrating interdisciplinary mastery. Exit is mobile — they can shift to other historical fields where composite approaches are valued.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historians_of_complex_causality, beneficiary,
    institutional, biographical, mobile, global).

% Receive institutional recognition and resources for bridging history, theology, political science, and sociology. Their professional identity is tied to the composite paradigm; exit is mobile but identity_locked tendencies exist because their training invests them in the framework.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_scholars, beneficiary,
    organized, biographical, mobile, global).

% Produce single-driver accounts (theological, political, economic). Face increasing difficulty placing work in flagship journals, securing grants, or getting graduate students. Exit is constrained: they can publish in niche venues or shift fields, but their expertise is devalued in the dominant paradigm.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, monocausal_narrativists, payer,
    moderate, biographical, constrained, global).

% Maintain theological coherence as the primary explanatory key for the Reformation. Their work is marginalized in secular academia but thrives in confessional institutions. Exit is identity_locked: their professional and religious self-concept is fused with the theological-climb reading; leaving the paradigm would mean abandoning a core identity commitment.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_historians, payer,
    organized, generational, identity_locked, global).

% Argue that Luther's doctrinal breakthrough is the irreducible cause. They are structurally excluded from agenda-setting in mainstream historiography because their framework is deemed reductionist. Their exit is trapped: they cannot change the dominant paradigm but cannot abandon their reading without theological crisis.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, theological_climb_advocates, excluded,
    powerful, generational, trapped, global).

% Argue that secular rulers exploited theology to seize church assets. They are excluded from the composite reading's coordination because their materialist framing is treated as 'one strand among many' rather than the primary driver. Exit is trapped: their interpretive community is institutionally marginalized but internally cohesive.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_swap_advocates, excluded,
    powerful, generational, trapped, global).

% Set methodological standards through hiring, tenure criteria, grant panels, and journal editorships. They arbitrage between paradigms by funding 'interdisciplinary' centers while maintaining departmental boundaries. They could change the constraint by altering incentives but benefit from the current arrangement's stability.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, academic_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Affected by historiographical framing of their origins (legitimacy, identity, ecumenical relations) but not direct participants in academic discourse. They observe from outside and occasionally intervene through funding confessional institutions or public commentary.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, religious_communities, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, academic_institutions).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates four irreducible causal strands (theological, institutional, political, denominational) into a single historiographical framework, preventing any one strand from monopolizing explanation and enabling scholars to trace interactions across domains.
% TRANSFER_FUNCTION: Moves professional legitimacy, publication space, grant funding, and curricular authority from monocausal narrativists and confessional historians to historians of complex causality and interdisciplinary scholars, as the price of participating in the dominant paradigm.
% ABSENT_VOICES: Theological climb advocates and political swap advocates are structurally excluded; they would argue that their single-driver frameworks are not merely 'strands' but the primary causal engine, and that the composite reading dilutes explanatory power by refusing to adjudicate primacy. They are kept out by peer-review norms that treat monocausal arguments as methodologically naive.
% DISAPPEARANCE_RATIONALE: If the composite reading vanished overnight, monocausal narratives would regain explanatory primacy in their respective subfields, confessional history would reassert theological coherence as the master key, political historians would re-center state formation, and interdisciplinary centers would lose their justification — the historiographical landscape would reorganize around competing single-driver paradigms.
% FOUNDING_PROBLEM: The Reformation resisted all monocausal explanations: purely theological accounts ignored political economy; purely political accounts ignored doctrinal sincerity; purely institutional accounts ignored popular reception. The composite reading was built to solve the problem of irreducible multi-causality without collapsing into eclecticism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by scholars outside the beneficiary set: philosophers of history (e.g., William Dray on narrative explanation), sociologists of knowledge (e.g., Randall Collins on intellectual networks), and historians of other overdetermined events (French Revolution, 1914) who confirm that irreducible multi-causality is a genuine explanatory challenge, not an academic fashion. No single discipline corroborates it; the corroboration is cross-disciplinary.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).
:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the asymmetric cost imposed on monocausal narrativists: they must either adopt the composite framework (incurring retraining cost) or accept marginalization. Suppression (0.35) is moderate: alternatives are not banned but are structurally disadvantaged in prestige venues. Theater ratio (0.22) is low because the composite reading's coordination function (integrating theology, politics, institutions) is genuinely functional for explaining the event's complexity. Accessibility collapse (0.38) is moderate: once a scholar adopts the composite lens, monocausal alternatives appear incomplete but not incoherent. Resistance (0.52) is substantial: confessional and political-swap readings persist in parallel scholarly communities. Measurements show a gradual rise in extractiveness and suppression from 1800 to 2025 as the composite reading became academically dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (academic institutions), the constraint appears as a rope: a genuine methodological advance that coordinates previously fragmented subfields. From the payer seats (monocausal narrativists, confessional historians), it appears as a snare: an enforced orthodoxy that extracts professional legitimacy from their work. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid nature but does not adjudicate the seat-level experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (historians_of_complex_causality, interdisciplinary_scholars) collect professional capital (grants, publications, hires) from the constraint's operation — their directionality d is low (near 0.15). Victims (monocausal_narrativists, confessional_historians) bear the cost of methodological displacement — their d is high (near 0.85). Excluded agents (theological_climb_advocates, political_swap_advocates) are not directly coordinated or extracted from but are structurally prevented from setting the agenda — their d is intermediate but with trapped exit options. The academic_institutions agenda setter sits near symmetric (d ~0.5) because they both maintain the constraint and incur administrative costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading's founding problem (making sense of the Reformation's irreducible complexity) remains live — new archival discoveries and theoretical frameworks (e.g., global history, material culture) continually refresh the need for multi-causal analysis. However, the constraint shows mandatrophy risk: the coordination function (integrating four causal strands) may have atrophied into a ritualized requirement to 'be interdisciplinary' even when a particular sub-event is genuinely monocausal. The theater ratio rise (0.1→0.22) suggests growing performative compliance. The constraint is not yet a piton because active enforcement (peer review, funding) remains strong and beneficiaries still gain substantially.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_reading_naturalness,
    'Is the composite overdetermination reading a genuine historiographical advance that reflects the irreducible complexity of the event, or a constructed constraint that benefits interdisciplinary scholars by marginalizing monocausal narratives?',
    'Comparative analysis of historiographical trajectories: if the composite reading gains acceptance only where interdisciplinary structures are institutionally rewarded, it may be partly constructed; if it resolves persistent anomalies in monocausal accounts across diverse scholarly communities, it is more likely a genuine advance.',
    'If constructed, the reading functions as a tangled_rope extracting legitimacy from simpler narratives; if genuine, it operates as a rope coordinating multiple causal strands without asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_reading_naturalness, conceptual, 'Naturalness of the composite reading vs. institutional construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Does the composite reading suppress alternatives through structural academic gatekeeping (journal peer review, hiring, funding) or through internalized historiographical norms that make monocausal accounts seem naive?',
    'Survey of historians'' self-reported methodological constraints and analysis of citation networks to see if monocausal works are excluded or merely marginalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — scholars carry the suppression with them even in open forums.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1800, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(refo_tr_t1850, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(refo_tr_t1900, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(refo_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(refo_tr_t2000, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(refo_tr_t2025, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(refo_be_t1800, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(refo_be_t1850, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1850, 0.3).
narrative_ontology:measurement(refo_be_t1900, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(refo_be_t2000, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(refo_be_t2025, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1800, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(refo_su_t1850, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(refo_su_t1900, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.32).
narrative_ontology:measurement(refo_su_t2000, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(refo_su_t2025, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.02).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% The Reformation event boundary kernel decomposes into three readings: composite_overdetermination (this story), theological_climb, and political_swap. The composite reading coordinates the other two by treating them as parallel strands; the siblings each foreground one strand as primary. The ε values differ: composite reading has moderate extraction (0.42) because it marginalizes monocausal narratives; theological_climb reading has lower extraction (benefits confessional historians); political_swap reading has higher extraction (benefits secular state narratives). They form a constraint family linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, institutional, 0.45).
constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
