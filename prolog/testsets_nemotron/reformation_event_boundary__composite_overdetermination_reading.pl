% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Reformation Event Boundary — Composite Overdetermination Reading
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story models the 'composite overdetermination reading' of
 *   the Reformation event boundary — the position that the Reformation was
 *   not a single event with one cause but an irreducibly overdetermined
 *   composite of theological innovation (climb), institutional collapse
 *   (drop), political realignment (swap), and denominational proliferation
 *   (emergence) operating in parallel. The reading treats historiographical
 *   overdetermination as a structural feature of the phenomenon, not a bug to
 *   be resolved. It is one of three contested readings of the kernel
 *   'reformation_event_boundary', alongside the theological_climb_reading and
 *   political_swap_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.48).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation Event Boundary — Composite Overdetermination Reading").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '0ba35e44-c657-4cff-a76f-7420b3899d9a').
narrative_ontology:cs_kernel_codification('0ba35e44-c657-4cff-a76f-7420b3899d9a', distributed).
narrative_ontology:cs_authority_grounding('0ba35e44-c657-4cff-a76f-7420b3899d9a', distributed).
narrative_ontology:cs_reading_relation('0ba35e44-c657-4cff-a76f-7420b3899d9a', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ba35e44-c657-4cff-a76f-7420b3899d9a', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('0ba35e44-c657-4cff-a76f-7420b3899d9a', foundational, reformation_irreducibly_polycentric).
narrative_ontology:cs_axiom_status(reformation_irreducibly_polycentric, holdable).
narrative_ontology:cs_axiom_grounding('0ba35e44-c657-4cff-a76f-7420b3899d9a', reformation_irreducibly_polycentric, empirically_contingent).
narrative_ontology:cs_axiom('0ba35e44-c657-4cff-a76f-7420b3899d9a', foundational, periodization_contestation_structural).
narrative_ontology:cs_axiom_status(periodization_contestation_structural, holdable).
narrative_ontology:cs_axiom_grounding('0ba35e44-c657-4cff-a76f-7420b3899d9a', periodization_contestation_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('0ba35e44-c657-4cff-a76f-7420b3899d9a', polycentric_reformation_field).
narrative_ontology:cs_drift_state('0ba35e44-c657-4cff-a76f-7420b3899d9a', contemporary_historiography, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0ba35e44-c657-4cff-a76f-7420b3899d9a', '2026-08-10T14:32:17Z').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, historical_scholars_composite_frame).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_methodologists).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformation_polycentric_institutions).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, monocausal_periodization_schemes).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_historiography_traditions).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, single_driver_causal_models).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historical_overdetermination_principle).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, parallel_cs_pattern_operation).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, periodization_contestation_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the composite reading as a live historiographical position; organize conferences, journals, and graduate training around polycentric periodization; benefit from the reading's legitimacy as a sophisticated alternative to confessional narratives.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historical_scholars_composite_frame, agenda_setter,
    organized, generational, mobile, global).

% Use the Reformation as a proving ground for multi-causal historical methods; the composite frame validates interdisciplinary approaches and resists reduction to any single discipline's explanatory schema.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_methodologists, beneficiary,
    organized, biographical, arbitrage, global).

% Research centers, institutes, and funding streams organized around polycentric Reformation studies; their institutional identity and resource flows depend on the composite reading remaining a live, fundable paradigm.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformation_polycentric_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Single-driver narratives (theological, political, economic) lose explanatory purchase when the composite frame is accepted; their proponents must either absorb the composite reading as a complicating layer or defend their periodization against it — both are costly.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, monocausal_periodization_schemes, payer,
    moderate, biographical, constrained, global).

% Traditions that read the Reformation through a single confessional lens (Lutheran, Reformed, Catholic, Radical) find their periodization and causal story disrupted; the composite frame treats their narrative as one thread among several, which is experienced as extraction from their self-understanding.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_historiography_traditions, payer,
    organized, generational, identity_locked, global).

% Models that attribute the Reformation to one primary cause (theology, politics, economics, printing, nationalism) must either expand to accommodate parallel drivers or concede incompleteness; the composite frame makes single-driver adequacy a contested claim.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, single_driver_causal_models, payer,
    moderate, biographical, constrained, global).

% PhD candidates and junior faculty must choose a periodization to enter the field; the composite frame's sophistication is a barrier to entry, but rejecting it marks them as unsophisticated — they are structurally excluded from shaping the debate.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, early_career_historians, excluded,
    powerless, immediate, trapped, national).

% Track the Reformation as a case of macro-historical transformation; the composite reading provides a richer empirical base for cross-civilizational comparison but does not determine their theoretical commitments.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, comparative_historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a historiographical framework that coordinates multiple causal strands (theological, political, institutional, social) without forcing them into a single narrative hierarchy; allows scholars to track parallel CS patterns (climb, drop, swap, emergence) as simultaneously operative.
% TRANSFER_FUNCTION: Moves explanatory authority and institutional resources from monocausal periodization schemes and confessional historiographies toward polycentric, interdisciplinary approaches; the composite frame's legitimacy transfers prestige and funding to scholars and institutions that can operate across the traditional divides.
% ABSENT_VOICES: Confessional communities for whom the Reformation is not a historical event but a living doctrinal identity — their self-understanding is not a 'reading' to be coordinated but a commitment that the composite frame treats as data. Also absent: scholars working in traditions where periodization is settled by institutional authority (e.g., certain national academies, confessional seminaries).
% DISAPPEARANCE_RATIONALE: If the composite reading vanished overnight, the field would revert to contested monocausal periodizations; confessional historiographies would regain unchallenged authority in their domains; interdisciplinary centers would lose their organizing rationale; early-career scholars would face a simpler but more polarized entry landscape.
% FOUNDING_PROBLEM: The Reformation's historiography was stuck in a zero-sum contest between confessional narratives (Lutheran, Catholic, Reformed) and secular reductionisms (political, economic, printing-press determinism); each claimed exclusive explanatory adequacy and each had institutional power to enforce its periodization.
% FOUNDING_PROBLEM_CORROBORATION: The composite reading's founding problem is attested by scholars outside its beneficiary set: social historians of early modern Europe (e.g., Scribner, Ozment) who document the irreducible plurality of Reformation experiences; historians of science tracking parallel epistemic shifts; philosophers of history (e.g., Ankersmit, White) who treat narrative plurality as a structural feature of historical representation. No single tradition owns the corroboration.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the composite frame's displacement of monocausal narratives: it extracts explanatory authority from single-driver models and confessional traditions without fully replacing them — they persist as contested positions. Suppression (0.48) is moderate: the composite frame does not actively silence alternatives but makes their exclusive claims structurally harder to maintain. Theater (0.31) captures the performative aspect: some polycentric scholarship performs sophistication without substantive engagement with all four sub-events. Accessibility collapse (0.41) is moderate: alternatives remain viable but require more work to defend. Resistance (0.73) is high: confessional traditions and monocausal models actively contest the composite frame. The measurement grid tracks the reading's institutional consolidation from 1517 (Luther's theses) to 2024, showing extractiveness and theater rising as the composite frame becomes established, while suppression plateaus once the reading achieves paradigm status.
 *
 * PERSPECTIVAL GAP:
 *   From the composite-frame agenda_setter seat, the constraint is genuine coordination: it solves the problem of historiographical fragmentation by providing a framework that honors all four sub-events. From the confessional_historiography_traditions payer seat (identity_locked), the same constraint operates as extraction: it treats their living tradition as a historical artifact among others. From the monocausal_periodization_schemes payer seat, it is a constraint that raises the cost of single-driver adequacy claims. The engine computes these divergences from the structural data; the authored claimed_type (tangled_rope) reflects the authoring seat's judgment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (composite-frame scholars, methodologists, polycentric institutions) occupy agenda_setter and beneficiary roles with mobile-to-constrained exit and organized-to-institutional power — they shape the field and collect prestige/resources. Victims (monocausal schemes, confessional traditions, single-driver models) occupy payer roles with constrained-to-identity_locked exit and moderate-to-organized power — they bear the cost of the composite frame's dominance but cannot easily exit their commitments. Early-career historians are excluded (trapped, powerless). Comparative sociologists observe analytically. The identity_locked exit of confessional_historiography_traditions is critical: their self-understanding is fused with their periodization, making the composite frame's challenge existential rather than merely academic.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading's founding problem — resolving the zero-sum contest between confessional narratives and secular reductionisms — remains contested (not dead). The reading has not become a piton: it continues to coordinate interdisciplinary work and generate new scholarship (theological_climb_reading and political_swap_reading are themselves active research programs, not ghosts). However, theater_ratio's rise from 0.08 to 0.31 suggests some performative maintenance: the 'polycentric' label is sometimes invoked without engaging all four CS patterns. This is monitored but does not yet indicate mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_eclecticism,
    'Does the composite reading genuinely integrate the four sub-events as structurally parallel, or does it merely list them eclectically without a unifying mechanism?',
    'Analyze whether composite-frame scholarship produces a single explanatory model with four coupled variables, or four independent mini-narratives bundled under one label. Citation network analysis of polycentric Reformation studies: do they cite across sub-events or stay siloed?',
    'If eclectic, the coordination function is performative and extractiveness is higher (the frame coordinates nothing real); if genuinely integrated, the coordination function is substantive and extractiveness is lower relative to the coordination achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_eclecticism, conceptual, 'Whether composite overdetermination is a real integration or a cover story for disciplinary fragmentation.').

omega_variable(
    confessional_identity_extraction,
    'When the composite frame treats confessional self-understanding as ''one thread among several'', is this analytic description or extractive displacement?',
    'Track whether confessional communities that engage with composite scholarship experience it as enriching their self-understanding or as eroding their identity-constitutive narrative. Longitudinal study of confessional seminaries'' curriculum changes in response to polycentric historiography.',
    'If extractive, the composite frame''s extraction from confessional_historiography_traditions is asymmetric and the constraint leans toward snare; if descriptive, the extraction is the cost of historiographical honesty and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_identity_extraction, preference, 'Whether the composite frame''s treatment of confessional narratives is extraction or description.').

omega_variable(
    periodization_completion_points,
    'Do the different completion points tracked by sibling readings (1521, 1555, 1648, 1713, ongoing) represent genuine structural differences in what counts as ''the Reformation'', or are they merely rhetorical moves?',
    'Map each reading''s periodization to its causal claims: does theological_climb_reading end at 1521 because justification-by-faith is settled? Does political_swap_reading end at 1648 because Westphalia settles the political order? Does composite_reading have no end because overdetermination is ongoing?',
    'If completion points map to genuine causal closure conditions, periodization contestation is structural; if they are rhetorical, the composite frame''s claim that ''periodization remains contested because different readings track different completion points'' is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_completion_points, empirical, 'Whether contested periodization reflects real causal structure or rhetorical strategy.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel ''reformation_event_boundary'' best framed as a single event-boundary with contested readings, or as a family of distinct constraints (theological, political, institutional, social) that the composite reading merely correlates?',
    'Test whether the four sub-events share a single ε-invariant referent: if changing the observable (theological, political, institutional) changes the constraint''s extractiveness/suppression profile, they are distinct constraints per the ε-invariance principle.',
    'If distinct constraints, the composite reading violates ε-invariance by bundling them; the kernel should be decomposed into four constraint stories linked by network.affects_constraints. If a single referent, the composite reading is the correct framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a single constraint or a family — the ε-invariance test for the composite frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_composite_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(reformation_composite_tr_t1555, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1555, 0.12).
narrative_ontology:measurement(reformation_composite_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.18).
narrative_ontology:measurement(reformation_composite_tr_t1800, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1800, 0.22).
narrative_ontology:measurement(reformation_composite_tr_t1900, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1900, 0.26).
narrative_ontology:measurement(reformation_composite_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(reformation_composite_tr_t1980, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(reformation_composite_tr_t2000, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(reformation_composite_tr_t2024, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(reformation_composite_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(reformation_composite_be_t1555, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1555, 0.28).
narrative_ontology:measurement(reformation_composite_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.35).
narrative_ontology:measurement(reformation_composite_be_t1800, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement(reformation_composite_be_t1900, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1900, 0.51).
narrative_ontology:measurement(reformation_composite_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.56).
narrative_ontology:measurement(reformation_composite_be_t1980, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1980, 0.59).
narrative_ontology:measurement(reformation_composite_be_t2000, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(reformation_composite_be_t2024, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(reformation_composite_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.22).
narrative_ontology:measurement(reformation_composite_su_t1555, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1555, 0.31).
narrative_ontology:measurement(reformation_composite_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.38).
narrative_ontology:measurement(reformation_composite_su_t1800, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1800, 0.43).
narrative_ontology:measurement(reformation_composite_su_t1900, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1900, 0.46).
narrative_ontology:measurement(reformation_composite_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.47).
narrative_ontology:measurement(reformation_composite_su_t1980, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(reformation_composite_su_t2000, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(reformation_composite_su_t2024, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% The kernel 'reformation_event_boundary' decomposes into three constraint stories: this composite_overdetermination_reading (tangled_rope, ε=0.62), theological_climb_reading (expected mountain/rope, ε≈0.15), and political_swap_reading (expected tangled_rope/snare, ε≈0.70). The composite reading coordinates the sibling readings' causal strands; the theological reading treats the doctrinal breakthrough as a mountain-like epistemic event; the political reading treats the asset seizure as extractive realignment. ε values differ because the referents differ: the composite frame's referent is the historiographical field, the theological reading's referent is the doctrinal claim, the political reading's referent is the asset transfer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, organized, 0.25).
constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
