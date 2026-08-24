% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Correct Latin as Continuous Living Practice (Continuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading of correct Latin asserts that legitimacy derives
 *   from unbroken living transmission: the Latin used by medieval scholars,
 *   ecclesiastics, and administrators is not a corruption of Classical Latin
 *   but its legitimate evolution. This reading validates medieval Latin
 *   literature, ecclesiastical usage, and neo-Latin production as authentic
 *   Latin, rejecting the Renaissance humanist and modern philological claim
 *   that only Classical forms are correct. The constraint operates as a
 *   normative standard in philology, education, and ecclesiastical
 *   governance. The claimed_type is mountain (natural continuity), but
 *   identifiable beneficiaries (medievalists, ecclesiastical users) and
 *   victims (classical purists, textual critics) exist, triggering
 *   false-summit evaluation. The epsilon of 0.18 reflects the reading's own
 *   assessment: from its perspective, the standing arrangement (continuous
 *   practice) extracts little — it simply is the language. The competing
 *   discontinuity reading would assess a much higher epsilon for this same
 *   arrangement.
 *
 * KEY AGENTS:
 *   - medieval_latinists: Primary beneficiary (organized/identity_locked) — their field gains legitimacy
 *   - continuous_tradition_practitioners: Primary beneficiary (institutional/identity_locked) — ecclesiastical and scholarly communities using Latin continuously
 *   - neo_latin_writers: Beneficiary (organized/constrained) — early modern writers using Latin for science/diplomacy
 *   - ecclesiastical_latin_users: Beneficiary (institutional/identity_locked) — Catholic Church as institutional carrier
 *   - classical_purists: Victim (organized/constrained) — Renaissance humanists and modern Classicists whose authority rests on Classical norm
 *   - reconstructionist_philologists: Victim (organized/mobile) — textual critics who reconstruct Classical Latin from manuscripts
 *   - textual_criticism_school: Victim (institutional/mobile) — editorial standards privileging Classical witnesses
 *   - analytical_observer: Observer (analytical/analytical) — sees full structural contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.18).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.12).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, mountain).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Correct Latin as Continuous Living Practice (Continuity Reading)").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:emerges_naturally(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '35fc1dca-3b72-49cd-ab82-db3b30a6c5ab').
narrative_ontology:cs_kernel_codification('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', fixed_text).
narrative_ontology:cs_authority_grounding('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', practice).
narrative_ontology:cs_interpretation_layer_present('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab').
narrative_ontology:cs_reading_relation('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', foundational, living_practice_defines_correctness).
narrative_ontology:cs_axiom_status(living_practice_defines_correctness, holdable).
narrative_ontology:cs_axiom_grounding('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', living_practice_defines_correctness, conventional).
narrative_ontology:cs_axiom('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', foundational, no_rupture_between_classical_and_medieval).
narrative_ontology:cs_axiom_status(no_rupture_between_classical_and_medieval, holdable).
narrative_ontology:cs_axiom_grounding('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', no_rupture_between_classical_and_medieval, empirically_contingent).
narrative_ontology:cs_reference_frame('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', patristic_early_medieval_continuity).
narrative_ontology:cs_drift_state('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', renaissance_humanist_reform, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('35fc1dca-3b72-49cd-ab82-db3b30a6c5ab', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_latinists).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, continuous_tradition_practitioners).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, neo_latin_writers).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, ecclesiastical_latin_users).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classical_purists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, reconstructionist_philologists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, textual_criticism_school).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, latin_continuity_thesis).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, living_practice_legitimacy).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, medieval_latin_authenticity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose field (medieval Latin studies) gains full legitimacy from the continuity reading. Their professional identity is constituted by the claim that medieval Latin is legitimate Latin, not a corrupted phase. Exit would mean abandoning the field's foundational premise.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_latinists, beneficiary,
    organized, biographical, identity_locked, global).

% Communities that have used Latin continuously since antiquity — primarily the Catholic Church (liturgical, canonical, curial Latin) but also scholarly republics of letters. They set the agenda for what counts as current Latin usage. Their institutional identity fuses with the continuity claim.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, continuous_tradition_practitioners, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, continuous_tradition_practitioners, agenda_setter).

% Early modern writers (1500-1800) who used Latin for science, diplomacy, and literature. The continuity reading validates their output as authentic Latin rather than 'bad imitation.' They have some exit (can write in vernaculars) but lose the international republic of letters.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, neo_latin_writers, beneficiary,
    organized, biographical, constrained, global).

% The Catholic Church as the largest institutional carrier of continuous Latin practice. Canon law, liturgy, and magisterial documents use evolved Latin forms. The continuity reading secures their legitimacy against Classical purist critique. Exit is institutionally impossible without schism.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, ecclesiastical_latin_users, beneficiary,
    institutional, civilizational, identity_locked, global).

% Renaissance humanists and modern Classicists who define correct Latin by Classical authors (Cicero, Caesar, Virgil). The continuity reading extracts authority from their norm: if medieval forms are equally 'correct,' the Classical standard loses its exclusive claim. They can retreat to Classical textual work but lose the broader 'Latin' category.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_purists, payer,
    organized, biographical, constrained, global).

% Textual critics who reconstruct Classical Latin from manuscript evidence. The continuity reading treats their reconstruction target as just one stage in a living tradition, not the norm. They have high mobility — can work entirely within Classical corpus — but their methodological premise (recovering the 'original' correct form) is undermined.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, reconstructionist_philologists, payer,
    organized, biographical, mobile, global).

% Editorial standards (Teubner, OCT, CSEL) that privilege Classical witnesses and normalize medieval texts to Classical norms. The continuity reading forces a split: medieval texts edited on their own terms vs. normalized. They set editorial agendas but pay the cost of maintaining two editorial paradigms.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_criticism_school, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, textual_criticism_school, agenda_setter).

% The analytical seat that sees the full structural contest: three readings of one kernel, each with different beneficiary/victim structures, different epsilon assessments, different cs_structure framings. No stake in the outcome.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, continuous_tradition_practitioners).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified legitimacy standard for all post-Classical Latin production (medieval, renaissance, neo-Latin, ecclesiastical) so that users across 1200+ years can recognize each other's Latin as correct without constant renegotiation.
% TRANSFER_FUNCTION: Moves legitimacy authority from Classical textual witnesses (monopoly of Classical purists) to living practice communities (medievalists, ecclesiastics, neo-Latinists). The transfer is not monetary but epistemic/institutional: the right to define 'correct Latin' shifts from reconstructors to continuators.
% ABSENT_VOICES: Vernacular language advocates (who see Latin itself as the constraint) and non-European Latin users (colonial Latin in Americas/Asia, whose forms are often excluded even by continuity-reading editors). Also: the manuscripts themselves — the material witnesses that neither reading fully controls.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, medieval Latin texts would revert to 'corrupt' status, ecclesiastical documents would lose linguistic legitimacy, neo-Latin literature would be reclassified as 'bad Latin,' and the field would reorganize around the Classical norm (discontinuity_reading) or the hybrid compromise. The partition of philology into Classical vs. Medieval departments would harden.
% FOUNDING_PROBLEM: After the Western Roman Empire's political collapse, Latin remained the sole written language of the church, law, and learning across Western Europe. The founding problem: how to maintain a unified written standard without a central academy, while the spoken language diversified into Romance. The continuity solution: correctness = whatever the living practice of the learned/ecclesiastical community produces.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (maintaining a unified written standard across a fragmenting polity) is dead — Romance languages are now the spoken standards, Latin is no longer a living administrative language, and the church has adopted vernacular liturgies. This is attested by historians of Latin (Waquet, Leonhardt) and sociolinguists (not by the continuity reading's beneficiaries, who argue the problem is live because Latin remains the church's official language).
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(correct_latin__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(correct_latin__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(correct_latin__continuity_reading),
    narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the continuity reading presents itself as descriptive of a natural process — language evolves continuously. Suppression is low (0.12) because the reading does not actively enforce; it validates. Theater is minimal (0.08) — the performance of 'living practice' is the practice itself. Accessibility_collapse is moderately high (0.72) because once you accept the continuity premise, alternative legitimacy criteria (Classical purity, textual reconstruction) appear incoherent. Resistance is moderate (0.35) because the discontinuity and hybrid readings maintain active scholarly communities. The temporal measurements show extraction rising during the Renaissance humanist period (1400-1600) when the continuity reading became a contested position rather than the default, then declining as philology professionalized and the field partitioned.
 *
 * PERSPECTIVAL GAP:
 *   From the medievalist/ecclesiastical seat, the constraint is a mountain — Latin simply continued. From the classical purist seat, it is a snare — a legitimacy claim that extracts authority from the Classical texts. From the textual critic seat, it is a tangled rope — it coordinates medieval text editing but extracts by declaring Classical norms inapplicable. The engine computes this divergence from the structural data: beneficiaries have identity_locked exit (cannot exit the tradition without losing professional identity), victims have constrained or mobile exit (can work within Classical corpus).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (medieval_latinists, continuous_tradition_practitioners, ecclesiastical_latin_users) are identity_locked — their professional/ecclesiastical identity is constituted by the continuity claim. Exit means abandoning their field's foundation. Victims (classical_purists, reconstructionist_philologists) have constrained or mobile exit — they can retreat to Classical corpus work, but lose influence over the broader 'Latin' category. The analytical observer sees the full contest. Directionality derives from beneficiary/victim declarations plus exit modulation: identity_locked beneficiaries get d near 0.0, constrained victims get d near 0.7-0.8.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling: the continuity reading is not pure extraction (snare) because it solves a genuine coordination problem — providing a unified legitimacy standard for all post-Classical Latin. But it is not pure coordination (rope) because it asymmetrically validates medieval/ecclesiastical forms at the expense of Classical norms. The false-summit mountain claim (natural continuity) masks this asymmetry. The mandatrophy question: was the continuity reading ever a genuine coordination mechanism (e.g., in the early medieval period when Latin was the only written language), or has it always been a legitimacy claim? The temporal data suggests it was coordination-dominant pre-1200, extraction-accumulating during the Renaissance contest, and now persists as a partitioned equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law of linguistic evolution, or a constructed normative claim that benefits identifiable scholarly communities?',
    'Comparative analysis of whether the continuity claim holds across all Romance language developments and neo-Latin communities, or whether it selectively validates certain medieval/neo-Latin forms while excluding others.',
    'If constructed, the constraint is a false summit (tangled_rope or snare) that extracts legitimacy from classical purists and textual critics to benefit medievalists and ecclesiastical users. If natural, it is a mountain with negligible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural-law vs. constructed-legitimacy ambiguity for the continuity reading of correct Latin').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping, editorial standards, curricular exclusion) or internalized (scholars self-censuring medieval forms as ''corrupt'')?',
    'Post-reform suppression trajectory: if suppression of medieval forms persists after explicit gatekeeping is removed (e.g., in journals that adopt inclusive policies), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after institutional exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in philological legitimacy contests').

omega_variable(
    cs_framing_underdetermination,
    'Does the continuity reading ground its authority in the living practice itself (practice-grounded) or in a claimed unbroken lineage from Classical antiquity (lineage-grounded)?',
    'Examine whether the reading''s normative force derives from the actual continuity of usage communities or from a historical narrative about that continuity. The two framings produce different cs_pattern classifications.',
    'If practice-grounded, authority_grounding = practice and interpretation_layer_present = true. If lineage-grounded, authority_grounding = lineage with fixed_text kernel_codification. The classification of drift_state direction changes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Commitment-system framing under-determination: practice vs. lineage authority grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_continuity_tr_t800, correct_latin__continuity_reading, theater_ratio, 800, 0.02).
narrative_ontology:measurement(correct_latin_continuity_tr_t1000, correct_latin__continuity_reading, theater_ratio, 1000, 0.03).
narrative_ontology:measurement(correct_latin_continuity_tr_t1200, correct_latin__continuity_reading, theater_ratio, 1200, 0.04).
narrative_ontology:measurement(correct_latin_continuity_tr_t1400, correct_latin__continuity_reading, theater_ratio, 1400, 0.05).
narrative_ontology:measurement(correct_latin_continuity_tr_t1500, correct_latin__continuity_reading, theater_ratio, 1500, 0.06).
narrative_ontology:measurement(correct_latin_continuity_tr_t1600, correct_latin__continuity_reading, theater_ratio, 1600, 0.07).
narrative_ontology:measurement(correct_latin_continuity_tr_t1800, correct_latin__continuity_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(correct_latin_continuity_tr_t1900, correct_latin__continuity_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(correct_latin_continuity_tr_t2000, correct_latin__continuity_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(correct_latin_continuity_tr_t2024, correct_latin__continuity_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(correct_latin_continuity_be_t800, correct_latin__continuity_reading, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(correct_latin_continuity_be_t1000, correct_latin__continuity_reading, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(correct_latin_continuity_be_t1200, correct_latin__continuity_reading, base_extractiveness, 1200, 0.1).
narrative_ontology:measurement(correct_latin_continuity_be_t1400, correct_latin__continuity_reading, base_extractiveness, 1400, 0.12).
narrative_ontology:measurement(correct_latin_continuity_be_t1500, correct_latin__continuity_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(correct_latin_continuity_be_t1600, correct_latin__continuity_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement(correct_latin_continuity_be_t1800, correct_latin__continuity_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(correct_latin_continuity_be_t1900, correct_latin__continuity_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(correct_latin_continuity_be_t2000, correct_latin__continuity_reading, base_extractiveness, 2000, 0.16).
narrative_ontology:measurement(correct_latin_continuity_be_t2024, correct_latin__continuity_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_continuity_su_t800, correct_latin__continuity_reading, suppression_requirement, 800, 0.03).
narrative_ontology:measurement(correct_latin_continuity_su_t1000, correct_latin__continuity_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(correct_latin_continuity_su_t1200, correct_latin__continuity_reading, suppression_requirement, 1200, 0.08).
narrative_ontology:measurement(correct_latin_continuity_su_t1400, correct_latin__continuity_reading, suppression_requirement, 1400, 0.1).
narrative_ontology:measurement(correct_latin_continuity_su_t1500, correct_latin__continuity_reading, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(correct_latin_continuity_su_t1600, correct_latin__continuity_reading, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement(correct_latin_continuity_su_t1800, correct_latin__continuity_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(correct_latin_continuity_su_t1900, correct_latin__continuity_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(correct_latin_continuity_su_t2000, correct_latin__continuity_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(correct_latin_continuity_su_t2024, correct_latin__continuity_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__continuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the correct_latin kernel family. The continuity_reading has epsilon ~0.18 (from its own lights); discontinuity_reading would author epsilon ~0.65 for the same standing arrangement (Classical norm as standing arrangement, continuity reading as extractive deviation); hybrid_reading would author epsilon ~0.35. They are structurally distinct constraints linked by network.affects_constraints. The continuity_reading influences the hybrid_reading (the hybrid must accommodate continuity's legitimacy claims) and coexists_with the discontinuity_reading (both remain live in different institutional homes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__continuity_reading, institutional, 0.1).
constraint_indexing:directionality_override(correct_latin__continuity_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
