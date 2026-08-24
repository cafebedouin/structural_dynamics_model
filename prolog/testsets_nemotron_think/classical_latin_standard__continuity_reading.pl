% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Living Latin Continuity Standard
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The continuity reading of the classical Latin standard holds that correct
 *   Latin is not a fixed Classical form but the living language transmitted
 *   through unbroken institutional practice from antiquity to the present. It
 *   treats natural linguistic drift — vocabulary expansion, syntactic change,
 *   semantic shift — as legitimate development authorized by the very
 *   institutions that keep Latin alive (the Church, the universities, the
 *   learned societies). This reading presents itself as a Mountain: a natural
 *   fact about how language transmission works when a language loses native
 *   speakers but retains institutional users. Its beneficiaries are the
 *   institutions that wield this authority (ecclesiastical, academic) and the
 *   scholars whose work it legitimizes. Its victims are minimal — primarily
 *   the reconstructionist philologists excluded from institutional authority
 *   and historical vernacular communities whose usage was labeled
 *   'barbarism.' Extractiveness is moderate (0.35): gatekeeping occurs
 *   through institutional access (who gets to teach, publish, officiate)
 *   rather than through systematic extraction of resources. Suppression is
 *   low (0.15): alternatives (reconstructionist Latin, spoken-Latin
 *   movements) exist and publish but lack institutional recognition. The
 *   constraint claims natural emergence (emerges_naturally: true) but
 *   declares beneficiaries, making it an FSM candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.35).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.15).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, mountain).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Living Latin Continuity Standard").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:emerges_naturally(classical_latin_standard__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '7139767e-39f8-4851-8628-f7fb617686c0').
narrative_ontology:cs_kernel_codification('7139767e-39f8-4851-8628-f7fb617686c0', implicit).
narrative_ontology:cs_authority_grounding('7139767e-39f8-4851-8628-f7fb617686c0', practice).
narrative_ontology:cs_interpretation_layer_present('7139767e-39f8-4851-8628-f7fb617686c0').
narrative_ontology:cs_reading_relation('7139767e-39f8-4851-8628-f7fb617686c0', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7139767e-39f8-4851-8628-f7fb617686c0', classical_latin_standard__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7139767e-39f8-4851-8628-f7fb617686c0', foundational, living_practice_is_authoritative).
narrative_ontology:cs_axiom_status(living_practice_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('7139767e-39f8-4851-8628-f7fb617686c0', living_practice_is_authoritative, conventional).
narrative_ontology:cs_axiom('7139767e-39f8-4851-8628-f7fb617686c0', foundational, natural_drift_is_legitimate_development).
narrative_ontology:cs_axiom_status(natural_drift_is_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('7139767e-39f8-4851-8628-f7fb617686c0', natural_drift_is_legitimate_development, conventional).
narrative_ontology:cs_reference_frame('7139767e-39f8-4851-8628-f7fb617686c0', unbroken_practice_continuity).
narrative_ontology:cs_drift_state('7139767e-39f8-4851-8628-f7fb617686c0', contemporary_ecclesiastical_academic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7139767e-39f8-4851-8628-f7fb617686c0', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, academic_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, classical_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, students_learners).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, students_learners).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, latin_continuity_thesis).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, natural_drift_legitimacy).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, practice_over_text_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain Latin as the living language of liturgy, canon law, and magisterial teaching through unbroken practice from antiquity. They set the standard for 'correct' Latin by continuing to use it authoritatively. Their institutional continuity gives them de facto authority over the language's development; they can adopt or reject linguistic changes by choosing whether to incorporate them into official usage.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Teach and study Latin as a continuous tradition from Classical through Medieval to Modern periods. They benefit from the continuity principle because it validates their curriculum (which spans all periods) and their editorial practices (which produce critical editions of post-Classical texts). Their exit is mobile — they could adopt reconstructionist pedagogy but would lose the coherence of a unified tradition.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, academic_institutions, beneficiary,
    institutional, generational, mobile, global).

% Produce scholarship, editions, and translations across the full Latin tradition. The continuity reading legitimizes their work on Medieval, Renaissance, and Neo-Latin texts as 'real Latin' rather than degenerate forms. They collect professional recognition and institutional positions from this legitimacy. Exit is mobile — individual scholars can adopt reconstructionist frameworks, but the field's center of gravity remains continuity-oriented.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_scholars, beneficiary,
    organized, biographical, mobile, global).

% Learn Latin through curricula that present it as a continuous living language. They benefit from a unified pedagogy that lets them read Cicero, Augustine, Aquinas, and Newton with the same grammatical framework. They pay the cost of mastering a broader, more variable corpus than a purified Classical Latin would require. Their exit is constrained — they must learn what their institution teaches.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, students_learners, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, students_learners, payer).

% Advocate for a return to Classical Latin norms based on textual archaeology, rejecting post-Classical developments as corruption. They are structurally excluded from institutional authority (ecclesiastical appointments, mainstream academic positions, standard curricula) because their reading denies the legitimacy of the living tradition those institutions embody. Their exit is constrained — they can publish in specialist venues but cannot access the institutional channels that define 'correct' Latin.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, philological_reconstructionists, excluded,
    organized, biographical, constrained, global).

% Communities (especially in Europe, 500-1500 CE) who spoke Latin as a living vernacular or liturgical language with local developments. Their usage was historically labeled 'barbarism' by the institutional standard even though it represented genuine living continuity. They had no voice in the institutional definition of correctness and no exit from the linguistic hierarchy that marginalized their forms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_latin_communities, excluded,
    powerless, biographical, trapped, local).

% Analyze Latin's transmission from outside the authority structure — historical linguists, sociolinguists, philosophers of language. They see the continuity reading as one of several coherent frameworks for Latin's normativity, not as a natural fact. Their analytical seat has no stake in the constraint's enforcement or benefits.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains intelligibility and shared authority of Latin across 2000+ years by treating linguistic drift as legitimate development rather than corruption, enabling continuous use in liturgy, law, and scholarship without requiring periodic 'restoration' to a Classical ideal.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional legitimacy from textual sources (Classical authors) to living practitioners (the Church, the academy); gatekeeps access to 'correct' Latin through institutional channels (ecclesiastical appointment, academic certification, editorial boards) rather than through textual fidelity alone.
% ABSENT_VOICES: Reconstructionist philologists who argue only Classical Latin is correct; vernacular Latin communities (historical) whose living developments were labeled barbarisms; modern spoken-Latin revival movements that operate outside institutional channels. The reconstructionists are excluded from institutional authority; the vernacular communities were historically silenced by the same institutional hierarchy; revival movements exist but lack institutional recognition.
% DISAPPEARANCE_RATIONALE: If the continuity principle vanished overnight, ecclesiastical Latin would lose its internal justification for post-Classical developments (e.g., theological terminology), academic Latin curricula would fragment into period-specific specializations, and the unified field of 'Latin studies' would split into Classical philology vs. Medieval/Neo-Latin studies with no shared standard. The institutional infrastructure that transmits Latin would need to rebuild its legitimacy on either reconstructionist or ad hoc grounds.
% FOUNDING_PROBLEM: After the Western Roman Empire's political collapse (5th c. CE), Latin survived as the language of the Church, law, and learning but had no living native speakers to authorize its evolution. The founding problem was: how can Latin remain a legitimate, authoritative language for new circumstances (new theology, new law, new science) without living native speakers to sanction change? The continuity reading solved this by making the unbroken practice of institutional users the authorizing mechanism — the Church and the schools *are* the living speakers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the institutions themselves (Vatican documents on Latin's role, university statutes maintaining Latin requirements) and by historians of education (e.g., Waquet's 'Latin: or the Empire of a Sign'). Reconstructionists contest whether the problem still exists, arguing that Latin's authoritative domains have shrunk to near-zero and the continuity principle now serves only institutional self-preservation. No neutral party corroborates the 'live' status without qualification.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(classical_latin_standard__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(classical_latin_standard__continuity_reading),
    narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The moderate extractiveness (0.35) reflects institutional gatekeeping: to have your Latin recognized as 'correct,' you must operate within the institutional channels (ecclesiastical appointment, academic peer review, recognized editorial boards). This is not monetary extraction but epistemic gatekeeping. Low suppression (0.15) because the continuity reading does not actively suppress alternatives — reconstructionist editions are published, spoken-Latin groups meet, but they are not *authorized* as 'correct Latin.' Theater ratio is very low (0.10) because the practice is genuine: the institutions actually use Latin daily for liturgy, administration, and scholarship. Accessibility collapse (0.40) is moderate: the continuity framework makes it difficult to imagine Latin authority *without* institutional continuity, but reconstructionist and hybrid frameworks remain conceptually available. Resistance (0.20) is low because the constraint is the dominant paradigm; resistance comes from the excluded reconstructionist minority.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical_institutions seat (agenda_setter, d ≈ 0.1), the constraint is a Mountain — the natural way language works when transmitted by a living institution. From the philological_reconstructionists seat (excluded, d ≈ 0.9), it is a Snare — an institutional monopoly that delegitimizes their textual-archaeology approach. From the students_learners seat (beneficiary/payer, d ≈ 0.5), it is a Rope — a genuine coordination mechanism that gives them a unified Latin, but with the cost of learning a broader corpus. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions are structural beneficiaries (d near 0): they collect authority and legitimacy from the constraint, control its evolution, and have arbitrage-grade exit (they could change the standard but choose not to). Academic institutions and classical scholars are beneficiaries (d ≈ 0.2-0.3): they collect professional legitimacy and coherent curricula, with mobile exit. Students are dual-positioned (beneficiary/payer, d ≈ 0.5): they gain a unified Latin but pay in learning effort. Reconstructionists and vernacular communities are excluded/trapped (d ≈ 0.8-1.0): they bear the cost of delegitimization with no institutional recourse. The engine derives d from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authorizing Latin's evolution without native speakers) remains live for ecclesiastical and academic users — they still need to generate new Latin terminology for modern concepts (e.g., 'computatrum' for computer, 'interrete' for internet). The continuity reading solves this by letting institutional practice authorize neologisms. Mandatrophy is not resolved: the arrangement still serves its founding function. However, the *scope* of the founding problem has narrowed — Latin's authoritative domains have contracted to the Church and niche academia — raising the question of whether the institutional gatekeeping is proportionate to the remaining coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construction,
    'Is the continuity principle a natural linguistic fact (languages transmitted by institutions naturally evolve) or an institutional construct that benefits the Church and academy by legitimizing their authority over Latin?',
    'Comparative analysis of other institutional languages without native speakers (Sanskrit in Hindu tradition, Classical Arabic in Islamic tradition, Hebrew pre-revival) — do they exhibit the same continuity principle naturally, or only where institutions explicitly claim it?',
    'If natural law, the Mountain claim holds and FSM does not fire. If institutional construct, FSM reclassifies to tangled_rope (coordination + asymmetric extraction via gatekeeping). The beneficiary declarations make this ambiguity structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construction, conceptual, 'False summit mountain ambiguity: natural transmission fact vs. institutional authority construct').

omega_variable(
    kernel_reading_identity,
    'This constraint is the continuity_reading of the classical_latin_standard kernel. What structural elements distinguish it from the reconstruction_reading and hybrid_reading?',
    'The kernel declares three readings. This reading''s axioms (living_practice_is_authoritative, natural_drift_is_legitimate_development) are the structural differentiators. Reconstruction_reading would foreclose natural_drift_is_legitimate_development. Hybrid_reading would coexist but add textual_fidelity_requirement.',
    'Routes committer-frame structure (kernel_id, reading_id, sibling relations) into the omega system per Rule 2. Prevents folding the kernel contest into this constraint''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment system kernel/reading structure for classical_latin_standard').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression (0.15) structural (institutions simply don''t enforce against alternatives) or internalized (reconstructionists and vernacular communities have absorbed the judgment that their Latin is ''incorrect'')?',
    'Post-exit trajectory study: if reconstructionists continue to publish and advocate without institutional penalties beyond non-recognition, suppression is structural. If they self-censor or abandon the field due to internalized illegitimacy, suppression has an internalized component.',
    'If internalized, effective suppression is higher than measured — the constraint''s power includes shaping what alternatives *can be imagined* as legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of reconstructionist alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clscr_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(clscr_tr_t3, classical_latin_standard__continuity_reading, theater_ratio, 3, 0.07).
narrative_ontology:measurement(clscr_tr_t6, classical_latin_standard__continuity_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(clscr_tr_t9, classical_latin_standard__continuity_reading, theater_ratio, 9, 0.09).
narrative_ontology:measurement(clscr_tr_t12, classical_latin_standard__continuity_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(clscr_tr_t15, classical_latin_standard__continuity_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(clscr_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(clscr_be_t3, classical_latin_standard__continuity_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(clscr_be_t6, classical_latin_standard__continuity_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(clscr_be_t9, classical_latin_standard__continuity_reading, base_extractiveness, 9, 0.33).
narrative_ontology:measurement(clscr_be_t12, classical_latin_standard__continuity_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(clscr_be_t15, classical_latin_standard__continuity_reading, base_extractiveness, 15, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(clscr_su_t0, classical_latin_standard__continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(clscr_su_t3, classical_latin_standard__continuity_reading, suppression_requirement, 3, 0.12).
narrative_ontology:measurement(clscr_su_t6, classical_latin_standard__continuity_reading, suppression_requirement, 6, 0.13).
narrative_ontology:measurement(clscr_su_t9, classical_latin_standard__continuity_reading, suppression_requirement, 9, 0.14).
narrative_ontology:measurement(clscr_su_t12, classical_latin_standard__continuity_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement(clscr_su_t15, classical_latin_standard__continuity_reading, suppression_requirement, 15, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'classical Latin standard' kernel into three readings per ε-invariance: continuity (this file, low extraction, Mountain claim), reconstruction (high extraction, Snare/Tangled Rope claim), hybrid (moderate extraction, Tangled Rope claim). The continuity reading is upstream — its claim of natural continuity is often cited as the 'traditional' position that hybrid and reconstruction readings react against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, institutional, 0.1).
constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, organized, 0.25).
constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, moderate, 0.5).
constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, powerless, 0.85).
constraint_indexing:directionality_override(classical_latin_standard__continuity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
