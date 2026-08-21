% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine
 *   domain: religious_studies/theology/textual_criticism
 *
 * SUMMARY:
 *   This constraint represents the 'exclusive inspiration' reading of the
 *   King James Version (KJV) of the Bible, which asserts that the KJV is the
 *   only truly inspired and inerrant English translation, rendering all
 *   others as corrupted or inferior. This reading creates a snare for those
 *   who seek biblical understanding through modern, more accessible
 *   translations, while benefiting a specific leadership and publishing
 *   ecosystem. The constraint's persistence relies heavily on active
 *   enforcement and suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.85).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.92).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious_studies/theology/textual_criticism").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'dab941e9-0237-45b5-a63a-425b9e2442a3').
narrative_ontology:cs_kernel_codification('dab941e9-0237-45b5-a63a-425b9e2442a3', fixed_text).
narrative_ontology:cs_authority_grounding('dab941e9-0237-45b5-a63a-425b9e2442a3', lineage).
narrative_ontology:cs_interpretation_layer_present('dab941e9-0237-45b5-a63a-425b9e2442a3').
narrative_ontology:cs_reading_relation('dab941e9-0237-45b5-a63a-425b9e2442a3', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('dab941e9-0237-45b5-a63a-425b9e2442a3', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('dab941e9-0237-45b5-a63a-425b9e2442a3', foundational, kjv_sole_inspired_english_text).
narrative_ontology:cs_axiom_status(kjv_sole_inspired_english_text, holdable).
narrative_ontology:cs_axiom_grounding('dab941e9-0237-45b5-a63a-425b9e2442a3', kjv_sole_inspired_english_text, theological).
narrative_ontology:cs_axiom('dab941e9-0237-45b5-a63a-425b9e2442a3', secondary, modern_translations_corrupted).
narrative_ontology:cs_axiom_status(modern_translations_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('dab941e9-0237-45b5-a63a-425b9e2442a3', modern_translations_corrupted, theological).
narrative_ontology:cs_reference_frame('dab941e9-0237-45b5-a63a-425b9e2442a3', providentially_preserved_text).
narrative_ontology:cs_drift_state('dab941e9-0237-45b5-a63a-425b9e2442a3', contemporary_linguistic_scholarship, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('dab941e9-0237-45b5-a63a-425b9e2442a3', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_bible_translators).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, academic_textual_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes and enforces the doctrine of KJV's exclusive inspiration, positioning themselves as the sole arbiters of 'true' scripture. They gain authority and control over congregants' access to biblical interpretation. Exit means abandoning their theological and institutional foundation.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the exclusive market for KJV Bibles and related study materials within KJV-Only churches. Their business model is directly tied to the constraint's persistence. Exit means losing their primary market.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers, beneficiary,
    organized, biographical, constrained, national).

% Their work is actively denounced as corrupted or inferior, leading to loss of credibility and market share within KJV-Only circles. They bear the cost of intellectual and professional marginalization. Exit means ceasing their work or abandoning their academic/theological principles.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_bible_translators, payer,
    moderate, biographical, constrained, global).

% Are compelled to use an archaic translation, often struggling with its language, and are forbidden from consulting modern versions for clarity, leading to reduced comprehension and spiritual growth. Their identity is often fused with their church community. Exit means leaving their faith community.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity, payer,
    powerless, immediate, identity_locked, local).

% Their scholarly work on ancient manuscripts and linguistic developments, which often informs modern translations, is dismissed as irrelevant or heretical. They bear the cost of having their expertise rejected by a significant segment of the religious public. Exit means abandoning their academic discipline or accepting the KJV-Only premise.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, academic_textual_critics, payer,
    analytical, generational, analytical, global).

% While not directly targeted, their use of modern translations is implicitly or explicitly condemned by KJV-Only adherents, creating division and undermining ecumenical efforts. They are excluded from the KJV-Only discourse as 'compromised'.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, mainstream_evangelical_churches, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unambiguous textual authority for a community, preventing theological disputes arising from translational differences and fostering a shared linguistic identity.
% TRANSFER_FUNCTION: Transfers interpretive authority and financial resources (via exclusive publishing) from diverse scholarly and publishing entities to the KJV-Only leadership and publishers, in exchange for perceived textual stability and doctrinal purity.
% ABSENT_VOICES: Scholars of biblical languages and textual criticism, as well as proponents of dynamic equivalence translation, are systematically excluded from the discourse, their expertise dismissed as undermining 'God's preserved word'. They would argue for the ongoing necessity of translation and textual scholarship.
% DISAPPEARANCE_RATIONALE: If the doctrine of KJV's exclusive inspiration vanished overnight, KJV-Only churches would face an immediate crisis of authority and identity. Congregants would seek out modern translations, publishers would lose their exclusive market, and the leadership's theological foundation would collapse, leading to a significant reorganization of their religious landscape.
% FOUNDING_PROBLEM: The proliferation of new English translations and textual scholarship was perceived as undermining biblical authority and creating confusion among believers, leading to a desire for a single, unassailable English text.
% FOUNDING_PROBLEM_CORROBORATION: KJV-Only adherents and leadership attest that the problem of 'corrupted' modern translations and theological confusion is still live. Mainstream textual critics and theologians, from outside the benefiting parties, corroborate the existence of ongoing debates about translation philosophy but dispute the KJV-Only solution as an overreaction that creates new problems.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the doctrine demands adherence to an archaic text, imposing cognitive and linguistic costs on congregants, and marginalizing the work of modern scholars and translators. Suppression is very high (0.92) due to the active denunciation of alternative translations, often accompanied by social and spiritual pressure within KJV-Only communities. Theater ratio is low (0.15) as the primary function is genuinely to enforce textual exclusivity, not merely to perform it; the 'defense of scripture' is a core, active mandate. The historical measurements show a rise in extractiveness and suppression as modern translations became more prevalent, necessitating greater enforcement of the KJV-Only position, with a slight recent dip as the movement faces demographic challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the KJV-Only leadership's perspective, this is a necessary defense of divine truth (a Mountain or Rope). From the perspective of modern translators and congregants, it is a highly extractive and suppressive snare that limits access to understanding and undermines scholarship. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership and publishers are clear beneficiaries, gaining authority and market share (d near 0.0). Modern translators, textual critics, and congregants seeking clarity are targets, bearing the costs of marginalization, reduced comprehension, and intellectual suppression (d near 1.0). Mainstream churches are excluded, their practices implicitly condemned.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_linguistic_authority,
    'Is the KJV-Only doctrine primarily a theological claim about divine preservation, or a linguistic claim about translational superiority?',
    'Analysis of KJV-Only apologetics: if arguments focus on divine intervention and providential preservation over linguistic accuracy or manuscript superiority, it''s theological. If it focuses on linguistic superiority, it''s a linguistic claim.',
    'If theological, the constraint is more resistant to empirical linguistic challenges. If linguistic, empirical evidence of superior modern translations would directly undermine its core premise, potentially reducing extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_linguistic_authority, conceptual, 'Distinguishing the grounding of the KJV-Only claim.').

omega_variable(
    identity_lock_strength,
    'How deeply is the identity of KJV-Only congregants fused with the KJV text and their community, and how much of their ''identity_locked'' exit option is due to this fusion versus structural barriers?',
    'Longitudinal studies of ex-KJV-Only congregants: tracking post-exit psychological and social costs, and the persistence of KJV-Only interpretive patterns after leaving the community.',
    'If identity fusion is the dominant mechanism, the effective suppression is higher and more internalized, making ''fixing'' the constraint more complex than merely removing external barriers. If structural barriers (e.g., social isolation) are primary, external interventions are more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the proportion of internalized vs. structural identity lock for congregants.').

omega_variable(
    mandatrophy_of_clarity_function,
    'Has the KJV''s original function of providing a clear, accessible English Bible for its time atrophied to the point where its continued exclusive use actively hinders comprehension for modern readers?',
    'Empirical studies on reading comprehension and retention among KJV-Only congregants compared to users of modern translations, controlling for education and literacy levels.',
    'If comprehension is significantly hindered, the constraint''s coordination function (providing access to scripture) has degraded into a pure extraction mechanism, strengthening the snare classification and highlighting a severe mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_clarity_function, empirical, 'Whether the KJV''s original clarity function has atrophied for modern readers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1900, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(kjv__tr_t1930, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(kjv__tr_t1960, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(kjv__tr_t1990, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1900, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(kjv__be_t1930, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1930, 0.7).
narrative_ontology:measurement(kjv__be_t1960, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1960, 0.8).
narrative_ontology:measurement(kjv__be_t1990, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1900, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(kjv__su_t1930, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1930, 0.8).
narrative_ontology:measurement(kjv__su_t1960, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(kjv__su_t1990, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(kjv__su_t2024, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, biblical_inerrancy_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
