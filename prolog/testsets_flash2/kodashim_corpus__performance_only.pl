% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus: Performance-Only Reading
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the Kodashim
 *   corpus within Rabbinic Judaism, where the detailed laws of sacrifice are
 *   understood as an archived blueprint awaiting a future messianic
 *   restoration for physical performance. In this reading, current study of
 *   these laws is primarily a form of preparation, not a substitute for the
 *   actual rites. The constraint is classified as a Snare due to its high
 *   extractiveness: it extracts devotion and resources from adherents based
 *   on a promise of future, currently unrealizable, performance, benefiting
 *   institutions that maintain this eschatological expectation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.85).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.75).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '6b35e80d-54eb-404e-aa1b-ddb57a0664cf').
narrative_ontology:cs_kernel_codification('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', fixed_text).
narrative_ontology:cs_authority_grounding('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', lineage).
narrative_ontology:cs_interpretation_layer_present('6b35e80d-54eb-404e-aa1b-ddb57a0664cf').
narrative_ontology:cs_reading_relation('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', foundational, physical_sacrifice_is_ultimate_mitzvah).
narrative_ontology:cs_axiom_status(physical_sacrifice_is_ultimate_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', physical_sacrifice_is_ultimate_mitzvah, theological).
narrative_ontology:cs_axiom('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', foundational, present_study_is_preparation_not_performance).
narrative_ontology:cs_axiom_status(present_study_is_preparation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', present_study_is_preparation_not_performance, conventional).
narrative_ontology:cs_reference_frame('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', pre_temple_destruction_sacrificial_practice).
narrative_ontology:cs_drift_state('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', contemporary_diaspora_rabbinic_judaism, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6b35e80d-54eb-404e-aa1b-ddb57a0664cf', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_students).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions derive their legitimacy and funding from the belief that the Kodashim corpus is a blueprint for future, physical sacrificial performance. They actively promote this reading, organize study, and prepare for a messianic era when sacrifices will resume. Their existence is predicated on the non-performance of the actual rites in the present, while maintaining the expectation of future performance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Students who dedicate their lives to the intricate study of Kodashim, believing they are preparing for a future, physical performance. They invest immense intellectual and spiritual energy into a practice that, in this reading, cannot be realized in their lifetime. Their devotion is extracted to maintain the legitimacy of the messianic-preparation institutions.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_students, payer,
    moderate, biographical, identity_locked, local).

% General adherents who support the messianic vision and the institutions that promote it, often through financial contributions and communal participation. They bear the cost of misallocated devotion and resources, believing they are contributing to a future that remains perpetually out of reach.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_adherents, payer,
    powerless, biographical, identity_locked, local).

% Academics and theologians who analyze the historical and structural evolution of rabbinic law, often questioning the contemporary relevance or the eschatological framing of the Kodashim corpus. They observe the extraction of devotion and resources without being directly subject to it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, critical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective expectation and preparation for a future messianic era where physical sacrificial rites will be restored, providing a shared eschatological vision and a framework for present-day religious devotion.
% TRANSFER_FUNCTION: Transfers spiritual and intellectual devotion, as well as material resources, from devoted students and lay adherents to messianic-preparation institutions, in exchange for the promise of future redemption and the maintenance of a specific religious identity.
% ABSENT_VOICES: Those who would argue for a complete reinterpretation or abandonment of the Kodashim corpus as a living blueprint, seeing it as an anachronism or a historical artifact. Their voices are suppressed by the institutional authority and the identity-locked nature of the messianic narrative.
% DISAPPEARANCE_RATIONALE: If this reading of the Kodashim corpus vanished, the messianic-preparation institutions would lose their primary source of legitimacy and funding, leading to a significant reorganization of religious devotion and institutional structures within this segment of Rabbinic Judaism. Devotion would be reallocated to other forms of religious practice or social engagement.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of physical sacrifices created a crisis of religious practice and continuity, leaving a void in the central ritual life of Judaism.
% FOUNDING_PROBLEM_CORROBORATION: Messianic-preparation institutions attest that the problem is profoundly live, as the Temple has not been rebuilt and sacrifices have not resumed. Critical scholars, while acknowledging the historical problem, would argue that the 'solution' offered by this reading is itself a source of ongoing extraction, rather than a genuine resolution to the original crisis.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the devotion and resources invested by students and adherents yield no direct, present-day ritual performance, only the maintenance of a future-oriented expectation. Suppression is high due to the identity-locked nature of religious commitment and the institutional authority that reinforces this reading. The theater ratio is very high, as the 'performance' of studying these laws is almost entirely symbolic preparation for an event that has not occurred for millennia, rather than actual ritual execution. The metrics reflect a system where the primary function (physical sacrifice) is absent, but the preparatory activity is highly organized and extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the messianic-preparation institutions, this is a vital 'Rope' of coordination, maintaining a crucial aspect of religious identity and future hope. From the perspective of the devoted students, it is a 'Snare' that captures their life's work in a performative loop, extracting their devotion for an unfulfilled promise. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions are clear beneficiaries, as their legitimacy and funding depend on this reading. Devoted students and lay adherents are victims, as their spiritual and material investments are channeled into an unrealizable present-day practice. Critical scholars act as observers, analyzing the structural dynamics without being subject to the constraint's extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timing_ambiguity,
    'Is the messianic restoration of sacrifices an imminent, empirically contingent event, or a perpetually deferred theological concept?',
    'Empirical observation of geopolitical and religious developments, or a formal theological re-evaluation by authoritative rabbinic bodies.',
    'If empirically contingent and perpetually deferred, the extractiveness of current ''preparation'' would be re-evaluated as higher, as the promise becomes less credible. If imminent, the extractiveness might be seen as a legitimate investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timing_ambiguity, empirical, 'Ambiguity regarding the temporal proximity and empirical nature of messianic restoration.').

omega_variable(
    devotion_allocation_efficiency,
    'Is the allocation of significant intellectual and spiritual devotion to the study of Kodashim, in the absence of physical performance, an efficient use of religious energy for the individual and community?',
    'Sociological studies of religious communities, psychological assessments of adherent well-being, and comparative theological analysis of alternative devotional practices.',
    'If inefficient, the ''victim'' status of devoted students would be amplified, highlighting the misallocation of their life''s work. If efficient (e.g., for identity formation or intellectual rigor), the extractiveness might be seen as a necessary cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devotion_allocation_efficiency, preference, 'Efficiency of devotional resource allocation in a performance-only context.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the Kodashim corpus fundamentally a ''blueprint for future action'' (performance_only), a ''text for present spiritual exercise'' (study_as_exercise), or a ''historical record of superseded practice'' (substitution_archive)?',
    'A meta-analysis of rabbinic responsa across different historical periods and geographical regions, focusing on explicit statements regarding the purpose and contemporary application of Kodashim study.',
    'If the ''blueprint'' framing is found to be a minority or historically recent interpretation, the legitimacy of institutions built upon it would be undermined, potentially reclassifying the constraint as a more severe Snare or even a Piton. If ''study_as_exercise'' or ''substitution_archive'' are found to be dominant, this reading would be seen as a misinterpretation, and its extractiveness would be re-evaluated in light of the alternative, less extractive, framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The fundamental conceptual framing of the Kodashim corpus as a kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.8).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__performance_only, theater_ratio, 10, 0.83).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.86).
narrative_ontology:measurement(koda_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.88).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.89).
narrative_ontology:measurement(koda_tr_t50, kodashim_corpus__performance_only, theater_ratio, 50, 0.9).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__performance_only, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(koda_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(koda_be_t50, kodashim_corpus__performance_only, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(koda_su_t10, kodashim_corpus__performance_only, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(koda_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(koda_su_t50, kodashim_corpus__performance_only, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. This 'performance_only' reading emphasizes future physical sacrifice, contrasting with 'study_as_exercise' (present spiritual performance) and 'substitution_archive' (historical record).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
