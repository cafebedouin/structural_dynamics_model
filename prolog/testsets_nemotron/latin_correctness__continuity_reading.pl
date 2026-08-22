% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Continuation of Classical Latin
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This constraint story represents the CONTINUITY READING of the contested
 *   kernel 'latin_correctness' — the view that medieval Latin is the
 *   legitimate, organic continuation of classical Latin. The kernel itself is
 *   the question: what counts as 'correct' Latin after antiquity? Three
 *   readings instantiate different constraints: continuity (this story),
 *   hybrid (classical norms for literary/rhetorical domains, medieval forms
 *   for technical/practical domains), and rupture (classical Latin is a fixed
 *   standard requiring reconstruction; medieval usage is corruption). This
 *   reading treats linguistic change as natural law — phonological shifts,
 *   vocabulary expansion, and syntactic adaptation are not deviations but
 *   evolution. The low extractiveness (0.12) reflects that medieval users
 *   were not exploited by the continuity claim; they were its authors and
 *   beneficiaries. The moderate theater_ratio (0.18) captures the growing
 *   performative classicism of the 12th–15th centuries (Cicero-imitation,
 *   artificial archaism) that coexisted with genuine continuity. Suppression
 *   is near-zero until the humanist period (post-1300), when the rupture
 *   reading begins actively marginalizing medieval forms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.12).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.08).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, mountain).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Legitimate Continuation of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:emerges_naturally(latin_correctness__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '5138920e-533f-4b41-bf09-d3cb76087d8e').
narrative_ontology:cs_kernel_codification('5138920e-533f-4b41-bf09-d3cb76087d8e', distributed).
narrative_ontology:cs_authority_grounding('5138920e-533f-4b41-bf09-d3cb76087d8e', practice).
narrative_ontology:cs_interpretation_layer_present('5138920e-533f-4b41-bf09-d3cb76087d8e').
narrative_ontology:cs_reading_relation('5138920e-533f-4b41-bf09-d3cb76087d8e', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_reading_relation('5138920e-533f-4b41-bf09-d3cb76087d8e', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('5138920e-533f-4b41-bf09-d3cb76087d8e', foundational, linguistic_change_is_natural_law).
narrative_ontology:cs_axiom_status(linguistic_change_is_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('5138920e-533f-4b41-bf09-d3cb76087d8e', linguistic_change_is_natural_law, empirically_contingent).
narrative_ontology:cs_axiom('5138920e-533f-4b41-bf09-d3cb76087d8e', foundational, living_usage_legitimates_written_standard).
narrative_ontology:cs_axiom_status(living_usage_legitimates_written_standard, holdable).
narrative_ontology:cs_axiom_grounding('5138920e-533f-4b41-bf09-d3cb76087d8e', living_usage_legitimates_written_standard, conventional).
narrative_ontology:cs_reference_frame('5138920e-533f-4b41-bf09-d3cb76087d8e', organic_continuity_from_antiquity).
narrative_ontology:cs_drift_state('5138920e-533f-4b41-bf09-d3cb76087d8e', humanist_rupture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5138920e-533f-4b41-bf09-d3cb76087d8e', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_latin_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(latin_correctness__continuity_reading, classical_purists).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, linguistic_continuity_thesis).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, organic_language_change_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clerics, administrators, scholars, and technical writers across medieval Europe who used Latin as their working language. They inherited a living tradition, adapted vocabulary and phonology to new domains (theology, law, science, administration), and faced no structural barrier to claiming legitimacy — their usage was the norm, not an exception. Exit means switching to vernacular writing, which many did over centuries without penalty to their Latin practice.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_latin_users, beneficiary,
    organized, generational, arbitrage, continental).

% Humanist scholars (14th–16th c.) who insisted on classical norms as the only legitimate Latin. They bore the cost of policing usage, editing texts to classical standards, and marginalizing medieval forms — but their position was a self-imposed disciplinary choice, not a structural imposition by the continuity tradition. Their 'payment' is the labor of purification; their exit is adopting the continuity view, which many eventually did.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classical_purists, payer,
    moderate, biographical, constrained, continental).

% Writers composing in Old French, Middle High German, Old Spanish, etc. The continuity reading does not exclude them — Latin and vernacular coexisted for centuries — but the kernel's legitimacy contest (which Latin is 'correct') rendered vernacular voices invisible in the philological frame until modern linguistics recentered them. They would object to the assumption that Latin correctness is the only linguistic legitimacy at stake.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_writers, excluded,
    moderate, biographical, mobile, regional).

% Scholars who study the full Latin tradition without adopting a correctness stance. They see continuity, rupture, and hybridity as analytic categories, not normative commitments. Their exit is methodological pluralism — they hold no seat in the correctness dispute.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, modern_philologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable written medium across medieval Europe for theology, law, administration, and science — enabling cross-regional communication without requiring spoken uniformity. The continuity of textual practice coordinates intellectual and institutional life across linguistic borders.
% TRANSFER_FUNCTION: Moves authority to define 'correct' Latin from ancient textual artifacts (which are fragmentary and unrepresentative of spoken practice) to the living community of users. The arrangement transfers legitimating power from a reconstructed past to a continuous present.
% ABSENT_VOICES: Vernacular-language communities whose literary traditions developed in dialogue with Latin but were structurally excluded from the 'correctness' debate. Also excluded: non-elite medieval Latin users (scribes, notaries, merchants) whose daily usage shaped the language but had no voice in normative philology.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, the legitimacy of medieval Latin texts — canon law, scholastic theology, scientific treatises, administrative records — would become contested. Modern editions, translations, and historical interpretations would shift toward a rupture framework, reclassifying medieval usage as 'corrupt' rather than 'evolved.' The institutional memory of the Church, universities, and European legal systems would lose its self-understanding as continuous with antiquity.
% FOUNDING_PROBLEM: How to maintain a transnational written standard for intellectual and institutional life after the spoken Latin of antiquity fragmented into Romance vernaculars, without freezing the language into a museum piece or surrendering to vernacular fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the historical fact that medieval institutions (Church, empire, universities) treated their Latin as continuous with antiquity — not as a revival or reconstruction. The founding problem is attested by the users themselves (medieval prefaces, curricula, chancery manuals), not only by modern apologists. No external beneficiary of the continuity claim disputes that the problem (transnational coordination via a stable written medium) was real and persistent.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(latin_correctness__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(latin_correctness__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(latin_correctness__continuity_reading),
    narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed as mountain because linguistic continuity is a structural feature of language change — no authority enforces it, no party extracts from it, and alternatives (vernacular writing, classical reconstruction) remain available throughout. The metrics describe a constraint that is overwhelmingly coordinative: extractiveness stays low (peaking at 0.15 during high humanist pressure), suppression is minimal until external challengers impose correctness standards, and theater rises only when performative archaism becomes a status signal among elites. The claim/metric independence is deliberate: the continuity reading CLAIMS mountain status; the metrics confirm it descriptively. No reconciliation needed.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading experiences itself as mountain (natural law); the rupture reading experiences the SAME historical phenomenon as snare (corruption enforced by ignorance). The hybrid reading experiences it as tangled_rope (coordination in some domains, extraction in others). This seat divergence is the point of the kernel: the same linguistic history instantiates different constraints depending on which reading's normative frame you occupy. The engine computes this from the structural data — we author the structure, not the verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval Latin users are structural beneficiaries (d ≈ 0.1): they inherit a living tradition, adapt it freely, and face no coercion to conform to ancient norms. Classical purists are payers (d ≈ 0.6) but by choice — they impose classical standards on themselves and others as a scholarly discipline, not because the continuity constraint forces them. Vernacular writers are excluded from the Latin correctness frame entirely (d irrelevant). Modern philologists are analytical observers (d = 0.5). The engine will compute per-seat classifications from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transnational written coordination) remains live — the Catholic Church still uses Latin as its official language, canon law is still promulgated in Latin, and the Vatican's diplomatic corps operates in Latin. The arrangement has not atrophied; it has narrowed in scope but deepened in institutional embedding. No mandatrophy: the constraint's function persists where its domain persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_boundary,
    'Is the continuity reading a genuine description of linguistic history, or a retrospective legitimization of medieval practice by modern scholars who benefit from a unified Latin tradition?',
    'Comparative analysis of medieval metalinguistic discourse: did medieval users themselves claim continuity with classical antiquity, or did they acknowledge a break? Corroboration from pre-humanist medieval sources (not Renaissance reconstructions).',
    'If medieval users saw themselves as speaking a different language (not ''Latin continued''), the continuity reading is a modern projection — a false summit mountain. If they claimed continuity, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_rupture_boundary, empirical, 'Whether the continuity claim reflects medieval self-understanding or modern scholarly projection.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''latin_correctness'' name a single contested question, or does it conflate distinct structural problems: (a) what Latin should be taught/used now, (b) how medieval texts should be edited, (c) whether Latin change follows natural laws?',
    'Disaggregate the kernel into sub-kernels by analyzing which readings answer which question. If readings align differently across sub-questions, the kernel is a conflation.',
    'If the kernel conflates multiple questions, the three readings are not mutually exclusive positions on one issue — they are answers to different questions. The constraint family decomposition would need restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a coherent contest or a category error.').

omega_variable(
    humanist_extraction_mechanism,
    'When humanist scholars (14th–16th c.) imposed classical norms, did they extract resources/status from medieval Latin users, or did they reform a shared tradition from within?',
    'Analyze patronage networks, academic appointments, and publication privileges: did humanist Latin become a gatekeeping credential that excluded non-humanist scholars from positions and resources?',
    'If humanist norms functioned as an exclusionary credential, the rupture reading''s extractiveness is higher than the continuity reading''s — and the hybrid reading''s domain partition may reflect a negotiated settlement. This would make the kernel contest partly about resource allocation, not just linguistic theory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_extraction_mechanism, empirical, 'Whether the rupture reading''s rise involved extraction from medieval Latin users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 500, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t500, latin_correctness__continuity_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t800, latin_correctness__continuity_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t1100, latin_correctness__continuity_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t1300, latin_correctness__continuity_reading, theater_ratio, 1300, 0.18).
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t1500, latin_correctness__continuity_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(latin_correctness__continuity_reading_tr_t1600, latin_correctness__continuity_reading, theater_ratio, 1600, 0.18).

% Extraction over time
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t500, latin_correctness__continuity_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t800, latin_correctness__continuity_reading, base_extractiveness, 800, 0.08).
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t1100, latin_correctness__continuity_reading, base_extractiveness, 1100, 0.1).
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t1300, latin_correctness__continuity_reading, base_extractiveness, 1300, 0.12).
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t1500, latin_correctness__continuity_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(latin_correctness__continuity_reading_be_t1600, latin_correctness__continuity_reading, base_extractiveness, 1600, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t500, latin_correctness__continuity_reading, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t800, latin_correctness__continuity_reading, suppression_requirement, 800, 0.05).
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t1100, latin_correctness__continuity_reading, suppression_requirement, 1100, 0.08).
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t1300, latin_correctness__continuity_reading, suppression_requirement, 1300, 0.12).
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t1500, latin_correctness__continuity_reading, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement(latin_correctness__continuity_reading_su_t1600, latin_correctness__continuity_reading, suppression_requirement, 1600, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three constraint stories: continuity_reading (this file, mountain, ε≈0.12), hybrid_reading (tangled_rope, ε≈0.35, domain-partitioned), and rupture_reading (snare, ε≈0.65, classical reconstruction as exclusionary standard). The continuity reading is the upstream claim — it describes the linguistic history that the other readings respond to. The hybrid reading partitions the continuity reading's domain; the rupture reading rejects its natural-law premise. All three share the referent (Latin usage 500–1600) but differ in ε, beneficiaries/victims, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
