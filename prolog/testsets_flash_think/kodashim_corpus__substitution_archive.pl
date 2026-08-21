% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the rabbinic understanding that prayer and
 *   Torah study have effectively replaced the sacrificial cult, and that the
 *   Kodashim order of the Mishnah (which details sacrificial laws) functions
 *   primarily as a memorial archive of superseded practices, rather than an
 *   active blueprint for current ritual. This reading claims continuity with
 *   the past while actively denying the immediate restoration of physical
 *   sacrifice, thereby centralizing rabbinic authority around text-based
 *   observance. The constraint is classified as a Tangled Rope because it
 *   provides a genuine coordination function (maintaining Jewish practice
 *   post-Temple) but also involves asymmetric extraction (from those who
 *   desire immediate sacrificial restoration) through active enforcement of
 *   its interpretive framework.
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: Agenda-setter/Beneficiary (institutional/arbitrage)
 *   - adherents_seeking_sacrificial_practice: Payer/Victim (powerless/identity_locked)
 *   - mainstream_adherents: Beneficiary (moderate/constrained)
 *   - messianic_restorationists: Excluded (powerless/identity_locked)
 *   - analytical_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.6).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.7).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'a9efe679-3dcd-492f-8a0b-0a77489a0898').
narrative_ontology:cs_kernel_codification('a9efe679-3dcd-492f-8a0b-0a77489a0898', fixed_text).
narrative_ontology:cs_authority_grounding('a9efe679-3dcd-492f-8a0b-0a77489a0898', lineage).
narrative_ontology:cs_interpretation_layer_present('a9efe679-3dcd-492f-8a0b-0a77489a0898').
narrative_ontology:cs_reading_relation('a9efe679-3dcd-492f-8a0b-0a77489a0898', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('a9efe679-3dcd-492f-8a0b-0a77489a0898', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('a9efe679-3dcd-492f-8a0b-0a77489a0898', foundational, prayer_and_study_replace_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_replace_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('a9efe679-3dcd-492f-8a0b-0a77489a0898', prayer_and_study_replace_sacrifice, conventional).
narrative_ontology:cs_axiom('a9efe679-3dcd-492f-8a0b-0a77489a0898', foundational, kodashim_as_memorial_archive).
narrative_ontology:cs_axiom_status(kodashim_as_memorial_archive, holdable).
narrative_ontology:cs_axiom_grounding('a9efe679-3dcd-492f-8a0b-0a77489a0898', kodashim_as_memorial_archive, conventional).
narrative_ontology:cs_reference_frame('a9efe679-3dcd-492f-8a0b-0a77489a0898', post_temple_rabbinic_synthesis).
narrative_ontology:cs_drift_state('a9efe679-3dcd-492f-8a0b-0a77489a0898', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a9efe679-3dcd-492f-8a0b-0a77489a0898', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, mainstream_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define and transmit the understanding that prayer and Torah study have replaced sacrificial worship, and that the Kodashim order of the Mishnah serves as a memorial archive of superseded practices. They benefit from the centralization of religious authority around text study and prayer.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% These adherents feel a spiritual longing for the restoration of physical sacrificial worship, but are told by the dominant rabbinic framework that such practices are currently obsolete and that prayer/study are the appropriate substitutes. They bear the cost of having their desired form of worship deemed invalid or deferred.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice, payer,
    powerless, biographical, identity_locked, local).

% These adherents benefit from a clear, accessible, and coherent framework for religious observance in the absence of the Temple. They accept prayer and study as the primary modes of worship, finding spiritual fulfillment within the established rabbinic tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, mainstream_adherents, beneficiary,
    moderate, biographical, constrained, global).

% This group actively anticipates and advocates for the immediate re-establishment of sacrificial worship, often viewing the rabbinic substitution as a temporary measure or even a deviation. They are largely marginalized or excluded from mainstream rabbinic discourse regarding current practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restorationists, excluded,
    powerless, generational, identity_locked, local).

% These scholars study the historical development of Jewish law and theology, analyzing the shifts from sacrificial to rabbinic modes of worship. They observe the structural implications of the 'substitution archive' reading without being bound by its normative claims.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, analytical_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible framework for Jewish religious practice and communal identity in the absence of the Temple, ensuring continuity of tradition through prayer and study as substitutes for sacrifice.
% TRANSFER_FUNCTION: Transfers the locus of religious authority and primary modes of worship from a priestly, Temple-based sacrificial system to a rabbinic, text-based system centered on prayer and Torah study.
% ABSENT_VOICES: Messianic restorationists and those who prioritize the immediate re-establishment of physical sacrificial practice are largely excluded from the normative discourse, as their views challenge the foundational premise of the substitution.
% DISAPPEARANCE_RATIONALE: If the understanding of Kodashim as a 'substitution archive' vanished, the entire edifice of post-Temple rabbinic Judaism, which defines religious life through prayer and study as the primary modes of worship, would be fundamentally destabilized. The current structure of Jewish religious practice would collapse, necessitating a radical re-evaluation of tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE and the subsequent cessation of sacrificial worship, which left a profound void in Jewish religious practice and threatened the continuity of the covenant.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the Temple's destruction, extensive rabbinic literature from the Mishnaic and Talmudic periods documenting the transition, and the ongoing reality of the absence of a rebuilt Temple all corroborate the founding problem's persistence. Independent historical and archaeological research also supports this context.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because while the framework provides essential continuity, it also extracts from those whose spiritual needs are tied to physical sacrifice by declaring it obsolete. Suppression is high (0.70) due to the active enforcement of the rabbinic interpretive tradition, which marginalizes or delegitimizes alternative views on sacrificial practice. The accessibility collapse (0.80) reflects the historical reality of the Temple's destruction and the subsequent rabbinic decrees that effectively closed off the alternative of physical sacrifice. Resistance (0.30) is low within mainstream Judaism, but present from fringe groups. Theater ratio is moderate (0.40) as the study of Kodashim maintains a performative connection to the past, but its primary function is archival and interpretive, not directly functional for current ritual.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic institutions, this constraint is a necessary and beneficial adaptation that ensured the survival of Judaism. From the perspective of adherents seeking sacrificial practice, it is an extractive reinterpretation that denies their spiritual longing and marginalizes their desired form of worship. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions are clear beneficiaries, as the constraint solidifies their authority and the centrality of their interpretive tradition. Adherents seeking sacrificial practice are victims, as their desired religious expression is suppressed. Mainstream adherents are beneficiaries of a stable religious framework. Messianic restorationists are excluded, as their views directly challenge the constraint's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from those desiring sacrifice) or a Snare (ignoring the genuine coordination function of maintaining Judaism post-Temple). The founding problem (destruction of the Temple) is still 'live', but the solution (substitution via prayer/study) has become a source of extraction for those who contest its permanence or completeness. The constraint's mandate has evolved from pure adaptation to also include the maintenance of a specific interpretive authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_restoration_ambiguity,
    'Is the rabbinic understanding of prayer and study as ''substitutes'' for sacrifice a permanent theological shift or a temporary measure awaiting messianic restoration?',
    'Theological consensus shift within major rabbinic movements, or the actual re-establishment of sacrificial practice (e.g., rebuilding of the Temple).',
    'If temporary, the extractiveness from adherents seeking sacrifice would be re-evaluated as a deferral rather than a denial, potentially lowering the effective extraction. If permanent, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_restoration_ambiguity, conceptual, 'Ambiguity regarding the permanence of the substitution of prayer/study for sacrifice.').

omega_variable(
    authority_maintenance_vs_spiritual_need,
    'To what extent does the ''substitution archive'' reading primarily serve to maintain rabbinic institutional authority versus genuinely addressing the spiritual needs of adherents?',
    'Sociological and theological analysis of internal rabbinic debates, and ethnographic studies of adherent experiences across different interpretive communities.',
    'If primarily authority maintenance, the extractiveness and suppression metrics would be seen as more intentional and less a byproduct of coordination. If primarily spiritual need, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_maintenance_vs_spiritual_need, empirical, 'The balance between institutional authority maintenance and spiritual fulfillment in the ''substitution archive'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__substitution_archive, theater_ratio, 400, 0.4).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__substitution_archive, theater_ratio, 800, 0.4).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__substitution_archive, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__substitution_archive, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__substitution_archive, base_extractiveness, 400, 0.53).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__substitution_archive, base_extractiveness, 800, 0.56).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__substitution_archive, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__substitution_archive, base_extractiveness, 1600, 0.59).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__substitution_archive, suppression_requirement, 400, 0.63).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__substitution_archive, suppression_requirement, 800, 0.66).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__substitution_archive, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(koda_su_t1600, kodashim_corpus__substitution_archive, suppression_requirement, 1600, 0.69).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. This 'substitution_archive' reading emphasizes the replacement of sacrifice by prayer/study and the archival nature of Kodashim, differing from 'performance_only' (husk awaiting restoration) and 'study_as_exercise' (study as mitzvah itself).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
