% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic-Archival Cultural Memory (Non-Halakhic Reading)
 *   domain: religious_law/cultural_continuity/commitment_system
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the sacrifice_obligation_kernel:
 *   the symbolic-archive reading, under which study of the sacrifice corpus
 *   (Kodashim) is undertaken as cultural-historical preservation, with no
 *   claim that the underlying obligation is being fulfilled,
 *   suspended-but-maintained, or merely prepared for. Sibling readings —
 *   study-as-exercise, performance-only, and messianic-suspension — are
 *   separate constraints with their own ε values, beneficiary/victim
 *   structures, and classifications; they are not described here except as
 *   named siblings for network linkage and omega documentation. Under this
 *   reading there is no binding norm to violate, so extraction and
 *   suppression are near-zero; the constraint functions as a genuine,
 *   low-overhead coordination mechanism for communal memory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.03).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic-Archival Cultural Memory (Non-Halakhic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/cultural_continuity/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '750ee42e-e9e3-4de7-ab06-347712b55ea6').
narrative_ontology:cs_kernel_codification('750ee42e-e9e3-4de7-ab06-347712b55ea6', fixed_text).
narrative_ontology:cs_authority_grounding('750ee42e-e9e3-4de7-ab06-347712b55ea6', practice).
narrative_ontology:cs_interpretation_layer_present('750ee42e-e9e3-4de7-ab06-347712b55ea6').
narrative_ontology:cs_reading_relation('750ee42e-e9e3-4de7-ab06-347712b55ea6', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('750ee42e-e9e3-4de7-ab06-347712b55ea6', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('750ee42e-e9e3-4de7-ab06-347712b55ea6', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('750ee42e-e9e3-4de7-ab06-347712b55ea6', foundational, study_carries_no_halakhic_obligation_claim).
narrative_ontology:cs_axiom_status(study_carries_no_halakhic_obligation_claim, holdable).
narrative_ontology:cs_axiom_grounding('750ee42e-e9e3-4de7-ab06-347712b55ea6', study_carries_no_halakhic_obligation_claim, conventional).
narrative_ontology:cs_axiom('750ee42e-e9e3-4de7-ab06-347712b55ea6', secondary, textual_preservation_is_sufficient_communal_purpose).
narrative_ontology:cs_axiom_status(textual_preservation_is_sufficient_communal_purpose, holdable).
narrative_ontology:cs_axiom_grounding('750ee42e-e9e3-4de7-ab06-347712b55ea6', textual_preservation_is_sufficient_communal_purpose, instrumental).
narrative_ontology:cs_reference_frame('750ee42e-e9e3-4de7-ab06-347712b55ea6', post_temple_textual_continuity_practice).
narrative_ontology:cs_drift_state('750ee42e-e9e3-4de7-ab06-347712b55ea6', contemporary_diaspora_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('750ee42e-e9e3-4de7-ab06-347712b55ea6', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, diaspora_communal_identity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, secular_and_liberal_jewish_scholars).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, textual_continuity_without_coercive_obligation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_preservation_independent_of_ritual_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities scattered across the diaspora use study of the sacrifice texts (Mishnah Zevachim, Kodashim more broadly) in study cycles, day-school curricula, and adult education as a way of maintaining continuity with the textual tradition without any claim that a mitzvah is being fulfilled or a debt discharged. The practice costs time and attention voluntarily given; no one is compelled to attend, and dropping the practice carries no claimed halakhic consequence.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, diaspora_communal_identity, beneficiary,
    organized, generational, mobile, global).

% Scholars in academic and non-Orthodox settings engage the sacrifice corpus as literature, history, and identity-formation material. They benefit from a framing that lets them study the texts seriously without adopting or being bound by a restoration theology or an active-mitzvah claim they may not hold. Their exit from the practice is frictionless — it is a matter of scholarly interest, not obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, secular_and_liberal_jewish_scholars, beneficiary,
    moderate, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, secular_and_liberal_jewish_scholars, observer).

% Communities and individuals who hold that sacrifice study either constitutes active mitzvah-performance or maintains readiness for a divinely-mandated restoration are not represented within this reading's framework; from inside the archive-reading, their claims are simply a different, non-adjudicated position rather than an error to be corrected. They would object that the archive framing drains the texts of religious force, but this constraint does not attempt to answer them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, restorationist_communities, excluded,
    organized, generational, identity_locked, global).

% Observe how a legal corpus can be retained as cultural patrimony after (or independent of) any claim to its binding force, drawing comparisons to other traditions that preserve obsolete legal or ritual texts as identity markers. They study the pattern rather than adjudicate among the competing internal readings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, comparative_religion_and_textual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared textual and historical reference point that lets a religiously and ideologically diverse Jewish population continue studying, teaching, and transmitting the sacrifice corpus without requiring agreement on its binding status — coordination around continuity of memory rather than around a shared obligation.
% TRANSFER_FUNCTION: Moves attention, curricular time, and scholarly labor toward preserving the sacrifice texts; moves nothing coercively from anyone, since no payment, compliance, or submission is extracted — participation is opt-in and its absence carries no claimed sanction under this reading.
% ABSENT_VOICES: Restorationist and messianic-suspension communities who hold the texts carry live obligatory or readiness force are not internal parties to this reading; they would object that treating the corpus as mere archive dissolves its religious stakes, but the archive reading does not depend on their agreement and does not attempt to bind them either.
% DISAPPEARANCE_RATIONALE: If this specific reading (sacrifice-study-as-symbolic-archive) vanished overnight, no material arrangement collapses: no one is bound by it, no institution enforces it, and no resource flow depends on it. Study of the texts would simply be re-described under a different reading (study-as-exercise, performance-only, or messianic-suspension) by whichever community holds that view; the texts themselves and communal study practices would persist, just under a different internal justification.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial system could no longer be physically enacted; communities faced the question of what to do with a large, detailed legal corpus (Kodashim) whose operative referent no longer existed — study of Kodashim historically served, among other things, as a way of keeping the corpus alive as text even where its practical applicability was suspended.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic literature and non-Orthodox denominational bodies (outside any community that holds a restorationist or performance-only view) attest that treating Kodashim study as cultural-historical preservation rather than active mitzvah-fulfillment or readiness-maintenance is itself a documented, centuries-old reading strategy, not a novel invention; restorationist authorities dispute that this exhausts the corpus's status, but that dispute is a sibling reading's claim, not evidence against this reading's coherence.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.02 (near-floor, not exactly zero, to reflect the small opportunity cost of curricular time voluntarily allocated) because no coercive transfer exists under this reading's own premises. Suppression is likewise near-zero (0.03): nothing prevents an individual from declining to study the sacrifice corpus, and no sanction is claimed for non-participation. Theater ratio is low (0.10) because the study activity is not performing a function it lacks — it openly claims only memory-preservation, not obligation-fulfillment, so there is no gap between claimed and actual function to inflate a theater measure. Accessibility collapse is low (0.08): alternative framings (the sibling readings) remain fully available and are held by other communities; this reading does not foreclose access to them. Resistance is low (0.05): because the reading makes no binding claim, it draws little active opposition on its own terms — opposition is directed at rival readings' stronger claims, not at this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (diaspora communal identity, secular and liberal scholars) sit near the full-beneficiary end: they receive a durable connection to tradition and scholarly material at negligible cost, and their exit options are mobile-to-arbitrage — they can disengage from the practice at will without consequence under this reading's own terms. There is no victim group because no one is coerced or extracted from; the restorationist communities who are excluded from this reading's internal frame are not victims of it, since this reading does not purport to bind them at all — it simply does not speak to their claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what to do with a legal corpus whose material referent, the Temple, no longer exists) is genuinely dead under this reading's own premises — there is no live obligation this reading claims to service. That the founding problem is 'dead' here does not indicate mandatrophy, because this reading does not claim to still be discharging the original obligation; it explicitly reframes the activity as memory-preservation rather than obligation-fulfillment. Mandatrophy would arise only if this reading secretly retained coercive or extractive machinery while claiming pure archival status — it does not, per the authored near-zero extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_dormant_obligation_boundary,
    'Is the symbolic-archive reading a genuinely distinct halakhic position, or is it a modern reframing that quietly abandons the messianic-suspension reading''s claim that the obligation remains dormant-but-real?',
    'Comparative analysis of denominational responsa and communal self-description: does the community holding this reading explicitly deny a live obligation (supporting archive-reading distinctness) or merely deprioritize it while retaining latent obligation-language (which would collapse it toward messianic_suspension_reading)?',
    'If the archive reading is shown to covertly retain dormant-obligation language, it is not a separate constraint but a disguised instance of messianic_suspension_reading, and the near-zero extraction authored here would be a measurement error rather than a structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_dormant_obligation_boundary, conceptual, 'Whether the archive reading is genuinely distinct from, or a euphemistic version of, the dormant-obligation reading.').

omega_variable(
    corroboration_source_independence,
    'Does corroboration for this reading''s founding-problem account come from sources genuinely independent of the reading''s own beneficiaries (secular/liberal Jewish institutions), or is the corroborating scholarship itself produced within communities that already hold this reading?',
    'Trace citation lineage of the historical claim that Kodashim study functioned as archival preservation across denominational lines, including Orthodox historiography that might independently corroborate the historical pattern without endorsing the archive reading''s normative conclusion.',
    'If corroboration is entirely internal to the beneficiary communities, the founding-problem account is weaker evidence than claimed and the R5 corroboration field should be revised toward ''no outside corroboration exists.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_source_independence, empirical, 'Whether outside corroboration of the founding-problem narrative is genuinely independent of this reading''s beneficiaries.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the sacrifice_obligation_kernel best modeled as a single ambiguous kernel with four readings, or does the underlying textual/legal tradition actually contain two nested kernels — one about the ontological status of the obligation (suspended vs. archived vs. active) and one about what counts as its fulfillment (study vs. performance)?',
    'Decompose the kernel further: test whether study_as_exercise_reading and performance_only_reading disagree only on fulfillment-mode while both presupposing an active/suspended obligation, whereas symbolic_archive_reading disagrees on a prior axis (whether any obligation-claim survives at all). If so, the four-reading flat structure understates the kernel''s actual layered structure.',
    'If two nested kernels are the more accurate model, this story''s cs_structure.reading_relations should be revised to reflect a two-tier dependency rather than four coequal siblings, and network linkage should be restructured accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the four declared readings reflect one kernel or two nested kernels (ontological status vs. fulfillment mode).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 40, 0.02).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.02).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 80, 0.02).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 100, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the sacrifice_obligation_kernel, decomposed per the ε-invariance principle: each reading has a structurally distinct ε and beneficiary/victim profile and must not be collapsed into a single averaged constraint. symbolic_archive_reading carries near-zero ε and no victim set (pure voluntary coordination); study_as_exercise_reading and performance_only_reading both presuppose a live obligation and will carry higher suppression/extraction profiles reflecting internal communal enforcement of correct practice; messianic_suspension_reading carries a dormant-but-real obligation with its own distinct enforcement and identity-lock dynamics. All four are linked bidirectionally via affects_constraints to preserve the family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
