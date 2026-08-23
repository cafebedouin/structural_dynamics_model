% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study-as-Performance Fulfillment of the Sacrificial Commandments (Kodashim Kernel)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   Within rabbinic Judaism, the sacrificial commandments (kodashim) bind a
 *   community that has lacked the altar, priesthood, and animal stock their
 *   performance requires since 70 CE. This story instantiates ONE reading of
 *   the contested kernel kodashim_commandment_status: the
 *   study_as_performance reading, crystallized in the amoraic academy
 *   (Menachot 110a, deriving from Hosea 14:3 'we will render for bulls the
 *   offering of our lips'), on which studying the sacrificial laws fulfills
 *   the commandment itself — the kernel remains occupied through intellectual
 *   engagement, with full commandment-force attributed to the study act. The
 *   epsilon referent is the standing arrangement under contest as this
 *   reading frames it: the community's obligation-relationship to the
 *   sacrificial corpus, discharged through study. Assessed by the reading's
 *   own lights, the arrangement extracts almost nothing: participation is
 *   voluntary, the practice confers discharged-obligation status on its
 *   practitioners, and no party bears imposed costs. The sibling readings —
 *   performance_only (commandment suspended as husk without the altar) and
 *   messianic_deferral (temporally suspended; study maintains readiness) —
 *   instantiate different constraints with different epsilon values and
 *   beneficiary structures; they are separate stories linked through
 *   network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - rabbinic_academies: agenda-setting seat (institutional/identity_locked) — authors, transmits, and administers the substitution doctrine; its core activity carries commandment-rank under this reading
 *   - torah_students_of_kodashim: primary beneficiary (moderate/identity_locked) — discharges the sacrificial obligation through study
 *   - post_temple_liturgy_communities: secondary beneficiary (organized/constrained) — sustains covenantal continuity via korbanot recitation on the reading's warrant
 *   - temple_restoration_activists: excluded voice (moderate/mobile) — holds that the reading entrenches permanent non-performance
 *   - karaite_scripturalists: excluded voice (moderate/mobile) — rejects the rabbinic framework in which the reading lives
 *   - halakhic_structural_analysts: analytical observer — maps the reallocation of commandment-force from altar to academy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.06).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study-as-Performance Fulfillment of the Sacrificial Commandments (Kodashim Kernel)").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'edb3a2e8-b4d0-4bc1-8c2c-d907c7188666').
narrative_ontology:cs_kernel_codification('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', fixed_text).
narrative_ontology:cs_authority_grounding('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', lineage).
narrative_ontology:cs_interpretation_layer_present('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666').
narrative_ontology:cs_reading_relation('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', kodashim_commandment_status__messianic_deferral, forecloses).
narrative_ontology:cs_axiom('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', foundational, study_fulfills_sacrifice_commandment).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_commandment, holdable).
narrative_ontology:cs_axiom_grounding('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', study_fulfills_sacrifice_commandment, theological).
narrative_ontology:cs_axiom('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', secondary, verbal_offering_substitutes_animal_offering).
narrative_ontology:cs_axiom_status(verbal_offering_substitutes_animal_offering, holdable).
narrative_ontology:cs_axiom_grounding('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', verbal_offering_substitutes_animal_offering, theological).
narrative_ontology:cs_reference_frame('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', continuously_binding_covenantal_corpus).
narrative_ontology:cs_drift_state('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', contemporary_post_destruction_dispersion, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('edb3a2e8-b4d0-4bc1-8c2c-d907c7188666', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_students_of_kodashim).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, post_temple_liturgy_communities).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, study_substitutes_for_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, hosea_verbal_offering_derivation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the substitution doctrine in the amoraic academies and transmit it today through curriculum, ordination, and liturgical custom. They administer which texts and practices count as fulfilling the sacrificial commandments, and under this reading their own core activity — study — carries commandment-rank. Leaving the reading would mean dissolving the transmission chain that constitutes them.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, rabbinic_academies, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, rabbinic_academies, beneficiary).

% Discharge the sacrificial obligations through scheduled study of the kodashim orders (Leviticus, the Mishnah and Talmud of Kodashim). The reading converts their study-time into fulfilled commandment without requiring altar, priesthood, or animals. Abandoning the practice would leave the commandment, as they understand it, undischarged — exit is fused with religious identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_students_of_kodashim, beneficiary,
    moderate, biographical, identity_locked, global).

% Recite the korbanot (sacrificial-passages) section of the daily liturgy on this reading's warrant, maintaining covenantal continuity with the sacrificial order without any cultic apparatus. Dropping the recitation is available but is framed within the community as a loss of commandment-fulfillment rather than a neutral simplification.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, post_temple_liturgy_communities, beneficiary,
    organized, generational, constrained, global).

% Contemporary movements preparing vessels, vestiges, and priestly lineages for renewed sacrifice. They hold that study-substitution entrenches permanent non-performance and quietly relocates the cult's center from altar to academy. They stand outside the rabbinic consensus in which the reading is adjudicated and have no seat in its application.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, temple_restoration_activists, excluded,
    moderate, biographical, mobile, regional).

% Reject the rabbinic oral-framework wholesale and with it any commandment-force attaching to rabbinically prescribed study practice. Historically they contested the substitution doctrine from outside the academy; they recognize neither the academies' authority to redefine fulfillment nor the derivation from the prophetic verse.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, karaite_scripturalists, excluded,
    moderate, generational, mobile, regional).

% Map how the reading reallocates commandment-force from altar-performance to text-engagement, and compare the structural consequences of the three competing readings of the same kernel. They collect no fulfillment and bear no obligation; their seat is analytic.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_structural_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a covenantal obligation-corpus operative for a community that lacks the apparatus its performance requires: the commandment stays defined, teachable, schedulable, and dischargeable through a practice (study) available in every place and era, so obligation-continuity survives the altar's absence instead of lapsing into ambiguity.
% TRANSFER_FUNCTION: Moves no material goods. It moves commandment-status onto the act of study — time and attention flow from obligated members into the text-curriculum, and discharged-obligation status flows back to the studier. Secondarily it relocates the cult's center of gravity from the priestly-altar complex to the academy.
% ABSENT_VOICES: Temple-restoration activists would object that substitution entrenches permanent non-performance and hardens a provisional accommodation into a steady state; Karaite scripturalists would object that rabbinic study-practice carries no commandment force at all. Both stand outside the academies where the reading is adjudicated. Historically, priestly families whose cultic role lapsed likewise had no seat in the amoraic deliberation that produced the doctrine.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the korbanot recitations in the daily liturgy lose their operative warrant, the obligation-continuity problem reopens, and the community must re-adopt one of the sibling readings — the curriculum, the liturgy, and the commandment-bookkeeping of observant communities worldwide would reorganize around whichever answer prevailed.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), a community bound by covenant to a large body of laws requiring an altar, priests, and animals had no means of performing them; the founding problem was preserving the commandments' force and the community's covenantal continuity without the cultic apparatus.
% FOUNDING_PROBLEM_CORROBORATION: All three readings share the same founding premise — the altar is gone — so the sibling readings themselves corroborate the problem while disputing the answer. Extra-traditional attestation includes Josephus's account of the cessation of the daily sacrifice and the archaeological record of the destroyed Temple. No party disputes the founding condition; the dispute is confined to the remedy.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at the identity_coordination floor (0.08): the only costs the arrangement imposes are the time and attention study requires, which are the coordination cost itself rather than rent skimmed above it. Suppression is minimal (0.06) because nothing enforces the reading — it propagates through curriculum and liturgy, and the sibling readings remain live, licit positions inside the same framework. Theater is low (0.12): where the practice is alive, study genuinely discharges the obligation as the reading defines it; the small theatrical fraction is rote korbanot recitation without engagement, tracked by omega rather than baked into the type. Accessibility_collapse is low (0.20) because understanding the reading does not eliminate alternatives — two rival readings coexist as live halakhic positions. Resistance is low (0.08): the reading meets little active opposition inside the framework; contestation comes from excluded outsiders. The claimed type follows from structure, independently of these metrics: a genuine coordination problem (obligation-continuity without an altar), net-benefiting participants, unsuppressed alternatives, minimal coercive overhead. The measurement series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer seat, so the classical payer/beneficiary divergence is absent; the operative divergence runs between insider and outsider seats. From the academies' seat the arrangement is continuity itself — the commandment never lapsed, and the academy inherited the altar's office. From the students' seat it is discharge — obligation met without apparatus. From the excluded seats the same structure reads as evasion: restoration activists see a provisional accommodation hardened into permanent non-performance; scripturalists see no commandment-force at all. The engine computes per-seat classifications from power and exit data; the excluded objections are commentary-grade (R3) and drive no override.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated participant derives a low d from the beneficiary declarations: students and liturgical communities are subsidized outright (the arrangement converts their practice into discharged obligation at no imposed cost); the academies sit marginally above pure beneficiary because they also administer the arrangement, but the arrangement they run pays them in the coin of commandment-rank for their own core activity. No agent bears extraction, so no seat approaches the target end. The excluded actors sit outside the d-computation entirely — they are not coordinated by the constraint; they contest it from beyond its framework. No directionality overrides are needed: the derivation from beneficiary structure already lands every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — obligation-continuity without an altar — is still live, so the mandate has not outlived its function and no mandatrophy is declared. The rope classification guards against two mislabels. It blocks the snare reading: although the arrangement concentrates interpretive authority in the academy, no one bears imposed costs, so there is no extraction asymmetry hiding behind a coordination story. It blocks the premature piton reading: liturgical recitation can go rote, but the piton test is atrophied function plus fix-cost asymmetry, and the function (discharge through engagement) is genuinely performed wherever the practice is alive; the rote-drift risk is carried as an empirical omega rather than pre-judged in the type. If the altar were ever restored, the restoration-counterfactual omega determines whether this reading reveals a latent sunset (an undeclared scaffold) or persists as steady-state coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which of the three readings correctly characterizes the sacrificial commandment''s status while the altar stands unavailable — discharged-through-study, suspended-husk, or deferred-to-restoration?',
    'Framework-internal adjudication (a restored altar or a novel halakhic crisis forcing re-derivation) or close conceptual analysis of the Hosea 14:3 derivation chain and its rivals within the amoraic sugya.',
    'If a sibling reading prevails, this constraint''s commandment-force attribution collapses: study becomes readiness-maintenance (messianic_deferral) or optional preparation (performance_only), and the beneficiary structure thins accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of kernel kodashim_commandment_status; sibling readings assign a different present-status to the same commandment.').

omega_variable(
    substitution_completeness,
    'Does study discharge the sacrificial obligation with full commandment-force, or does a residual performance-obligation survive that study merely palliates?',
    'Textual analysis of the amoraic sugya (Menachot 110a, ''I account it as though he offered'') and its commentators on whether the accounting is exhaustive equivalence or accommodation.',
    'A surviving residual would introduce a small unfilled-obligation pressure — a sliver of deficit inside an otherwise clean coordination arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_completeness, conceptual, 'Full-versus-partial depth of the study-for-sacrifice substitution.').

omega_variable(
    liturgical_rote_drift,
    'Is the kernel still occupied through genuine intellectual engagement, or has rote recitation of the korbanot liturgy degraded the practice toward theatrical maintenance?',
    'Comparative curricular and liturgical analysis across eras and communities: engagement depth, comprehension rates, and the ratio of recitation-with-understanding to recitation-as-habit.',
    'Rising theater would push this reading toward piton-flavored drift — a coordination arrangement kept alive by performing study rather than studying.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_rote_drift, empirical, 'Engagement-quality trajectory of the substituting practice.').

omega_variable(
    restoration_counterfactual_persistence,
    'If the altar were restored, would this reading dissolve (revealing a latent transitional character) or persist alongside resumed performance?',
    'Counterfactual analysis of the reading''s internal logic — its own texts claim permanence, not transition — together with survey of precedent communities'' stated intentions regarding a restored cult.',
    'Dissolution-on-restoration would reclassify this reading as an undeclared scaffold carrying an implicit sunset; persistence would confirm steady-state coordination status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_counterfactual_persistence, conceptual, 'Whether the reading carries an implicit sunset clause despite declaring none.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 200, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t200, kodashim_commandment_status__study_as_performance, theater_ratio, 200, 0.05).
narrative_ontology:measurement_basis(koda_tr_t200, observed).
narrative_ontology:measurement(koda_tr_t600, kodashim_commandment_status__study_as_performance, theater_ratio, 600, 0.07).
narrative_ontology:measurement_basis(koda_tr_t600, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.09).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1400, kodashim_commandment_status__study_as_performance, theater_ratio, 1400, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1400, observed).
narrative_ontology:measurement(koda_tr_t1800, kodashim_commandment_status__study_as_performance, theater_ratio, 1800, 0.11).
narrative_ontology:measurement_basis(koda_tr_t1800, observed).
narrative_ontology:measurement(koda_tr_t2025, kodashim_commandment_status__study_as_performance, theater_ratio, 2025, 0.12).
narrative_ontology:measurement_basis(koda_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t200, kodashim_commandment_status__study_as_performance, base_extractiveness, 200, 0.05).
narrative_ontology:measurement_basis(koda_be_t200, observed).
narrative_ontology:measurement(koda_be_t600, kodashim_commandment_status__study_as_performance, base_extractiveness, 600, 0.06).
narrative_ontology:measurement_basis(koda_be_t600, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1400, kodashim_commandment_status__study_as_performance, base_extractiveness, 1400, 0.07).
narrative_ontology:measurement_basis(koda_be_t1400, observed).
narrative_ontology:measurement(koda_be_t1800, kodashim_commandment_status__study_as_performance, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement_basis(koda_be_t1800, observed).
narrative_ontology:measurement(koda_be_t2025, kodashim_commandment_status__study_as_performance, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement_basis(koda_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% Kernel kodashim_commandment_status decomposes into three readings with distinct epsilon values and beneficiary structures: study_as_performance (this story; near-floor epsilon, no victims, full commandment-force now), performance_only (suspended husk; study loses commandment-force and becomes optional), and messianic_deferral (temporal suspension; study as readiness-maintenance with an implicit restoration horizon). The colloquial label 'what happens to the sacrifice commandments without a Temple' conflates these; each is authored as its own epsilon-invariant story and linked here. The upstream shared premise (the altar is gone) is common to all three; the readings diverge on the commandment's present operative status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
