% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin as Medieval Practice Correctable by Textual Evidence (Hybrid Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the contested 'correct
 *   Latin' kernel: correctness is neither pure living-practice continuity nor
 *   pure classical reconstruction, but medieval practice treated as partially
 *   legitimate — its grammatical core accepted as genuine transmission —
 *   while its orthography, vocabulary, and select constructions remain
 *   subject to correction against classical textual evidence. This is a
 *   distinct constraint from the continuity reading (which validates medieval
 *   forms wholesale as evolved Latin) and the discontinuity reading (which
 *   treats medieval Latin as corruption requiring full reconstruction). Each
 *   reading has its own beneficiary/victim structure and its own epsilon;
 *   they are not the same constraint viewed three ways. The hybrid reading's
 *   defining structural feature is selective correction: some medieval forms
 *   are vindicated, others overridden, and the editors who administer that
 *   selection accrue durable authority from the very existence of a boundary
 *   to police.
 *
 * KEY AGENTS:
 *   - humanist_textual_editors: administer the correction, deciding case-by-case which medieval forms stand (institutional/arbitrage)
 *   - reformist_latin_teachers: adopt and benefit from the corrected curriculum (organized/mobile)
 *   - manuscript_collation_scholars: build ongoing authority from the correction project's continuation (organized/arbitrage)
 *   - vernacular_trained_clerics and provincial_notaries: bear retraining and reputational costs from selective reclassification of their existing competence (moderate-powerless/constrained-trapped)
 *   - monastic_scribes_of_traditional_orthography: have some practices vindicated and others overridden by outside editorial judgment (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin as Medieval Practice Correctable by Textual Evidence (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '9f0e1d23-9d13-44dd-8b8b-53d95518a9e4').
narrative_ontology:cs_kernel_codification('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', distributed).
narrative_ontology:cs_authority_grounding('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', expertise).
narrative_ontology:cs_interpretation_layer_present('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4').
narrative_ontology:cs_reading_relation('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', foundational, grammatical_core_survives_transmission).
narrative_ontology:cs_axiom_status(grammatical_core_survives_transmission, holdable).
narrative_ontology:cs_axiom_grounding('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', grammatical_core_survives_transmission, empirically_contingent).
narrative_ontology:cs_axiom('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', foundational, surface_features_correctable_by_textual_evidence).
narrative_ontology:cs_axiom_status(surface_features_correctable_by_textual_evidence, holdable).
narrative_ontology:cs_axiom_grounding('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', surface_features_correctable_by_textual_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', classical_grammar_with_medieval_transmission).
narrative_ontology:cs_drift_state('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', post_manuscript_collation_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f0e1d23-9d13-44dd-8b8b-53d95518a9e4', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_textual_editors).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, reformist_latin_teachers).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, manuscript_collation_scholars).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, vernacular_trained_clerics).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, provincial_notaries).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, monastic_scribes_of_traditional_orthography).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, textual_evidence_as_corrective_authority).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, grammatical_core_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compare medieval manuscript readings against recovered classical sources and issue corrected orthography, vocabulary, and select syntax as the standard for instruction and copying. They administer the correction process itself, deciding case by case which medieval forms stand and which are emended, and their labor is what gives the hybrid position its practical machinery.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_textual_editors, agenda_setter,
    institutional, generational, arbitrage, continental).

% Adopt the corrected curricula, gaining prestige and patronage from courts and cathedral schools that want their clerks trained in the newly authoritative hybrid Latin. Their exit options are real — they can teach older forms elsewhere — but the reformed curriculum is where advancement now lies.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, reformist_latin_teachers, beneficiary,
    organized, biographical, mobile, regional).

% Build careers and institutional standing on the collation work that identifies which medieval readings are corruptions and which are legitimate transmission. Their authority depends on the correction project continuing indefinitely, since a fully settled text would end their function.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, manuscript_collation_scholars, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, manuscript_collation_scholars, agenda_setter).

% Learned Latin through inherited medieval practice — the forms taught to them by their own masters — and now find substantial portions of that training marked as error requiring textual correction. They cannot simply retrain without cost, and their existing competence is partially devalued by the new standard even though it was never fully rejected.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_trained_clerics, payer,
    moderate, biographical, constrained, regional).

% Draft legal and administrative documents in the Latin they were trained in, often far from centers where the corrected standard is taught. When their documents are later judged deficient by the corrected standard, they bear reputational and sometimes material costs for a shift in authority they had no part in and little access to.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, provincial_notaries, payer,
    powerless, biographical, trapped, local).

% Have maintained centuries-old copying conventions inherited through their houses. The hybrid standard treats some of their orthographic habits as correctable error, requiring retraining or acceptance of diminished authority over the texts they have long been custodians of, while other of their practices are validated as legitimate transmission — the boundary is drawn by outside editors, not by them.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, monastic_scribes_of_traditional_orthography, payer,
    moderate, generational, constrained, regional).

% Serve as the evidentiary standard against which medieval practice is checked; they are not an actor but the reference against which correction is justified.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_source_texts, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, classical_source_texts).

% Fund and endorse whichever standard of correct Latin serves their administrative and prestige interests, shifting patronage toward the hybrid-standard institutions without being bound by the internal philological dispute themselves.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, ecclesiastical_and_court_patrons, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, textually-anchored standard for what counts as correct Latin that allows administrative, ecclesiastical, and educational documents to be judged by a consistent norm across regions, while preserving the grammatical continuity that makes existing medieval training partially usable.
% TRANSFER_FUNCTION: Moves linguistic authority and pedagogical prestige from those trained purely in inherited medieval practice toward those with access to classical manuscript comparison and textual scholarship; moves reputational and retraining costs onto provincial and monastic practitioners whose practice is selectively reclassified as error.
% ABSENT_VOICES: Provincial notaries and rural monastic scribes whose practice is judged against a standard set by continental collation centers have no seat in determining which of their inherited forms count as legitimate continuity versus correctable corruption; they experience the verdict, not the deliberation.
% DISAPPEARANCE_RATIONALE: Editors and reformist teachers would say the world rearranges substantially — the correction apparatus, the prestige economy of collation scholarship, and curricular reform all depend on the hybrid standard's continued authority. Provincial and monastic practitioners would say relatively little changes for their daily practice, since they would continue using inherited forms regardless of which distant standard nominally applies; the dispute is over who gets to say their practice is or isn't correct, not over what they actually do.
% FOUNDING_PROBLEM: Written Latin across medieval Christendom had drifted into regionally divergent forms, undermining administrative and liturgical intelligibility across jurisdictions, while wholesale reversion to classical forms threatened to invalidate the grammatical competence of the existing clerical and notarial class.
% FOUNDING_PROBLEM_CORROBORATION: Humanist editors and reformist teachers attest the correction problem remains live, citing continuing orthographic and lexical divergence in provincial documents. Independent evidence is thinner: some contemporary court chroniclers outside the editorial circles note that administrative Latin functioned adequately across regions before the correction campaigns began, suggesting part of the 'problem' is a standard newly imposed by the correcting institutions rather than a pre-existing crisis; no fully disinterested corroborating source exists, since even chroniclers were embedded in one patronage network or another.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, contested).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits meaningfully below the discontinuity reading's expected extraction because the hybrid reading vindicates a genuine grammatical core in medieval practice rather than declaring it wholesale corruption — much of what practitioners already know remains valid. But it is well above the continuity reading's expected floor because the correction apparatus still transfers authority and cost: some practice is overridden, and the overriding class (editors, collation scholars) accrues durable institutional position from administering the boundary. Suppression (0.38) and resistance (0.45) reflect that the correction is contested but not coercively total — provincial and monastic actors can and do resist particular emendations, and enforcement operates through prestige and patronage rather than force. Theater ratio (0.30) captures that a portion of collation activity has begun to serve the scholars' own institutional continuation rather than actual correctness gains, but this is a moderate, not dominant, share of the activity.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist editors and collation scholars sit near the beneficiary end: they administer the standard, their labor defines what counts as correction, and their institutional position depends on the correction project's ongoing existence. Reformist teachers benefit through mobility and prestige access but retain real exit options. Vernacular clerics, provincial notaries, and monastic scribes sit toward the target end in proportion to their exit constraint: notaries are most trapped (local, powerless, no access to the collation centers that set the standard), monastic scribes are moderately constrained (regional networks, some institutional standing of their own), and clerics fall between. Classical source texts are marked as a non-agent beneficiary — the evidentiary standard the correction cites, not an actor who collects anything.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mandatrophy in one direction and risks it in another. It resists the discontinuity reading's totalizing revision (treating all medieval practice as corrupt) by preserving the grammatical core as legitimate, which prevents the wholesale devaluation of an entire practicing class's competence. But the ongoing, open-ended nature of the correction project — collation scholars whose authority depends on the boundary between legitimate-transmission and correctable-corruption never fully closing — is a structural risk: a founding problem (regional divergence undermining intelligibility) that could in principle be solved once and for all instead sustains an indefinitely renewable editorial function. The founding_problem_status is marked contested precisely because corroboration outside the benefiting editorial and teaching classes is thin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_boundary_stability,
    'Is the boundary between ''legitimate transmitted core'' and ''correctable corruption'' a stable, principled distinction, or does it shift opportunistically to expand the correcting editors'' jurisdiction over time?',
    'Track whether the categories of medieval forms marked ''correctable'' expand faster than new textual evidence would independently justify — compare rate of newly-emended categories against rate of newly-discovered classical source material.',
    'If the boundary expands independent of new evidence, the hybrid reading is drifting toward the discontinuity reading''s extraction profile while retaining the hybrid reading''s coordination cover story; if stable, the partial-continuity claim is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_boundary_stability, empirical, 'Whether the corrective boundary in the hybrid reading is principled or expansionary.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings disagree — is it about which linguistic FEATURES count as legitimate (grammar vs. orthography vs. vocabulary), or about the underlying THEORY of what makes a form ''correct'' (living use vs. textual fidelity vs. weighted mixture)?',
    'Compare the reading-specific correction protocols against the same disputed manuscript passages: continuity_reading would accept nearly all; discontinuity_reading would reject most non-classical forms; hybrid_reading accepts grammar, corrects orthography/vocabulary. The disagreement is located in the acceptance criteria applied to the same evidentiary base, not in access to different evidence.',
    'Confirms the three readings are genuinely distinct constraints (different beneficiary/victim structures, different epsilon) rather than the same constraint under different rhetorical framing — supports the decomposition into three separate stories rather than one story with a hidden parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating the structural disagreement among sibling kernel readings.').

omega_variable(
    corroboration_independence,
    'Does any corroboration of the founding problem''s continued liveness exist from a source with no stake in either the editorial correction apparatus or the teaching institutions that benefit from it?',
    'Search administrative and legal records (notarial archives, court proceedings) for evidence of actual communication failure attributable to Latin divergence, independent of philological commentary produced by invested parties.',
    'If no independent corroboration exists, the founding_problem_status should lean toward ''dead-but-persisting-through-inertia'' rather than genuinely contested, which would push the classification toward piton-adjacent dynamics for the collation-scholarship function specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_independence, empirical, 'Whether the founding problem''s persistence is independently attested or self-reported by beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(corr_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(corr_tr_t80, correct_latin__hybrid_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(corr_tr_t120, correct_latin__hybrid_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(corr_tr_t160, correct_latin__hybrid_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement(corr_tr_t200, correct_latin__hybrid_reading, theater_ratio, 200, 0.3).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(corr_be_t80, correct_latin__hybrid_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(corr_be_t120, correct_latin__hybrid_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement(corr_be_t160, correct_latin__hybrid_reading, base_extractiveness, 160, 0.4).
narrative_ontology:measurement(corr_be_t200, correct_latin__hybrid_reading, base_extractiveness, 200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(corr_su_t40, correct_latin__hybrid_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(corr_su_t80, correct_latin__hybrid_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(corr_su_t120, correct_latin__hybrid_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(corr_su_t160, correct_latin__hybrid_reading, suppression_requirement, 160, 0.36).
narrative_ontology:measurement(corr_su_t200, correct_latin__hybrid_reading, suppression_requirement, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_reading member of the correct_latin kernel family (3 stories: continuity_reading, discontinuity_reading, hybrid_reading). Each reading instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim set, and classification, per the epsilon-invariance principle — they are not one constraint measured three ways. The hybrid reading is expected to sit between the other two on extraction (partial vindication reduces extraction relative to discontinuity_reading; selective correction and its administering class raise extraction relative to continuity_reading). influences discontinuity_reading because the hybrid reading's partial vindication of medieval grammar reduces the resource base and legitimacy available for a full-reconstruction program; coexists_with continuity_reading because both readings can be held by different practitioner communities without either being logically foreclosed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
