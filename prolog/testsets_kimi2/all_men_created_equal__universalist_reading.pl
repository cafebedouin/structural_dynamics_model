% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint instantiates the universalist reading of the 'all men are
 *   created equal' kernel: the principle that equality is a universal moral
 *   commitment requiring iterative expansion regardless of founder intent. It
 *   is one of three structurally distinct readings of the same
 *   natural-language text (originalist, textualist-paradox, universalist),
 *   each with distinct epsilon, beneficiary structure, and classification.
 *   The universalist reading treats the Declaration's and Constitution's
 *   equality language as a horizon that legitimizes the polity precisely by
 *   generating recursive demands for inclusion. The claim/metric independence
 *   is maintained: the reading is claimed as tangled_rope (genuine
 *   coordination function plus asymmetric extraction) while metrics are
 *   authored descriptively.
 *
 * KEY AGENTS:
 *   - Federal judiciary (agenda_setter/beneficiary): Institutional authority derived from interpretive expansion, constrained exit.
 *   - Marginalized groups claiming inclusion (beneficiary): Organized claimants using the principle for rights expansion, constrained exit.
 *   - Groups denied equal status (payer): Powerless agents bearing the dignitary and material costs of incomplete equality, trapped exit.
 *   - Originalist legal scholars (observer): Analytical contestants foreclosed by the universalist premise but active in discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.48).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.55).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'b3c205dc-1ea0-4255-9393-a92f82d99e7e').
narrative_ontology:cs_kernel_codification('b3c205dc-1ea0-4255-9393-a92f82d99e7e', fixed_text).
narrative_ontology:cs_authority_grounding('b3c205dc-1ea0-4255-9393-a92f82d99e7e', lineage).
narrative_ontology:cs_interpretation_layer_present('b3c205dc-1ea0-4255-9393-a92f82d99e7e').
narrative_ontology:cs_reading_relation('b3c205dc-1ea0-4255-9393-a92f82d99e7e', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b3c205dc-1ea0-4255-9393-a92f82d99e7e', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('b3c205dc-1ea0-4255-9393-a92f82d99e7e', foundational, human_equality_universal_moral_status).
narrative_ontology:cs_axiom_status(human_equality_universal_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('b3c205dc-1ea0-4255-9393-a92f82d99e7e', human_equality_universal_moral_status, deontological).
narrative_ontology:cs_axiom('b3c205dc-1ea0-4255-9393-a92f82d99e7e', foundational, constitutional_text_transcends_authorial_intent).
narrative_ontology:cs_axiom_status(constitutional_text_transcends_authorial_intent, holdable).
narrative_ontology:cs_axiom_grounding('b3c205dc-1ea0-4255-9393-a92f82d99e7e', constitutional_text_transcends_authorial_intent, conventional).
narrative_ontology:cs_reference_frame('b3c205dc-1ea0-4255-9393-a92f82d99e7e', universal_human_equality_horizon).
narrative_ontology:cs_drift_state('b3c205dc-1ea0-4255-9393-a92f82d99e7e', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b3c205dc-1ea0-4255-9393-a92f82d99e7e', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_denied_equal_status).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, living_constitutionalism).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, moral_progress_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets 'all men are created equal' as a universal moral commitment requiring iterative expansion beyond founder intent. Derives institutional authority from the power to define the boundaries of equal protection and due process. Cannot exit the constitutional system but can shift its interpretive center of gravity through precedent.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, federal_judiciary, beneficiary).

% Invoke the universalist principle to demand inclusion in political, economic, and social life. Each successful expansion validates the reading while revealing prior exclusions. Exit from the constitutional order is not a practical option; their strategy is recursive appeal to the text and the courts.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, biographical, constrained, national).

% Bear the material and dignitary costs of being excluded from a constitutional order that rhetorically promises universal equality. Their continued exclusion is what makes the iterative-expansion dynamic visible and necessary. No exit from the national jurisdiction is available or acknowledged by the constraint.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_denied_equal_status, payer,
    powerless, biographical, trapped, national).

% Contest the universalist reading by arguing that equality is bounded by eighteenth-century social taxonomy and founder intent. They are analytically engaged but structurally excluded from legitimacy within the universalist framework; their reading is foreclosed by the universalist premise yet persists in public discourse.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_legal_scholars, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative grammar through which diverse groups can demand inclusion without revolutionary rupture, coordinating political legitimacy under a single constitutional text across profound social change.
% TRANSFER_FUNCTION: Transfers rights-recognition and political legitimacy from exclusionary historical orders to newly included groups; transfers the costs of structural adjustment and continued exclusion to groups denied equal status and to institutional defenders of the old order.
% ABSENT_VOICES: Originalist interpreters who reject expansion beyond founder intent; radical critics who reject the liberal equality framework entirely, such as some indigenous sovereignty, prison abolitionist, and anarchist positions that view the principle as a legitimizing fiction of settler-colonial or carceral capitalism; persons outside the national jurisdiction for whom the text claims no authority but whose material conditions are affected by its export.
% DISAPPEARANCE_RATIONALE: If the universalist principle vanished, constitutional jurisprudence would lose its primary engine of rights expansion; marginalized groups would lose their most potent textual anchor for inclusionary claims; federal judicial authority would contract sharply; and the political order would face a legitimation crisis as the gap between universal promise and particular reality could no longer be managed through iterative reform.
% FOUNDING_PROBLEM: How to legitimate a republic founded on liberty while accommodating slavery, indigenous dispossession, and limited suffrage; how to maintain political cohesion under a text whose language exceeds its original social referent.
% FOUNDING_PROBLEM_CORROBORATION: Historians and critical legal scholars outside the beneficiary set document the contradiction between universalist language and restricted practice; abolitionists, suffragists, and civil rights activists attested the founding problem from the victim seat, not the beneficiary seat; originalist scholars corroborate that the founders did not intend universal equality, underscoring the gap the universalist reading claims to bridge.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the universalist principle coordinates genuine political inclusion while simultaneously legitimizing a constitutional order that continues to extract from those not yet included. Suppression (0.55) reflects the active marginalization of originalist and hierarchical alternatives in mainstream constitutional doctrine. Theater ratio (0.40) captures the performative dimension: the principle is ritually invoked in civic discourse while material inequality persists. Accessibility collapse (0.45) is incomplete because originalist and radical alternatives remain structurally available. Resistance (0.60) is substantial due to ongoing originalist counter-mobilization and backlash against rights expansion. The measurement grid is shared: all metrics are authored at the same six time points.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and marginalized groups experience the constraint as a coordination mechanism that stabilizes and expands rights. Groups denied equal status experience it as extraction legitimized by unfulfilled promise. Originalist scholars experience it as a usurpation of legitimate textual boundaries. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is a beneficiary (low d, authority accrues to the interpreter) and agenda-setter. Marginalized groups claiming inclusion are beneficiaries (low-mid d, they gain rights-recognition). Groups denied equal status are the target (high d, they bear the costs of exclusion within a system that promises inclusion). Originalist scholars are observers (mid d, they neither gain nor lose directly but contest the frame).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents mislabeling it as pure coordination (rope), which would ignore the ongoing extraction from those denied equal status, and prevents mislabeling it as pure extraction (snare), which would deny the genuine coordination function of providing a non-revolutionary path to inclusion. The founding problem is contested rather than dead: the universalist reading claims the problem is the perpetual gap between promise and reality, while critics argue the arrangement has outlived its legitimizing function. The temporal measurements show fluctuating extraction and rising theater, consistent with a coordination mechanism accumulating performative overhead over centuries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_universalism,
    'Is the universalist equality principle a discovered moral fact inherent in the constitutional text, or a retrospectively constructed interpretive tradition projected onto the founders'' language?',
    'Historical sociology of constitutional doctrine tracing the emergence of universalist claims in specific political moments (abolition, Reconstruction, New Deal, Civil Rights) versus textual-philological analysis of eighteenth-century equality concepts.',
    'If constructed, the constraint''s coordination function is a legitimation narrative rather than a recovered intent, increasing its theater_ratio and shifting its classification toward snare; if discovered, its moral authority is independent of historical practice, strengthening its rope-like coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_universalism, conceptual, 'Whether universalism is discovered or constructed').

omega_variable(
    coordination_cost_or_extractive_deferral,
    'Is the incomplete realization of universal equality a necessary coordination cost of pluralist democracy, or an extractive deferral that legitimizes ongoing hierarchy by promising eventual redemption?',
    'Comparative analysis of jurisdictions with and without universalist constitutional equality principles, measuring material inequality and social-movement outcomes; evaluation of whether iterative expansion correlates with net extraction reduction or with legitimation stability.',
    'If a necessary cost, the moderate extractiveness is the price of large-scale coordination and the classification remains tangled_rope; if extractive deferral, the constraint extracts from the same groups it promises to include, pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_or_extractive_deferral, conceptual, 'Whether incomplete equality is cost or deferral').

omega_variable(
    suppression_as_hegemony_or_consensus,
    'Is the suppression of originalist alternatives in constitutional discourse a hegemonic exclusion by institutional elites, or the emergence of a genuine constitutional consensus?',
    'Demographic and ideological mapping of the legal professoriate and bench over time; measurement of originalist representation in elite law schools, courts, and bar associations; comparison with public opinion polls on originalism versus living constitutionalism.',
    'If hegemonic, suppression is higher than measured and the constraint is more extractive toward excluded interpreters; if consensus, the suppression metric accurately reflects a settled coordination equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_as_hegemony_or_consensus, empirical, 'Whether suppression of originalism is hegemony or consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(universalist_eq_tr_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(universalist_eq_tr_t48, all_men_created_equal__universalist_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(universalist_eq_tr_t96, all_men_created_equal__universalist_reading, theater_ratio, 96, 0.32).
narrative_ontology:measurement(universalist_eq_tr_t144, all_men_created_equal__universalist_reading, theater_ratio, 144, 0.36).
narrative_ontology:measurement(universalist_eq_tr_t192, all_men_created_equal__universalist_reading, theater_ratio, 192, 0.38).
narrative_ontology:measurement(universalist_eq_tr_t240, all_men_created_equal__universalist_reading, theater_ratio, 240, 0.4).

% Extraction over time
narrative_ontology:measurement(universalist_eq_be_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(universalist_eq_be_t48, all_men_created_equal__universalist_reading, base_extractiveness, 48, 0.55).
narrative_ontology:measurement(universalist_eq_be_t96, all_men_created_equal__universalist_reading, base_extractiveness, 96, 0.48).
narrative_ontology:measurement(universalist_eq_be_t144, all_men_created_equal__universalist_reading, base_extractiveness, 144, 0.42).
narrative_ontology:measurement(universalist_eq_be_t192, all_men_created_equal__universalist_reading, base_extractiveness, 192, 0.4).
narrative_ontology:measurement(universalist_eq_be_t240, all_men_created_equal__universalist_reading, base_extractiveness, 240, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(universalist_eq_su_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(universalist_eq_su_t48, all_men_created_equal__universalist_reading, suppression_requirement, 48, 0.4).
narrative_ontology:measurement(universalist_eq_su_t96, all_men_created_equal__universalist_reading, suppression_requirement, 96, 0.55).
narrative_ontology:measurement(universalist_eq_su_t144, all_men_created_equal__universalist_reading, suppression_requirement, 144, 0.5).
narrative_ontology:measurement(universalist_eq_su_t192, all_men_created_equal__universalist_reading, suppression_requirement, 192, 0.45).
narrative_ontology:measurement(universalist_eq_su_t240, all_men_created_equal__universalist_reading, suppression_requirement, 240, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The natural-language phrase 'all men are created equal' decomposes into at least three structurally distinct constraint readings: originalist (bounded by founder intent), textualist-paradox (performative contradiction), and universalist (iterative expansion regardless of intent). Each reading has distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
