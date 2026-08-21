% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Interpretation of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the originalist interpretation of the phrase
 *   'All Men Are Created Equal' from the Declaration of Independence, where
 *   the scope of 'men' and 'equal' is strictly bounded by the social taxonomy
 *   and intent of the 18th-century founders. This reading actively limits the
 *   application of equality to historically privileged groups, thereby
 *   maintaining existing power structures. The constraint is claimed as a
 *   'snare' because its coordination story (fidelity to founding principles)
 *   serves as a cover for substantial and ongoing extraction from
 *   historically excluded groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.85).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.9).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Interpretation of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10').
narrative_ontology:cs_kernel_codification('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', fixed_text).
narrative_ontology:cs_authority_grounding('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', lineage).
narrative_ontology:cs_interpretation_layer_present('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10').
narrative_ontology:cs_reading_relation('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', all_men_created_equal__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', foundational, original_public_meaning_governs).
narrative_ontology:cs_axiom_status(original_public_meaning_governs, holdable).
narrative_ontology:cs_axiom_grounding('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', original_public_meaning_governs, conventional).
narrative_ontology:cs_axiom('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', foundational, founders_intent_limits_scope).
narrative_ontology:cs_axiom_status(founders_intent_limits_scope, holdable).
narrative_ontology:cs_axiom_grounding('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', founders_intent_limits_scope, conventional).
narrative_ontology:cs_reference_frame('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', founding_era_social_order).
narrative_ontology:cs_drift_state('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bd55a4ab-6787-4d9d-a9d1-8993c2ba8f10', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elites_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, dominant_social_groups).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_persons_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_men).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, constitutional_conservatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As inheritors of the founding political and social order, they benefit from and actively defend an interpretation of equality that maintains historical hierarchies and limits the scope of rights to those originally envisioned by the founders. They shape legal and political discourse.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elites_descendants, agenda_setter,
    institutional, generational, arbitrage, national).

% These groups, historically aligned with the founders' vision of society, benefit from the limited application of equality, which reinforces their social and economic advantages. They often support originalist interpretations in legal and political spheres.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, dominant_social_groups, beneficiary,
    powerful, biographical, mobile, national).

% As direct descendants of those explicitly excluded from the founders' definition of 'men,' they bear the ongoing costs of systemic inequality, denied reparations, and limited access to full citizenship rights under this interpretation. Their claims for justice are structurally suppressed.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_persons_descendants, payer,
    powerless, generational, trapped, national).

% Historically dispossessed and denied full recognition under the originalist framework, they continue to face challenges to sovereignty and land rights, as their claims are often outside the scope of the 18th-century social taxonomy. Their resistance is met with legal and political barriers.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% While having gained some rights over time, women's claims for full equality, particularly in areas not explicitly considered by the founders, are often challenged or limited by originalist interpretations, requiring constant struggle against a restrictive framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    moderate, biographical, constrained, national).

% Initially excluded from full political participation, their rights were expanded, but the originalist framework can still be used to limit economic and social welfare claims not explicitly recognized at the founding.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_propertied_men, payer,
    moderate, biographical, constrained, national).

% These scholars and jurists actively interpret and apply the 'All Men Are Created Equal' clause through the lens of original intent and 18th-century understanding, providing the intellectual and legal justification for the constraint's narrow scope. They are key enforcers of this reading.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, analytical, national).

% These groups advocate for a broader, evolving understanding of equality that applies universally, regardless of historical context. Their arguments are often dismissed or marginalized within the originalist interpretive framework, making them structurally excluded from the dominant legal discourse.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, historically bounded definition of political equality and citizenship, intended to provide a consistent framework for governance and rights within the specific social and political context of the American founding.
% TRANSFER_FUNCTION: Transfers political, social, and economic power, as well as the full scope of rights, from historically excluded groups (such as enslaved persons, indigenous peoples, and women) to the founding elite and their descendants, by narrowly defining the beneficiaries of 'equality'.
% ABSENT_VOICES: Enslaved persons, indigenous peoples, women, and non-propertied men were largely excluded from the founding discourse; their voices would challenge the narrow definition of equality and demand universal application. Contemporary universalist legal scholars and advocates are also structurally excluded from the interpretive framework, as their arguments are often deemed irrelevant to original intent.
% DISAPPEARANCE_RATIONALE: If this originalist interpretation vanished overnight, the legal and social landscape of American equality would fundamentally shift. It would necessitate a radical re-evaluation of rights, reparations, and political inclusion for historically marginalized groups, leading to a profound reorganization of power structures and legal precedents.
% FOUNDING_PROBLEM: To establish a legitimate basis for self-governance and individual rights among a specific, limited population (propertied white men), while navigating the existing social hierarchies, economic realities (e.g., slavery), and the need for a unified national identity.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and proponents attest that the founding problem of constitutional interpretation through original intent is still live. However, historians, political scientists, and universalist legal scholars (outside the originalist community) attest that the problem of limited equality is largely 'dead' in terms of moral and social progress, and that the originalist framework now serves to maintain existing power structures rather than solve a contemporary problem. Legislative hearing testimony and independent academic analysis support the shifted-function reading.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the denial of full equality and rights to a significant portion of the population based on historical interpretations. Suppression (0.90) is high due to the active legal and political enforcement of this narrow interpretation, which systematically marginalizes alternative, more expansive readings. The theater ratio (0.40) indicates a significant gap between the rhetorical ideal of 'equality' and its practical, limited application, with a portion of the interpretive effort dedicated to maintaining this performative contradiction. Accessibility collapse (0.75) is high because legal and political avenues for challenging this interpretation are severely constrained, while resistance (0.80) remains high due to continuous advocacy from marginalized groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries and agenda-setters, this constraint is a legitimate and necessary adherence to foundational principles, ensuring stability and fidelity to the nation's origins. From the perspective of victims and excluded groups, it is a mechanism of ongoing oppression and exclusion, using historical interpretation as a justification for maintaining power imbalances. The engine's classification as a 'snare' reflects the latter, measuring the structural extraction despite the former's 'rope' or 'mountain' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding elites and their descendants, along with dominant social groups, are the primary beneficiaries and agenda-setters, as this interpretation reinforces their historical advantages and power. Historically excluded groups (descendants of enslaved persons, indigenous peoples, women, non-propertied men) are the primary targets and payers, bearing the costs of denied rights and systemic inequality. Originalist legal scholars act as agenda-setters by actively shaping and enforcing this interpretation. Universalist advocates are excluded, as their arguments are deemed outside the legitimate interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_fidelity_vs_power_maintenance,
    'Is the originalist interpretation primarily driven by a genuine commitment to historical fidelity, or does it function as a justification for maintaining existing power structures and social hierarchies?',
    'Comparative legal analysis of originalist applications across different domains (e.g., property rights vs. civil rights) to identify consistent patterns of outcome-oriented reasoning, or historical sociological studies of the beneficiaries'' interests.',
    'If primarily power maintenance, the constraint''s extractiveness is confirmed as inherent to its function, reinforcing its ''snare'' classification. If genuine fidelity, the extractiveness might be re-evaluated as an unintended consequence of a ''mountain''-like commitment to historical truth, though still extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_fidelity_vs_power_maintenance, conceptual, 'Ambiguity between interpretive principle and political outcome.').

omega_variable(
    scope_of_equality_evolution,
    'To what extent can the concept of ''equality'' evolve beyond 18th-century social taxonomy while maintaining fidelity to the Declaration''s foundational principles?',
    'Philosophical and legal arguments exploring the ''abstract principles'' vs. ''concrete applications'' distinction in constitutional interpretation, or analysis of how other foundational texts have been reinterpreted over time to accommodate changing social norms.',
    'If evolution is possible, the originalist reading''s suppression of broader equality claims is revealed as a choice, not a necessity, strengthening the ''snare'' classification. If evolution is deemed impossible without abandoning the principles, the constraint''s ''mountain''-like immutability is reinforced, though its extractiveness remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_equality_evolution, conceptual, 'The inherent tension between universal language and historically bounded application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.2).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__originalist_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__originalist_reading, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(all__tr_t1990, all_men_created_equal__originalist_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(all__tr_t2023, all_men_created_equal__originalist_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.9).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.85).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__originalist_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__originalist_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(all__be_t1990, all_men_created_equal__originalist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(all__be_t2023, all_men_created_equal__originalist_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.95).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.9).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__originalist_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__originalist_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(all__su_t1990, all_men_created_equal__originalist_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(all__su_t2023, all_men_created_equal__originalist_reading, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, voting_rights_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'All Men Are Created Equal' kernel. It represents the originalist interpretation, which emphasizes historical intent and 18th-century social taxonomy, leading to a narrow scope of equality. It is linked to sibling readings that offer universalist and textualist-paradoxical interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
