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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the Declaration of
 *   Independence's phrase 'all men are created equal,' which interprets its
 *   scope strictly according to the 18th-century social taxonomy and the
 *   perceived intent of the founders. This reading limits equality to a
 *   narrow set of individuals (primarily white, landowning men) and excludes
 *   women, enslaved people, and indigenous populations. It is a foundational
 *   element of certain constitutional interpretations that resist the
 *   expansion of rights.
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
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '338e4ef6-8237-4103-be73-7ff01f972ef0').
narrative_ontology:cs_kernel_codification('338e4ef6-8237-4103-be73-7ff01f972ef0', fixed_text).
narrative_ontology:cs_authority_grounding('338e4ef6-8237-4103-be73-7ff01f972ef0', lineage).
narrative_ontology:cs_interpretation_layer_present('338e4ef6-8237-4103-be73-7ff01f972ef0').
narrative_ontology:cs_reading_relation('338e4ef6-8237-4103-be73-7ff01f972ef0', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('338e4ef6-8237-4103-be73-7ff01f972ef0', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('338e4ef6-8237-4103-be73-7ff01f972ef0', foundational, equality_scope_fixed_at_founding).
narrative_ontology:cs_axiom_status(equality_scope_fixed_at_founding, holdable).
narrative_ontology:cs_axiom_grounding('338e4ef6-8237-4103-be73-7ff01f972ef0', equality_scope_fixed_at_founding, conventional).
narrative_ontology:cs_axiom('338e4ef6-8237-4103-be73-7ff01f972ef0', secondary, judicial_restraint_prevents_evolution).
narrative_ontology:cs_axiom_status(judicial_restraint_prevents_evolution, holdable).
narrative_ontology:cs_axiom_grounding('338e4ef6-8237-4103-be73-7ff01f972ef0', judicial_restraint_prevents_evolution, deontological).
narrative_ontology:cs_reference_frame('338e4ef6-8237-4103-be73-7ff01f972ef0', eighteenth_century_social_taxonomy).
narrative_ontology:cs_drift_state('338e4ef6-8237-4103-be73-7ff01f972ef0', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('338e4ef6-8237-4103-be73-7ff01f972ef0', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, conservative_political_factions).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, african_americans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_landowning_men).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, constitutional_stasis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical power structures and interpretations that legitimize their inherited status and influence. They actively promote originalist interpretations in legal and political discourse.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    institutional, generational, arbitrage, national).

% Actively interpret and apply the 'all men are created equal' clause strictly according to the perceived intent of the 18th-century founders, thereby limiting its scope to exclude historically marginalized groups. Their careers and influence are built on this interpretive framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_scholars, agenda_setter,
    organized, biographical, constrained, national).

% Leverage originalist interpretations to justify policies that maintain existing social hierarchies and resist expansion of rights, aligning with their political agenda. They benefit from the stability and predictability of a fixed constitutional meaning.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, conservative_political_factions, beneficiary,
    powerful, generational, mobile, national).

% Historically and contemporaneously bear the costs of an interpretation that denied their full humanity and citizenship for centuries, and continues to limit the scope of equality claims based on historical exclusion. Their struggle for rights is a direct challenge to this constraint.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, african_americans, payer,
    powerless, generational, trapped, national).

% Experience the constraint as a barrier to full legal and social equality, as their rights were not explicitly considered or included in the 18th-century understanding of 'men.' Their fight for suffrage and equal protection directly confronts this narrow interpretation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    moderate, generational, constrained, national).

% Are excluded from the originalist scope of equality, as their sovereignty and rights were systematically denied by the founding generation. This interpretation perpetuates their marginalization within the constitutional framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% Were initially excluded from the full rights of citizenship under the 18th-century taxonomy, demonstrating the narrowness of the originalist interpretation even for those nominally included in 'men.' While their status has changed, their historical exclusion highlights the constraint's initial boundaries.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_landowning_men, payer,
    powerless, biographical, constrained, local).

% Advocate for an evolving interpretation of equality that expands to include all persons, regardless of historical context. They are excluded from the dominant originalist discourse in certain legal and political arenas, where their arguments are dismissed as 'activist' or 'unfaithful' to the founders.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_legal_scholars, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically bounded framework for constitutional interpretation, aiming to prevent judicial activism and ensure fidelity to the original compact. It coordinates legal reasoning around a fixed historical point.
% TRANSFER_FUNCTION: Transfers the authority to define 'equality' from contemporary society to the historical intent of the 18th-century founders, thereby limiting the scope of rights for groups not recognized as equal at that time. This transfers power and privilege to those who align with or descend from the original beneficiaries.
% ABSENT_VOICES: The voices of all historically excluded groups (African Americans, women, indigenous peoples, non-landowning men) were absent from the 18th-century drafting of the Declaration and Constitution. Their contemporary descendants and advocates continue to be marginalized when originalist interpretations are strictly applied, as their lived experiences and claims for equality are deemed outside the 'original intent.'
% DISAPPEARANCE_RATIONALE: If the originalist reading of 'all men are created equal' vanished, the legal and political landscape of the United States would fundamentally shift. Arguments for equality would no longer be constrained by 18th-century social norms, potentially leading to a rapid expansion of rights, reinterpretation of existing laws, and a significant redistribution of power and resources towards historically marginalized groups. The entire constitutional framework would need re-evaluation.
% FOUNDING_PROBLEM: The problem of establishing a legitimate government based on popular sovereignty while simultaneously preserving existing social hierarchies and the institution of slavery, requiring a definition of 'equality' that was both aspirational and narrowly applied.
% FOUNDING_PROBLEM_CORROBORATION: While originalist scholars argue the problem of judicial overreach is still live, the specific 18th-century social taxonomy that bounded 'equality' (e.g., excluding enslaved people, women, and non-landowners) is widely considered dead by most historians and legal scholars outside the originalist camp. The problem has shifted from justifying exclusion to justifying the *persistence* of an exclusionary interpretation in a society that nominally rejects those exclusions. Independent historical analysis and contemporary social consensus corroborate the obsolescence of the original social taxonomy.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).

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
 *   Extractiveness is high (0.85) because this reading systematically denies full equality to large segments of the population, thereby preserving privilege for others. Suppression is also high (0.90) as it requires active legal and political enforcement to maintain a narrow interpretation against evolving societal norms and demands for universal rights. The theater ratio is moderate (0.40), reflecting the ongoing performance of 'fidelity to the founders' even as the original context becomes increasingly anachronistic. The slight dip in extractiveness and suppression around 1965 reflects the Civil Rights era, but the resurgence by 2024 indicates a hardening of originalist positions in response to further demands for equality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this constraint is a legitimate and necessary 'rope' that coordinates constitutional interpretation and preserves foundational principles. From the perspective of the victims, it is a 'snare' that perpetuates historical injustices and actively suppresses their claims for full equality. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (founding_elite_descendants, originalist_legal_scholars, conservative_political_factions) gain from the preservation of historical power structures and the stability of a fixed constitutional meaning. Victims (african_americans, women, indigenous_peoples, non_landowning_men) bear the direct costs of exclusion and limited rights. Universalist legal scholars are excluded from the interpretive process when this reading dominates. The directionality for beneficiaries is low (subsidized by the constraint), and for victims, it is high (targeted by the constraint).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_scope_ambiguity,
    'Is the ''original intent'' of the founders regarding equality genuinely knowable and consistently applicable, or is it inherently ambiguous and selectively invoked?',
    'Comprehensive historical and textual analysis by a diverse, non-partisan body of scholars, explicitly acknowledging the limitations and biases of historical sources.',
    'If unknowable or selectively invoked, the constraint''s claim to objective, fixed meaning collapses, revealing its reliance on contemporary power dynamics rather than historical fidelity. This would shift its classification further towards a Snare or Tangled Rope, as the coordination story (fixed meaning) would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_scope_ambiguity, empirical, 'Ambiguity of ''original intent'' as a fixed interpretive anchor.').

omega_variable(
    mandate_shift_from_coordination_to_extraction,
    'Has the primary function of this originalist reading shifted from coordinating constitutional interpretation to extracting political and social privilege for specific groups?',
    'Longitudinal analysis of judicial decisions and legislative outcomes: if the reading consistently produces outcomes that benefit a narrow set of actors at the expense of broader equality, the shift is confirmed.',
    'Confirmation of a shift would reclassify the constraint from a (claimed) Rope to a Snare, as its coordination function would be revealed as a cover for extraction. This would trigger a Mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_shift_from_coordination_to_extraction, empirical, 'Shift from coordinating interpretation to extracting privilege.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal precedent, institutional inertia) or internalized (belief in the legitimacy of originalist interpretations by those it disadvantages)?',
    'Sociological studies of belief systems among historically disadvantaged groups regarding constitutional interpretation. If a significant portion internalizes the originalist frame, suppression is partly internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would make exit options like ''resistance'' more costly and less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.1).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.2).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__originalist_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__originalist_reading, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.95).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.9).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__originalist_reading, base_extractiveness, 1920, 0.88).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__originalist_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.95).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.9).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__originalist_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__originalist_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, baker_v_carr_one_person_one_vote_originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, brown_v_board_separate_but_equal_originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'all_men_created_equal' kernel. Its narrow, historically bounded interpretation directly influences and is influenced by other readings, particularly the 'universalist_reading' (which seeks to expand its scope) and the 'textualist_paradox_reading' (which highlights its internal contradictions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
