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
 *   human_readable: Originalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'originalist reading' of the phrase 'all
 *   men are created equal' from the US Declaration of Independence. In this
 *   reading, the scope of 'men' is strictly limited to the social taxonomy of
 *   the 18th century, primarily propertied white men, and the founders'
 *   intent governs its application. This interpretation serves to maintain
 *   historical power structures and resist the expansion of rights to
 *   historically marginalized groups. The high extractiveness and suppression
 *   reflect the ongoing effort required to maintain this narrow
 *   interpretation against challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.85).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.9).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0').
narrative_ontology:cs_kernel_codification('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', fixed_text).
narrative_ontology:cs_authority_grounding('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', lineage).
narrative_ontology:cs_interpretation_layer_present('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0').
narrative_ontology:cs_reading_relation('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', original_public_meaning_supremacy, conventional).
narrative_ontology:cs_axiom('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', foundational, constitutional_stability_over_evolution).
narrative_ontology:cs_axiom_status(constitutional_stability_over_evolution, holdable).
narrative_ontology:cs_axiom_grounding('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', constitutional_stability_over_evolution, instrumental).
narrative_ontology:cs_reference_frame('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', founding_era_social_taxonomy).
narrative_ontology:cs_drift_state('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', contemporary_civil_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ecf3bf08-3ec0-4d24-ab4c-7dba1fb304e0', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, conservative_political_factions).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, african_americans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_men).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, constitutional_conservatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical power structures and interpretations that legitimize their inherited status and influence. This reading maintains the historical narrative that justifies their position.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    institutional, generational, arbitrage, national).

% Actively interpret and promote the constraint, shaping legal discourse and judicial appointments. Their careers and intellectual authority are built upon defending this specific reading of the founding document.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, biographical, constrained, national).

% Leverage this reading to justify policies that limit the expansion of rights, maintain traditional social hierarchies, and resist progressive legal reforms. It provides a powerful rhetorical and legal tool for their agenda.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, conservative_political_factions, beneficiary,
    organized, immediate, mobile, national).

% Historically and presently bear the costs of a reading that excluded them from the promise of equality, leading to systemic discrimination, disenfranchisement, and violence. Their struggle for full inclusion is a direct challenge to this constraint.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, african_americans, payer,
    powerless, generational, trapped, national).

% Were excluded from the original definition of 'men' and continue to face legal and social barriers rooted in historical interpretations that denied them equal rights and status. Their fight for gender equality directly confronts this constraint.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    powerless, generational, trapped, national).

% Were not considered in the original framing of equality, leading to centuries of dispossession, cultural destruction, and denial of sovereignty. This reading perpetuates their marginalization within the constitutional framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% While eventually included, they were initially excluded from the full promise of equality, demonstrating the narrow, class-based nature of the original intent. Their historical struggle highlights the constraint's initial extractive scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_propertied_men, payer,
    powerless, biographical, constrained, national).

% Advocate for a broader, evolving interpretation of equality that extends to all human beings, regardless of historical context. Their arguments are often dismissed by originalists as judicial activism or anachronistic imposition.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_legal_scholars, excluded,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal interpretation around a fixed historical understanding, providing a stable (though narrow) framework for constitutional adjudication and limiting judicial discretion.
% TRANSFER_FUNCTION: Transfers political and social power, as well as material resources, from historically excluded groups to those who align with or benefit from the original, narrow definition of equality.
% ABSENT_VOICES: The voices of those historically excluded from the definition of 'men' (African Americans, women, indigenous peoples, non-propertied men) were absent from the founding discourse and continue to be marginalized in originalist interpretations. Universalist scholars are also excluded from the interpretive consensus.
% DISAPPEARANCE_RATIONALE: If this originalist reading vanished, the legal and political landscape would fundamentally shift. Arguments for expanding rights would gain immediate traction, historical injustices would be re-evaluated, and the power dynamics between different social groups would be profoundly altered, leading to a significant rearrangement of the constitutional order.
% FOUNDING_PROBLEM: To establish a new nation's foundational principles, including a statement of human rights, while navigating the existing social and economic realities of the 18th century, particularly slavery and patriarchal norms.
% FOUNDING_PROBLEM_CORROBORATION: Historians and critical legal scholars widely corroborate that the social problems of the 18th century (e.g., slavery) that necessitated the narrow definition of 'men' are no longer live in the same form. While some originalist scholars argue the problem of judicial overreach remains live, the specific historical context that justified the narrow scope of equality is largely considered dead by independent academic consensus.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because this reading actively denies equality to broad segments of the population, thereby preserving unearned advantages for others. Suppression is also very high (0.9) as it requires active legal and political enforcement to prevent the expansion of equality and to suppress alternative, more inclusive interpretations. The theater ratio is moderate (0.6) because while there is genuine intellectual effort in originalist scholarship, a significant portion of its function is performative – maintaining a facade of historical fidelity to justify contemporary power arrangements. Accessibility collapse is 0.7 because while legal challenges exist, the entrenched nature of this interpretation makes fundamental change difficult. Resistance is 0.8, reflecting the continuous and intense struggle by civil rights movements and legal advocates against this narrow interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this reading provides a stable, principled, and historically faithful framework for governance. From the perspective of the victims, it is a tool of oppression and exclusion, actively denying their fundamental rights. The engine's classification will highlight this divergence, showing a Snare for victims and potentially a Tangled Rope or even a Rope for beneficiaries, depending on their specific power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (founding_elite_descendants, originalist_legal_scholars, conservative_political_factions) gain from the preservation of historical power and the justification of their current positions. Victims (African_Americans, women, indigenous_peoples, non_propertied_men) bear the direct costs of exclusion and discrimination. Originalist scholars act as agenda-setters, actively shaping the interpretation and its enforcement. Universalist scholars are excluded, their arguments often dismissed as outside the legitimate interpretive frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to establish foundational principles) has largely outlived its original, narrow function. The 'founding problem' of navigating 18th-century social realities is 'dead' in its original form. The persistence of this reading, despite the obsolescence of its original context, indicates a mandatrophic state where the constraint continues to extract and suppress, serving new beneficiaries rather than its original purpose. The high theater ratio and extractiveness, coupled with the 'dead' founding problem, prevent mislabeling this as genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_scope_ambiguity,
    'Is ''original intent'' a fixed, discoverable historical fact, or is it itself a contested interpretive construct that evolves with the interpreter''s present-day concerns?',
    'Historical-linguistic analysis of founding-era texts by non-originalist scholars, comparing the consistency of ''original intent'' claims across different originalist interpretations over time.',
    'If ''original intent'' is a construct, the constraint''s claim to historical fidelity is weakened, increasing its theater ratio and exposing its extractive function more clearly. If it is a fixed fact, the constraint''s legitimacy (for its proponents) is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_scope_ambiguity, conceptual, 'Ambiguity regarding the nature and discoverability of ''original intent''.').

omega_variable(
    historical_exclusion_justification,
    'To what extent was the exclusion of certain groups from ''all men'' a necessary compromise for the founding of the nation, versus an expression of inherent prejudice that could have been avoided?',
    'Counterfactual historical analysis by independent historians, exploring alternative constitutional framings and their potential consequences for national unity and the abolition of slavery.',
    'If the exclusion was a ''necessary evil'', it might slightly reduce the perceived extractiveness for some observers (though not for victims). If it was avoidable prejudice, it reinforces the Snare classification and highlights the moral costs of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_exclusion_justification, empirical, 'Whether historical exclusions were pragmatic necessities or expressions of prejudice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal precedent, institutional inertia) or internalized (cognitive patterns that persist after barrier removal, e.g., self-censorship or belief in one''s own inferiority)?',
    'Post-legal-reform suppression trajectory: if suppression persists after the extractive legal mechanisms are removed, reclassify as partially internalized. Sociological studies on the persistence of internalized oppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after legal exit. This would deepen the Snare classification by revealing a more insidious form of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of historical and ongoing discrimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__originalist_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(all__tr_t100, all_men_created_equal__originalist_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(all__tr_t150, all_men_created_equal__originalist_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement(all__tr_t200, all_men_created_equal__originalist_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(all__tr_t250, all_men_created_equal__originalist_reading, theater_ratio, 250, 0.6).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__originalist_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(all__be_t100, all_men_created_equal__originalist_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(all__be_t150, all_men_created_equal__originalist_reading, base_extractiveness, 150, 0.83).
narrative_ontology:measurement(all__be_t200, all_men_created_equal__originalist_reading, base_extractiveness, 200, 0.84).
narrative_ontology:measurement(all__be_t250, all_men_created_equal__originalist_reading, base_extractiveness, 250, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__originalist_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(all__su_t100, all_men_created_equal__originalist_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(all__su_t150, all_men_created_equal__originalist_reading, suppression_requirement, 150, 0.89).
narrative_ontology:measurement(all__su_t200, all_men_created_equal__originalist_reading, suppression_requirement, 200, 0.9).
narrative_ontology:measurement(all__su_t250, all_men_created_equal__originalist_reading, suppression_requirement, 250, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, constitutional_interpretation_doctrine).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, voting_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. This originalist reading emphasizes historical context and founders' intent, leading to a narrow, extractive application of equality. It is linked to 'universalist_reading' and 'textualist_paradox_reading' which offer broader or critical interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
