% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment (Collective Right Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, asserting that it protects the right of states to maintain
 *   militias, not an individual's right to own firearms. This reading
 *   emphasizes the institutional aspect of the amendment, viewing it as a
 *   structural provision for state security rather than a personal liberty.
 *   The low extractiveness reflects its narrow institutional scope, primarily
 *   affecting the balance of power between state and federal governments
 *   regarding militia organization and firearms regulation, rather than
 *   directly extracting from individuals. The 'mountain' claim reflects the
 *   assertion of this reading as the original and fixed constitutional
 *   intent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '20d69ca7-0a95-4517-90ac-004beee89d87').
narrative_ontology:cs_kernel_codification('20d69ca7-0a95-4517-90ac-004beee89d87', fixed_text).
narrative_ontology:cs_authority_grounding('20d69ca7-0a95-4517-90ac-004beee89d87', lineage).
narrative_ontology:cs_interpretation_layer_present('20d69ca7-0a95-4517-90ac-004beee89d87').
narrative_ontology:cs_reading_relation('20d69ca7-0a95-4517-90ac-004beee89d87', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('20d69ca7-0a95-4517-90ac-004beee89d87', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('20d69ca7-0a95-4517-90ac-004beee89d87', foundational, militia_clause_is_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_is_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('20d69ca7-0a95-4517-90ac-004beee89d87', militia_clause_is_operative_clause, conventional).
narrative_ontology:cs_axiom('20d69ca7-0a95-4517-90ac-004beee89d87', foundational, right_of_the_people_is_collective).
narrative_ontology:cs_axiom_status(right_of_the_people_is_collective, holdable).
narrative_ontology:cs_axiom_grounding('20d69ca7-0a95-4517-90ac-004beee89d87', right_of_the_people_is_collective, conventional).
narrative_ontology:cs_reference_frame('20d69ca7-0a95-4517-90ac-004beee89d87', founding_era_militia_focus).
narrative_ontology:cs_drift_state('20d69ca7-0a95-4517-90ac-004beee89d87', contemporary_individual_rights_advocacy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('20d69ca7-0a95-4517-90ac-004beee89d87', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, gun_rights_advocates).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, republican_self_defense_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional recognition of their authority to organize and maintain militias, providing a basis for state-level defense and public order. This reading grants them broad regulatory power over firearms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Are constitutionally recognized as legitimate entities for state defense, providing a framework for their existence and operation under state control. This reading validates their institutional role.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Are not granted an individual right to bear arms by this reading, meaning their ability to own firearms is subject to state regulation without a constitutional claim. Their self-conception of a right is not recognized.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_citizens, excluded,
    powerless, biographical, identity_locked, national).

% Interprets and enforces the Second Amendment, with this reading granting it the authority to uphold state regulatory power over firearms, consistent with the militia clause. Its role is to adjudicate disputes between states and individuals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Bear the cost of this reading by having their claims for individual gun ownership rights denied or severely curtailed. They must pursue legislative or alternative constitutional interpretations to achieve their goals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, payer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the constitutional authority of states to maintain organized militias for collective defense and public order, ensuring a framework for state-level security forces.
% TRANSFER_FUNCTION: Transfers the authority over firearms regulation from individuals to state governments, allowing states to control arms necessary for their militias without individual constitutional impediment.
% ABSENT_VOICES: Individual gun owners and advocates for an individual right to self-defense are excluded from the constitutional framework established by this reading; they would argue for a broader interpretation of 'the right of the people'.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, state governments would lose a clear constitutional basis for their militia authority and broad firearms regulation, leading to a significant shift in the balance of power regarding gun control and state defense capabilities. The legal landscape would be reconfigured.
% FOUNDING_PROBLEM: The need to ensure states could maintain well-regulated militias for their security and defense against both foreign threats and internal insurrections, following the weaknesses of the Articles of Confederation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside of state governments corroborate the historical context of the founding problem, emphasizing the post-Revolutionary War concerns about national defense and state autonomy. Contemporary state defense forces and national guard units continue to fulfill this function.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.25) reflect that, within this reading's framework, the constraint primarily defines institutional authority rather than imposing direct costs on a broad populace. It is a structural feature of the federal system. Resistance is low because, within this specific interpretive frame, the reading is considered settled constitutional law. The 'mountain' claim is based on the assertion that this interpretation represents the fixed, original meaning of the amendment, an unchangeable constitutional principle.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading is a foundational element of their sovereignty and security, a clear constitutional mandate. From the perspective of individual citizens who believe in an individual right to bear arms, this reading is a form of suppression, denying what they perceive as a fundamental liberty. The engine's classification will highlight this divergence between the claimed 'mountain' status and the experience of 'excluded' or 'payer' seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are beneficiaries, as this reading affirms their constitutional authority and existence. Individual citizens, particularly those advocating for personal gun ownership rights, are excluded or bear the cost of non-recognition. The federal government acts as an agenda-setter, upholding this interpretation through its judicial and executive functions. Gun rights advocates are payers, as their claims are denied by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the ''collective right'' reading the sole original intent of the Second Amendment, or did the framers also intend to protect an individual right?',
    'Further historical and legal scholarship, including analysis of newly discovered primary sources or re-evaluation of existing ones, focusing on the debates and understandings of ''the right of the people'' at the time of ratification.',
    'If evidence strongly supports an individual right component, this reading''s ''mountain'' claim would be challenged, potentially reclassifying it as a ''tangled_rope'' or ''snare'' from the perspective of individual citizens, as its naturalness would be undermined by a constructed, extractive interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity regarding the original intent of the Second Amendment''s scope.').

omega_variable(
    constitutional_evolution_vs_originalism,
    'Should the Second Amendment''s interpretation evolve with societal changes (e.g., modern weaponry, standing armies), or should it be strictly bound by original intent?',
    'Judicial precedent (e.g., Supreme Court rulings explicitly adopting a living constitutionalist approach to the Second Amendment) or a constitutional amendment clarifying its scope.',
    'If an evolutionary approach is adopted, this ''collective right'' reading might be deemed ''overridden'' or ''atrophied'' in practice, even if its original intent is acknowledged, leading to a reclassification away from ''mountain'' towards ''piton'' or ''snare'' if its persistence is seen as purely inertial or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_evolution_vs_originalism, conceptual, 'Conceptual debate over originalism versus living constitutionalism in interpreting the Second Amendment.').

omega_variable(
    collective_vs_individual_right_framing,
    'Is the ''right of the people'' in the Second Amendment fundamentally a collective right tied to militia service, or does it inherently include an individual component?',
    'Philosophical and legal arguments clarifying the nature of rights in the US Constitution, particularly how ''the people'' is understood in different amendments (e.g., First vs. Fourth vs. Second).',
    'If ''the right of the people'' is conceptually proven to always include an individual component, this reading would be seen as structurally incomplete or flawed, potentially leading to its reclassification as a ''snare'' for individuals whose rights are denied by this narrow framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_right_framing, conceptual, 'Conceptual ambiguity in the framing of ''the right of the people'' as collective versus individual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__collective_right_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__collective_right_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(seco_tr_t15, second_amendment_scope__collective_right_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__collective_right_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__collective_right_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__collective_right_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(seco_be_t15, second_amendment_scope__collective_right_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__collective_right_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__collective_right_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__collective_right_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(seco_su_t15, second_amendment_scope__collective_right_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__collective_right_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_gun_control_laws).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, federal_firearms_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_scope' kernel. This 'collective_right_reading' emphasizes state authority over individual rights, contrasting with the 'individual_right_reading' and 'civic_right_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
