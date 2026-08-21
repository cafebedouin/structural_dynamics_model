% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as primarily
 *   protecting the authority of state governments to maintain organized
 *   militias, rather than an individual's right to own firearms for any
 *   purpose outside of militia service. Under this reading, individual gun
 *   ownership is subject to broad state and federal regulation, and
 *   prohibitions on certain types of arms or owners are seen as
 *   constitutionally permissible. The low extractiveness reflects that, from
 *   this reading's perspective, the constraint primarily coordinates state
 *   and federal power, with individual 'victims' being those whose desired
 *   gun ownership is curtailed for the collective good of public safety and
 *   state defense.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '6c1744b1-050b-4a3e-84c8-a4521313cf67').
narrative_ontology:cs_kernel_codification('6c1744b1-050b-4a3e-84c8-a4521313cf67', fixed_text).
narrative_ontology:cs_authority_grounding('6c1744b1-050b-4a3e-84c8-a4521313cf67', lineage).
narrative_ontology:cs_interpretation_layer_present('6c1744b1-050b-4a3e-84c8-a4521313cf67').
narrative_ontology:cs_reading_relation('6c1744b1-050b-4a3e-84c8-a4521313cf67', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6c1744b1-050b-4a3e-84c8-a4521313cf67', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('6c1744b1-050b-4a3e-84c8-a4521313cf67', foundational, militia_clause_governs_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_governs_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('6c1744b1-050b-4a3e-84c8-a4521313cf67', militia_clause_governs_operative_clause, conventional).
narrative_ontology:cs_axiom('6c1744b1-050b-4a3e-84c8-a4521313cf67', foundational, individual_arms_ownership_subordinate_to_state_power).
narrative_ontology:cs_axiom_status(individual_arms_ownership_subordinate_to_state_power, holdable).
narrative_ontology:cs_axiom_grounding('6c1744b1-050b-4a3e-84c8-a4521313cf67', individual_arms_ownership_subordinate_to_state_power, conventional).
narrative_ontology:cs_reference_frame('6c1744b1-050b-4a3e-84c8-a4521313cf67', original_constitutional_text_and_historical_context).
narrative_ontology:cs_drift_state('6c1744b1-050b-4a3e-84c8-a4521313cf67', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6c1744b1-050b-4a3e-84c8-a4521313cf67', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militias).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary rights-holders, state governments possess the authority to organize and arm militias, and to regulate or prohibit individual firearm ownership outside of that organized context. They benefit from the flexibility to manage public safety and defense as they see fit.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the constitutional recognition of their necessity for state security. Their existence and arming are protected, ensuring a ready force for state defense and order, distinct from federal military power.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of this interpretation through potential restrictions or prohibitions on their right to own firearms for personal use, as their ownership is not constitutionally protected unless tied to organized militia service. Their options are to comply, challenge through litigation, or engage in political advocacy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia, payer,
    moderate, biographical, constrained, local).

% Is constrained from disarming state militias but retains broad authority to regulate individual firearm ownership, as the right is not seen as an individual liberty against federal power. Benefits from clear lines of authority regarding national defense and public safety.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Provide the intellectual framework and historical arguments supporting the collective right interpretation, emphasizing the historical context of state militias and the framers' intent. Their analysis reinforces the constraint's legitimacy within this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_scholars_collective_right, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between state and federal power regarding armed force, ensuring states can maintain a militia for their security without federal interference, while allowing for federal regulation of individual arms.
% TRANSFER_FUNCTION: Transfers authority over individual firearm ownership from individuals to state governments (and to the federal government for general regulation), in exchange for the protection of state militia capacity.
% ABSENT_VOICES: Advocates for an expansive individual right to bear arms are structurally excluded from this reading's core premise; they would argue that the right is a fundamental individual liberty, not contingent on militia service.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape around gun control would fundamentally shift. State governments would lose a key justification for plenary regulation of individual arms, and the federal government's regulatory authority would be challenged on new grounds, leading to a significant reorganization of gun laws and enforcement.
% FOUNDING_PROBLEM: The founding problem was to balance the need for state security through militias with concerns about a powerful standing federal army, ensuring states could defend themselves without creating a federal monopoly on armed force.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars (outside of direct beneficiaries like state governments) corroborate that the historical context of the Second Amendment was deeply rooted in concerns about state militias and federal power, supporting the idea that this problem remains relevant in debates about federalism and state autonomy.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading frames the constraint as a necessary coordination mechanism for federalism and state security, with any 'extraction' from individuals being a legitimate cost for the collective good. Suppression (0.25) is also relatively low, as the enforcement of gun control measures is seen as a routine exercise of state power, not an oppressive act. Accessibility collapse (0.7) is high because, within this framework, the 'alternative' of unregulated individual gun ownership is largely foreclosed by the constitutional text itself. Resistance (0.3) is moderate, reflecting ongoing political and legal challenges from those advocating for an individual right, but not a fundamental challenge to the collective right's premise.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and organized militias, this is a foundational 'rope' that secures their authority and capacity. From the perspective of individual gun owners outside of militia contexts, it can feel more like a 'snare' or 'tangled_rope' due to the restrictions it enables on their personal liberties. The engine's per-seat classification would capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are clear beneficiaries (d near 0.0) as their authority and existence are constitutionally protected. The federal government also benefits from clear regulatory authority. Individual gun owners outside of militia contexts are the primary 'victims' or targets (d near 1.0) as their desired actions are curtailed. Legal scholars supporting this view are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legitimate state regulatory power as pure extraction by grounding it in a coordination function (balancing state and federal power, ensuring state defense). It acknowledges that while individuals may bear costs, these are framed as necessary for a broader constitutional order, rather than arbitrary rent-seeking. The 'live' status of the founding problem (balancing state and federal power) suggests that, from this perspective, the constraint's mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_definition_ambiguity,
    'What constitutes an ''organized militia'' in the contemporary context, and does it include all able-bodied citizens or only formally organized state guard units?',
    'Supreme Court ruling or legislative clarification defining ''militia'' for Second Amendment purposes.',
    'A narrow definition (e.g., only National Guard) would further restrict individual rights outside that context, potentially increasing perceived extraction. A broad definition (e.g., all citizens capable of bearing arms) might create a pathway for individual rights claims within a collective framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''militia'' affects the scope of the collective right.').

omega_variable(
    historical_intent_corroboration,
    'To what extent does historical evidence definitively support a purely collective right interpretation, as opposed to an individual right or a civic republican right?',
    'Further historical and legal scholarship, potentially leading to a consensus among constitutional historians or a definitive Supreme Court re-evaluation of historical sources.',
    'Stronger corroboration would solidify this reading''s legitimacy and reduce resistance. Weaker corroboration might shift the interpretive landscape towards individual or civic republican readings, increasing the perceived extractiveness of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_intent_corroboration, empirical, 'Uncertainty regarding the historical intent behind the Second Amendment''s phrasing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1900, 0.23).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1950, 0.24).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, gun_control_legislation_state_level).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, federal_firearms_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
