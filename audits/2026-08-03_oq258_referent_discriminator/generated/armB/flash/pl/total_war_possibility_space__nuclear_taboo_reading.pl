% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint represents the 'nuclear taboo' reading of the total war
 *   possibility space. It posits that total war, particularly with nuclear
 *   weapons, became normatively prohibited through a constructed taboo,
 *   independent of purely material deterrence. While the material capability
 *   for total war persists, its normative acceptability has been foreclosed,
 *   leading to the development of norm-enforcement mechanisms like the
 *   non-proliferation regime. The constraint is claimed as a Rope due to its
 *   genuine coordination function in preventing global catastrophe, but its
 *   persistence relies on active normative enforcement and the suppression of
 *   alternative strategic framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.2).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.6).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'b071d556-96fc-45a8-b489-76aff2734b30').
narrative_ontology:cs_kernel_codification('b071d556-96fc-45a8-b489-76aff2734b30', distributed).
narrative_ontology:cs_authority_grounding('b071d556-96fc-45a8-b489-76aff2734b30', practice).
narrative_ontology:cs_interpretation_layer_present('b071d556-96fc-45a8-b489-76aff2734b30').
narrative_ontology:cs_reading_relation('b071d556-96fc-45a8-b489-76aff2734b30', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('b071d556-96fc-45a8-b489-76aff2734b30', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('b071d556-96fc-45a8-b489-76aff2734b30', foundational, nuclear_weapons_are_unusable).
narrative_ontology:cs_axiom_status(nuclear_weapons_are_unusable, holdable).
narrative_ontology:cs_axiom_grounding('b071d556-96fc-45a8-b489-76aff2734b30', nuclear_weapons_are_unusable, deontological).
narrative_ontology:cs_axiom('b071d556-96fc-45a8-b489-76aff2734b30', foundational, normative_prohibition_is_causally_effective).
narrative_ontology:cs_axiom_status(normative_prohibition_is_causally_effective, holdable).
narrative_ontology:cs_axiom_grounding('b071d556-96fc-45a8-b489-76aff2734b30', normative_prohibition_is_causally_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('b071d556-96fc-45a8-b489-76aff2734b30', post_hiroshima_normative_shift).
narrative_ontology:cs_drift_state('b071d556-96fc-45a8-b489-76aff2734b30', contemporary_geopolitical_challenges, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('b071d556-96fc-45a8-b489-76aff2734b30', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, rogue_states_or_non_state_actors).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_dynamics_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_ir_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the capability for total war but largely adhere to the taboo, actively promoting non-proliferation and no-first-use norms. They benefit from the stability the taboo provides but bear the cost of maintaining it and foregoing certain strategic options.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the reduced risk of total war, which allows for conventional conflict without escalation to existential threats. They are constrained by the non-proliferation regime, which limits their strategic autonomy but enhances their security.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% The ultimate beneficiary of the taboo, as it prevents existential catastrophe. This 'stakeholder' has no agency but is the object of protection. Its 'exit option' is non-existence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Academics, activists, and diplomats who actively construct, promote, and reinforce the nuclear taboo through discourse, advocacy, and institutionalization efforts. Their influence is critical to the taboo's persistence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, agenda_setter,
    moderate, biographical, mobile, global).

% Actors who might seek to develop or use nuclear weapons but are constrained by the non-proliferation regime and the global normative pressure of the taboo. They bear the costs of international isolation and potential military intervention if they violate the norm.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, rogue_states_or_non_state_actors, payer,
    powerless, immediate, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global state behavior by establishing a shared normative boundary against the use of nuclear weapons, preventing escalation to total war and fostering a framework for conventional conflict management.
% TRANSFER_FUNCTION: Transfers strategic freedom (the option of total war) from states to a collective normative constraint, in exchange for global stability and reduced existential risk.
% ABSENT_VOICES: Future generations, who would unequivocally object to the breakdown of the taboo, are absent from current decision-making. Additionally, any state or non-state actor that believes nuclear weapons are a legitimate tool for achieving strategic objectives is excluded from the norm-setting discourse.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the strategic landscape would fundamentally alter. Nuclear weapons would become thinkable as first-strike options, leading to rapid escalation in conflicts, a breakdown of non-proliferation efforts, and a dramatically increased risk of global catastrophe. The entire international security architecture would need to be rebuilt.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after their initial use in WWII, creating a need for a mechanism to prevent their future use and the resulting total war.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the ongoing risk of proliferation attest to the live nature of the problem. International treaties, diplomatic efforts, and public discourse consistently reinforce the need to prevent nuclear war, corroborated by a broad consensus among states and international organizations.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the constraint primarily limits strategic options rather than directly extracting resources, and the benefits of global stability are widely distributed. Suppression is moderate (0.6) due to the active enforcement of non-proliferation norms and the social/political costs for states violating the taboo. Theater ratio is low (0.1) as the taboo is a genuinely held and actively maintained norm, not merely a performance. Accessibility collapse is high (0.7) because the normative space for total war has largely collapsed, even if the material possibility remains. Resistance is low (0.15) as most states adhere to the norm, though some actors challenge it.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers, while beneficiaries of the stability, also act as agenda-setters in maintaining the taboo, experiencing it as a strategic limitation. Non-nuclear states primarily experience it as a beneficial constraint on global conflict. Rogue states or non-state actors, however, experience it as a suppressive force limiting their strategic options.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and non-nuclear states are beneficiaries, as the taboo reduces existential risk and provides a framework for international security. Global humanity is the ultimate beneficiary. Rogue states or non-state actors are targets, as the taboo actively suppresses their potential strategic choices. Norm entrepreneurs are agenda-setters, actively shaping and enforcing the norm.
 *
 * MANDATROPHY ANALYSIS:
 *   The nuclear taboo is a live constraint addressing a persistent problem (the existential threat of nuclear war). Its coordination function remains vital, preventing it from degrading into a Piton. While it involves some suppression, this is directed at preventing catastrophic outcomes, distinguishing it from a Snare. The active role of norm entrepreneurs and the ongoing enforcement of non-proliferation demonstrate its continued function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causality,
    'To what extent is the absence of total war due to the nuclear taboo (normative prohibition) versus material deterrence (fear of retaliation)?',
    'Counterfactual historical analysis, examination of decision-making processes in crises, and comparative studies of non-nuclear WMD taboos. If states with nuclear capabilities consistently refrain from use even when deterrence is ambiguous, it strengthens the taboo hypothesis.',
    'If deterrence is the primary driver, this constraint would be reclassified closer to a Mountain (material reality) or a Snare (coercive threat). If the taboo is primary, its Rope classification is reinforced, highlighting the role of constructed norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, conceptual, 'Ambiguity in the causal mechanism preventing total war.').

omega_variable(
    norm_entrepreneur_exit_impact,
    'What would be the impact on the nuclear taboo if key norm entrepreneurs (academics, activists, diplomats) ceased to actively promote and reinforce it?',
    'Observing the trajectory of other international norms that lost their ''champions'' or ''custodians''. A rapid erosion of adherence or increased rhetorical challenges to the taboo would indicate its dependence on active normative work.',
    'If the taboo significantly weakens, it would suggest a higher degree of active enforcement (suppression) is required, potentially shifting the classification towards a Tangled Rope or even a Snare if the underlying coordination function is revealed as cover for power projection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_impact, empirical, 'Dependence of the taboo on active norm entrepreneurship.').

omega_variable(
    non_nuclear_powers_constraint_differentiation,
    'Does the nuclear taboo operate differently for non-nuclear powers, particularly those facing existential threats, compared to established nuclear states?',
    'Comparative case studies of states that have pursued or considered nuclear weapons programs despite the taboo, analyzing their motivations and the international responses. If the costs and pressures are disproportionately higher for non-nuclear states, it indicates an asymmetric application of the norm.',
    'If the constraint is significantly more extractive or suppressive for non-nuclear powers, it would suggest a Tangled Rope classification, where the ''coordination'' of non-proliferation comes at a higher cost for some actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_powers_constraint_differentiation, empirical, 'Asymmetric application of the nuclear taboo across states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, conventional_arms_control_treaties).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. The other readings are 'deterrence_equilibrium_reading' (total war deterred by mutual vulnerability) and 'space_contraction_reading' (total war removed from thinkable space). Each reading has distinct structural properties and implications for international security.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
