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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the nuclear taboo as a constructed normative
 *   prohibition against total war, particularly nuclear war, that operates
 *   independently of material deterrence. It posits that while states retain
 *   the material capability for total war, a powerful international norm
 *   makes such action illegitimate and unthinkable. The constraint is claimed
 *   as a Rope because it genuinely coordinates behavior for collective
 *   benefit, but its maintenance requires active enforcement by norm
 *   entrepreneurs and self-restraint by nuclear powers, leading to some
 *   extraction (the cost of foregoing options) and suppression (of
 *   alternative strategic framings).
 *
 * KEY AGENTS:
 *   - global_population: Primary beneficiary (powerless/trapped)
 *   - international_norm_entrepreneurs: Agenda-setter (organized/constrained)
 *   - nuclear_weapon_states: Payer (institutional/constrained)
 *   - non_nuclear_weapon_states: Beneficiary (moderate/mobile)
 *   - military_planners: Payer (institutional/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.2).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.6).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'ff056477-ab5f-4b6f-b3c3-e40d52cda13a').
narrative_ontology:cs_kernel_codification('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', distributed).
narrative_ontology:cs_authority_grounding('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', practice).
narrative_ontology:cs_interpretation_layer_present('ff056477-ab5f-4b6f-b3c3-e40d52cda13a').
narrative_ontology:cs_reading_relation('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', foundational, total_war_is_normatively_illegitimate).
narrative_ontology:cs_axiom_status(total_war_is_normatively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', total_war_is_normatively_illegitimate, deontological).
narrative_ontology:cs_axiom('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', secondary, norms_constrain_material_capability).
narrative_ontology:cs_axiom_status(norms_constrain_material_capability, holdable).
narrative_ontology:cs_axiom_grounding('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', norms_constrain_material_capability, conventional).
narrative_ontology:cs_reference_frame('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', post_hiroshima_normative_consensus).
narrative_ontology:cs_drift_state('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', contemporary_geopolitical_flux, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('ff056477-ab5f-4b6f-b3c3-e40d52cda13a', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_population).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_norm_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, military_planners).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_cascade_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_international_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, which would entail catastrophic loss of life and societal collapse. Has no direct agency in maintaining the taboo but is the ultimate beneficiary of its persistence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_population, beneficiary,
    powerless, generational, trapped, global).

% States, NGOs, and individuals who actively promote and reinforce the taboo against total war, particularly nuclear war. They invest political capital and diplomatic effort in non-proliferation regimes, arms control, and rhetorical condemnation of nuclear use. Their influence is crucial for the taboo's maintenance.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_norm_entrepreneurs, agenda_setter,
    organized, biographical, constrained, global).

% Bear the cost of self-restraint, foregoing the option of total war even when militarily capable. They are subject to international pressure and internal normative constraints, which limit their strategic choices. While they possess the means, the taboo makes the use of total war illegitimate.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% Benefit from the reduced risk of total war, which would disproportionately affect them. They also participate in reinforcing the taboo through international forums and non-proliferation treaties, but their direct costs are lower than nuclear states.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, beneficiary,
    moderate, biographical, mobile, global).

% Are constrained in their strategic options, having to plan for limited conflicts and avoid escalation paths that would violate the taboo. Their professional identity is shaped by the normative limits on warfare, even if material capabilities suggest broader options.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, military_planners, payer,
    institutional, immediate, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international behavior by establishing a shared normative boundary against the use of total war, particularly nuclear weapons, thereby preventing escalation and fostering a more stable international environment.
% TRANSFER_FUNCTION: Transfers the option of total war from the realm of legitimate strategic choice to the realm of the unthinkable, imposing a normative cost on any actor contemplating it. This transfers security benefits to the global population by reducing existential risk.
% ABSENT_VOICES: Hardline strategists or revisionist states who might argue for the utility of total war as a legitimate instrument of policy are marginalized in international discourse. Their voices are suppressed by the normative consensus, making their arguments politically costly.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the strategic landscape would fundamentally shift. Nuclear weapons would become a more thinkable instrument of war, leading to increased proliferation, heightened risk of escalation, and a breakdown of existing arms control regimes. The world would reorganize around a more dangerous, less constrained form of conflict.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after Hiroshima and Nagasaki, and the realization that total war could lead to mutual annihilation, created an urgent need for a normative constraint on their use.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Cold War, international relations scholars (especially constructivists), and peace activists corroborate that the problem of nuclear annihilation remains live. The continued existence of nuclear arsenals and the potential for miscalculation or escalation mean the taboo's function is ongoing, as attested by UN resolutions and non-proliferation treaty reviews from outside the immediate nuclear powers.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).

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
 *   Extractiveness is low (0.2) because the primary 'cost' is the foregoing of a catastrophic option, which is a net benefit. Suppression is moderate (0.6) as the taboo requires active normative enforcement and marginalization of dissenting views. Theater ratio is moderate (0.4) as some diplomatic activity and rhetoric serve to reinforce the norm performatively, even when the underlying strategic realities might be more complex. Accessibility collapse is high (0.7) because the taboo makes total war largely unthinkable as a policy option. Resistance is low (0.1) because the taboo is widely accepted, though some revisionist actors may challenge it.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states experience this constraint as a limitation on their sovereign power and strategic options (payer seat), while the global population and non-nuclear states experience it as a vital safeguard against existential threat (beneficiary seat). International norm entrepreneurs actively work to maintain and strengthen the taboo, viewing it as a successful coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The global population and non-nuclear states are clear beneficiaries (low d) as they gain security from the taboo. International norm entrepreneurs are agenda-setters and beneficiaries (low d) as they shape and benefit from the norm's success. Nuclear weapon states and military planners are payers (high d) as they bear the cost of self-restraint and limited strategic options, even if they also benefit from global stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) remains highly live. The classification as Rope prevents mislabeling it as a Snare, which would imply pure extraction without genuine coordination. The low extractiveness and moderate suppression reflect the collective benefit derived from the normative constraint, even with the costs of maintaining it. The rising theater ratio over time suggests a growing performative aspect to norm maintenance, but the core function remains vital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causality,
    'To what extent is the absence of total war due to the nuclear taboo (normative constraint) versus nuclear deterrence (material constraint)?',
    'Comparative historical analysis of periods of high and low taboo salience, controlling for deterrence postures; counterfactual analysis of non-nuclear total war scenarios.',
    'If deterrence is the dominant factor, this constraint''s extractiveness and suppression might be lower (as the constraint is ''natural'' to the material reality), and its classification might shift towards Mountain or a less extractive Rope. If the taboo is dominant, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, empirical, 'Distinguishing the causal weight of normative taboo versus material deterrence in preventing total war.').

omega_variable(
    taboo_erosion_threshold,
    'At what point does the erosion of the nuclear taboo (e.g., through rhetorical challenges, near-use incidents, or proliferation) lead to a qualitative shift in the possibility space of total war?',
    'Event-history analysis of international crises, expert elicitation on ''red lines,'' and observation of state behavior following challenges to the taboo.',
    'If the taboo erodes significantly, the constraint could shift towards a Tangled Rope or Snare, as the costs of maintaining it rise and the benefits diminish, or even collapse entirely, leading to a ''world rearranges'' scenario with higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_erosion_threshold, empirical, 'Identifying the threshold at which the nuclear taboo''s effectiveness degrades.').

omega_variable(
    non_nuclear_powers_taboo_differential,
    'Does the nuclear taboo operate with the same force and structure for non-nuclear weapon states as it does for nuclear weapon states?',
    'Analysis of non-nuclear states'' strategic doctrines, public discourse, and participation in non-proliferation efforts, compared to nuclear states'' behavior.',
    'If the taboo is weaker or differently structured for non-nuclear states, it implies a more fragmented and potentially less stable global constraint, with differential directionality and extractiveness across actor types. This would suggest a more complex, multi-layered constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_powers_taboo_differential, empirical, 'Assessing the differential impact and enforcement of the nuclear taboo on non-nuclear weapon states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.08).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_possibility_space' kernel, focusing on the normative taboo. It is linked to the 'deterrence_equilibrium_reading' and 'space_contraction_reading' which offer alternative explanations for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
