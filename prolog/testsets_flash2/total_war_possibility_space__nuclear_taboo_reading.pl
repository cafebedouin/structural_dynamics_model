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
 *   human_readable: Nuclear Taboo on Total War (Normative Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'nuclear taboo' reading of the total war
 *   possibility space. It posits that total war, particularly nuclear war,
 *   remains materially possible but is normatively prohibited by a
 *   constructed international taboo. This taboo is maintained by active
 *   norm-entrepreneurs and global civil society, and it constrains nuclear
 *   powers from openly contemplating or threatening total war. The constraint
 *   is claimed as a Rope because it provides a genuine coordination function
 *   (global security) with relatively low extraction, but its persistence
 *   relies on active enforcement of the norm, making it susceptible to
 *   erosion if norm entrepreneurs exit or if the normative consensus weakens.
 *   The metrics reflect a constraint that has become more established and
 *   suppressive over time, but with a relatively stable, low level of
 *   extraction, consistent with a coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.3).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War (Normative Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '28e166cf-c2f5-40db-9a11-8630c30e0d09').
narrative_ontology:cs_kernel_codification('28e166cf-c2f5-40db-9a11-8630c30e0d09', implicit).
narrative_ontology:cs_authority_grounding('28e166cf-c2f5-40db-9a11-8630c30e0d09', practice).
narrative_ontology:cs_interpretation_layer_present('28e166cf-c2f5-40db-9a11-8630c30e0d09').
narrative_ontology:cs_reading_relation('28e166cf-c2f5-40db-9a11-8630c30e0d09', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('28e166cf-c2f5-40db-9a11-8630c30e0d09', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('28e166cf-c2f5-40db-9a11-8630c30e0d09', foundational, total_war_is_normatively_unacceptable).
narrative_ontology:cs_axiom_status(total_war_is_normatively_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('28e166cf-c2f5-40db-9a11-8630c30e0d09', total_war_is_normatively_unacceptable, deontological).
narrative_ontology:cs_axiom('28e166cf-c2f5-40db-9a11-8630c30e0d09', foundational, norms_constrain_material_power).
narrative_ontology:cs_axiom_status(norms_constrain_material_power, holdable).
narrative_ontology:cs_axiom_grounding('28e166cf-c2f5-40db-9a11-8630c30e0d09', norms_constrain_material_power, empirically_contingent).
narrative_ontology:cs_reference_frame('28e166cf-c2f5-40db-9a11-8630c30e0d09', post_hiroshima_normative_shift).
narrative_ontology:cs_drift_state('28e166cf-c2f5-40db-9a11-8630c30e0d09', contemporary_geopolitical_flux, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('28e166cf-c2f5-40db-9a11-8630c30e0d09', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_civil_society).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, revisionist_nuclear_powers).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, military_planners_of_total_war).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_ir_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_diffusion_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and reinforce the nuclear taboo through diplomatic initiatives, public campaigns, and institutional pressure. They invest political capital in maintaining the normative barrier against total war, especially nuclear war. Their exit would weaken the taboo's enforcement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_norm_entrepreneurs, agenda_setter,
    organized, generational, constrained, global).

% Benefit from the normative prohibition on total war, which reduces the existential threat from nuclear-armed states. They actively support non-proliferation regimes and disarmament efforts, reinforcing the taboo as a collective security measure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    moderate, biographical, constrained, global).

% Are constrained by the taboo from openly contemplating or threatening total war, even when they possess the material capability. They face international opprobrium and potential sanctions if they violate the norm. Their identity as 'responsible' nuclear powers is tied to upholding the taboo, even if they chafe under its restrictions.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, revisionist_nuclear_powers, payer,
    institutional, generational, identity_locked, global).

% Are professionally constrained from developing or advocating for total war scenarios, despite the material possibility. The taboo shapes strategic doctrine and resource allocation, making such planning politically and professionally untenable. Their professional identity is fused with the prevailing strategic culture.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, military_planners_of_total_war, payer,
    institutional, immediate, identity_locked, national).

% Benefits from the reduced likelihood of total war. They act as a diffuse but powerful force for norm reinforcement, mobilizing public opinion against any perceived erosion of the taboo. Their collective action provides a bottom-up pressure on states.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_civil_society, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international behavior by establishing a shared normative boundary against the use of total war, particularly nuclear weapons, fostering a collective sense of restraint and responsibility among states.
% TRANSFER_FUNCTION: Transfers the 'cost' of foregone strategic options (total war) from nuclear-armed states and military planners to the international community, in exchange for enhanced global security and reduced existential risk. It also transfers legitimacy and influence to norm-upholding actors.
% ABSENT_VOICES: Hardline strategists or extreme nationalist factions within nuclear powers, who might argue for the utility of total war as a legitimate strategic option, are marginalized or silenced by the prevailing normative consensus. Their views are excluded from mainstream policy discourse.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished, the strategic landscape would fundamentally shift. Nuclear weapons would become 'normal' weapons, increasing the risk of proliferation and use. States would re-evaluate their security doctrines, potentially leading to a return to pre-taboo strategic thinking and a much higher probability of large-scale conflict.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after Hiroshima and Nagasaki, which made the concept of 'total war' strategically and morally untenable, necessitating a new framework for international security.
% FOUNDING_PROBLEM_CORROBORATION: International treaties (NPT), UN resolutions, and the consistent rhetoric of most world leaders corroborate the ongoing problem of nuclear proliferation and the need for normative restraint. Historians and political scientists outside the immediate beneficiary group attest to the historical development and persistence of the taboo.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.3) because the primary 'cost' is the foregone option of total war, which is a collective benefit. Suppression is high (0.7) because the taboo requires active diplomatic, political, and social enforcement to prevent its erosion. Accessibility collapse is high (0.75) because the taboo makes the option of total war largely unthinkable in mainstream discourse. Resistance is low (0.1) because the taboo is widely accepted, though some revisionist actors may subtly challenge its boundaries. Theater ratio is low (0.2) as the norm is genuinely upheld, though some performative elements exist in diplomatic rhetoric. The time series shows a strengthening of the taboo (rising suppression and extractiveness as the norm became more entrenched) over the Cold War, with some stabilization and minor fluctuations in the post-Cold War era.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of norm entrepreneurs and non-nuclear states, the taboo is a vital, beneficial coordination mechanism. From the perspective of revisionist nuclear powers, it is a constraint on their sovereignty and strategic options, though one they largely adhere to due to the high costs of violation. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International norm entrepreneurs and global civil society are beneficiaries and agenda-setters, actively shaping and reinforcing the taboo. Non-nuclear states are clear beneficiaries, gaining security from the restraint of nuclear powers. Revisionist nuclear powers and military planners are payers, constrained by the taboo from exercising their full material capabilities. Their 'identity-locked' exit reflects the high cost of violating the norm, which would entail severe international isolation and reputational damage.
 *
 * MANDATROPHY ANALYSIS:
 *   The nuclear taboo's mandate remains live: the problem of existential nuclear threat persists. The constraint prevents mislabeling genuine collective security (Rope) as pure extraction (Snare) by acknowledging the shared benefit of avoiding total war, while still recognizing the active enforcement required to maintain the norm. The low extractiveness and high suppression are consistent with a robust, actively maintained coordination mechanism that imposes costs for collective good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_strength_measurement,
    'How can the ''strength'' of the nuclear taboo be empirically measured, independent of observed non-use?',
    'Content analysis of strategic doctrines, diplomatic rhetoric, public opinion surveys, and analysis of ''near-miss'' incidents to assess the salience of normative considerations versus material deterrence.',
    'A robust, independent measure of taboo strength would provide stronger evidence for the constructivist claim, potentially shifting the classification towards a more stable Rope or even a Mountain (if truly internalized). Weak measures would favor deterrence-based readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_strength_measurement, empirical, 'Empirical measurement of normative strength vs. material factors.').

omega_variable(
    taboo_vs_deterrence_causality,
    'Is the non-use of nuclear weapons primarily due to the normative taboo, or to material deterrence (mutual assured destruction)?',
    'Counterfactual historical analysis, comparative case studies of states with varying nuclear doctrines and normative commitments, and expert elicitation on decision-making during crises.',
    'If deterrence is primary, this reading''s extractiveness might be lower (as the constraint is more ''natural'' from a realist perspective) and its classification might shift towards a Mountain or a different type of Rope. If the taboo is primary, the current classification as a Rope with active enforcement is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, conceptual, 'Causal primacy of normative taboo vs. material deterrence.').

omega_variable(
    non_nuclear_states_constraint_divergence,
    'Does the nuclear taboo operate differently for non-nuclear states considering proliferation, compared to existing nuclear powers?',
    'Comparative analysis of non-proliferation efforts and the motivations of ''threshold states'' versus the strategic behavior of established nuclear powers.',
    'If the constraint is significantly weaker or different for non-nuclear states, it suggests the taboo is not universally applied or internalized, potentially increasing extractiveness for existing nuclear powers (who bear the cost of restraint) and shifting the classification towards a Tangled Rope or Snare for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_states_constraint_divergence, empirical, 'Differential impact of the taboo on nuclear vs. non-nuclear states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_possibility_space' kernel. It focuses on the normative prohibition (taboo) as the primary constraint, distinct from material deterrence or strategic space contraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
