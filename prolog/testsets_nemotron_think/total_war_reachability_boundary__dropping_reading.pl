% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary — Dropping Probability Reading
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint story captures the 'dropping_reading' of the
 *   total_war_reachability_boundary kernel — the position that total war has
 *   declined in probability since the Cold War peak but remains structurally
 *   reachable, and that nuclear deterrence functions as a coordination
 *   equilibrium (rope) rather than a natural law (mountain). The constraint
 *   is the standing arrangement of mutual vulnerability maintained by
 *   nuclear-armed states. Its extractiveness derives from the permanent
 *   imposition of existential risk on populations who cannot exit, while its
 *   coordination function is the avoidance of deliberate great-power war. The
 *   reading acknowledges the probability drop (fewer crises, lower alert
 *   levels, reduced arsenals) but insists the reachability boundary has not
 *   contracted to zero — the coordination equilibrium is maintained by active
 *   enforcement (modernization, exercises, signaling) and could fail.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.55).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary — Dropping Probability Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'a3835d49-06bc-4133-b836-dd8cf20b4e01').
narrative_ontology:cs_kernel_codification('a3835d49-06bc-4133-b836-dd8cf20b4e01', distributed).
narrative_ontology:cs_authority_grounding('a3835d49-06bc-4133-b836-dd8cf20b4e01', extraction).
narrative_ontology:cs_interpretation_layer_present('a3835d49-06bc-4133-b836-dd8cf20b4e01').
narrative_ontology:cs_reading_relation('a3835d49-06bc-4133-b836-dd8cf20b4e01', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3835d49-06bc-4133-b836-dd8cf20b4e01', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('a3835d49-06bc-4133-b836-dd8cf20b4e01', foundational, deterrence_is_coordination_equilibrium_not_natural_law).
narrative_ontology:cs_axiom_status(deterrence_is_coordination_equilibrium_not_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('a3835d49-06bc-4133-b836-dd8cf20b4e01', deterrence_is_coordination_equilibrium_not_natural_law, empirically_contingent).
narrative_ontology:cs_axiom('a3835d49-06bc-4133-b836-dd8cf20b4e01', foundational, total_war_reachability_persists_despite_probability_decline).
narrative_ontology:cs_axiom_status(total_war_reachability_persists_despite_probability_decline, holdable).
narrative_ontology:cs_axiom_grounding('a3835d49-06bc-4133-b836-dd8cf20b4e01', total_war_reachability_persists_despite_probability_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('a3835d49-06bc-4133-b836-dd8cf20b4e01', mutual_assured_destruction_stability).
narrative_ontology:cs_drift_state('a3835d49-06bc-4133-b836-dd8cf20b4e01', post_cold_war_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a3835d49-06bc-4133-b836-dd8cf20b4e01', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_security_establishments).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, deterrence_theory_practitioners).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, future_generations_existential_risk).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, deterrence_stability_as_coordination_equilibrium).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutual_assured_destruction_credibility_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and set declaratory doctrine, force posture, and targeting policy. They administer the deterrence equilibrium through continuous capability maintenance, signaling, and crisis management. They benefit from the credibility of mutual vulnerability — it constrains adversary behavior and confers great-power status. Exit from the deterrence framework would require verified disarmament or regime change; both are structurally constrained by the equilibrium itself.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_armed_states, beneficiary).

% The complex of military commands, weapons laboratories, intelligence agencies, and defense industries that design, maintain, and operate nuclear forces. Their budgets, institutional missions, and professional identities are constituted by the deterrence arrangement. They gain resources, authority, and career coherence from the credibility requirement. Exit would mean institutional dissolution or radical repurposing — professional identity is fused to the deterrence function.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_security_establishments, beneficiary,
    organized, biographical, identity_locked, national).

% Academic strategists, think-tank analysts, and government advisers whose expertise is certified by the deterrence paradigm. They produce the intellectual infrastructure that legitimizes the arrangement. Their epistemic authority, funding streams, and professional standing depend on the coordination-equilibrium framing remaining dominant. Exit would require abandoning the paradigm that defines their field.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, deterrence_theory_practitioners, beneficiary,
    organized, biographical, identity_locked, global).

% Civilian populations in nuclear-armed states and their allies who live under the permanent threat of deliberate or accidental nuclear use. They bear the existential risk, the opportunity costs of nuclear spending, and the psychological burden of vulnerability. They have no meaningful exit — relocation does not remove fallout or climate effects, and political channels for disarmament are structurally blocked by the equilibrium's own logic.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, generational, trapped, global).

% States party to the NPT that forswore nuclear weapons in exchange for disarmament commitments that have not been fulfilled. They bear the risks of nuclear conflict without the deterrent benefits, and their security is hostage to escalation dynamics they cannot influence. Exit options are limited: acquiring nuclear weapons triggers sanction and preventive war risks; relying on extended deterrence perpetuates dependency.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states, excluded).

% The abstract but structurally real constituency that inherits the accumulated risk of deterrence failure, nuclear winter, and long-term radiological contamination. They cannot consent, organize, or exit. Their inclusion as a stakeholder is a structural acknowledgment that the constraint's extraction extends beyond the present.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, future_generations_existential_risk, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(total_war_reachability_boundary__dropping_reading, future_generations_existential_risk).

% Diplomatic actors, NGOs, and civil-society networks pushing for risk reduction, no-first-use, and disarmament. They are excluded from the core deterrence decision-making circle; their proposals are filtered through the credibility requirement that the equilibrium itself enforces. They can mobilize public opinion and achieve treaty agreements, but the coordination equilibrium reasserts itself after each cycle.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_advocates, excluded,
    organized, biographical, mobile, global).

% Scholars and analysts who study the deterrence arrangement from outside the practitioner community. They see the full structure — the coordination function, the extraction, the identity locks — but their analysis does not alter the constraint's operation. Their exit is analytical: they can change frameworks, but the constraint persists regardless.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_studies_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the security dilemma among nuclear-armed adversaries by establishing a mutually recognized vulnerability that makes major war irrational — a coordination equilibrium where each side's restraint is the price of its own survival.
% TRANSFER_FUNCTION: Moves existential risk and resource allocation from nuclear-armed states' security establishments to their populations and non-nuclear states. The coordination benefit (avoided great-power war) accrues to the armed states; the costs (permanent catastrophe risk, diversion of resources, foregone disarmament) are externalized.
% ABSENT_VOICES: Future generations (structurally excluded by time), populations in the global south disproportionately affected by nuclear winter scenarios (excluded by geopolitical marginalization), and radiation-exposed communities from testing and accidents (excluded by secrecy and liability shields). They would object to the permanent risk imposition but have no seat at the deterrence table.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished overnight, nuclear-armed states would face immediate pressure to either disarm verifiably or re-arm competitively. The security architecture of the post-1945 order would collapse; alliance structures, extended deterrence commitments, and non-proliferation bargains would all require renegotiation. The world would not stay the same — but whether it rearranges toward disarmament or cascading proliferation is contested.
% FOUNDING_PROBLEM: How to prevent great-power war in an anarchic international system where offensive military technology had made total war unwinnable but still thinkable. The 1945 atomic bombings demonstrated that the next total war would be civilization-ending; deterrence was built as the coordination mechanism to make that war unreachable in practice.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear-armed states and their security establishments attest the founding problem is live — great-power competition persists, and deterrence remains the only proven barrier. Arms control advocates, humanitarian initiative states, and independent strategic analysts attest the problem has mutated: the risk of accidental or unauthorized use, the erosion of crisis stability, and the failure of disarmament obligations mean the arrangement now generates the very catastrophe it was built to prevent. The 2017 TPNW negotiation and the 2022-2024 nuclear threats in the Ukraine conflict are cited by non-beneficiary parties as evidence the founding problem has been inverted.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the constraint externalizes existential risk onto powerless populations while concentrating security benefits on the armed states. Suppression (0.55) is moderate — the constraint does not primarily rely on coercion but on the structural logic of mutual vulnerability; however, the credibility requirement suppresses disarmament advocacy and alternative security architectures. Theater ratio (0.28) reflects that the coordination function (war avoidance) is real but increasingly performed through ritualized signaling rather than genuine stability. Accessibility collapse (0.42) is moderate — alternatives (disarmament, common security, non-nuclear deterrence) exist but are rendered inaccessible by the equilibrium's own incentive structure. Resistance (0.48) is significant — the TPNW, humanitarian initiative, and recurring disarmament movements demonstrate sustained pushback, but the equilibrium absorbs it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, the constraint appears as a successful rope — a coordination equilibrium that has prevented great-power war for 79 years. From the payer seats, it appears as a snare — an extraction mechanism that imposes unbounded catastrophic risk without consent. The engine computes this divergence from the structural data: the same constraint is rope for the coordinators and snare for the coordinated. The claimed_type (tangled_rope) captures the hybrid reality — genuine coordination function AND asymmetric extraction — which neither pure seat captures alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states and their security establishments are structural beneficiaries (d ≈ 0.1-0.2): they collect the coordination benefit (survival, status, resources) and control the rules. Populations under threat and non-nuclear states are structural targets (d ≈ 0.8-0.9): they bear the risk and cost with no control over the arrangement. The identity_locked exit for security establishments and deterrence practitioners reflects professional identity fusion — their expertise and institutional purpose are constituted by the constraint. The analytical observer seat (d = 0.5) sees the full structure without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power total war) is contested: beneficiaries say it persists; payers say the arrangement now generates the risk it was built to contain. This mismatch (status=contested, disappearance=world_rearranges) flags potential mandatrophy — the arrangement may have outlived its coordinating function relative to its extractive overhead. The theater_ratio rise from 0.15 to 0.28 suggests increasing performative maintenance. The extractiveness rise from 0.52 (1991) to 0.68 (2024) despite probability drop suggests extraction accumulation on a stable coordination base — classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the dropping_reading a distinct constraint from the contraction_reading and contingent_reachability_reading, or are they observable-dependent classifications of the same underlying arrangement?',
    'Apply the ε-invariance test: if measuring deterrence stability via crisis frequency yields low ε but measuring via modernization spending yields high ε, they are different constraints. The dropping_reading authors ε for the standing mutual-vulnerability arrangement as a coordination game with defection risk.',
    'If the readings are one constraint, the corpus double-counts. If they are three, each gets its own ε, stakeholders, and classification — linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three kernel readings are structurally distinct constraints or measurement perspectives on one constraint.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Where does the genuine coordination function of mutual vulnerability end and the extractive maintenance of nuclear establishments begin?',
    'Counterfactual: if arsenals were reduced to minimum deterrence (hundreds not thousands of warheads), would the coordination equilibrium hold? If yes, the excess is extraction. If no, the current force structure is the coordination cost.',
    'A wide gap reclassifies more of the constraint as snare; a narrow gap supports the tangled_rope claim with a smaller extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'The structural separability of deterrence''s coordination function from its extractive overhead.').

omega_variable(
    probability_drop_vs_reachability_persistence,
    'Does the historical decline in deliberate-use probability represent a genuine contraction of the reachability boundary, or a temporary fluctuation within a stable coordination equilibrium?',
    'Track whether the probability drop correlates with structural changes (arsenal reductions, doctrinal shifts, communication links) or with contingent factors (leader psychology, absence of flashpoints). The 2022-2024 nuclear signaling in the Ukraine conflict tests the stability of the drop.',
    'If structural, the boundary has genuinely contracted (supporting contraction_reading). If contingent, the equilibrium remains fragile (supporting dropping_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_drop_vs_reachability_persistence, empirical, 'Whether the probability decline is structural or contingent.').

omega_variable(
    identity_lock_mechanism_security_establishments,
    'Is the identity_locked exit for nuclear security establishments professional (career path dependence), institutional (organizational mission fusion), or ideological (deterrence as constitutive worldview)?',
    'Survey career trajectories of former nuclear commanders and lab directors: do they advocate for disarmament after exit (professional), defend the paradigm from outside (ideological), or disappear from the discourse (institutional)?',
    'Professional lock implies exit is possible with career transition; ideological lock implies the constraint constitutes the agent''s epistemic framework; institutional lock implies the organization cannot reform without dissolution. Each implies different classification stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_security_establishments, empirical, 'The specific identity-fusion mechanism binding security establishments to the deterrence arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_dr_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(twrb_dr_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.22).
narrative_ontology:measurement(twrb_dr_tr_t1985, total_war_reachability_boundary__dropping_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(twrb_dr_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.31).
narrative_ontology:measurement(twrb_dr_tr_t2001, total_war_reachability_boundary__dropping_reading, theater_ratio, 2001, 0.29).
narrative_ontology:measurement(twrb_dr_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(twrb_dr_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(twrb_dr_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(twrb_dr_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.72).
narrative_ontology:measurement(twrb_dr_be_t1985, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(twrb_dr_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.52).
narrative_ontology:measurement(twrb_dr_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(twrb_dr_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(twrb_dr_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(twrb_dr_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(twrb_dr_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(twrb_dr_su_t1985, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(twrb_dr_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(twrb_dr_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.51).
narrative_ontology:measurement(twrb_dr_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(twrb_dr_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.1).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, extended_deterrence_commitments).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, strategic_arms_control_treaties).

% DUAL FORMULATION NOTE:
% This constraint (dropping_reading) and its siblings (contraction_reading, contingent_reachability_reading) form a constraint family decomposing the natural-language concept 'nuclear deterrence stability'. The contraction_reading claims the reachability boundary has moved to zero (mountain); the contingent_reachability_reading claims the current low-reachability state is a piton (atrophied capability); this reading claims the boundary persists at low probability with active maintenance (tangled_rope). Their ε values differ: contraction_reading ε ≈ 0.05 (negligible extraction from a natural law); contingent_reachability_reading ε ≈ 0.35 (piton extraction from theatrical maintenance); dropping_reading ε = 0.68 (tangled_rope extraction from active equilibrium maintenance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, organized, 0.2).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerless, 0.9).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
