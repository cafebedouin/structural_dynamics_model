% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Moral Personhood — Categorical Impermissibility of Property Status
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_status_kernel. It asserts that animals are moral persons with a
 *   basic right not to be property; property status itself is the injustice;
 *   all human use of animals is categorically impermissible regardless of
 *   welfare conditions. The constraint is the standing arrangement of animal
 *   property status — the legal, economic, and social regime that classifies
 *   animals as ownable, usable, disposable things. The abolitionist reading
 *   evaluates this arrangement as a snare: pure extraction enforced by law,
 *   with animals as the trapped victims and human industries as the
 *   beneficiaries. Welfare regulations are not mitigations; they are the
 *   constraint's legitimation layer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading of Animal Moral Personhood — Categorical Impermissibility of Property Status").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d').
narrative_ontology:cs_kernel_codification('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', distributed).
narrative_ontology:cs_authority_grounding('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', distributed).
narrative_ontology:cs_reading_relation('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', foundational, property_status_is_intrinsic_injustice).
narrative_ontology:cs_axiom_status(property_status_is_intrinsic_injustice, holdable).
narrative_ontology:cs_axiom_grounding('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', property_status_is_intrinsic_injustice, deontological).
narrative_ontology:cs_axiom('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', foundational, all_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(all_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', all_use_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', secondary, welfare_regulation_cannot_justify_use).
narrative_ontology:cs_axiom_status(welfare_regulation_cannot_justify_use, holdable).
narrative_ontology:cs_axiom_grounding('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', welfare_regulation_cannot_justify_use, deontological).
narrative_ontology:cs_reference_frame('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', animals_as_moral_persons_with_right_against_property).
narrative_ontology:cs_drift_state('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', contemporary_animal_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3ea21a5e-443e-46e5-b14a-0af6ebb1eb1d', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, exotic_pet_trade).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, entertainment_exploitation_sectors).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, legal_property_regime).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_as_moral_persons).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_in_agriculture).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_in_research).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_in_entertainment).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_in_pet_trade).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, animals_are_moral_persons).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, property_status_is_intrinsic_injustice).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, all_use_categorically_impermissible).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, welfare_regulation_cannot_justify_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every sentient animal whose life is structured by property status — farmed, experimented upon, exhibited, kept — bears the full cost of the constraint. They have no exit, no voice, no legal standing. Their bodies, labor, reproduction, and lives are the substance extracted. The abolitionist reading names them as the primary victims: the constraint *is* their subjection.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_as_moral_persons, payer,
    powerless, biographical, trapped, universal).

% Billions annually bred, confined, mutilated, and killed for food. The property regime makes their bodies commodities; welfare regulations govern *how* they are commodified, not *whether*. From the abolitionist seat, every husbandry practice is a rights violation; the constraint extracts their entire existence.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_in_agriculture, payer,
    powerless, biographical, trapped, universal).

% Tens of millions used in experiments where their suffering and death are protocolized. Property status enables their allocation as research tools; welfare oversight (IACUCs, 3Rs) regulates the *manner* of use. The abolitionist reading sees the oversight itself as legitimation theater — the constraint extracts their agency and lives.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_in_research, payer,
    powerless, biographical, trapped, universal).

% Animals in zoos, aquariums, circuses, film, racing, fighting — kept for human amusement. Property status permits their captivity and training; welfare standards address housing and handling. The abolitionist reading classifies the entire enterprise as rights-violating extraction; the constraint takes their freedom and natural behavior.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_in_entertainment, payer,
    powerless, biographical, trapped, universal).

% Breeding mills, exotic pet capture, puppy farms — animals produced and traded as property. Welfare laws address cruelty minimums; property law enables the market. The abolitionist reading sees the trade itself as the injustice: animals are not objects to be owned, bred, or sold. The constraint extracts their autonomy and subjects them to human preferences.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_in_pet_trade, payer,
    powerless, biographical, trapped, universal).

% Multi-trillion-dollar global industry built on animal property status. Collects the economic surplus from treating sentient beings as production units. Welfare regulations are absorbed as cost of business; the property regime is the asset. The abolitionist reading identifies this sector as the primary concentrated beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Universities, pharma, CROs, funding agencies — depend on unfettered access to animal bodies as research tools. Property status secures the supply chain; welfare oversight provides social license. The abolitionist reading sees the research establishment as a major beneficiary whose epistemic authority rests on the constraint's persistence.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Global wildlife trade, captive breeding for exotic pets, zoonotic disease vectors. Property status enables capture, breeding, and sale of wild animals. Welfare laws are minimal and poorly enforced. The abolitionist reading classifies this as extractive beneficiary — the constraint *is* the market.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, exotic_pet_trade, beneficiary,
    organized, biographical, mobile, global).

% Zoos, aquariums, circuses, racing, media production — profit from animal captivity and performance. Property status permits ownership and control; welfare standards provide accreditation cover. The abolitionist reading identifies these as beneficiaries extracting labor and life from animals who cannot consent.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, entertainment_exploitation_sectors, beneficiary,
    organized, biographical, mobile, global).

% The body of law — statutes, case law, regulatory codes — that defines animals as property, allocates ownership rights, and constrains welfare obligations within that frame. It administers the constraint, sets the boundaries of permissible use, and resists reclassification. The abolitionist reading sees the legal regime as the constraint's architect and enforcer.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_property_regime, agenda_setter,
    institutional, civilizational, analytical, universal).

% Organizations and individuals pursuing incremental welfare reforms within the property framework. They are excluded from the abolitionist reading's frame because their strategy *accepts* the property premise — they negotiate the terms of use, not the fact of it. The abolitionist reading sees them as structurally complicit; the welfarist reading sees abolitionists as counterproductive. They occupy different constraint stories.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfarist_advocates, excluded,
    organized, biographical, constrained, national).

% Activists, philosophers, lawyers working to dismantle animal property status. Their identity and professional commitment are fused to the abolitionist frame — exit means abandoning the core moral commitment. They administer the *counter-constraint* (the demand for abolition) but are themselves subject to the existing property regime's suppression (legal marginalization, professional risk).
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, agenda_setter,
    moderate, generational, identity_locked, global).

% Scholars analyzing the kernel's readings — abolitionist, welfarist, property — as competing normative frameworks. They do not bear the constraint's costs nor collect its benefits. Their seat is the engine's analytical perspective: computing per-seat classifications from the structural data authored here.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, moral_philosophy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The property regime coordinates human-animal relations by providing a single, legally stable framework for allocating, using, and disposing of animal bodies and labor. It solves the coordination problem of *who may do what to whom* by assigning all decision rights to owners. The abolitionist reading denies this is genuine coordination — it is a coordination *of extraction*, not of mutual benefit.
% TRANSFER_FUNCTION: Moves the entirety of animals' lives — their bodies, reproductive capacity, labor, freedom, and future — from animals to human owners and industries. The transfer is total: animals retain no rights against use. Welfare regulations transfer *some* protection back (minimums of food, space, pain relief) but only within the ownership frame; the net flow is overwhelmingly from animals to humans.
% ABSENT_VOICES: The animals themselves — the primary victims — are structurally excluded from the legal and political conversation. They cannot testify, sue, vote, or organize. Their interests are represented only by human advocates who disagree on strategy (abolition vs. welfare). Future generations of animals are also absent — the constraint's persistence condemns them to the same status.
% DISAPPEARANCE_RATIONALE: If animal property status vanished overnight, the global food system, biomedical research pipeline, pet trade, and entertainment industries would face immediate, total restructuring. Legal personhood for animals would require new frameworks for guardianship, habitat protection, and conflict resolution. Human diets, medical research paradigms, and economic sectors would reorganize around non-use. The world would rearrange fundamentally.
% FOUNDING_PROBLEM: The property regime was not 'built to solve a problem' in the engineering sense — it emerged from the historical entanglement of human domination, agricultural domestication, and Roman law's classification of living beings as *res* (things). The abolitionist reading argues the *actual* founding problem was: how to secure human control over animal bodies for food, labor, and status without moral or legal friction. The property classification *was* the solution — it rendered domination legally invisible.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (e.g., Francione, Cochrane, Donaldson & Kymlicka) documents that animal property status originates in domination, not mutual coordination. No non-abolitionist source corroborates a 'coordination problem' that property status solves for animals. The property reading asserts economic efficiency; the welfare reading asserts harm reduction — both *within* the property frame. Neither identifies a problem *for animals* that property status solves.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximal (0.95) because the constraint transfers *everything* from animals to humans — lives, freedom, bodies, futures — with zero return to animals. Suppression is very high (0.88) because the constraint depends on legal enforcement (property law, anti-cruelty statutes that exempt standard practices, ag-gag laws, research licensing) and on the physical impossibility of animal exit. Theater ratio is low but rising (0.15) because welfare reforms increasingly perform 'humane' cover for unchanged extraction. Accessibility collapse is moderate (0.35) because alternatives (veganism, non-animal research, sanctuary models) exist and are growing — the constraint does not *fully* collapse imagination of other worlds. Resistance is high (0.82) because the abolitionist movement, animal liberation direct action, and growing public opposition actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the animal-as-moral-person seat (powerless, trapped, universal scope), the constraint is a snare of total extraction. From the industry beneficiary seats (institutional, arbitrage, global scope), the same constraint appears as a rope or mountain — 'this is how the world works,' 'we provide care,' 'property rights enable efficiency.' From the legal property regime seat (agenda_setter, institutional, civilizational), the constraint is a scaffold it administers — but with no sunset clause, only entrenchment. The engine computes these divergences from the structural data; the abolitionist reading *claims* snare for all victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals (all victim seats) are full targets: d → 1.0. They bear 100% of the extraction with zero benefit, zero exit, zero power. Human industries (beneficiary seats) are full beneficiaries: d → 0.0. They collect the entire surplus, control the rules, and have arbitrage-grade exit (capital mobility, regulatory capture). The legal regime (agenda_setter) sits at d ≈ 0.1 — it administers the constraint and bears some enforcement cost, but its legitimacy depends on the constraint's persistence. Abolitionist advocates (agenda_setter, identity_locked) have d ≈ 0.3 — they oppose the constraint but their identity is fused to the struggle; exit means moral abandonment. Welfarist advocates (excluded) have d ≈ 0.4 — they operate within the constraint, negotiating its terms. Observers have d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading resolves mandatrophy by refusing the coordination-extraction confusion entirely. The property regime *presents* as coordination (stable allocation, welfare standards) but *operates* as extraction (total transfer from powerless to powerful). The mandate 'animals as property' has outlived any plausible coordination function for animals — it never had one. The constraint persists because beneficiaries control the agenda, not because it solves a shared problem. The abolitionist reading names the mandate as *the problem*, not a degraded solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_effect_on_abolition,
    'Do welfare reforms delay abolition by legitimating the property regime, or advance it by expanding the circle of moral concern and building institutional capacity?',
    'Longitudinal analysis of jurisdictions with strong welfare laws: track subsequent abolitionist policy adoption, public opinion shifts, and industry adaptation. Compare to jurisdictions without welfare reforms.',
    'If welfare reforms delay abolition, the welfare_reading''s constraint is a scaffold for the property regime. If they advance abolition, the welfare_reading is a genuine transitional scaffold toward abolition. The abolitionist reading''s strategic assessment depends on this empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_effect_on_abolition, empirical, 'Strategic valence of welfare reforms for abolitionist trajectory').

omega_variable(
    legal_personhood_feasibility,
    'Can legal personhood for animals be instantiated within existing legal systems, or does it require a constitutional rupture that current institutions will block?',
    'Track non-human personhood cases globally (e.g., Argentina''s orangutan, Colombia''s bear, India''s rivers, US habeas corpus petitions). Analyze judicial reasoning, legislative response, and enforcement outcomes.',
    'If personhood is incrementally achievable, the abolitionist constraint has a legal pathway. If it requires rupture, the constraint''s suppression profile is higher than measured — the legal regime will deploy full institutional force to block it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_personhood_feasibility, empirical, 'Institutional feasibility of the abolitionist remedy').

omega_variable(
    kernel_framing_underdetermination,
    'Does the animal_status_kernel admit a fourth reading — ''relational_obligation'' — where animals are neither property nor rights-bearing persons, but beings to whom humans have special duties arising from vulnerability and dependency?',
    'Map the philosophical literature (Korsgaard, Nussbaum, Palmer, Donaldson & Kymlicka''s citizenship model) to see if a distinct structural position exists that is not reducible to the three declared readings.',
    'If a fourth reading exists with distinct beneficiary/victim structure, the kernel decomposition is incomplete. The abolitionist reading''s claim to foreclose property_reading might not extend to this alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Completeness of the declared kernel reading set').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__abolitionist_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(anim_tr_t150, animal_status_kernel__abolitionist_reading, theater_ratio, 150, 0.14).
narrative_ontology:measurement(anim_tr_t200, animal_status_kernel__abolitionist_reading, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.98).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.97).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__abolitionist_reading, base_extractiveness, 100, 0.96).
narrative_ontology:measurement(anim_be_t150, animal_status_kernel__abolitionist_reading, base_extractiveness, 150, 0.95).
narrative_ontology:measurement(anim_be_t200, animal_status_kernel__abolitionist_reading, base_extractiveness, 200, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__abolitionist_reading, suppression_requirement, 100, 0.89).
narrative_ontology:measurement(anim_su_t150, animal_status_kernel__abolitionist_reading, suppression_requirement, 150, 0.88).
narrative_ontology:measurement(anim_su_t200, animal_status_kernel__abolitionist_reading, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__abolitionist_reading, 0.08).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_welfare_regulation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_agriculture_subsidies).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, biomedical_research_funding).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, wildlife_trade_law).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).

% DUAL FORMULATION NOTE:
% This is the abolitionist_reading of the animal_status_kernel. The property_reading and welfare_reading are sibling constraints with distinct ε values, beneficiary/victim structures, and claimed types. The abolitionist reading claims snare (pure extraction); the welfare_reading claims tangled_rope (coordination + extraction); the property_reading claims rope or mountain (coordination/natural law). They are linked by network.affects_constraints. The ε-invariance principle requires separate stories because the same observable (e.g., 'animal use') yields radically different ε under each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, moderate, 0.3).
constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
