% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   climate_response_imperative kernel: the degrowth reading, which holds
 *   that mitigation and adaptation both require structural economic
 *   transformation in the Global North — reduced consumption, working-time
 *   reduction, redistribution, and post-growth institution-building — rather
 *   than relying on continued growth decoupled via technology (the
 *   mitigation_priority_reading) or on resilience spending without addressing
 *   throughput (the adaptation_priority_reading). Under this reading,
 *   present-day Global North working, middle, and carbon-industry populations
 *   enter the victim set directly, bearing reduced consumption and labor
 *   reorganization, while future generations globally and present Global
 *   South populations are the beneficiaries. This reading explicitly declines
 *   reliance on unproven large-scale carbon dioxide removal, treating that
 *   reliance as itself a risk-transfer onto future generations rather than a
 *   solution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.58).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '454cc305-c6ca-4697-9c53-479fa7462e87').
narrative_ontology:cs_kernel_codification('454cc305-c6ca-4697-9c53-479fa7462e87', distributed).
narrative_ontology:cs_authority_grounding('454cc305-c6ca-4697-9c53-479fa7462e87', distributed).
narrative_ontology:cs_reading_relation('454cc305-c6ca-4697-9c53-479fa7462e87', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('454cc305-c6ca-4697-9c53-479fa7462e87', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('454cc305-c6ca-4697-9c53-479fa7462e87', foundational, growth_compatible_decoupling_insufficient_at_required_speed).
narrative_ontology:cs_axiom_status(growth_compatible_decoupling_insufficient_at_required_speed, holdable).
narrative_ontology:cs_axiom_grounding('454cc305-c6ca-4697-9c53-479fa7462e87', growth_compatible_decoupling_insufficient_at_required_speed, empirically_contingent).
narrative_ontology:cs_axiom('454cc305-c6ca-4697-9c53-479fa7462e87', foundational, present_generation_consumption_may_be_justly_curtailed_for_future_and_global_south_benefit).
narrative_ontology:cs_axiom_status(present_generation_consumption_may_be_justly_curtailed_for_future_and_global_south_benefit, holdable).
narrative_ontology:cs_axiom_grounding('454cc305-c6ca-4697-9c53-479fa7462e87', present_generation_consumption_may_be_justly_curtailed_for_future_and_global_south_benefit, deontological).
narrative_ontology:cs_axiom('454cc305-c6ca-4697-9c53-479fa7462e87', secondary, unproven_carbon_removal_is_illegitimate_risk_transfer_not_a_mitigation_substitute).
narrative_ontology:cs_axiom_status(unproven_carbon_removal_is_illegitimate_risk_transfer_not_a_mitigation_substitute, holdable).
narrative_ontology:cs_axiom_grounding('454cc305-c6ca-4697-9c53-479fa7462e87', unproven_carbon_removal_is_illegitimate_risk_transfer_not_a_mitigation_substitute, instrumental).
narrative_ontology:cs_reference_frame('454cc305-c6ca-4697-9c53-479fa7462e87', growth_as_default_economic_organizing_principle).
narrative_ontology:cs_drift_state('454cc305-c6ca-4697-9c53-479fa7462e87', post_ipcc_ar6_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('454cc305-c6ca-4697-9c53-479fa7462e87', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, post_growth_institution_builders).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_working_and_middle_classes).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, carbon_intensive_industry_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, degrowth_feasibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would bear reduced consumption, shorter working hours re-organized around non-market provisioning, and redistribution of accumulated wealth and consumption headroom toward global adaptation finance and domestic post-growth transition. Cannot exit the jurisdiction or the transformation program without losing access to the social safety nets being restructured around it; mobility is limited by wages, housing, and citizenship.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_working_and_middle_classes, payer,
    moderate, biographical, constrained, national).

% Face direct job loss or forced retraining as fossil-intensive sectors are wound down under a post-growth mandate. Have little say in the pace of transition and are geographically and skill-concentrated in ways that make relocation or reskilling costly and slow.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_intensive_industry_workers, payer,
    powerless, biographical, trapped, regional).

% Absorb higher prices, rationed access, or normatively mandated consumption reduction across housing, transport, and goods. Their exit option is largely notional — emigration to a jurisdiction without comparable transformation pressure, which most cannot exercise.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, national).

% Inherit a stabilized climate and functioning post-growth institutions if the transformation succeeds, but have no voice in whether it is undertaken, no vote, and no capacity to consent to the terms being set on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Receive the primary adaptation and mitigation benefit if Global North consumption and emissions fall and redistributed resources flow south, since they bear disproportionate physical climate exposure with least historical responsibility. Cannot compel Global North transformation and depend entirely on Global North domestic political will.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Design and advocate the policy architecture — wealth caps, working-time reduction, public provisioning systems, redistribution mechanisms — and administer pilot programs. They set the terms of the transformation, staff its institutions, and can revise its pace and shape; unlike those subject to it, their material position is not directly at stake in the same way.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_institution_builders, agenda_setter,
    organized, generational, mobile, national).

% Would lose asset value and political influence under a genuine post-growth transition and are structurally opposed to it, but their objections are treated as illegitimate rent-defense within the degrowth framing rather than admitted as a competing claim to be weighed. They retain capital mobility other stakeholders lack.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_and_growth_dependent_capital, excluded,
    powerful, biographical, arbitrage, global).

% Model the feasibility, distributional incidence, and political viability of degrowth pathways versus green-growth or technology-led pathways, without a direct stake in either outcome.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_economists_and_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a deliberate, planned contraction and redistribution of Global North material throughput so that aggregate emissions and resource extraction fall fast enough to preserve a livable climate for future generations and reduce the adaptation burden imposed on the Global South, without relying on unproven large-scale carbon removal.
% TRANSFER_FUNCTION: Moves consumption capacity, working time, and accumulated wealth from present Global North populations (especially carbon-intensive sectors) toward future generations globally and toward present Global South populations, via reduced extraction, redistribution mechanisms, and adaptation finance.
% ABSENT_VOICES: Future generations and the global poor most exposed to climate damage have no seat in the political processes that would decide whether and how fast this transformation happens; fossil-capital and growth-dependent industry are excluded from having their objections treated as legitimate rather than as obstruction.
% DISAPPEARANCE_RATIONALE: Proponents hold that without this transformation the world rearranges catastrophically via crossed climate tipping points, mass displacement, and accelerating adaptation costs that dwarf any near-term consumption sacrifice. Opponents and rival-reading advocates hold that mitigation via technology and markets, or adaptation-first resilience spending, could substitute for the degrowth pathway without the same distributional shock to Global North populations — so whether 'the world rearranges' depends on which reading is adopted, which is exactly the kernel-level dispute this constraint is one reading of.
% FOUNDING_PROBLEM: Global emissions and resource throughput, concentrated overwhelmingly in Global North historical and current consumption, are incompatible with remaining within safe planetary boundaries under any plausible technology-substitution or efficiency-only pathway; the founding problem is that growth-compatible mitigation appears mathematically insufficient at the required speed.
% FOUNDING_PROBLEM_CORROBORATION: Independent physical-science assessments (IPCC working group reports, planetary boundaries research) corroborate that current decoupling rates are insufficient for 1.5-2C pathways without either extraordinary technological acceleration or absolute consumption reduction — this comes from bodies outside the degrowth advocacy movement itself. However, mainstream economic institutions (IMF, OECD, most national treasuries) dispute that degrowth specifically, rather than green growth or carbon pricing, is the necessary response, so the specific transformation-is-necessary claim is corroborated only partially and remains actively contested outside the beneficiary/advocate set.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high and rising (0.35 to 0.58) because the transformation, if implemented, imposes real and growing material costs on identifiable present-day populations even though the transfer serves a genuine coordination function (planetary boundary maintenance). Suppression rises over the interval (0.30 to 0.62) because a program of this scope cannot be sustained on voluntary participation alone — it requires increasing regulatory, fiscal, and normative enforcement (wealth taxation, working-time mandates, consumption caps) as the pace of implementation increases and resistance from displaced industries and consumption-habituated populations grows. Theater ratio starts moderate (0.5) reflecting early-stage symbolic commitments (declarations, pledges, pilot programs) and falls somewhat (to 0.4) as programs mature into substantive policy, though it remains non-trivial because political cover mechanisms (carbon offsets marketed as equivalent, greenwashed corporate pledges) persist alongside genuine transformation. Accessibility collapse is moderate-low (0.35) because meaningful alternative pathways (green growth, adaptation-first) remain live and contested rather than foreclosed. Resistance is high (0.78) reflecting the genuine, organized opposition from affected industries, consumption-habituated populations, and rival-reading advocates.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-growth institution builders are the agenda-setters: they design the architecture and are organizationally mobile even as the populations subject to the transformation are not. Global North working/middle classes, carbon-industry workers, and consumers are payers with constrained or trapped exit — they cannot easily relocate away from the jurisdiction implementing the transformation, nor can they opt out of labor-market restructuring. Future generations and Global South populations are structural beneficiaries with zero voice and zero exit — they cannot advocate for themselves in present-day political processes, which is exactly the absent-voices structure this reading foregrounds. Fossil-capital is excluded rather than coordinated: their objections are treated within this reading as illegitimate defense of extraction rather than a claim requiring accommodation, which is a genuine structural asymmetry worth naming even from within the reading that holds this exclusion is justified.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that growth-compatible decoupling is insufficient at required speed — retains substantial corroboration from physical-science bodies outside the degrowth advocacy movement, which argues against mandatrophy in the strict sense (the problem is not dead). However, the specific claim that DEGROWTH SPECIFICALLY (rather than green growth, carbon pricing, or adaptation-first approaches) is the necessary response is corroborated only partially and disputed by mainstream economic institutions — meaning the founding-problem-status is honestly contested, not settled in the reading's favor. This is precisely the kind of claim the kernel decomposition exists to isolate: the physical science warrant for SOME transformation is much stronger than the specific warrant for THIS transformation's particular institutional prescription.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_vs_growth_compatible_mitigation,
    'Is genuine emissions reduction at the required pace achievable within a growth-compatible framework (decoupling, technology substitution, carbon pricing), or does physical throughput reduction require abandoning growth as the organizing economic objective?',
    'Empirical tracking of decoupling rates (absolute vs. relative decoupling) against required emissions trajectories over the next one to two decades; comparative study of jurisdictions pursuing green-growth versus post-growth policy architectures.',
    'If growth-compatible decoupling proves sufficient, this reading''s core premise (that transformation beyond growth is necessary) is undermined and the mitigation_priority_reading''s premise is vindicated instead; if decoupling proves structurally insufficient, this reading''s premise is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_feasibility_vs_growth_compatible_mitigation, empirical, 'Whether the degrowth reading''s core empirical premise about decoupling limits holds up against evidence.').

omega_variable(
    distributional_design_variance,
    'Does the degrowth transformation, as actually implemented, distribute its costs progressively (wealthy consumers and carbon-intensive capital bear the burden) or regressively (working and middle classes bear disproportionate costs while wealth escapes via capital mobility)?',
    'Track actual policy design and incidence analysis of implemented degrowth-adjacent policies (wealth taxes, carbon rationing, working-time reduction) for distributional outcomes within the Global North.',
    'Progressive implementation would support a rope-leaning reading (genuine internal coordination with fairly distributed costs); regressive implementation would support the tangled_rope or even snare reading for Global North working classes specifically, even as the global coordination function toward future generations and Global South remains intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_design_variance, empirical, 'Whether the transformation''s domestic cost distribution is progressive or regressive in practice.').

omega_variable(
    consent_of_future_generations,
    'Can present-day political processes legitimately bind future generations and non-voting populations (Global South) to a transformation program they cannot consent to or object to, even when the program is designed for their benefit?',
    'This is not empirically resolvable; it depends on the theory of intergenerational and transnational political legitimacy adopted, which is itself contested across democratic theory traditions.',
    'If legitimate binding without consent is rejected as a normative matter, the beneficiary-designation of future generations and Global South does not by itself justify the constraint''s extraction from present Global North populations, regardless of the physical-science merits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_of_future_generations, preference, 'Whether beneficiary status without voice or consent is a sufficient legitimacy basis for this reading''s coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(clim_tr_t6, climate_response_imperative__degrowth_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(clim_tr_t12, climate_response_imperative__degrowth_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(clim_tr_t18, climate_response_imperative__degrowth_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__degrowth_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_response_imperative__degrowth_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_response_imperative__degrowth_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(clim_be_t18, climate_response_imperative__degrowth_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__degrowth_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_imperative__degrowth_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(clim_su_t12, climate_response_imperative__degrowth_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(clim_su_t18, climate_response_imperative__degrowth_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__degrowth_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_imperative kernel. mitigation_priority_reading and adaptation_priority_reading are separate constraint files with their own ε, stakeholders, and classification. The degrowth_reading differs from both in its beneficiary/victim structure (present Global North populations as victims, future generations and Global South as beneficiaries) and in its explicit rejection of unproven carbon removal as a mitigation substitute. Network edges here mark that policy adoption of this reading structurally influences the legitimacy and resource availability of the sibling readings — resources and political capital committed to degrowth transformation reduce what is available for technology-led mitigation investment or adaptation-first resilience spending, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
