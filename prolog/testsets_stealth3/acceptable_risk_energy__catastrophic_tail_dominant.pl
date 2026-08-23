% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Doctrine (Energy Pathway Regulation)
 *   domain: decision-theoretic/political-economic
 *
 * SUMMARY:
 *   The catastrophic-tail-dominant doctrine is the operative rule by which
 *   energy regulators and publics decide which risks are tolerable:
 *   low-probability, high-consequence events (reactor accidents with
 *   contamination) are weighted as if categorically intolerable, and avoiding
 *   them justifies accepting higher aggregate harm elsewhere. In practice the
 *   rule concentrates regulatory attention on catastrophic nuclear scenarios,
 *   ratchets requirements onto the licensed fleet after every accident
 *   worldwide, and — through the economics it creates — walls off new
 *   construction of the lowest-mortality-per-TWh generation pathway, leaving
 *   the displaced demand to combustion sources whose continuous,
 *   statistically dispersed death toll registers nowhere as a discrete event.
 *   The arrangement has a genuine protective function (tail prevention is
 *   real; post-accident retrofits measurably improved safety) AND an
 *   asymmetric discounting structure (identifiable incumbents collect the
 *   displaced market share while diffuse populations bear the substituted
 *   harm). This story instantiates ONE reading of the acceptable_risk_energy
 *   kernel — the catastrophic_tail_dominant reading — as a clean,
 *   epsilon-invariant constraint; the sibling readings are separate files
 *   linked through the network block. The epsilon referent is the standing
 *   tail-dominant arrangement itself, valued by this reading's own lights:
 *   those lights register the arrangement's self-undermining tension, since
 *   the same rule that dominates the reactor-accident tail feeds the climate
 *   tail it would also have to dominate if applied consistently. KEY AGENTS
 *   (by structural relationship): - nuclear_safety_regulators: Agenda-setting
 *   administrator (institutional/constrained) — sets risk criteria, collects
 *   mandate and budget from stringency - fossil_fuel_generators: Primary
 *   beneficiary (powerful/arbitrage) — collects displaced generation share -
 *   anti_nuclear_advocacy_network: Identity-fused beneficiary
 *   (organized/identity_locked) — purpose and funding constituted by the
 *   framing - renewable_energy_producers: Secondary beneficiary
 *   (organized/mobile) — inherits investment headroom - nuclear_operators:
 *   Primary target with dual position (powerful/trapped) — bears suppression,
 *   partially shielded by liability socialization - electricity_ratepayers:
 *   Target (powerless/trapped) — pays compliance and substitution costs -
 *   fossil_pollution_exposed_communities: Target (powerless/trapped) — bears
 *   the discounted distributed toll - future_generations: Non-present bearer
 *   of cumulative costs (listed, agent=false) - expected_value_analysts:
 *   Excluded voice (moderate/mobile) — holds the rival accounting, no seat -
 *   climate_policy_bodies: Analytical observer (institutional/analytical) —
 *   counts the aggregate harm
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.76).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Doctrine (Energy Pathway Regulation)").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "decision-theoretic/political-economic").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '39be3cf9-f828-459c-86f1-b72266f2ec06').
narrative_ontology:cs_kernel_codification('39be3cf9-f828-459c-86f1-b72266f2ec06', formalized).
narrative_ontology:cs_authority_grounding('39be3cf9-f828-459c-86f1-b72266f2ec06', expertise).
narrative_ontology:cs_interpretation_layer_present('39be3cf9-f828-459c-86f1-b72266f2ec06').
narrative_ontology:cs_reading_relation('39be3cf9-f828-459c-86f1-b72266f2ec06', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('39be3cf9-f828-459c-86f1-b72266f2ec06', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('39be3cf9-f828-459c-86f1-b72266f2ec06', foundational, catastrophic_outcomes_weighted_infinite).
narrative_ontology:cs_axiom_status(catastrophic_outcomes_weighted_infinite, holdable).
narrative_ontology:cs_axiom_grounding('39be3cf9-f828-459c-86f1-b72266f2ec06', catastrophic_outcomes_weighted_infinite, deontological).
narrative_ontology:cs_axiom('39be3cf9-f828-459c-86f1-b72266f2ec06', foundational, distributed_harms_discountable).
narrative_ontology:cs_axiom_status(distributed_harms_discountable, holdable).
narrative_ontology:cs_axiom_grounding('39be3cf9-f828-459c-86f1-b72266f2ec06', distributed_harms_discountable, empirically_contingent).
narrative_ontology:cs_reference_frame('39be3cf9-f828-459c-86f1-b72266f2ec06', lexicographic_catastrophe_priority).
narrative_ontology:cs_drift_state('39be3cf9-f828-459c-86f1-b72266f2ec06', contemporary_climate_integration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39be3cf9-f828-459c-86f1-b72266f2ec06', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_network).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_producers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_operators).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, fossil_pollution_exposed_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_operators).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, linear_no_threshold_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the numerical risk criteria new reactors must satisfy, run probabilistic safety assessments, and layer retrofit requirements onto the existing fleet after every accident anywhere in the world. Agency budgets, staffing, and statutory authority expand with the stringency of catastrophic-scenario controls, and career advancement runs through the safety mission. Stepping outside the framework would mean dismantling the agency's own mandate, so exit is available only at the cost of professional self-erasure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_safety_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Sell the generation that fills the gap wherever nuclear capacity is cancelled or retired; every excluded reactor enlarges their dispatch share and asset utilization. They need not campaign for the doctrine — funding the broader reliability discourse that favors their fleet suffices. Capital moves freely across fuels and jurisdictions, so no jurisdiction's policy can trap them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_generators, beneficiary,
    powerful, biographical, arbitrage, global).

% Campaign organizations, affiliated experts, and movement media whose purpose, funding pipelines, and member identities are constituted by opposition to nuclear technology. Catastrophic-scenario imagery is their core communicative asset; conceding that the pathway is comparably safe would dissolve the organization's reason to exist. Membership built the identity, so exit would require renouncing the self.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_network, beneficiary,
    organized, civilizational, identity_locked, global).

% Develop wind, solar, and storage capacity that inherits investment flows and mandate headroom when the competing low-carbon source is excluded from planning. They advocate decarbonization targets their technology can meet alone once nuclear is walled off, and capital reallocates across projects and markets freely.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_producers, beneficiary,
    organized, biographical, mobile, continental).

% Operate the licensed fleet under requirements that escalate with every foreign accident, while new-build licensing stays effectively closed in most jurisdictions. Sunk assets, decommissioning liabilities, and site-specific infrastructure make leaving the pathway equivalent to total loss. The same regime that walls off their growth also socializes their worst-case losses through liability caps and state backstops, so they hold a genuine secondary position on the collecting side.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_operators, payer,
    powerful, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_operators, beneficiary).

% Pay the composite bill: compliance costs baked into tariffs, higher wholesale prices where firm low-carbon capacity was retired, and the fiscal backstops standing behind liability caps. They cannot choose their grid's generation mix, negotiate the pricing, or opt out of the system that bills them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers, payer,
    powerless, biographical, trapped, national).

% Live downwind and downstream of the combustion generation filling the gap, bearing elevated mortality and morbidity as a continuous, statistically dispersed toll that registers nowhere as a discrete event. Income and housing constrain mobility, and the harm arrives without a moment, site, or scene to organize against.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_pollution_exposed_communities, payer,
    powerless, biographical, trapped, regional).

% Will inherit the cumulative climate burden of the combustion generation that filled the gap left by the excluded pathway, along with the decommissioned sites and stranded wastes left behind. They hold no seat in any licensing proceeding and have no mechanism to object. Listed for completeness of the harm accounting; not a present actor.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__catastrophic_tail_dominant, future_generations).

% Actuarial, epidemiological, and energy-systems analysts who compute mortality per terawatt-hour and monetized comparisons across pathways. Licensing proceedings admit catastrophic-scenario testimony but give their aggregate accounting no formal seat; they publish critiques that alter no license terms and move to whichever institution will host the work.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_analysts, excluded,
    moderate, biographical, mobile, global).

% Assessment bodies and treaty processes that count aggregate emissions and mortality across all pathways. They document that excluding the low-carbon pathway raises the aggregate harm the doctrine was built to avoid, and their findings feed political pressure without touching licensing criteria directly. They observe the full structure from a seat that neither collects nor pays.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_policy_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_generators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a polity decides which technological risks are tolerable when consequences are catastrophic, irreversible, and uncompensable after the fact: it concentrates regulatory attention and resources on preventing low-probability, high-consequence events, and provides publics an assurance that worst cases are being actively guarded against rather than traded off against averages.
% TRANSFER_FUNCTION: Moves decision weight, and through it money, health burden, and market share: shifts generation investment away from the nuclear pathway toward combustion and renewable alternatives; transfers the health cost of displaced generation onto pollution-exposed populations; transfers compliance costs to operators and ratepayers; and delivers the displaced revenue stream to the incumbents of the surviving pathways.
% ABSENT_VOICES: Holders of the mortality-per-TWh accounting (actuarial and epidemiological analysts) have no formal seat in licensing proceedings, which admit catastrophic-scenario testimony; the populations bearing the substituted fossil toll are present only as background statistics; future generations are present in no forum at all. The unanimity of the licensing record partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, licensing regimes would re-price risk by expected harm, new reactor construction would restart in most jurisdictions within the decade, fossil dispatch shares would fall as the low-carbon competitor returned, and the advocacy-and-compliance ecosystem organized around catastrophic-scenario politics would lose its object — the energy system would reorganize around the pathway the doctrine currently walls off.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of public assurance after catastrophic imagery became politically decisive: how to demonstrate that worst-case nuclear scenarios — reactor accidents producing contamination on the Chernobyl scale, and the weapons-adjacent fears attached to the technology — would never be tolerated as the price of electricity.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties and cuts both ways: the historical record of Chernobyl and Fukushima, attested by independent international bodies, confirms the tail was once real and remains physically possible; the same bodies' casualty accounting (UNSCEAR, WHO) together with passive-safety engineering and IPCC pathway analysis attests that the worst-case magnitude has narrowed dramatically and that the founding problem, as originally posed, is substantially solved — supporting the reading that the arrangement now persists beyond its warrant. No source inside the fossil or advocacy beneficiary sets is relied on for either half.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) but not maximal: the arrangement delivers a real service (catastrophic-scenario prevention, safety-culture spillovers to other industries) alongside the pathway-suppression externality, and by this reading's own lights part of the aggregate harm is an authorized cost of tail avoidance rather than theft — what registers as extraction even internally is the incumbent capture of the displaced share and the self-undermining climate feedback. Suppression (0.76) is authored as a raw structural property, unscaled by power or scope: the pathway is excluded by licensing closure, financing premia, and mobilized opposition, not merely disadvantaged. Theater ratio (0.32) reflects growing compliance documentation and redundant proceduralism that no longer tracks marginal risk reduction, while the enforcement core remains functional. Accessibility collapse is moderate (0.45): rival decision frameworks survive intact in actuarial practice and academic literature — the doctrine dominates institutional licensing without erasing its alternatives. Resistance (0.60) is sustained: mortality-per-TWh accounting, cost-benefit mandates, industry litigation, and climate-policy pressure all push against the doctrine continuously. The temporal series run on one shared grid (t=0,9,18,27,36,45, mapping approximately 1979-2024) so every metric is authored at every examined point; the trajectory is a step-ratchet rather than smooth drift — each major accident (t~0 Three Mile Island, t~7 Chernobyl, t~32 Fukushima) steps suppression up, with partial relaxation after t~40 as restart programs and climate commitments reopen the question. Extraction dips at the endpoint as renewables begin absorbing displaced share instead of fossil generation. The oscillation is not intermittent reinforcement; it is externally driven by accident events, documented here as the ratchet mechanism the doctrine runs on.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical regulatory text. From the regulator's seat the doctrine is a professional mission: stringency is diligence, and budget growth with mandate is institutional success. From the operator's seat it is a walled garden: obligations compound with every foreign accident while the exit door (new build) is bricked shut — though the same regime socializes their worst-case losses, a dual position that tempers but does not reverse their target-side exposure. From the exposed-community seat the doctrine is invisible: their harm arrives as statistics without a scene, which is precisely why it carries no political weight — the doctrine's salience asymmetry (one dramatizable catastrophe outweighs ten thousand dispersed deaths) is the shield under which the substitution proceeds. From the fossil generator's seat it is quiet opportunity requiring no advocacy. From the advocacy network's seat it is vigilance that must never relax. Coalition analysis for the powerless payer seats: their coalition potential is structurally crippled because the harm is statistical — there is no incident, site, or moment to organize around, whereas the advocacy seat mobilizes on vivid imagery. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place fossil_fuel_generators, anti_nuclear_advocacy_network, and renewable_energy_producers near the beneficiary pole (d near 0.05-0.15): the first collects displaced revenue with arbitrage-grade capital mobility, the second collects identity and funding with no exit conceivable, the third collects investment headroom with mobile capital. Victim declarations place nuclear_operators (trapped, sunk assets, dual-positioned via liability socialization — d near 0.75-0.8), electricity_ratepayers (trapped, d near 0.85), and fossil_pollution_exposed_communities (trapped, d near 0.9) near the target pole. future_generations is listed with agent=false and correctly contributes nothing to directionality — a non-present actor must not feed the arithmetic as if it collected or paid at a seat. nuclear_safety_regulators declare no beneficiary/victim position; the derivation falls back toward symmetric, but structurally their mandate, staffing, and authority scale with the doctrine's stringency — a beneficiary-side pull the fallback understates. I deliberately do NOT author a directionality override for this: overrides key on power atoms, and the institutional atom is shared with climate_policy_bodies (an observer) while the powerful atom is shared by opposed seats (fossil beneficiary, operator target) — any override would conflate structurally opposed agents. The coarse-grain error is noted here as an interpretive caveat instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assuring publics that worst-case nuclear scenarios would never be tolerated — is contested rather than dead: the tail remains physically possible (corroborated by the historical record of Chernobyl and Fukushima from outside the beneficiary set), yet its magnitude has narrowed dramatically (passive-safety designs, international casualty accounting), and the doctrine's scope has quietly expanded from worst-case assurance into general pathway governance. The classification prevents mislabeling in both directions. Coding the arrangement as pure extraction would erase the genuine coordination service — tail prevention is a real collective-action problem, and even the doctrine's sharpest critics concede the post-accident safety improvements were real. Coding it as pure coordination would erase the asymmetric discounting through which identifiable incumbents collect the displaced share while diffuse populations bear the substituted toll. The tangled-rope structure holds both: coordination function and extraction run through the same licensing machinery, and the enforcement requirement is load-bearing — without active regulatory closure and mobilized opposition, financing would reopen the pathway within a decade. Mandatrophy is not declared resolved: the function has thinned at the margin (theater share rising, scope drifting past the founding warrant) but the enforcement core still performs its named service.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint instantiates the catastrophic_tail_dominant reading of the acceptable_risk_energy kernel; what structural changes would the sibling readings (expected_value_dominant, option_value_preserving) produce if they governed the same arrangement?',
    'Author and classify the sibling stories, then diff victim sets, epsilon, and enforcement profiles across the three readings of the shared kernel.',
    'Under expected_value_dominant the nuclear victim set empties (mortality-per-TWh accounting favors the pathway) and fossil mortality enters as governing counted harm; under option_value_preserving pathway suppression is priced as destroyed decision flexibility rather than accepted tail-avoidance cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame membership: one reading of a three-reading kernel; sibling deltas routed here per Rule 2.').

omega_variable(
    tail_weight_commensurability,
    'Where the readings disagree: can the weight assigned to low-probability catastrophic outcomes be finite and commensurable with aggregated statistical harms, or is it lexically infinite?',
    'Decision-theoretic analysis of whether any preference-elicitation procedure can recover finite tail weights that reproduce the doctrine''s actual pathway exclusions.',
    'Recoverable finite weights would collapse this reading into the expected-value sibling with adjusted parameters; demonstrated incommensurability entrenches the foreclosure relation and stabilizes this reading''s separate existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_weight_commensurability, conceptual, 'Location of the inter-reading disagreement: commensurability of infinite versus finite tail weights.').

omega_variable(
    lnt_low_dose_validity,
    'Is the linear-no-threshold dose-response model empirically warranted in the low-dose regime, or does a threshold or hormetic model hold?',
    'Large-cohort epidemiology of low-dose occupational and environmental exposure cross-checked against mechanistic radiobiology.',
    'A real low-dose threshold would shrink the catastrophic-risk calculus that drives pathway suppression; strict LNT inflates it. Either resolution re-prices epsilon and reshapes the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lnt_low_dose_validity, empirical, 'Empirical foundation of the radiation-risk arithmetic the doctrine runs on.').

omega_variable(
    climate_tail_self_application,
    'Does this reading''s own axiom, applied consistently, classify climate-system catastrophe as a dominating tail — and if so, does the arrangement it governs violate its own axiom by suppressing a low-carbon pathway?',
    'Internal consistency audit: apply the reading''s lexicographic rule to the full set of energy-pathway tails including climate disruption, and compare the resulting pathway policy to the observed arrangement.',
    'If climate tails dominate under the reading''s own rule, the standing arrangement is self-undermining and its extraction rises even by the reading''s lights; if the reading stipulates ''catastrophic'' to mean acute technological events only, the discounting of fossil and climate harm is definitional and the current valuation is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_tail_self_application, conceptual, 'Whether the reading''s scope restriction on ''catastrophic'' is principled or ad hoc.').

omega_variable(
    public_risk_perception_internalization,
    'Is the suppression of the nuclear pathway maintained by structural barriers (licensing economics, financing costs) or by internalized perception (availability-cascade dread that persists after barriers are removed)?',
    'Post-liberalization trajectory in jurisdictions that restart construction: if public opposition and financing premia persist after regulatory barriers fall, the internalized component dominates.',
    'An internalized component makes suppression sticky beyond regulatory change — removing the licensing wall would not restore the pathway, and effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_risk_perception_internalization, empirical, 'Structural versus internalized suppression mechanism for the pathway exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(are_ctd_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(are_ctd_tr_t0, observed).
narrative_ontology:measurement(are_ctd_tr_t9, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 9, 0.24).
narrative_ontology:measurement_basis(are_ctd_tr_t9, observed).
narrative_ontology:measurement(are_ctd_tr_t18, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(are_ctd_tr_t18, observed).
narrative_ontology:measurement(are_ctd_tr_t27, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 27, 0.3).
narrative_ontology:measurement_basis(are_ctd_tr_t27, observed).
narrative_ontology:measurement(are_ctd_tr_t36, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 36, 0.34).
narrative_ontology:measurement_basis(are_ctd_tr_t36, observed).
narrative_ontology:measurement(are_ctd_tr_t45, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 45, 0.32).
narrative_ontology:measurement_basis(are_ctd_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(are_ctd_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(are_ctd_be_t0, observed).
narrative_ontology:measurement(are_ctd_be_t9, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(are_ctd_be_t9, observed).
narrative_ontology:measurement(are_ctd_be_t18, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(are_ctd_be_t18, observed).
narrative_ontology:measurement(are_ctd_be_t27, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 27, 0.63).
narrative_ontology:measurement_basis(are_ctd_be_t27, observed).
narrative_ontology:measurement(are_ctd_be_t36, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(are_ctd_be_t36, observed).
narrative_ontology:measurement(are_ctd_be_t45, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 45, 0.62).
narrative_ontology:measurement_basis(are_ctd_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(are_ctd_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(are_ctd_su_t0, observed).
narrative_ontology:measurement(are_ctd_su_t9, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 9, 0.68).
narrative_ontology:measurement_basis(are_ctd_su_t9, observed).
narrative_ontology:measurement(are_ctd_su_t18, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(are_ctd_su_t18, observed).
narrative_ontology:measurement(are_ctd_su_t27, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 27, 0.74).
narrative_ontology:measurement_basis(are_ctd_su_t27, observed).
narrative_ontology:measurement(are_ctd_su_t36, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 36, 0.8).
narrative_ontology:measurement_basis(are_ctd_su_t36, observed).
narrative_ontology:measurement(are_ctd_su_t45, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 45, 0.76).
narrative_ontology:measurement_basis(are_ctd_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The colloquial label 'acceptable risk' decomposes into three structurally distinct claims per the epsilon-invariance principle: (1) catastrophic_tail_dominant — this story, where reactor-accident potential carries lexical weight and the nuclear pathway sits in the victim set; (2) expected_value_dominant — where all pathway harms are finite, commensurable, and counted, emptying the nuclear victim set and elevating fossil mortality to governing status; (3) option_value_preserving — where the coordination object is decision flexibility itself and suppression is priced as destroyed option value. The three readings share one kernel and one referent arrangement but author different epsilon, different victim sets, and different types; forcing them into one story would make epsilon observable-dependent, which is the decomposition failure mode. Family linkage runs through network.affects_constraints in all three files. Upstream/downstream: the expected-value reading is the analytic baseline this reading defines itself against, so this story's edges point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
