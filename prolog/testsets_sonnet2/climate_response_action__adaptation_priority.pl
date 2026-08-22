% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response: Resilience Investment Under Accepted Warming
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation_priority reading of the
 *   climate_response_action kernel: the claim that resilience infrastructure
 *   and adaptive capacity investment should be the dominant near-term
 *   response, treating further temperature rise as effectively locked in and
 *   prioritizing protection of the most vulnerable. The ε authored here is
 *   for the standing adaptation-first arrangement as this reading's own
 *   advocates would describe its actual operation (not for an idealized
 *   universal-protection counterfactual) — real-world adaptation finance is
 *   concentrated on high-value, politically enfranchised populations, leaving
 *   a $350B North-South gap even as the framing is presented as protecting
 *   'vulnerable populations' generically. The sibling readings
 *   (mitigation_priority, degrowth_transformation) are separate constraints
 *   with their own ε and stakeholder structures; this story does not average
 *   across them or hedge its extraction value against theirs.
 *
 * KEY AGENTS:
 *   - donor_nation_governments: agenda-setter and beneficiary, institutional/arbitrage — controls allocation of adaptation finance
 *   - resilience_infrastructure_contractors: beneficiary, organized/mobile — captures the expanding resilience contract market
 *   - fossil_fuel_incumbents: indirect beneficiary, institutional/arbitrage — gains political cover from inevitability framing
 *   - low_lying_island_states: primary target, powerless/trapped — bears unbounded existential cost the framework cannot fully offset
 *   - subsistence_farmers_global_south and unprotected_informal_settlements: targets, powerless/trapped — underfunded relative to exposure
 *   - future_generations: target, powerless/trapped, civilizational/universal scope — inherits the accepted warming ceiling
 *   - independent_climate_scientists: analytical observer — assesses whether the inevitability premise is physical or political
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.61).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.48).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response: Resilience Investment Under Accepted Warming").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '96dcb293-2a01-4125-b841-4dc6f40410ea').
narrative_ontology:cs_kernel_codification('96dcb293-2a01-4125-b841-4dc6f40410ea', distributed).
narrative_ontology:cs_authority_grounding('96dcb293-2a01-4125-b841-4dc6f40410ea', distributed).
narrative_ontology:cs_reading_relation('96dcb293-2a01-4125-b841-4dc6f40410ea', climate_response_action__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('96dcb293-2a01-4125-b841-4dc6f40410ea', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('96dcb293-2a01-4125-b841-4dc6f40410ea', foundational, warming_lock_in_justifies_resource_reallocation).
narrative_ontology:cs_axiom_status(warming_lock_in_justifies_resource_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('96dcb293-2a01-4125-b841-4dc6f40410ea', warming_lock_in_justifies_resource_reallocation, empirically_contingent).
narrative_ontology:cs_axiom('96dcb293-2a01-4125-b841-4dc6f40410ea', foundational, protection_of_vulnerable_populations_is_primary_obligation).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_populations_is_primary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('96dcb293-2a01-4125-b841-4dc6f40410ea', protection_of_vulnerable_populations_is_primary_obligation, deontological).
narrative_ontology:cs_reference_frame('96dcb293-2a01-4125-b841-4dc6f40410ea', post_paris_agreement_finance_architecture).
narrative_ontology:cs_drift_state('96dcb293-2a01-4125-b841-4dc6f40410ea', post_loss_and_damage_fund_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96dcb293-2a01-4125-b841-4dc6f40410ea', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_coastal_property_owners).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, donor_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, low_lying_island_states).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, subsistence_farmers_global_south).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, unprotected_informal_settlements).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, temperature_rise_inevitability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international adaptation finance, decide which resilience projects get funded and where, and can shift domestic resources toward protecting their own coastlines and cities first. They frame adaptation as pragmatic realism given decades of failed mitigation, which also lets them avoid the deeper costs of emissions cuts at home.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, donor_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, donor_nation_governments, beneficiary).

% Win large seawall, drainage, relocation, and early-warning-system contracts as adaptation becomes the dominant funding stream. Their revenue depends on warming being treated as fixed and adaptation as the permanent, expanding solution rather than a bridge to be phased out.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, resilience_infrastructure_contractors, beneficiary,
    organized, biographical, mobile, global).

% Receive the first and most robust seawalls, levees, and insurance backstops because their property tax base and political voice attract resilience spending. Can also relocate or diversify assets if protection fails, so downside is bounded.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, wealthy_coastal_property_owners, beneficiary,
    powerful, biographical, arbitrage, national).

% Benefit indirectly: an adaptation-first framing that treats further warming as inevitable reduces near-term political pressure for aggressive emissions cuts, extending the operating runway for existing fossil assets while resilience spending absorbs public attention and finance.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Face existential land loss that no resilience infrastructure budget can fully offset — sea walls do not scale to whole-nation submersion. They bear the accepted-warming premise as an unbounded cost with no adaptation ceiling that saves their territory, and cannot exit the physical geography that defines their sovereignty.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, low_lying_island_states, payer,
    powerless, civilizational, trapped, global).

% Depend on rainfall and temperature patterns that are destabilizing faster than local adaptation finance arrives. The $350B North-South financing gap means the irrigation, drought-resistant seed, and relocation support they need is chronically underfunded relative to what wealthier regions secure for themselves.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, subsistence_farmers_global_south, payer,
    powerless, generational, trapped, regional).

% Live in flood- and heat-exposed urban peripheries that lack the property tax base or political leverage to attract resilience investment, so protection is built around them, not for them, even within wealthy countries.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, unprotected_informal_settlements, payer,
    powerless, immediate, trapped, local).

% Inherit whatever level of warming the adaptation-priority approach accepts as the ceiling, plus the compounding physical and financial costs of protecting against a moving target. They have no voice in current allocation decisions and no capacity to renegotiate the accepted-warming premise after the fact.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Argue that accepting temperature rise as inevitable forecloses the emissions-reduction pathways that would reduce the total burden adaptation has to carry, and that resilience-first framing lets high emitters buy their way out of mitigation obligations. Present at UN climate forums but structurally outvoted by donor-nation finance ministries who control the purse.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_justice_advocates, excluded,
    moderate, generational, constrained, global).

% Model the physical limits of adaptation — how much sea-level rise, heat stress, and crop failure resilience infrastructure can actually absorb — and can independently assess whether 'accepting warming as inevitable' reflects a genuine physical ceiling or a politically convenient one.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, independent_climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, donor_nation_governments).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools capital and engineering capacity to build sea walls, drought-resilient agriculture, early-warning systems, and managed retreat programs so that vulnerable populations are not left to face escalating climate impacts with zero protective infrastructure — a genuine and urgent coordination problem given warming already locked into the system.
% TRANSFER_FUNCTION: Moves adaptation finance and infrastructure investment preferentially toward wealthy, politically enfranchised, high-property-value populations and regions, while the $350B North-South financing gap leaves low-income and geographically exposed populations under-protected relative to their exposure; simultaneously reduces near-term political pressure on high-emitting incumbents by normalizing further warming as a fixed cost.
% ABSENT_VOICES: Future generations have no seat in current finance allocation. Low-lying island states participate in UN forums but lack the fiscal leverage to compel financing commitments. Climate justice advocates who argue the inevitability framing is itself a policy choice, not a physical necessity, are present in discourse but structurally outvoted in budget-setting bodies.
% DISAPPEARANCE_RATIONALE: If adaptation-priority framing and its associated finance flows vanished overnight, resilience contractors would lose their dominant revenue stream, donor nations would face renewed pressure to either fund mitigation at scale or accept unmanaged climate damages, and the political cover that lets emissions reduction be deferred would collapse — forcing the mitigation and degrowth readings back into direct contest for the response framework.
% FOUNDING_PROBLEM: By the early 2020s, cumulative emissions had already locked in significant warming regardless of near-term mitigation success, and vulnerable populations were experiencing climate impacts with essentially no protective infrastructure — the founding problem was the gap between locked-in physical risk and existing protective capacity.
% FOUNDING_PROBLEM_CORROBORATION: Independent climate scientists corroborate that some warming is now physically locked in regardless of policy choice, making some adaptation genuinely necessary. However, IPCC working group economists and climate justice researchers outside the donor-government and contractor beneficiary set argue the 'inevitability' framing is being used to justify treating adaptation as a substitute for, rather than a complement to, aggressive mitigation — a policy choice being presented as physical necessity.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects real, escalating asymmetry: adaptation finance flows disproportionately to high-value protected populations while the financing gap leaves the most exposed populations under-resourced relative to their risk, and the framing provides political cover for continued high emissions. Suppression (0.48) is moderate — this is not primarily coercive; it operates through agenda control over what counts as 'realistic' policy and which populations get modeled as protectable. Theater ratio (0.34) captures a meaningful but not dominant gap between resilience-investment announcements and delivered, verified protective capacity, particularly in South-South and least-developed-country contexts where pledged finance often exceeds disbursed finance. Accessibility collapse (0.50) is moderate: alternative framings (mitigation-first, degrowth) remain live and contested internationally, they have not been foreclosed, only out-competed for finance and political attention. Resistance (0.58) is substantial and organized — climate justice movements, island-state coalitions, and some scientific bodies actively contest the inevitability premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Donor nations and resilience contractors sit near the full-beneficiary end: they set allocation rules, capture contract revenue, and have arbitrage-grade exit (capital and political attention can redeploy elsewhere). Wealthy coastal property owners benefit concretely from protection and retain exit options via asset mobility. Low-lying island states, subsistence farmers, and informal settlement residents sit near the full-target end: trapped exit options (no alternative geography, no fiscal capacity to self-fund protection), and the constraint's operation extracts through underfunding relative to need rather than through direct payment extraction. Future generations are a special case — analytically powerless and temporally unable to participate in current allocation, which is why civilizational time horizon and universal spatial scope are used despite their zero direct agency today.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — locked-in warming outpacing existing protective infrastructure — retains a live physical component (independent climate science corroborates some warming is now unavoidable regardless of policy). But the constraint's contested status arises because the same physical fact is being used to justify a resource-allocation pattern (concentrated protection for enfranchised populations, deferred mitigation pressure for incumbents) that goes beyond what the physical premise alone requires. This is not classic mandatrophy (a fully dead mandate persisting through inertia) — the coordination function is genuinely still needed — but it is a case where the mandate's legitimate core (some adaptation is now unavoidable) is doing cover work for an allocation pattern (whose protection, whose finance gap, whose delayed mitigation) that a narrower, corroborated version of the mandate would not by itself justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_premise_physical_vs_political,
    'Is the ''temperature rise is inevitable, prioritize adaptation'' premise a genuine physical ceiling given current emissions trajectories, or a politically convenient framing that understates how much warming remains avoidable through aggressive near-term mitigation?',
    'Compare adaptation-priority advocates'' warming ceiling assumptions against independent IPCC-aligned emissions-pathway modeling; check whether adaptation finance commitments are being used as a substitute for, versus a complement to, mitigation commitments in the same national budgets.',
    'If political rather than purely physical, the constraint''s coordination story (protecting the vulnerable from unavoidable harm) is partly cover for deferring mitigation costs onto future generations and low-emitting states — strengthening the tangled_rope classification. If substantially physical, the coordination function is more clearly load-bearing and less purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_premise_physical_vs_political, empirical, 'Whether accepted-warming premise reflects physical necessity or political convenience for high emitters.').

omega_variable(
    financing_gap_closure_trajectory,
    'Will the $350B North-South adaptation financing gap close over the interval, or is it structurally durable given donor-nation domestic political incentives to protect their own populations first?',
    'Track disbursed (not merely pledged) adaptation finance by recipient region against documented climate exposure indices over a multi-year window.',
    'A durable or widening gap would confirm the tangled_rope classification''s asymmetric-extraction gate; a closing gap would push the constraint toward genuine rope (coordination with converging benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_gap_closure_trajectory, empirical, 'Whether the North-South adaptation finance gap is closing or structurally persistent.').

omega_variable(
    reading_framing_choice_ambiguity,
    'Is ''adaptation priority'' best modeled as a genuinely distinct policy commitment, or as the residual default that emerges whenever mitigation and degrowth commitments fail politically — i.e., is it a chosen reading or a fallback state?',
    'Examine whether adaptation-priority advocates hold the position as a first-best policy choice versus whether adaptation spending simply rises as a residual whenever mitigation targets are missed.',
    'If adaptation priority is mostly a fallback rather than a chosen framework, its classification as an independent kernel reading (versus a symptom of mitigation_priority''s failure) would need reassessment, though the ε and stakeholder structure of the standing arrangement would not change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_choice_ambiguity, conceptual, 'Whether adaptation-priority is a genuine independent policy commitment or a fallback from failed mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t4, climate_response_action__adaptation_priority, theater_ratio, 4, 0.22).
narrative_ontology:measurement(clim_tr_t8, climate_response_action__adaptation_priority, theater_ratio, 8, 0.26).
narrative_ontology:measurement(clim_tr_t12, climate_response_action__adaptation_priority, theater_ratio, 12, 0.29).
narrative_ontology:measurement(clim_tr_t16, climate_response_action__adaptation_priority, theater_ratio, 16, 0.32).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__adaptation_priority, theater_ratio, 20, 0.34).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t4, climate_response_action__adaptation_priority, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(clim_be_t8, climate_response_action__adaptation_priority, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(clim_be_t12, climate_response_action__adaptation_priority, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(clim_be_t16, climate_response_action__adaptation_priority, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_response_action__adaptation_priority, base_extractiveness, 20, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(clim_su_t4, climate_response_action__adaptation_priority, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(clim_su_t8, climate_response_action__adaptation_priority, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(clim_su_t12, climate_response_action__adaptation_priority, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(clim_su_t16, climate_response_action__adaptation_priority, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(clim_su_t20, climate_response_action__adaptation_priority, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This story is one of three constraint-family members decomposing the natural-language 'climate response' kernel per the ε-invariance principle. Each reading (adaptation_priority, mitigation_priority, degrowth_transformation) is authored as a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, linked here via affects_constraints. The adaptation_priority reading's expansion of resilience finance and its inevitability premise create downstream pressure on the mitigation_priority reading (reducing political urgency for emissions cuts) and stand in direct tension with the degrowth_transformation reading's rejection of technological/infrastructural fixes as adequate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
