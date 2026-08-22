% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk Standard for Nuclear Energy Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint is the comparative-risk reading of the
 *   acceptable-risk-for-energy kernel: nuclear risk is judged acceptable not
 *   against any absolute safety threshold but against the demonstrated,
 *   ongoing harms of the fossil fuel alternatives it would displace — coal
 *   particulate mortality and climate destabilization. Under this reading,
 *   temporal urgency (the need to decarbonize before catastrophic warming) is
 *   treated as outweighing intergenerational waste-custodianship concerns,
 *   and nuclear acceptability becomes conditional on the counterfactual
 *   energy mix rather than fixed. Two sibling readings of the same kernel are
 *   NOT part of this constraint: catastrophic_tail_dominant treats
 *   irreversibility and intergenerational burden as decisive regardless of
 *   fossil comparison, and expected_value_dominant computes acceptability
 *   from probability-weighted annual costs without privileging either
 *   comparison framing or tail-risk aversion. Each is authored as its own
 *   separate constraint with its own epsilon; this file speaks only for the
 *   comparative-risk reading.
 *
 * KEY AGENTS:
 *   - nuclear_industry_operators: institutional beneficiary and co-agenda-setter who benefit from the comparative framing enabling licensing
 *   - grid_decarbonization_planners: institutional agenda-setter who adopts the standard to preserve nuclear in the decarbonization portfolio
 *   - nuclear_host_community_residents: powerless, trapped payers bearing concentrated local catastrophic-tail exposure
 *   - future_generations_waste_custodians: civilizational-horizon payers with no present voice, whose burden is explicitly subordinated to present urgency under this reading
 *   - climate_vulnerable_populations_deferred: global powerless population whose position is structurally ambiguous — beneficiary if the standard accelerates deployment, payer if it produces delay
 *   - catastrophic_tail_risk_advocates: excluded voices whose decision procedure is heard but not institutionally load-bearing
 *   - independent_risk_assessment_bodies: analytical observers who corroborate parts of the founding problem while disputing the framework's completeness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.52).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.48).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk Standard for Nuclear Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '0b742a56-7131-4444-b1bc-5a49e13dbb0c').
narrative_ontology:cs_kernel_codification('0b742a56-7131-4444-b1bc-5a49e13dbb0c', distributed).
narrative_ontology:cs_authority_grounding('0b742a56-7131-4444-b1bc-5a49e13dbb0c', expertise).
narrative_ontology:cs_interpretation_layer_present('0b742a56-7131-4444-b1bc-5a49e13dbb0c').
narrative_ontology:cs_reading_relation('0b742a56-7131-4444-b1bc-5a49e13dbb0c', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('0b742a56-7131-4444-b1bc-5a49e13dbb0c', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('0b742a56-7131-4444-b1bc-5a49e13dbb0c', foundational, acceptability_is_relative_to_counterfactual_energy_mix).
narrative_ontology:cs_axiom_status(acceptability_is_relative_to_counterfactual_energy_mix, holdable).
narrative_ontology:cs_axiom_grounding('0b742a56-7131-4444-b1bc-5a49e13dbb0c', acceptability_is_relative_to_counterfactual_energy_mix, instrumental).
narrative_ontology:cs_axiom('0b742a56-7131-4444-b1bc-5a49e13dbb0c', foundational, temporal_decarbonization_urgency_outweighs_intergenerational_waste_burden).
narrative_ontology:cs_axiom_status(temporal_decarbonization_urgency_outweighs_intergenerational_waste_burden, holdable).
narrative_ontology:cs_axiom_grounding('0b742a56-7131-4444-b1bc-5a49e13dbb0c', temporal_decarbonization_urgency_outweighs_intergenerational_waste_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('0b742a56-7131-4444-b1bc-5a49e13dbb0c', post_three_mile_island_risk_regulatory_synthesis).
narrative_ontology:cs_drift_state('0b742a56-7131-4444-b1bc-5a49e13dbb0c', post_paris_agreement_climate_urgency_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0b742a56-7131-4444-b1bc-5a49e13dbb0c', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_displaced_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_community_residents).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_custodians).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_deferred).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_deferred).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and operate reactors whose licensing depends on regulators accepting a comparative rather than absolute risk standard. They fund the comparative-risk framing in regulatory proceedings and public communications, arguing that any finite nuclear risk is acceptable because the counterfactual (coal, gas, unabated climate change) is worse. This framing lets them proceed with siting and licensing that an absolute-threshold standard would block or substantially delay.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators, agenda_setter).

% Government energy planners and utility regulators who adopt the comparative standard to keep nuclear in the decarbonization portfolio. They set the acceptability criteria used in licensing and permitting decisions, weighing nuclear's risks against coal mortality and climate damage rather than against a fixed safety bar. Their exit from this framework is constrained by binding emissions targets that make foreclosing nuclear politically costly.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners, beneficiary).

% Communities near coal plants and in climate-exposed regions who benefit when nuclear displaces fossil generation — reduced particulate mortality, reduced warming contribution. They have no direct role in setting the comparative standard; they benefit passively and cannot articulate preferences about tail risk versus chronic harm trade-offs in the process that decides on their behalf.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_displaced_populations, beneficiary,
    powerless, biographical, trapped, regional).

% People living near reactor sites and waste storage facilities who bear the concentrated low-probability, high-consequence risk that the comparative standard treats as acceptable because it is smaller than diffuse coal or climate harms borne by everyone else. They cannot relocate the risk or opt out of hosting the facility once sited; their local, involuntary exposure is traded off against a global, statistical benefit they do not individually experience.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_community_residents, payer,
    powerless, biographical, trapped, local).

% Populations centuries hence who will manage or be exposed to long-lived nuclear waste and decommissioning legacies. The comparative-risk standard explicitly subordinates their intergenerational burden to present-day fossil substitution urgency — the reading's founding logic is that averting near-term climate catastrophe outweighs deferred waste stewardship costs. They have no seat in current licensing decisions and cannot corroborate or contest the tradeoff being made in their name.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_custodians, payer,
    powerless, civilizational, trapped, national).

% Populations in climate-exposed regions (small island states, drought-prone agricultural zones) whose fate depends on how fast decarbonization proceeds. They benefit if the comparative standard accelerates nuclear deployment and displaces fossil generation quickly enough; they pay if licensing delays under the comparative framework (siting fights, cost overruns, public opposition) slow deployment below the pace climate stabilization requires. Their position is structurally ambiguous — the same standard that claims to protect them can also produce delay that harms them.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_deferred, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_deferred, beneficiary).

% Analysts and advocacy groups who argue that irreversibility and intergenerational burden should set an absolute ceiling on acceptable nuclear risk regardless of fossil comparisons. Their framework is treated as a minority position in licensing proceedings dominated by comparative-risk methodology; they participate in public comment but the comparative standard's institutional adoption largely forecloses their preferred decision procedure from being load-bearing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_risk_advocates, excluded,
    moderate, civilizational, constrained, national).

% Academic and international bodies (IAEA technical panels, university risk researchers) who evaluate the comparative-risk methodology's internal consistency and its handling of tail risk versus chronic harm without being financially tied to either nuclear operators or fossil incumbents. They publish findings that sometimes validate and sometimes challenge the comparative framework's treatment of catastrophic possibilities.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, independent_risk_assessment_bodies, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable decision procedure for licensing nuclear facilities under genuine uncertainty and competing harms: instead of demanding an unachievable absolute safety guarantee, it lets regulators weigh nuclear's risks against the well-documented, ongoing harms of the fossil alternative, enabling decarbonization to proceed.
% TRANSFER_FUNCTION: Moves concentrated, low-probability catastrophic and long-duration waste-custodianship risk onto nuclear host communities and future generations, in exchange for diffuse, probabilistic risk reduction (avoided coal mortality, avoided climate damage) distributed across fossil-fuel-displaced and climate-vulnerable populations globally and over time.
% ABSENT_VOICES: Nuclear host community residents rarely have veto power proportional to their concentrated exposure; future waste custodians have no representation at all in present licensing; catastrophic-tail-risk advocates are heard in comment periods but their decision procedure is not the one institutionally adopted.
% DISAPPEARANCE_RATIONALE: If the comparative-risk standard were replaced by an absolute-threshold standard overnight, most currently licensed and planned nuclear facilities would fail licensing or face immediate operational review, decarbonization plans that assume nuclear's contribution would need to be rewritten, and coal/gas plants slated for retirement would likely run longer — the entire energy transition planning apparatus in nuclear-inclusive jurisdictions would need restructuring.
% FOUNDING_PROBLEM: Regulators needed a way to license nuclear power despite irreducible catastrophic-tail uncertainty, at a moment when the visible, quantifiable harms of fossil fuel combustion (air pollution deaths, and later climate change) made an absolute zero-risk standard for any energy source practically incoherent and self-defeating.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk assessment bodies (IAEA safety panels, IPCC-adjacent climate economists) corroborate that fossil fuel combustion produces well-documented ongoing mortality and climate forcing that provides real comparative grounding — this is not solely industry self-assertion. However, those same independent bodies, along with catastrophic-tail-risk advocates, dispute whether the comparative framework adequately weights irreversibility and intergenerational burden, so the founding problem's continued primacy over absolute-threshold alternatives is corroborated as real but contested in scope.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.52 because the comparative standard does perform real coordination work (it is not pure rent-seeking cover) while also transferring concentrated, involuntary risk onto host communities and unrepresented future custodians who receive no proportional say. Suppression at 0.48 reflects that catastrophic-tail-risk advocates are not silenced outright but are structurally out-argued by an institutionally entrenched comparative methodology that dominates licensing procedure. Theater ratio is modest (0.30) — most of the activity is genuine risk-comparison analysis, though some public communication by nuclear operators overstates the settledness of the comparative framework's superiority to obscure the still-contested tail-risk question. All three temporal series share one grid (T=0 to 40) to avoid the type-dating distortion the alignment rule warns against.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry operators and grid planners sit near the beneficiary end: institutional power, arbitrage or constrained exit, and direct collection of licensing approval or portfolio benefit from the comparative standard's adoption. Nuclear host community residents and future waste custodians sit near the full-target end: powerless, trapped, bearing concentrated risk they cannot exit and cannot renegotiate — the classic derivation from victim declaration plus trapped exit. Climate-vulnerable populations occupy an unusual dual position, coded here as both beneficiary and payer, because the comparative standard's success or failure at accelerating deployment directly determines whether they experience the benefit (faster decarbonization) or the cost (delay under contested licensing) — this ambiguity is intentional and reflects real structural uncertainty, not authorial indecision.
 *
 * MANDATROPHY ANALYSIS:
 *   The comparative-risk standard prevents the mislabeling of decarbonization-enabling nuclear licensing as either pure coordination (which would erase the real, concentrated cost borne by host communities and future waste custodians) or pure extraction (which would ignore the genuine and well-corroborated harm-avoidance the standard achieves relative to continued fossil generation). Classifying it as tangled_rope rather than rope or snare captures both halves: real coordination function (enabling decarbonization under irreducible uncertainty) coexisting with real asymmetric extraction (concentrated risk transferred to powerless, trapped populations) — and it requires active enforcement (regulatory adoption of comparative rather than absolute methodology) to hold against the catastrophic-tail-dominant alternative that would foreclose much current licensing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_baseline_manipulation,
    'Is the fossil-fuel comparison baseline used to justify nuclear acceptability an honest counterfactual, or is it selectively drawn (e.g., ignoring renewable-plus-storage alternatives) to make nuclear appear comparatively safer than a fuller alternative set would show?',
    'Independent techno-economic analysis comparing licensing-era comparative risk assessments against the full contemporaneous alternative energy portfolio (including renewables and storage, not only coal) available at the time of each licensing decision.',
    'If the comparison baseline was narrowed to coal/gas only when lower-risk alternatives existed, the comparative standard''s coordination claim weakens substantially and the extraction component becomes dominant, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_baseline_manipulation, empirical, 'Whether the comparative baseline is honestly drawn or selectively narrowed.').

omega_variable(
    intergenerational_discounting_legitimacy,
    'Is it legitimate to discount future waste-custodianship burden against present climate urgency, or does this discounting structurally disenfranchise future generations who have no voice in the present tradeoff?',
    'Philosophical and legal analysis of intergenerational representation doctrines, cross-referenced with long-term waste management cost realizations as they occur (empirical check on whether present discounting proves justified in hindsight).',
    'If the discounting is found illegitimate, the comparative-risk reading''s core distinguishing axiom (temporal urgency overrides intergenerational burden) is undermined, strengthening the catastrophic_tail_dominant sibling''s claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_legitimacy, conceptual, 'Whether temporal urgency legitimately overrides intergenerational waste burden.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why has the comparative-risk reading become institutionally dominant over the catastrophic-tail-dominant and expected-value-dominant readings in most licensing jurisdictions — genuine analytical superiority, or because it is the reading most favorable to incumbent nuclear and utility interests?',
    'Comparative institutional history across jurisdictions with different dominant readings (e.g., jurisdictions that adopted more precautionary/absolute standards) to see whether outcomes differ in ways that track reading choice versus other confounds.',
    'If institutional dominance tracks incumbent interest rather than analytical merit, the comparative-risk reading''s coordination claim is partly a cover story, pushing this constraint''s true classification closer to snare from the host-community and future-custodian seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading''s institutional dominance reflects merit or incumbent capture — the committer-structure question routed here per Rule 2.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.22).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.24).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 24, 0.26).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 32, 0.28).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language concept 'acceptable nuclear risk' under the acceptable_risk_for_energy kernel, per the epsilon-invariance principle. comparative_risk_dominant (this file) authors moderate epsilon (0.52) reflecting genuine coordination function alongside concentrated involuntary risk transfer. catastrophic_tail_dominant is expected to author lower epsilon at the point of licensing decisions it would block (precautionary refusal has low extraction from those protected, but forecloses the fossil-displacement benefit this reading captures). expected_value_dominant is expected to author its own distinct epsilon computed from strict probability-weighted cost aggregation without the comparative or precautionary framing devices. The three are linked bidirectionally; none should be treated as measuring the same underlying epsilon by a different observable — they are structurally distinct decision procedures with different victim sets and different institutional footprints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
