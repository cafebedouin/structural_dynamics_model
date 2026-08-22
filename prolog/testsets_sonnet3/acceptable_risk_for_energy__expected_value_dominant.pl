% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Acceptable Risk Standard for Nuclear Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the expected_value_dominant reading of the
 *   acceptable_risk_for_energy kernel: acceptability of nuclear (and other
 *   low-probability-high-consequence) energy sources is determined by
 *   annualized expected cost — probability multiplied by consequence, summed
 *   across the portfolio, and weighed against annualized climate benefit.
 *   Under this reading, nuclear substantially exits the victim set relative
 *   to fossil alternatives because its annualized expected mortality and cost
 *   figures are low, waste disposal is treated as a bounded engineering and
 *   financing problem rather than an open custodial wound, and framings that
 *   privilege rare catastrophic tails over the annualized average are treated
 *   as a methodological error to be corrected rather than a legitimate
 *   alternative accounting. The sibling readings (catastrophic_tail_dominant,
 *   comparative_risk_dominant) are NOT described here except as named
 *   siblings in the omega variables and cs_structure fields — each is its own
 *   constraint, with its own ε, victim set, and stakeholder structure.
 *
 * KEY AGENTS:
 *   - nuclear_utility_operators: institutional beneficiary/agenda_setter — collects licensing predictability and low liability exposure from the expected-value standard
 *   - regulatory_risk_analysts: institutional agenda_setter — administers and legitimizes the probability-times-consequence methodology
 *   - host_community_residents_near_reactors: powerless, trapped payer — bears concentrated tail exposure the annualized average discounts away
 *   - future_generations_waste_stewardship: powerless, civilizational-horizon payer — inherits custodial burden priced as a bounded engineering cost
 *   - catastrophic_risk_advocates: excluded organized voice — argues the annualization unit itself is the wrong instrument, procedurally required to translate objections into the dominant metric to be heard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.28).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.22).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.28).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Acceptable Risk Standard for Nuclear Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'c17c82b1-fa06-4dc4-a7f0-4549af140a90').
narrative_ontology:cs_kernel_codification('c17c82b1-fa06-4dc4-a7f0-4549af140a90', formalized).
narrative_ontology:cs_authority_grounding('c17c82b1-fa06-4dc4-a7f0-4549af140a90', expertise).
narrative_ontology:cs_interpretation_layer_present('c17c82b1-fa06-4dc4-a7f0-4549af140a90').
narrative_ontology:cs_reading_relation('c17c82b1-fa06-4dc4-a7f0-4549af140a90', acceptable_risk_for_energy__acceptable_risk_for_energy_catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('c17c82b1-fa06-4dc4-a7f0-4549af140a90', acceptable_risk_for_energy__acceptable_risk_for_energy_comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('c17c82b1-fa06-4dc4-a7f0-4549af140a90', foundational, probability_weighted_consequence_is_commensurable_across_hazard_types).
narrative_ontology:cs_axiom_status(probability_weighted_consequence_is_commensurable_across_hazard_types, holdable).
narrative_ontology:cs_axiom_grounding('c17c82b1-fa06-4dc4-a7f0-4549af140a90', probability_weighted_consequence_is_commensurable_across_hazard_types, instrumental).
narrative_ontology:cs_axiom('c17c82b1-fa06-4dc4-a7f0-4549af140a90', secondary, annualized_discounting_of_intergenerational_custodial_burden_is_legitimate).
narrative_ontology:cs_axiom_status(annualized_discounting_of_intergenerational_custodial_burden_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c17c82b1-fa06-4dc4-a7f0-4549af140a90', annualized_discounting_of_intergenerational_custodial_burden_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('c17c82b1-fa06-4dc4-a7f0-4549af140a90', cost_benefit_annualized_expected_value_standard).
narrative_ontology:cs_drift_state('c17c82b1-fa06-4dc4-a7f0-4549af140a90', post_fukushima_risk_reassessment_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c17c82b1-fa06-4dc4-a7f0-4549af140a90', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, regulatory_risk_analysts).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_planners).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_ratepayers_from_decarbonization).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, host_community_residents_near_reactors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations_waste_stewardship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate reactor fleets and finance license renewals using expected-annual-cost risk models that are accepted by regulators as the governing standard. Benefit directly when this framing keeps insurance liability caps low and licensing timelines predictable; helped shape the regulatory methodology through decades of technical intervenor participation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators, agenda_setter).

% Design and apply the probability-times-consequence framework in licensing dockets, cost-benefit reviews, and siting decisions. Their professional standing and methodology rest on the expected-value approach being treated as the settled, quantitatively rigorous standard rather than one contested framing among several.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_risk_analysts, agenda_setter,
    institutional, generational, analytical, national).

% Use the expected-value comparison (nuclear annual risk vs. avoided carbon emissions) to justify nuclear inclusion in decarbonization portfolios. Benefit from a framing that lets nuclear's low expected mortality per terawatt-hour outweigh coal's continuous emissions in the same annualized unit.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_planners, beneficiary,
    institutional, generational, constrained, national).

% Receive lower-carbon electricity and avoided fossil health externalities under portfolios that include nuclear, justified by the expected-value calculus. Do not participate in the risk-methodology debate; benefit passively from its policy output.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, grid_ratepayers_from_decarbonization, beneficiary,
    moderate, biographical, constrained, national).

% Live within the evacuation and contamination radius. Bear the full tail-consequence exposure that the expected-value framework discounts by low annual probability; if a low-probability event occurs, it occurs entirely to them, not distributed across the population used to compute the annualized average. Cannot relocate the plant or meaningfully price their own exposure into the calculus that governs siting.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, host_community_residents_near_reactors, payer,
    powerless, generational, trapped, local).

% Inherit spent-fuel stewardship obligations spanning millennia. The expected-value framework treats disposal as a solvable engineering and cost-amortization problem with a bounded annualized price tag, converting an intergenerational custodial burden with no consenting party into a line item discounted at standard rates.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations_waste_stewardship, payer,
    powerless, civilizational, trapped, national).

% Argue that annualized probability-times-consequence math is the wrong unit for irreversible, geographically concentrated, intergenerational harms, and that low-probability catastrophic tails should dominate the acceptability judgment regardless of their annualized average. Participate in dockets but are procedurally required to translate their objection into the annualized-cost framework to be heard at all, which structurally forecloses their preferred metric before deliberation starts.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, catastrophic_risk_advocates, excluded,
    organized, civilizational, constrained, national).

% Hold that nuclear acceptability should be assessed only relative to the actually-available alternative energy sources' risk profiles, not against an absolute expected-value threshold. Their comparative framing is compatible with different licensing conclusions depending on the alternative fuel mix, and is sidelined when the expected-value standard is applied as if it produces an absolute, portfolio-independent verdict.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, comparative_risk_analysts, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single quantitative decision rule — annualized expected cost, weighting rare severe events by their probability — that lets regulators, utilities, and legislators compare energy sources on one commensurable scale instead of adjudicating each siting or licensing dispute from first principles.
% TRANSFER_FUNCTION: Moves siting and catastrophic-tail exposure from the diffuse population used to compute the annualized average onto the concentrated host communities actually located near facilities, while moving decarbonization credit and licensing predictability to utility operators and climate planners; moves waste-stewardship burden from the present generation of decision-makers onto future generations who hold no seat in the calculation.
% ABSENT_VOICES: Catastrophic-tail advocates and comparative-risk analysts are procedurally present in licensing dockets but must translate their objections into the annualized expected-value unit to be heard, which structurally forecloses the argument that annualization is itself the wrong instrument. Future generations inheriting waste stewardship have no representative seat at all.
% DISAPPEARANCE_RATIONALE: If the expected-value standard were abandoned, licensing and siting decisions would have to be re-justified under either a catastrophic-tail-dominant threshold (which could exclude nuclear from acceptable siting near population centers regardless of annualized cost) or a comparative-risk standard (which ties nuclear's acceptability to the alternative fuel mix rather than an absolute number) — utility financing, insurance liability caps, and decarbonization portfolio math built on the expected-value figure would all require re-derivation.
% FOUNDING_PROBLEM: Mid-20th-century nuclear regulation needed a way to compare a novel, low-frequency, high-consequence technology against familiar, high-frequency, low-consequence hazards (industrial accidents, fossil combustion) using a common metric that regulators, insurers, and legislators could all apply consistently.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear utility operators and regulatory risk analysts attest the framework remains the necessary and sufficient standard for commensurable cross-technology comparison. Independent risk theorists and host-community advocacy groups outside the beneficiary set — citing post-Fukushima and post-Chernobyl retrospective analyses — attest that the annualization step itself, not merely the input probabilities, systematically understates irreversible and geographically concentrated harm, making the founding problem's original solution contested rather than settled.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28) because, by this reading's own lights, the standing arrangement genuinely does compare nuclear favorably against fossil alternatives on the metric it uses — the reading is not describing a naked transfer, it is describing a methodology that most of its own stakeholders experience as functioning coordination. Suppression is low (0.22): the reading does not need to coercively silence catastrophic-tail framing to persist, since it operates by definitional exclusion (translate your objection into our unit, or you have no standing) rather than active repression — this is a softer form of suppression than force, so it scores low rather than zero. Theater ratio is low (0.15): the licensing math genuinely does the coordination work claimed for it; there is little pure performance layered on top. Accessibility collapse is moderate (0.35): comparative-risk and catastrophic-tail framings remain articulable and are litigated in dockets, they are just structurally disadvantaged, not eliminated. Resistance is moderate (0.45): host communities and catastrophic-risk advocates actively contest the framework in licensing proceedings, courts, and public comment, but rarely succeed in displacing the metric itself.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-analyst and utility-operator seats, the expected-value standard is a rope: it solves a genuine cross-technology commensurability problem and its own stakeholders are net beneficiaries of the resulting decarbonization-friendly licensing environment. From the host-community and future-generations seats, the same standard functions closer to a tangled rope or worse: the annualization step is precisely the mechanism that converts their concentrated, irreversible exposure into someone else's acceptable average. The engine computes these per-seat divergences from the declared power/exit/scope data; this story does not resolve the gap, it documents it.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators, regulatory risk analysts, and climate policy planners sit near the beneficiary end: institutional power, generational or civilizational time horizons they can plan around, and arbitrage or analytical exit options, because the standard's persistence directly serves their planning, financing, and legitimacy needs. Host community residents and future generations sit near the full-target end: powerless, trapped or civilizationally exposed with zero exit, because the standard's central operation — annualizing rare catastrophic consequence into a small expected figure — is precisely what converts their concentrated exposure into a policy-acceptable number. Grid ratepayers sit closer to symmetric-beneficiary: they receive decarbonization benefit without bearing siting risk directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing one commensurable metric to compare a novel low-frequency/high-consequence technology against familiar high-frequency/low-consequence hazards — remains partially live: energy portfolio comparison genuinely requires some cross-technology metric. But whether THIS metric (strict probability-times-consequence annualization) remains the right instrument, versus having calcified into a mandate that forecloses catastrophic-tail and comparative framings by definitional fiat, is exactly the contested question the founding_problem_status of 'contested' is meant to flag — not resolved by this story, but held open as the R5 genealogy interview requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    annualization_unit_contest,
    'Is annualized expected cost (probability x consequence, summed to a yearly figure) the structurally correct unit for adjudicating acceptability of irreversible, geographically concentrated, intergenerational hazards, or does the annualization step itself systematically discount catastrophic tails regardless of how accurately the underlying probabilities are estimated?',
    'This is the central disagreement between this reading and the catastrophic_tail_dominant sibling reading (see network.affects_constraints and cs_structure.reading_relations). No empirical measurement resolves it because it is a question about which decision unit is normatively appropriate, not about the accuracy of any given probability estimate; it would require either a philosophical/legal settlement on discounting irreversible harm, or an institutional shift in which framework governs licensing.',
    'If the annualization unit is rejected as inappropriate for irreversible concentrated harms, this reading''s low victim-exclusion of nuclear collapses and the catastrophic_tail_dominant reading''s much larger victim set becomes the operative constraint for the same physical facilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(annualization_unit_contest, conceptual, 'Whether annualized expected-value is the correct decision unit for catastrophic, irreversible, intergenerational hazards — the core axis separating this reading from catastrophic_tail_dominant.').

omega_variable(
    absolute_vs_comparative_threshold,
    'Does expected-value acceptability function as an absolute threshold (nuclear is acceptable if its annualized expected cost is low in itself) or is it implicitly comparative (nuclear is acceptable because its annualized expected cost is lower than the fossil alternative it displaces)?',
    'Examine whether licensing decisions under this framework have ever approved a facility whose expected-value figure was low in absolute terms but the alternative fuel mix was also low-risk (i.e., would the standard still favor nuclear siting in a hypothetical all-renewable-baseline scenario) — this would reveal whether the expected_value_dominant reading is doing genuinely independent work or is a disguised version of comparative_risk_dominant.',
    'If expected-value acceptability turns out to be doing no independent normative work beyond ''better than the fossil baseline,'' this reading and comparative_risk_dominant may not be as structurally distinct as the kernel manifest presents them, which would call for re-examining the kernel decomposition itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_vs_comparative_threshold, conceptual, 'Whether this reading''s absolute-threshold framing is structurally independent of the comparative_risk_dominant sibling''s relative framing, or secretly reduces to it.').

omega_variable(
    waste_disposal_solvability,
    'Is spent-fuel and long-term waste stewardship genuinely a bounded, solvable engineering and cost-amortization problem, as this reading treats it, or does treating it that way understate an open-ended custodial burden with no time horizon at which the problem is actually closed?',
    'Track whether any deep geological repository or equivalent disposal solution reaches an actual operational closure date with monitoring costs terminating as originally projected, versus repositories whose projected closure dates have repeatedly extended (e.g., multi-decade siting delays) — a pattern of indefinite extension would be evidence against the ''solvable engineering problem'' framing this reading depends on.',
    'If waste disposal does not actually close on the projected timeline, the annualized cost figure this reading uses for acceptability understates the true expected cost by treating an open-ended liability as a bounded one — directly inflating the ε this reading should carry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_solvability, empirical, 'Whether nuclear waste disposal is genuinely bounded/solvable as this reading assumes, or open-ended in a way the annualized-cost framing conceals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.11).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.12).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.13).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.14).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy_catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy_comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the acceptable_risk_for_energy kernel. catastrophic_tail_dominant weights irreversible, low-probability, high-consequence events far more heavily and correspondingly authors a much larger victim set and higher ε for the same underlying facilities. comparative_risk_dominant makes acceptability entirely relative to the available alternative fuel mix rather than absolute, which decouples nuclear's acceptability from any fixed expected-value threshold. All three share the same underlying nuclear/energy-siting subject matter but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different classifications — per the ε-invariance principle, they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
