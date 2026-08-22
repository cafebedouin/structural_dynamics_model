% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Reading of Acceptable Nuclear Risk
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the catastrophic-tail-dominant reading of the
 *   contested 'acceptable risk for energy' kernel: the position that
 *   low-probability, high-consequence, irreversible harms (reactor accidents,
 *   multi-millennial waste storage failure) should dominate the acceptability
 *   calculus regardless of expected-value comparison to alternative energy
 *   sources. Under this reading, nuclear power enters the victim set not
 *   because it is uniquely dangerous in expectation but because
 *   tail-weighting treats any uncontained catastrophic possibility as
 *   disqualifying on its own terms, independent of frequency. The reading
 *   suppresses probabilistic trade-off framing (treating it as a category
 *   error when applied to irreversible harms) and treats long-duration waste
 *   disposal as an unresolved constraint on legitimacy rather than as a
 *   bounded engineering problem to be optimized. Sibling readings —
 *   expected_value_dominant and comparative_risk_dominant — are NOT part of
 *   this constraint; they are separate constraints with their own epsilon
 *   values, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - precautionary_regulatory_agencies: agenda_setter (institutional/analytical) — administers and enforces the tail-dominant licensing standard
 *   - anti_nuclear_advocacy_organizations: beneficiary (organized/mobile) — political and organizational relevance sustained by the framing
 *   - fossil_incumbent_competitors: beneficiary (powerful/arbitrage) — indirect market benefit from delayed nuclear competition
 *   - nuclear_industry_operators: payer (powerful/constrained) — bears licensing cost and delay under the standard
 *   - displaced_coal_region_communities: payer (powerless/trapped) — bears continued fossil harm while nuclear replacement is foreclosed
 *   - future_generations_bearing_waste_burden: payer/beneficiary (powerless/trapped) — the constraint's stated moral referent and an uncompensated party
 *   - expected_value_energy_planners: excluded (moderate/constrained) — analytical framework structurally barred from the licensing forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Reading of Acceptable Nuclear Risk").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '38e2b25f-59db-4828-8698-1df9a66b6488').
narrative_ontology:cs_kernel_codification('38e2b25f-59db-4828-8698-1df9a66b6488', distributed).
narrative_ontology:cs_authority_grounding('38e2b25f-59db-4828-8698-1df9a66b6488', distributed).
narrative_ontology:cs_reading_relation('38e2b25f-59db-4828-8698-1df9a66b6488', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('38e2b25f-59db-4828-8698-1df9a66b6488', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('38e2b25f-59db-4828-8698-1df9a66b6488', foundational, irreversible_intergenerational_harm_overrides_probability_weighting).
narrative_ontology:cs_axiom_status(irreversible_intergenerational_harm_overrides_probability_weighting, holdable).
narrative_ontology:cs_axiom_grounding('38e2b25f-59db-4828-8698-1df9a66b6488', irreversible_intergenerational_harm_overrides_probability_weighting, deontological).
narrative_ontology:cs_axiom('38e2b25f-59db-4828-8698-1df9a66b6488', secondary, consent_of_future_generations_cannot_be_proxied_by_present_expected_value).
narrative_ontology:cs_axiom_status(consent_of_future_generations_cannot_be_proxied_by_present_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('38e2b25f-59db-4828-8698-1df9a66b6488', consent_of_future_generations_cannot_be_proxied_by_present_expected_value, deontological).
narrative_ontology:cs_reference_frame('38e2b25f-59db-4828-8698-1df9a66b6488', precautionary_principle_primacy).
narrative_ontology:cs_drift_state('38e2b25f-59db-4828-8698-1df9a66b6488', post_fukushima_relicensing_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38e2b25f-59db-4828-8698-1df9a66b6488', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_incumbent_competitors).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_agencies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry_operators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, displaced_coal_region_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_burden).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timelines).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_burden).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_priority_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set licensing and siting rules that weight worst-case, low-probability scenarios (core melt, containment breach, long-term waste migration) far above their probability-weighted expected cost, and enforce these through permitting delay, litigation exposure, and design mandates. They administer the framework and could relax it, but institutional legitimacy and public mandate are built on maintaining the precautionary posture.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Gain political relevance, funding, and legal standing by keeping catastrophic-tail framing dominant in public discourse and litigation. They face no cost if the framing suppresses nuclear expansion; their organizational purpose is substantially served by the constraint's persistence.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Benefit indirectly: every nuclear project delayed or cancelled under catastrophic-tail licensing burden extends the market window for existing fossil generation capacity. They do not need to argue against nuclear directly — the constraint does that work for them.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_incumbent_competitors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear multi-decade licensing timelines, cost overruns from worst-case design requirements, and permanent litigation exposure calibrated to tail scenarios rather than expected harm. Cannot exit the framework and still operate — the license to build and run a reactor is gated entirely by satisfying the catastrophic-tail standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry_operators, payer,
    powerful, biographical, constrained, national).

% Live with the continued operation of fossil plants (and their measurable, non-tail health and climate harms) because nuclear replacement capacity is delayed or foreclosed by the catastrophic-tail standard. They have no seat in the licensing debate and cannot relocate the economic base tied to the existing plants.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, displaced_coal_region_communities, payer,
    powerless, biographical, trapped, regional).

% Named as the moral justification for the tail-dominant standard (waste persists for millennia, so present decisions bind them) yet are simultaneously the group whose interests the standard claims to protect by foreclosing waste-generating build-out. They cannot consent, object, or verify that the tradeoff made on their behalf was correctly weighted.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_burden, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_burden, beneficiary).

% Not an actor but the aggregate decarbonization schedule that bears the opportunity cost when low-carbon capacity that could have displaced fossil generation is delayed by tail-dominant licensing requirements. Included for narrative completeness as the thing the constraint trades against, not as a party with standing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timelines, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timelines).

% Analysts who would frame acceptability as probability-weighted expected harm against comparative energy-source mortality data. Their framework is structurally excluded from the licensing conversation because the catastrophic-tail standard treats any probability-weighting of irreversible harm as illegitimate on its face, not merely as a losing argument.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, expected_value_energy_planners, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine collective commitment: that decisions binding future generations to irreversible, uncontainable, or unlimited-duration harms should not be authorized on the basis of low estimated probability alone, regardless of expected-value calculus.
% TRANSFER_FUNCTION: Moves decision authority away from probability-weighted engineering and economic analysis and toward worst-case scenario framing; moves the cost of continued fossil generation onto displaced communities and the climate timeline, while moving political and organizational capital toward precautionary agencies and anti-nuclear advocacy groups.
% ABSENT_VOICES: Expected-value energy planners and comparative-risk analysts would argue that treating one class of low-probability harm as absolute while ignoring the certain, ongoing harms of the fossil alternative it forecloses is itself an unweighted and inconsistent standard; they are excluded from the licensing forum entirely, not merely outvoted within it.
% DISAPPEARANCE_RATIONALE: If catastrophic-tail-dominant framing vanished from licensing and public discourse overnight, nuclear siting and design review would shift toward probability-weighted or comparative-risk standards, licensing timelines would compress, advocacy organizations built around the tail-risk framing would lose political relevance, and fossil incumbents would lose a structural delay mechanism against their principal low-carbon competitor.
% FOUNDING_PROBLEM: Built to solve a genuine problem: nuclear accidents and waste disposal decisions can impose harms that are effectively irreversible and span timescales beyond any single generation's ability to consent to or correct for, and ordinary expected-value calculus systematically underweights such harms because it treats probability and consequence as freely interchangeable.
% FOUNDING_PROBLEM_CORROBORATION: Risk theorists working outside both the nuclear industry and anti-nuclear advocacy (e.g., decision theorists studying fat-tailed and irreversible-harm domains) corroborate that the underlying asymmetry problem is real and not solved by expected-value framing alone. However, comparative-risk analysts and IPCC-adjacent climate economists — also outside the benefiting parties — attest that the founding problem, as currently operationalized in nuclear licensing specifically, has been decoupled from its original justification and now functions to foreclose a demonstrably lower-tail-risk alternative to fossil generation, making the status contested rather than settled.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that under this reading the standard's persistence transfers real costs — indefinitely deferred fossil-plant retirement, foregone decarbonization capacity, uncompensated waste-siting burden on future generations — while concentrating political and competitive benefit on advocacy organizations and fossil incumbents who bear none of those costs. Suppression (0.71) is high because the reading's core mechanism is precisely the exclusion of probabilistic trade-off framing from the legitimate discourse: expected-value and comparative-risk arguments are not defeated on the merits within this reading, they are ruled inadmissible as a category. Theater ratio is moderate-low (0.28): the underlying concern about irreversibility is substantively real, not merely performative, but a growing share of licensing activity defends the precautionary posture itself rather than assessing actual tail risk with updated engineering data (reflected in the rising measurement series).
 *
 * DIRECTIONALITY LOGIC:
 *   Precautionary regulatory agencies and anti-nuclear advocacy organizations sit near the beneficiary end: their institutional and political capital is built on maintaining tail-dominant framing, and they bear none of the foreclosed-alternative costs. Fossil incumbents benefit indirectly through market timing without needing to participate in the framing fight at all. Nuclear operators sit near the full-target end: trapped by licensing dependency, unable to exit the framework and still operate. Displaced coal-region communities and future generations are structurally powerless targets — the latter is the reading's own stated moral referent, which makes their lack of any actual voice or compensation mechanism a load-bearing irony rather than an incidental gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — irreversible, intergenerational harm systematically underweighted by naive expected-value math — remains genuinely live as an abstract decision-theoretic concern (corroborated by risk theorists outside both camps). But as OPERATIONALIZED specifically against nuclear licensing while leaving fossil generation's certain, non-tail, ongoing harms outside the same standard, the founding problem's application has drifted from solving the underweighting problem to selectively foreclosing one substitute technology. This is exactly the tangled_rope signature: real coordination function (genuine irreversibility concern) plus asymmetric extraction (one technology bears the tail standard, its highest-carbon substitute does not) sustained by active enforcement (licensing gatekeeping, litigation exposure). Classifying it as a pure snare would erase the real decision-theoretic problem it responds to; classifying it as a pure rope would erase the asymmetric cost it imposes on nuclear operators, displaced communities, and the climate timeline relative to the untaxed fossil alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_asymmetry_selectivity,
    'Is the tail-dominant standard applied consistently to all energy sources with irreversible or intergenerational harm potential (e.g., cumulative climate forcing, aquifer contamination from other industrial activity), or selectively to nuclear alone?',
    'Comparative regulatory audit: catalog which energy and industrial technologies are licensed under tail-dominant versus expected-value or comparative-risk standards, and whether the standard applied correlates with technology type independent of actual tail-risk magnitude.',
    'If selectively applied to nuclear while fossil generation''s own irreversible harms (climate tipping points, particulate mortality) are assessed under expected-value logic, the tangled_rope classification is reinforced — the coordination rationale is real but the enforcement is asymmetric. If applied consistently across technologies, the constraint reads closer to a genuine, if costly, precautionary rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_asymmetry_selectivity, empirical, 'Whether tail-dominant framing is applied consistently across energy sources or selectively against nuclear.').

omega_variable(
    future_generations_proxy_legitimacy,
    'Does the precautionary standard, as currently administered, actually track the interests of future generations, or has invoking them become a rhetorical fixture disconnected from any mechanism for representing or verifying those interests?',
    'Examine whether waste-disposal engineering and siting decisions made under this standard have been revised in response to updated long-term safety data, versus whether the standard''s stringency has remained static regardless of engineering progress — a static standard suggests the future-generations referent is symbolic rather than operative.',
    'If the standard is unresponsive to engineering improvements in waste containment, the intergenerational justification functions more as legitimating cover for the beneficiary coalition than as an active decision procedure — strengthening the case for suppression being extraction-serving rather than purely protective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_proxy_legitimacy, conceptual, 'Whether the future-generations justification is operative or symbolic.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and the expected_value_dominant / comparative_risk_dominant siblings actually sit — is it a disagreement about facts (true accident probabilities, true waste containment timelines) or a disagreement about the legitimacy of probability-weighting irreversible harms at all?',
    'Structured elicitation comparing whether proponents of each reading would change position given updated probability estimates (suggesting factual disagreement) versus whether they reject probability-weighting as the wrong tool regardless of the numbers (suggesting a genuine framework-level disagreement, i.e., a true kernel contest rather than a resolvable empirical dispute).',
    'If the disagreement is purely factual, the three readings could in principle converge on updated data and this decomposition into separate constraints would eventually collapse into one. If it is framework-level, the three-way kernel split is a stable structural feature that no amount of updated accident-probability data will resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the kernel contest is fact-driven (convergent) or framework-driven (stable disagreement).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.19).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.22).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.25).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.27).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_licensing).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'acceptable nuclear risk' concept per the epsilon-invariance principle. catastrophic_tail_dominant (this story) authors high suppression of probability-weighting and treats waste disposal as a constraint rather than an engineering problem, yielding epsilon=0.58 with a tangled_rope claim. expected_value_dominant and comparative_risk_dominant are separate stories with their own stakeholder sets, victim sets, and lower authored epsilon (each admits probability-weighted or comparative trade-off framing that this reading suppresses). All three link to each other and to the downstream nuclear_waste_disposal_licensing constraint, which the tail-dominant reading most directly reshapes by converting a bounded engineering problem into an open-ended legitimacy constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
