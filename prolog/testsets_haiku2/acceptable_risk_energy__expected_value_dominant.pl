% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Minimization in Energy Policy (Expected-Value Dominant Reading)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel:
 *   'acceptable risk in energy policy.' The kernel itself is a commitment to
 *   making energy infrastructure decisions under uncertainty using some risk
 *   calculus. This reading—expected-value-dominant—asserts that acceptable
 *   risk means minimizing aggregate expected harm (mortality-per-TWh),
 *   computed by probability-weighting all outcomes. Under this reading,
 *   fossil fuel's chronic, continuous harms (occupational mortality, air
 *   pollution) are counted at full probability (they happen every year);
 *   nuclear catastrophic accidents are counted at their empirical probability
 *   (0.0001–0.01). The calculation yields a ranking of pathways by expected
 *   aggregate mortality, which guides policy. The reading's core benefit is
 *   commensurability: it provides a single numerical basis for comparing
 *   incommensurable harm types. Its core burden is borne by populations
 *   subject to chronic, certain harm (fossil workers and
 *   air-pollution-exposed communities), whose deaths are weighted at 100%
 *   while catastrophic risks are discounted. Sibling readings
 *   (catastrophic-tail-dominant, option-value-preserving) contest this
 *   weighting, arguing it misses critical distinctions between certain
 *   chronic harm and low-probability catastrophic risk, or that deep
 *   uncertainty demands flexibility rather than optimization. This story
 *   describes the expected-value-dominant reading as structurally
 *   instantiated—the framework that dominates energy policy institutions, the
 *   suppression required to maintain it, and the distribution of extraction
 *   burden it imposes.
 *
 * KEY AGENTS:
 *   - Energy policy decision-makers (institutional agenda-setters): adopt and enforce expected-value framework
 *   - Fossil fuel industry (institutional beneficiary): benefits from chronic-harm-as-baseline legitimacy
 *   - Fossil-exposed workers (powerless payers): bear full probability of occupational mortality
 *   - Air-pollution-affected populations (powerless payers): bear full probability of chronic health harms
 *   - Mining community residents (organized payers): bear environmental costs of resource extraction
 *   - Nuclear industry and advocates (organized beneficiaries): benefit from catastrophe-probability discounting
 *   - Alternative pathway advocates (moderate payers): constrained by framework's decision priority
 *   - Risk assessment scientific community (institutional observers): produce metrics but do not author framework choice
 *   - Catastrophic-tail advocates (excluded): would argue for tail-risk prioritization but are structurally excluded from decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.72).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value Risk Minimization in Energy Policy (Expected-Value Dominant Reading)").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '24a37f47-2a10-41fb-b98a-08471b6e397a').
narrative_ontology:cs_kernel_codification('24a37f47-2a10-41fb-b98a-08471b6e397a', distributed).
narrative_ontology:cs_authority_grounding('24a37f47-2a10-41fb-b98a-08471b6e397a', extraction).
narrative_ontology:cs_interpretation_layer_present('24a37f47-2a10-41fb-b98a-08471b6e397a').
narrative_ontology:cs_reading_relation('24a37f47-2a10-41fb-b98a-08471b6e397a', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('24a37f47-2a10-41fb-b98a-08471b6e397a', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('24a37f47-2a10-41fb-b98a-08471b6e397a', foundational, probabilistic_harm_aggregation).
narrative_ontology:cs_axiom_status(probabilistic_harm_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('24a37f47-2a10-41fb-b98a-08471b6e397a', probabilistic_harm_aggregation, empirically_contingent).
narrative_ontology:cs_axiom('24a37f47-2a10-41fb-b98a-08471b6e397a', foundational, expected_value_maximization_criterion).
narrative_ontology:cs_axiom_status(expected_value_maximization_criterion, holdable).
narrative_ontology:cs_axiom_grounding('24a37f47-2a10-41fb-b98a-08471b6e397a', expected_value_maximization_criterion, instrumental).
narrative_ontology:cs_reference_frame('24a37f47-2a10-41fb-b98a-08471b6e397a', commensurable_energy_risk_ranking).
narrative_ontology:cs_drift_state('24a37f47-2a10-41fb-b98a-08471b6e397a', contemporary_climate_urgency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24a37f47-2a10-41fb-b98a-08471b6e397a', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, expected_value_framework_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_exposed_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, mining_community_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_and_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, alternative_pathway_advocates).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_utility_maximization_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, probabilistic_risk_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The expected-value framework, by discounting catastrophic nuclear outcomes by their (low) probability, renders fossil fuel's chronic, distributed harms (air pollution, occupational disease) as numerically dominant under aggregate mortality metrics only at very high confidence thresholds. The framework's acceptance allows continued fossil deployment where the alternative (nuclear or rapid renewables transition) would be framed as riskier per expected-value calculation. The industry benefits from the framework's institutional adoption because it sustains the policy environment that permits incremental fossil expansion under risk-minimization rhetoric.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Adopt the expected-value framework as the decision standard for energy infrastructure investment and decommissioning. They enforce it by excluding alternative framings (tail-risk prioritization, option-value preservation) from impact assessments and regulatory approval processes. They justify the framework as objective, science-based, and maximally efficient. They bear the political cost if a low-probability catastrophe occurs under their framework (though the framework's ex-ante logic would still vindicate the choice).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_policy_decision_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Face occupational mortality and morbidity (mining accidents, coal dust disease, oil-field injuries) that are continuous and well-documented. Under expected-value framing, their deaths appear as inevitable baseline costs of energy production, weighted at full probability (they happen every year). The framework does not distinguish chronic, localized, inescapable exposure from probabilistic catastrophic risk. Their exit is trapped: dependence on mine or field employment for survival, limited geographic mobility, few alternative livelihoods.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_exposed_workers, payer,
    powerless, biographical, trapped, local).

% Populations downwind and downwind-over-decades from coal, gas, and oil combustion sites experience elevated respiratory disease, cardiovascular mortality, and reduced life expectancy. These harms are quantified in mortality-per-TWh metrics but are diffuse, chronic, and politically invisible relative to spectacular accidents. Their geographic constraint (living in industrial regions) and economic constraint (jobs, housing proximity) mean exit is costly. The framework counts their deaths but renders them as baseline costs of incumbent energy systems.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations, payer,
    powerless, biographical, constrained, regional).

% Face environmental degradation, water contamination, subsidence, and community dissolution from coal, uranium, and rare-earth mining. They organize resistance but operate at a power disadvantage relative to the institutional decision-makers and industry. Under expected-value framing, mining deaths and environmental costs are counted but rendered as distributed costs of energy production. Relocation is costly and culturally dislocating; staying means accepting environmental risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, mining_community_residents, payer,
    organized, generational, constrained, regional).

% The expected-value framework, by probability-discounting catastrophic accidents, renders nuclear's per-TWh mortality as competitive with or superior to fossil alternatives in aggregate expected-harm calculations. This framework sustains policy space for nuclear expansion in regions where catastrophic-tail concerns would otherwise dominate. They benefit from the framework's institutional legitimacy as the decision standard, even as they also advocate for higher nuclear deployment under alternative readings.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_and_advocates, beneficiary,
    organized, generational, mobile, global).

% Advocate for rapid renewable deployment, demand-side management, and distributed generation as pathways to reduce energy mortality without either fossil or nuclear reliance. The expected-value framework constrains their policy space by requiring that any alternative pathway must show lower expected aggregate mortality under probabilistic weighting. Their exit from the framework is constrained by the institutional dominance of expected-value reasoning in regulatory bodies; they can present alternative analyses but operate within a decision structure that privileges expected-value aggregation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, alternative_pathway_advocates, payer,
    moderate, biographical, constrained, global).

% Produces the mortality-per-TWh metrics and risk calculations that feed the expected-value framework. They document the empirical facts (fossil chronic harms, nuclear accident probabilities) but do not author the normative choice to aggregate by expected value rather than alternative criteria. Their analytical seat provides the data; the framework choice sits with decision-makers.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, risk_assessment_scientific_community, observer,
    institutional, biographical, analytical, global).

% Argue that low-probability, high-impact outcomes (nuclear meltdown, solar minimum disruption, cascading grid failure) should be prioritized in energy risk assessment even if they reduce expected aggregate mortality. They are structurally excluded from regulatory approval processes that adopt the expected-value standard as the decision criterion. Their voice would argue for constraining nuclear deployment, preserving options, or accepting higher expected mortality to avoid tail risks—a fundamentally different risk calculus that the expected-value framework is designed to override.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_tail_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a quantitative, aggregatable decision criterion for comparing heterogeneous energy pathways on a single mortality scale: expected value of harm per unit energy. Solves the problem of comparing fatal occupational hazards, chronic pollution exposure, and low-probability catastrophic events on a common numeraire, enabling policy optimization under uncertainty.
% TRANSFER_FUNCTION: Moves policy legitimacy and resource allocation from lower-probability-but-catastrophic pathways (nuclear, concentrated-risk renewables) toward higher-probability-but-distributed-harm pathways (fossil fuel) by weighting fossil chronic deaths at full probability (1.0) and nuclear catastrophes at their empirical probability (0.01–0.0001). This transfer occurs as a reallocation of acceptable risk burden: fossil-exposed workers and pollution-affected populations bear the full, continuous cost; nuclear communities bear a discounted catastrophic risk. The framework itself collects no direct rents but sustains the institutional legitimacy of fossil deployment by rendering the decision 'scientific' and 'objective.'
% ABSENT_VOICES: Populations with moral frameworks that reject probabilistic aggregation of deaths (those who hold that causing ten certain deaths is categorically different from causing one death at 10x probability) are structurally excluded from regulatory decision-making under this framework. Communities bearing chronic fossil harm (mining residents, air-pollution-exposed) have limited voice in the framework's adoption because decision-making occurs at national/global scale while their experience is local. Catastrophic-tail advocates (who prioritize avoiding low-probability catastrophic outcomes) are excluded by the framework's design. Beneficiaries of the framework (fossil industry, some policy-makers) set the terms and close the deliberative space.
% DISAPPEARANCE_RATIONALE: If the expected-value-dominant framework disappeared as a policy standard, decision-making would fracture into competing criteria: some jurisdictions would adopt tail-risk prioritization (constraining nuclear and concentrated risks, accepting higher expected aggregate mortality); others would preserve option-value reasoning (deploying diverse pathways to hedge deep uncertainty); still others might return to ad-hoc political contestation over energy risk. Energy infrastructure deployment patterns would shift—nuclear might face higher approval barriers in some regions, fossil deployment might be further constrained where catastrophic risks are politically salient, renewable deployment might accelerate in uncertainty-preserving jurisdictions. The framework's disappearance would reallocate policy risk and disallow aggregation across heterogeneous harm types.
% FOUNDING_PROBLEM: Energy infrastructure imposes mortality risk across all pathways (fossil occupational hazards and pollution, nuclear accident probability, renewable manufacturing and deployment risks). Early energy policy lacked a quantitative framework for comparing these incommensurable harms on a single scale. The expected-value framework was adopted to solve the problem: apply probabilistic reasoning to make risk commensurable, enabling rational choice among pathways.
% FOUNDING_PROBLEM_CORROBORATION: Energy policy analysts and risk assessment communities attest the founding problem is solved and the expected-value framework is the scientifically justified standard (published in journals, adopted by regulatory bodies). Fossil-exposed workers and mining communities attest the framework has shifted policy risk onto their shoulders by rendering chronic harms as baseline and catastrophic risks as discounted. Catastrophic-tail advocates and some climate scientists attest that the founding problem—comparing incommensurable harms—cannot be solved by expected-value aggregation alone; they argue the framework is a policy choice masquerading as scientific objectivity. Academic literature on decision theory (Sunstein, Morgenstern, Taleb) documents the normative choice embedded in expected-value maximization: it is not a neutral fact, but a framework that privileges certain value functions (aggregate harm minimization) over others (tail-risk aversion, option preservation).
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the framework's core mechanism: it aggregates away from the visible, certain harms borne by powerless populations and renders them as baseline costs, while discounting catastrophic risks that would otherwise dominate policy. The framework is extractive because it systematically weights fossil chronic harms at full probability while nuclear catastrophes are discounted—the aggregate effect is to sustain fossil deployment by making the decision appear mathematically rather than politically justified. Suppression (0.72) is high because the framework's persistence depends on active maintenance of the expected-value calculation as THE decision standard and exclusion of alternative framings (tail-risk, option-value) from regulatory approval processes. Without active suppression of alternatives, the framework would face immediate challenge from catastrophic-tail advocates and option-value preservationists. Theater (0.41) is moderate: the framework IS performing real coordination work (providing a single quantitative basis for comparing pathways), but roughly 41% of the effort is theater—presenting the decision as objective and mathematical when the choice to aggregate by expected value rather than tail-risk or option-value is itself a normative commitment that is not empirically discovered but politically selected. Accessibility collapse (0.58) is moderate: the framework's logic is publicly available and understood by policy professionals, but once it is adopted as the institutional standard, alternatives become harder to access—deliberative space closes around the expected-value calculus. Resistance (0.64) is substantial: fossil-exposed workers organize, mining communities resist, catastrophic-tail advocates publish, alternative frameworks are debated in academic literature—but the framework's institutional embeddedness means resistance does not dislodge it. Measurements track rising extractiveness and suppression requirement over the 40-year interval as the framework becomes more institutionalized and more actively defended against challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the policy-maker seat and the expected-value advocate seat, the framework appears to solve a genuine problem: energy infrastructure decision-making under uncertainty with heterogeneous harm types. The framework's adoption is justified as scientific rationality—applying probability theory to make risk commensurable. From this seat, alternatives (tail-risk, option-value) appear as departures from rationality, special pleading for particular pathways, or failure to optimize. From the fossil-exposed worker seat and mining community seat, the framework appears as a mechanism for legitimating their chronic sacrifice. Their deaths are rendered as baseline costs (probabilities: 1.0 for occupational hazard, 1.0 for air pollution) while nuclear catastrophes are discounted (probabilities: 0.0001). The mathematical appearance of objectivity masks a political choice to prioritize expected aggregate harm over fairness, precaution, or consent. These perspectives cannot be reconciled within the framework itself because they disagree on the normative priority: the framework embeds expected-value maximization as THE decision criterion; alternative seats would choose differently. The engine captures this divergence through directionality: beneficiary seats get lower d (toward 0), payer seats get higher d (toward 1), and per-seat types diverge as a result.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel industry occupies the beneficiary seat (d near 0.2): the framework legitimates continued fossil deployment by rendering chronic harms as inevitable baseline. Expected-value framework advocates occupy an agenda-setter seat (d near 0.4–0.5): they implement and enforce the framework but also bear political exposure if a catastrophe occurs under their chosen standard. Fossil-exposed workers occupy the target seat (d near 0.85–0.9): their deaths are counted at full probability under the framework, their exit options are trapped (occupational dependence, geographic constraint), and they are not consulted on the framework choice. Air-pollution-affected populations are similar (d near 0.80): chronic exposure with constrained exit options. Mining communities are organized but still powerless relative to institutional decision-makers (d near 0.75). Nuclear advocates occupy a mixed seat (d near 0.35–0.45): they benefit from probability discounting but also face elevated scrutiny and deployment constraints in many jurisdictions. Alternative pathway advocates face mild extraction (d near 0.55–0.65): the framework constrains their policy space but does not eliminate it entirely. The divergence in directionality across seats should produce different computed classifications: from the beneficiary seat (fossil industry, policy-makers), the constraint may appear as rope (genuine coordination solving an incommensurability problem); from the payer seat (exposed workers), it appears as snare (pure extraction, suppression, constrained exit). The engine computes these per-seat differences from the structural data; the claimed type is Tangled Rope (both coordination function and asymmetric extraction present).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (commensurability of heterogeneous energy harms) is CONTESTED in status. Decision-makers and framework advocates attest the problem is solved: the framework provides a single numerical basis for comparing occupational hazards, pollution, and catastrophic risks. Fossil-exposed populations, catastrophic-tail advocates, and alternative pathway scholars attest the founding problem is NOT solved—it is relocated. The framework achieves aggregation by discarding normative distinctions (certain vs. probable harm, concentrated vs. distributed risk) that are morally and decision-theoretically relevant. Under this reading, the constraint's mandate has not been fulfilled; instead, it has been substituted. The original mandate was to make commensurable incommensurable harms. The framework's response is to impose a single metric (expected mortality per TWh) that renders the incommensurability invisible rather than resolved. This is mandate drift—the original problem persists but is now hidden under the guise of solution. Classification as Tangled Rope (not Snare) reflects the constraint's real coordination component: it DOES provide a decision criterion that enables policy choices that were otherwise paralyzed by incommensurability. But the extraction component is substantial: the framework systematically weights certain chronic harms at full probability while discounting catastrophic risks, which benefits pathways (fossil fuel, some forms of nuclear) that would lose if tail-risk or option-value reasoning were applied. The two-components reading (coordination + extraction) is more accurate than pure extraction, but the mandatrophy analysis makes clear that the coordination component is illusory—it solves a false version of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probabilistic_aggregation_axiom_contestation,
    'Is aggregating deaths probabilistically (counting one certain death and one 1%-probability death as equivalent to 1.01 expected deaths) a neutral technical choice or a normative claim about acceptable risk that privileges certain value functions over others?',
    'Philosophical and decision-theoretic analysis of alternative aggregation schemes (tail-risk prioritization, option-value, minimax). This is not empirically resolvable but is conceptually clarifiable: document whether expected-value maximization is THE rational choice or A choice among normatively distinct alternatives.',
    'If aggregation is revealed as normative (not technical), the framework''s claim to objectivity collapses and alternative readings (catastrophic-tail-dominant, option-value-preserving) become structurally on equal footing. The framework shifts from being ''the right answer'' to being ''one contested decision criterion.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probabilistic_aggregation_axiom_contestation, conceptual, 'Whether expected-value aggregation is a neutral technical method or embeds a normative value choice.').

omega_variable(
    commensurability_across_harm_types,
    'Can occupational hazards (which workers enter knowingly, with some compensatory benefit), chronic pollution exposure (which is imposed non-consensually on third parties), and low-probability catastrophic accidents (which disrupt entire regions) be meaningfully aggregated on a single mortality scale without losing critical distinctions?',
    'Ethical analysis and empirical research on how affected populations experience and trade off different harm types. Do communities actually regard 100 occupational deaths as equivalent to 100 air-pollution deaths at the population level?',
    'If these harm types are incommensurable on the relevant moral/decision-theoretic grounds, the mortality-per-TWh metric becomes a category error, and the framework''s aggregation becomes indefensible regardless of empirical accuracy. Classification could shift from Tangled Rope (coordination + extraction) to Snare (pure extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commensurability_across_harm_types, conceptual, 'Whether different types of energy harm are commensurable on a single mortality scale or require separate decision criteria.').

omega_variable(
    distributional_asymmetry_visibility,
    'Are the suppressed voices and excluded framings of this constraint known and deliberately overridden, or are they simply absent from the deliberative space such that decision-makers genuinely believe the framework is unopposed?',
    'Documentary analysis of regulatory proceedings, policy papers, and stakeholder comments. If catastrophic-tail and option-value readings appear in the record but are explicitly rejected, suppression is active. If they are absent, suppression is structural (the framework closes the conversation before alternatives are articulated).',
    'If suppression is structural rather than active, the constraint''s classification remains Tangled Rope (coordination + asymmetric extraction) but the theater_ratio interpretation shifts: the performance is unintentional (decision-makers believe they are applying neutral science) rather than deliberate. If suppression is active, theater_ratio interpretation becomes harder—the constraint knowingly overrides alternatives, which is more extraction-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_asymmetry_visibility, empirical, 'Whether suppression of alternative framings is active enforcement or structural inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__expected_value_dominant, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__expected_value_dominant, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__expected_value_dominant, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(acce_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(acce_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(acce_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family decomposed per the ε-invariance principle (OQ-26). The contested kernel 'acceptable risk in energy policy' instantiates three structurally distinct constraints, each with its own ε, beneficiary/victim set, and classification. The expected-value-dominant reading (this story) discounts catastrophic probability; the catastrophic-tail-dominant reading priority-weights low-probability outcomes; the option-value-preserving reading preserves flexibility. Each reading is ε-invariant (does not change how the constraint is measured); together they form a constraint family linked by network.affects_constraints and representing institutional contestation over which risk calculus is legitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, powerless, 0.87).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
