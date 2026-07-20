% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Expected-Value Dominant Acceptable Risk Framework for Energy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint is the expected_value_dominant reading of the
 *   acceptable_risk_energy kernel. It instantiates the decision rule that
 *   acceptable energy risk is the pathway minimizing aggregate expected
 *   mortality per unit energy (mortality-per-TWh). The framework coordinates
 *   policy by commensurating deaths from mining, air pollution, and accidents
 *   across technologies, but it asymmetrically extracts from fossil fuel
 *   actors by suppressing their pathway while discounting nuclear tail risks
 *   by probability. The low-carbon energy sector captures the regulatory and
 *   market gains. This story is authored as a clean Îµ-invariant constraint:
 *   it does not describe its sibling readings inside the constraint, leaving
 *   that decomposition to the kernel's network links and omega variables.
 *
 * KEY AGENTS:
 *   - energy_policy_modelers: Agenda-setter (institutional/analytical/global) â constructs and administers the mortality-per-TWh framework
 *   - low_carbon_energy_sector: Primary beneficiary (organized/constrained/global) â collects regulatory and market advantages from fossil suppression and tail-risk discounting
 *   - fossil_fuel_industry: Primary target (powerful/constrained/global) â bears regulatory suppression and market exclusion justified by the metric
 *   - fossil_dependent_communities: Secondary target (powerless/trapped/regional) â bears economic devastation from pathway suppression without voice in metric design
 *   - tail_risk_advocates: Excluded voice (moderate/constrained/global) â structurally discounted by expected-value calculus
 *   - energy_consumers: Diffuse payer with secondary benefit (organized/constrained/national) â bears transition costs and lost energy optionality while receiving air-quality improvements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value Dominant Acceptable Risk Framework for Energy").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'd86197ab-2b3a-4ad9-9f0e-c66ddbb8188e').
narrative_ontology:cs_kernel_codification('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', formalized).
narrative_ontology:cs_authority_grounding('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', expertise).
narrative_ontology:cs_interpretation_layer_present('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e').
narrative_ontology:cs_reading_relation('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', acceptable_risk_energy__catastrophic_tail_dominant, influences).
narrative_ontology:cs_reading_relation('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', foundational, aggregate_expected_harm_minimization).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_minimization, holdable).
narrative_ontology:cs_axiom_grounding('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', aggregate_expected_harm_minimization, instrumental).
narrative_ontology:cs_axiom('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', foundational, commensurable_mortality_across_pathways).
narrative_ontology:cs_axiom_status(commensurable_mortality_across_pathways, holdable).
narrative_ontology:cs_axiom_grounding('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', commensurable_mortality_across_pathways, conventional).
narrative_ontology:cs_reference_frame('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', expected_value_optimization_framework).
narrative_ontology:cs_drift_state('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', post_fukushima_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d86197ab-2b3a-4ad9-9f0e-c66ddbb8188e', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_dependent_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constructs and maintains the mortality-per-TWh comparison frameworks that define acceptable risk across energy pathways. Their professional authority, funding streams, and institutional influence depend on the dominance of expected-value methods in regulatory and investment discourse. They set the parameters for how deaths are counted, discounted, and compared.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_policy_modelers, agenda_setter,
    institutional, generational, analytical, global).

% Collects regulatory preference, subsidies, and social license from a comparative risk framework that suppresses fossil competitors and discounts low-probability catastrophic accidentsâparticularly benefiting nuclear operators who gain favorable rankings when tail events are probability-weighted. The sector has become structurally dependent on this metric for market access and financing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_sector, beneficiary,
    organized, biographical, constrained, global).

% Bears regulatory suppression, divestment pressure, and licensing exclusion caused by a comparative metric that classifies fossil operations as unacceptably harmful on an expected-value basis. Cannot easily exit the framework because global climate and energy governance increasingly embeds mortality-per-TWh rankings into binding standards.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry, payer,
    powerful, biographical, constrained, global).

% Experience stranded assets, job loss, and regional economic decline as the fossil pathway is suppressed by regulatory frameworks derived from the expected-value metric. Their health harms are counted in the data, but their economic survival is not; they have minimal voice in the analytical process that determines their viability.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_dependent_communities, payer,
    powerless, generational, trapped, regional).

% Argue that low-probability catastrophic outcomesâsevere nuclear accidents, irreversible contaminationâshould dominate energy acceptability judgments. Their perspective is structurally discounted by the expected-value calculus, which treats tail events as vanishingly small probability-weighted terms rather than decision-relevant thresholds.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, tail_risk_advocates, excluded,
    moderate, civilizational, constrained, global).

% Receive the purported air-quality and mortality-reduction benefits of a low-carbon transition driven by the framework, while bearing higher energy costs, reduced optionality, and transition risks from the suppression of established fossil infrastructure. They cannot individually opt out of the policy-regime the metric underwrites.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_consumers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, energy_consumers, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_sector).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common quantitative metric (mortality-per-TWh) to compare health risks across disparate energy technologies, enabling regulators and investors to rank generation pathways by aggregate expected harm and thereby coordinate capital allocation and licensing decisions.
% TRANSFER_FUNCTION: Moves regulatory legitimacy, capital access, and social license away from fossil fuel pathways toward nuclear and renewable pathways, justified by the expected mortality calculus; simultaneously transfers the burden of proof to fossil operators to demonstrate lower expected harm than low-carbon alternatives.
% ABSENT_VOICES: Tail-risk advocates who treat catastrophic low-probability events as non-commensurable thresholds, and fossil-dependent communities whose economic extinction is not captured by mortality statistics; they are audible in public discourse but structurally excluded from the dominant analytical framework that determines pathway acceptability.
% DISAPPEARANCE_RATIONALE: If expected-value dominance vanished overnight, energy regulators would lose their primary cross-technology comparison tool, fossil fuel investments would resurge in the near term as their suppression lifted, nuclear build-out would slow as tail-risk fears re-entered the calculus, and capital would reallocate across a radically different risk-evaluation landscape.
% FOUNDING_PROBLEM: Industrial energy production causes measurable deaths from mining accidents, air pollution, and operational hazards; without a common comparative metric, societies could not rank the human cost of different generation technologies and would default to incoherent, politically driven energy choices.
% FOUNDING_PROBLEM_CORROBORATION: Independent public health bodies such as the WHO and non-partisan energy regulators in non-fossil-dependent jurisdictions attest that comparative risk assessment remains necessary; however, fossil fuel industry analysts and philosophers of risk contest that expected-value aggregation is the appropriate tool, with corroboration for the 'live' status coming from external epidemiological research, while contestation is documented in legislative testimony and academic critiques of risk commensurability.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the framework systematically transfers capital and legitimacy away from fossil actors toward low-carbon sectors, using a metric that discounts the catastrophic risks most threatening to its primary beneficiaries. Suppression (0.75) is high because the framework's dominance requires active enforcement: fossil licensing restrictions, divestment mandates, and the epistemic suppression of alternative risk framings. Theater ratio (0.30) reflects moderate performative maintenanceâthe framework is presented as neutral public health science, yet an increasing share of its deployment defends pathway choice rather than transparent harm reduction. Accessibility collapse (0.65) captures that once the mortality-per-TWh lens is accepted, alternative risk vocabularies appear irrational, though they persist at the institutional margins. Resistance (0.60) reflects sustained fossil-industry lobbying and some governmental pushback against expected-value energy governance. The temporal series show monotonic drift: as the framework institutionalized from 1970â2010, its extractive and suppressive character intensified.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (energy_policy_modelers) experiences the constraint as a genuine scientific-coordination achievement that solves an otherwise intractable cross-technology comparison problem. The payer seats (fossil industry and dependent communities) experience the same structure as an extractive regime that weaponizes health statistics to destroy their economic viability. The excluded seat (tail-risk advocates) experiences it as a dangerous epistemic closure that renders catastrophic futures cognitively invisible. The engine computes these divergent classifications from the same structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (low_carbon_energy_sector) and agenda-setters (modelers) derive low directionality: the constraint subsidizes their authority and market position. Victims (fossil industry, fossil communities) derive high directionality: the constraint extracts regulatory legitimacy and economic opportunity from them. Tail-risk advocates are not formal victims in the extraction chain but bear high directionality as their epistemic concerns are structurally suppressed. Consumers sit near symmetric (0.5) because they simultaneously receive air-quality benefits and pay transition costs. No override is needed: the structural derivation chain captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problemâcomparing energy-related harmsâremains live, so mandatrophy is not resolved. The classification as tangled_rope rather than snare is protected by the genuine coordination function: mortality-per-TWh comparison does solve a real information-coordination problem that would otherwise produce incoherent energy choices. However, the constraint has been captured by pathway interests, as evidenced by the suppression of fossil alternatives and the discounting of nuclear tail risk. If the coordination function atrophied further and the metric became pure performative cover for suppression, the drift path would move toward snare; if the framework were reformed to neutrally compare all harms without pathway capture, it could move toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ev_kernel_reading_position,
    'This constraint instantiates the expected_value_dominant reading of the acceptable_risk_energy kernel. If the catastrophic_tail_dominant reading were adopted as the primary regulatory framework, would the low_carbon_energy_sector lose its beneficiary status and would nuclear operators become payers?',
    'Comparative jurisdictional analysis of regulators adopting post-accident tail-risk moratoria versus expected-value regimes; track whether nuclear sector classification flips from beneficiary to payer across regimes.',
    'If the nuclear sector flips from beneficiary to payer under tail-risk dominance, the current beneficiary structure is reading-dependent rather than structurally inherent, confirming the Îµ-invariance decomposition and validating separate constraint stories per reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ev_kernel_reading_position, empirical, 'Whether the beneficiary structure is stable across kernel readings.').

omega_variable(
    fossil_victim_instrumentalization,
    'Does the expected-value framework genuinely treat fossil fuel mortality as a victim class to be protected, or does it instrumentalize fossil-death statistics to justify suppression of the fossil pathway for the benefit of low-carbon sectors?',
    'Evaluate whether jurisdictions using this framework show greater mortality reduction in fossil-dependent communities or greater market-share transfer to low-carbon sectors; compare public health outcomes against capital-flow outcomes.',
    'If instrumental, the coordination function is subordinate to extraction, pushing classification toward snare. If genuine harm-reduction is primary, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_victim_instrumentalization, empirical, 'Whether fossil mortality data is used for protection or suppression.').

omega_variable(
    tail_risk_empirical_status,
    'Are low-probability nuclear catastrophic accidents correctly discounted by probability in expected-value calculations, or does empirical evidence suggest probabilistic risk assessment systematically understates tail risk?',
    'Meta-analysis of PRA accuracy against observed severe accident frequency; inclusion of intergenerational and spatially displaced harm in mortality accounting.',
    'If tail risks are under-weighted, the framework extracts from future and distant publics by externalizing catastrophic exposure, raising effective extractiveness and potentially reclassifying nuclear sector benefits as subsidized by hidden risk transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_empirical_status, empirical, 'Accuracy of nuclear tail-risk probability estimates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_ev_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acceptable_risk_ev_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.25).
narrative_ontology:measurement(acceptable_risk_ev_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.3).
narrative_ontology:measurement(acceptable_risk_ev_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.35).
narrative_ontology:measurement(acceptable_risk_ev_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_ev_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acceptable_risk_ev_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(acceptable_risk_ev_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(acceptable_risk_ev_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(acceptable_risk_ev_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_ev_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acceptable_risk_ev_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(acceptable_risk_ev_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(acceptable_risk_ev_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(acceptable_risk_ev_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is the expected_value_dominant reading of the acceptable_risk_energy kernel. Its siblings instantiate different constraints from the same kernel due to Îµ-invariance: each reading selects different observables (expected mortality vs catastrophic tail vs option value), produces different Îµ values, and has different beneficiary/victim structures. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
