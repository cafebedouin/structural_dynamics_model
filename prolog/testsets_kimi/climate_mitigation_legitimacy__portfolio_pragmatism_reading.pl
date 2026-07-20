% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Decarbonization Portfolio (Portfolio Pragmatism Reading)
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   The portfolio pragmatism reading of climate mitigation legitimacy asserts
 *   that optimal decarbonization requires a technology-neutral portfolio
 *   inclusive of both nuclear and renewable generation. This constraint
 *   operates in energy governance by framing technology exclusivity as
 *   illegitimate, thereby stabilizing broad political coalitions but
 *   diverting capital and policy support from renewable-only pathways to
 *   nuclear-inclusive portfolios. It is claimed as coordination (broad
 *   coalition, regional optimization) but exhibits asymmetric extraction: the
 *   nuclear industry gains legitimacy and financing it would not command
 *   under a pure market or renewable-priority frame, while
 *   renewable-exclusive developers and household ratepayers bear the costs of
 *   diversification. This reading instantiates one branch of the
 *   climate_mitigation_legitimacy kernel, alongside baseload_necessity,
 *   renewable_primacy, and degrowth_sufficiency readings.
 *
 * KEY AGENTS:
 *   - nuclear_industry: Primary beneficiary (powerful/constrained) â gains climate legitimacy and capital access
 *   - renewable_exclusive_investors: Primary payer (powerful/constrained) â lose exclusive subsidy access and policy priority
 *   - international_climate_institutions: Agenda setter (institutional/constrained) â enforces neutrality framing
 *   - diversified_energy_utilities: Secondary beneficiary (powerful/mobile) â gains portfolio flexibility
 *   - anti_nuclear_advocacy: Excluded voice (organized/identity_locked) â pushed outside policy consensus
 *   - household_ratepayers: Diffuse payer (powerless/trapped) â bear capital cost through rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.5).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Decarbonization Portfolio (Portfolio Pragmatism Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '50a2382f-66bb-4d8d-9292-8adbad0021cb').
narrative_ontology:cs_kernel_codification('50a2382f-66bb-4d8d-9292-8adbad0021cb', formalized).
narrative_ontology:cs_authority_grounding('50a2382f-66bb-4d8d-9292-8adbad0021cb', expertise).
narrative_ontology:cs_interpretation_layer_present('50a2382f-66bb-4d8d-9292-8adbad0021cb').
narrative_ontology:cs_reading_relation('50a2382f-66bb-4d8d-9292-8adbad0021cb', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('50a2382f-66bb-4d8d-9292-8adbad0021cb', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('50a2382f-66bb-4d8d-9292-8adbad0021cb', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('50a2382f-66bb-4d8d-9292-8adbad0021cb', foundational, technology_neutrality_mandate).
narrative_ontology:cs_axiom_status(technology_neutrality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('50a2382f-66bb-4d8d-9292-8adbad0021cb', technology_neutrality_mandate, instrumental).
narrative_ontology:cs_axiom('50a2382f-66bb-4d8d-9292-8adbad0021cb', foundational, portfolio_diversification_value).
narrative_ontology:cs_axiom_status(portfolio_diversification_value, holdable).
narrative_ontology:cs_axiom_grounding('50a2382f-66bb-4d8d-9292-8adbad0021cb', portfolio_diversification_value, empirically_contingent).
narrative_ontology:cs_reference_frame('50a2382f-66bb-4d8d-9292-8adbad0021cb', technocratic_optimization).
narrative_ontology:cs_drift_state('50a2382f-66bb-4d8d-9292-8adbad0021cb', post_renewable_cost_revolution, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('50a2382f-66bb-4d8d-9292-8adbad0021cb', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_utilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_exclusive_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, household_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains regulatory inclusion, green taxonomy access, and climate finance eligibility under a technology-neutral frame. Commercial viability depends on continued governmental recognition as a low-carbon option; cannot exit the policy framework without losing the market created by that recognition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, global).

% Can allocate capital across nuclear and renewable assets based on regional economics and regulatory signals. Benefits from portfolio flexibility and reduced concentration risk in any single technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_utilities, beneficiary,
    powerful, biographical, mobile, national).

% Face diluted access to green subsidies and climate capital when portfolio mandates require nuclear co-investment. Their business models assumed preferential policy support for renewables alone, and the neutrality frame blocks exclusive taxonomies that would concentrate returns on pure-play developers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_exclusive_investors, payer,
    powerful, biographical, constrained, global).

% Bear the capital costs of diversified generation portfolios through regulated rates and utility bills. Cannot opt out of the grid or choose a renewable-only supply path in jurisdictions where portfolio pragmatism mandates nuclear inclusion.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, household_ratepayers, payer,
    powerless, biographical, trapped, national).

% Frame decarbonization scenarios and assessment reports. Under portfolio pragmatism they maintain analytical neutrality across technologies, which stabilizes broad political coalitions but constrains their ability to endorse rapid renewable-only scaling or exclude nuclear from green finance frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, international_climate_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Their foundational opposition to nuclear power is treated as ideological rather than pragmatic in policy forums that adopt technology-neutral framing. They would object to nuclear inclusion but are marginalized from mainstream climate finance and taxonomy debates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, anti_nuclear_advocacy, excluded,
    organized, generational, identity_locked, global).

% Evaluate cost-optimal decarbonization pathways. Note that technology-neutral frameworks often reflect political coalition-management as much as engineering optimization, but provide no seat in the capital flows.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a broad political and investment coalition for decarbonization by preventing exclusion of any major low-carbon technology, enabling regional variation in optimal deployment mix and reducing technology-concentration risk.
% TRANSFER_FUNCTION: Moves capital subsidies, regulatory permits, green taxonomy status, and grid planning priority from renewable-exclusive pathways to diversified nuclear-plus-renewable portfolios; moves policy legitimacy from technology-preference advocacy to technology-neutral expertise.
% ABSENT_VOICES: Anti-nuclear movements advocating complete phase-out, renewable-purist developers arguing for 100% renewable systems, and degrowth advocates questioning large-scale generation expansion are structurally sidelined in policy forums that adopt neutrality framing.
% DISAPPEARANCE_RATIONALE: If the portfolio-pragmatism constraint vanished, subsidy flows and green taxonomies would shift toward whichever technology had current political advantage; nuclear projects would lose access to climate finance in many jurisdictions; renewable-only developers would regain exclusive access to green capital; and the political coalition backing aggressive carbon targets would fracture along technology lines, with baseload and renewable camps returning to blocking positions.
% FOUNDING_PROBLEM: Climate mitigation policy was gridlocked by technology tribalism: nuclear and renewable advocacy camps blocked each other's agendas, preventing stable, long-term decarbonization investment at the speed required by emissions targets.
% FOUNDING_PROBLEM_CORROBORATION: International climate institutions and diversified utilities attest the coalition-management problem is still live and that neutrality prevents paralysis. Renewable-exclusive investors and anti-nuclear advocates attest the problem is superseded by cheap renewables and the constraint now functions to prop nuclear economics; independent energy economists note coalition benefits but question whether the portfolio is cost-optimal relative to regional renewable abundance.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50) reflects the wedge between technology-neutral policy and least-cost regional deployment: in regions with high renewable potential and low nuclear competitiveness, the portfolio mandate forces capital into nuclear that would otherwise flow to cheaper renewables. Suppression (0.58) tracks the institutional effort required to maintain nuclear's place in green taxonomy and climate finance against renewable-purist opposition. Theater ratio (0.43) captures the growing gap between 'all options on the table' rhetoric and actual deployment economics favoring renewables. Accessibility collapse (0.45) is moderate: renewable-only pathways remain live in many jurisdictions but are increasingly excluded from mainstream climate finance eligibility. Resistance (0.55) comes from anti-nuclear movements and renewable developers who contest the neutrality frame.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear industry experiences this constraint as coordinative (access to previously excluded green capital and regulatory support). Renewable-exclusive investors experience it as extractive (diverted capital, lost policy priority). The engine computes this divergence from beneficiary/victim declarations and exit asymmetry: nuclear industry exit is constrained but improving, while renewable investors face constrained exit and identity-locked opposition to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and diversified utilities are structural beneficiaries (low d, subsidized by the constraint's operation). Renewable-exclusive investors and household ratepayers are structural targets (high d, extraction concentrated on them). International climate institutions sit near symmetric: they benefit from coalition stability but pay credibility costs when neutrality appears to contradict cost-optimal analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and victim identification for Tangled Rope. The genuine coordination (broad decarbonization coalition, grid reliability assurance) is separable from the extraction (nuclear subsidy via neutrality mandate). If the founding problem (technology tribalism blocking climate action) were dead, the constraint would drift toward Snare or Piton; measurements show rising extraction and theater, suggesting Mandatrophy is contested but not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_cost_competitiveness,
    'Is nuclear generation actually cost-competitive or reliability-necessary in the regions where portfolio pragmatism enforces its inclusion?',
    'Comprehensive LCOE meta-analysis incorporating system costs, capacity value, and regional resource profiles; regulatory discovery where nuclear receives mandated carve-outs.',
    'A wide gap between nuclear cost and renewable-plus-storage cost would establish the portfolio mandate as a subsidy transferring wealth from ratepayers and renewable investors to the nuclear industry; a narrow gap would validate the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_cost_competitiveness, empirical, 'Whether nuclear inclusion is cost-justified or subsidy-dependent.').

omega_variable(
    coalition_stability_vs_speed,
    'Does the technology-neutral frame accelerate aggregate decarbonization by preventing political backlash, or delay it by diverting capital from the fastest-deploying technology?',
    'Cross-jurisdictional regression of emissions-reduction velocity against technology-policy framing, controlling for income and renewable endowment.',
    'If neutrality delays peak deployment, the constraint extracts from climate outcomes themselves and functions as a slower road; if it accelerates by maintaining coalitions, the extraction is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_stability_vs_speed, empirical, 'Whether portfolio pragmatism trades speed for coalition breadth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmlppr_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cmlppr_tr_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(cmlppr_tr_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(cmlppr_tr_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(cmlppr_tr_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(cmlppr_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.43).

% Extraction over time
narrative_ontology:measurement(cmlppr_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cmlppr_be_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(cmlppr_be_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(cmlppr_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(cmlppr_be_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(cmlppr_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(cmlppr_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cmlppr_su_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(cmlppr_su_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(cmlppr_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(cmlppr_su_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(cmlppr_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel. The colloquial label 'climate mitigation legitimacy' decomposes into structurally distinct claims: portfolio pragmatism (technology-neutral mix), baseload necessity (dispatchable requirement), renewable primacy (sufficiency of renewables), and degrowth sufficiency (demand reduction). Each reading has a distinct epsilon, beneficiary structure, and classification. Network edges link the family members for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
