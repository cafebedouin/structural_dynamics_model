% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth/Sufficiency Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'climate_mitigation_legitimacy'. The degrowth/sufficiency reading asserts
 *   that decarbonization must proceed through demand reduction and energy
 *   downsizing, rendering large-scale generation expansion unnecessary. It
 *   structurally victimizes both nuclear and renewable sectors as
 *   growth-dependent industrial paradigms, while privileging sufficiency
 *   advocates and, structurally, fossil fuel incumbents who face less
 *   competitive pressure from zero-carbon buildout. The constraint is
 *   authored as a tangled rope: it coordinates a genuine political and
 *   academic coalition around sufficiency norms, while asymmetrically
 *   extracting from industrial-scale clean-energy sectors and developing
 *   economies.
 *
 * KEY AGENTS:
 *   - degrowth_sufficiency_advocates: Primary agenda-setter (organized/global) â enforces the frame through funding, peer review, and policy advisement
 *   - fossil_fuel_incumbents: Structural beneficiary (powerful/global) â retains market share via blocked zero-carbon competition
 *   - nuclear_sector: Primary target (powerful/constrained) â denied legitimacy and investment under sufficiency framing
 *   - renewable_energy_sector: Primary target (powerful/constrained) â large-scale buildout delegitimized as green growth
 *   - energy_intensive_industries: Secondary target (powerful/constrained) â pressured to downsize rather than decarbonize supply
 *   - global_south_development: Excluded victim (powerless/trapped) â energy needs marginalized in high-income sufficiency discourse
 *   - climate_pragmatists: Analytical observer (institutional/analytical) â documents empirical gap between sufficiency rhetoric and emission outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.76).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth/Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '3a67d517-7234-4d8d-97bd-07fb894a0437').
narrative_ontology:cs_kernel_codification('3a67d517-7234-4d8d-97bd-07fb894a0437', distributed).
narrative_ontology:cs_authority_grounding('3a67d517-7234-4d8d-97bd-07fb894a0437', distributed).
narrative_ontology:cs_reading_relation('3a67d517-7234-4d8d-97bd-07fb894a0437', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a67d517-7234-4d8d-97bd-07fb894a0437', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a67d517-7234-4d8d-97bd-07fb894a0437', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('3a67d517-7234-4d8d-97bd-07fb894a0437', foundational, demand_reduction_as_necessary_path).
narrative_ontology:cs_axiom_status(demand_reduction_as_necessary_path, holdable).
narrative_ontology:cs_axiom_grounding('3a67d517-7234-4d8d-97bd-07fb894a0437', demand_reduction_as_necessary_path, empirically_contingent).
narrative_ontology:cs_axiom('3a67d517-7234-4d8d-97bd-07fb894a0437', foundational, large_scale_technology_intrinsically_domineering).
narrative_ontology:cs_axiom_status(large_scale_technology_intrinsically_domineering, holdable).
narrative_ontology:cs_axiom_grounding('3a67d517-7234-4d8d-97bd-07fb894a0437', large_scale_technology_intrinsically_domineering, deontological).
narrative_ontology:cs_reference_frame('3a67d517-7234-4d8d-97bd-07fb894a0437', sufficiency_based_mitigation).
narrative_ontology:cs_drift_state('3a67d517-7234-4d8d-97bd-07fb894a0437', post_renewable_cost_revolution, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a67d517-7234-4d8d-97bd-07fb894a0437', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_sufficiency_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinate academic and policy discourse around demand reduction and energy sufficiency as the sole legitimate path to decarbonization. Frame large-scale generation as growth-dependent and ecologically destructive. Set funding priorities, conference agendas, peer-review norms, and policy advisement to exclude buildout-oriented solutions and enforce the sufficiency frame.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_sufficiency_advocates, agenda_setter,
    organized, generational, mobile, global).

% Benefit structurally from the suppression of large-scale zero-carbon generation competition. As nuclear and renewable buildout is delayed or blocked by sufficiency framing, incumbent fossil assets retain market share, defer stranding, and continue extracting rents from persistent supply-constrained energy markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents, beneficiary,
    powerful, generational, arbitrage, global).

% Loses policy legitimacy and investment access as nuclear power is categorized as growth-dependent large-scale technology incompatible with sufficiency norms. Projects face heightened social-license and regulatory barriers in jurisdictions where degrowth frames dominate climate institutions, even where nuclear offers zero-carbon baseload.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_sector, payer,
    powerful, biographical, constrained, global).

% Industrial-scale wind, solar, and storage buildout is delegitimized as 'green growth' that perpetuates extractive metabolic dynamics. Utility-scale projects face opposition from sufficiency coalitions even when zero-carbon, compressing deployment pathways and diverting capital toward smaller, less impactful installations.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_sector, payer,
    powerful, biographical, constrained, global).

% Face policy pressure to downsize or relocate as the constraint treats industrial energy demand as illegitimate rather than as a target for clean-supply substitution. Carbon-leakage risk rises where jurisdictions adopt sufficiency-only frames that do not decarbonize production but simply displace it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Developing economies requiring energy expansion for poverty alleviation and industrialization are excluded from climate-legitimacy discourse when the frame treats all energy growth as problematic. Their voices are marginalized in international climate institutions dominated by sufficiency framings originating in high-income contexts.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_development, excluded,
    powerless, generational, trapped, global).

% Observe that empirically, renewable cost declines and electrification needs make large-scale clean-generation expansion necessary and feasible. Document the gap between sufficiency rhetoric and emission outcomes in jurisdictions where demand-only strategies fail to displace fossil fuels at the speed required by climate targets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_pragmatists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates climate action around shared demand-reduction targets, energy-sufficiency norms, and anti-growth solidarity among high-income advocates, creating a unified political identity and policy bloc against expansionist industrial paradigms.
% TRANSFER_FUNCTION: Moves policy legitimacy, research funding, and institutional authority from large-scale zero-carbon generation sectors and energy-intensive industries to sufficiency advocacy networks, while structurally preserving fossil-fuel incumbency by blocking rapid zero-carbon substitution.
% ABSENT_VOICES: Developing economies requiring energy expansion for development, and industrial workers in energy-intensive sectors, are largely absent from sufficiency-framed climate discourse; they would contest the universalization of demand reduction but are not seated in the institutions where this constraint is enforced.
% DISAPPEARANCE_RATIONALE: If the degrowth sufficiency frame vanished overnight, climate policy would rapidly reorient toward supply-side buildout of nuclear and renewables, fossil incumbents would face accelerated competitive pressure, energy-intensive industries would shift toward clean supply substitution rather than downsizing, and developing economies would gain legitimacy for expansionist clean-energy programs.
% FOUNDING_PROBLEM: Runaway industrial growth causing ecological overshoot and emissions; perceived inability of technological supply-side solutions to address underlying metabolic throughput and consumption-driven demand.
% FOUNDING_PROBLEM_CORROBORATION: Degrowth scholars attest the problem is live, citing continued ecological overshoot. Mainstream climate scientists, energy-system modelers, and development economists attest the founding problem has been partially superseded by empirical evidence that zero-carbon supply expansion can decouple emissions from growth; they corroborate from outside the sufficiency beneficiary set. No independent corroboration from the global south development community supports the universal demand-reduction framing.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the constraint suppresses viable zero-carbon supply options and preserves fossil market share. Suppression (0.71) reflects the active institutional enforcement required to maintain the frame against mounting empirical evidence of feasible renewable and nuclear expansion. Theater ratio (0.48) captures the growing divergence between sufficiency rhetoric and actual emission trajectories in jurisdictions where demand-only strategies dominate. Accessibility collapse (0.64) measures how thoroughly large-scale generation alternatives become unthinkable once the frame is institutionalized. Resistance (0.73) is high due to pushback from energy industries, growth-oriented governments, and development advocates.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (degrowth advocates) experiences the constraint as necessary coordination of a shared political project against industrial ecocide. The payer seats (nuclear, renewables, energy-intensive industries) experience the same structure as enforced extraction that denies them legitimacy and market access. The excluded seat (global south development) experiences it as epistemic violence that renders their development needs invisible. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth sufficiency advocates are full beneficiaries (d near 0.0) because the constraint subsidizes their authority, funding, and policy access. Fossil fuel incumbents are also beneficiaries (d near 0.1) because the constraint structurally preserves their market position by suppressing zero-carbon competitors. Nuclear and renewable sectors are full targets (d near 1.0) because the constraint extracts policy space and capital access from them. Global south development sits near full target (d near 0.95) because the constraint extracts developmental legitimacy. Climate pragmatists occupy an analytical position with no directional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents mislabeling this constraint as pure snare or pure rope. Pure snare would require that the coordination story be mere cover for fossil-fuel extraction; but the sufficiency coalition is a genuine coordination community with shared norms and collective-action capacity. Pure rope would require symmetric benefit; but the asymmetric victimization of nuclear, renewable, and industrial sectors, plus the structural preservation of fossil incumbency, makes extraction undeniable. Tangled rope captures the hybrid reality: real coordination plus real extraction enforced through the same institutional machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fossil_fuel_benefit_structure,
    'Does the degrowth sufficiency constraint structurally benefit fossil fuel incumbents by suppressing their zero-carbon competitors, or is this a spurious correlation?',
    'Comparative analysis of fossil market share and stranded-asset timelines in jurisdictions with strong sufficiency framing versus portfolio-pragmatism framing, controlling for renewable resource endowments.',
    'If structural, the constraint''s extraction is redirected to fossil incumbents and the coordination function may operate as cover; if spurious, the constraint remains a genuine coordination frame with asymmetric side effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_benefit_structure, empirical, 'Whether fossil fuel incumbency is a structural beneficiary of the sufficiency frame.').

omega_variable(
    empirical_sufficiency_of_demand_reduction,
    'Can demand reduction and sufficiency measures alone achieve climate targets compatible with development needs without large-scale clean generation expansion?',
    'Integrated assessment modeling and historical trajectory analysis comparing sufficiency-only pathways against portfolio pathways against observed emission outcomes.',
    'If demand-only is insufficient, the constraint''s coordination function fails empirically and the frame operates as extraction from viable alternatives; if sufficient, the authored extraction metric overstates harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_sufficiency_of_demand_reduction, empirical, 'Empirical test of whether demand reduction alone can satisfy mitigation targets.').

omega_variable(
    cs_framing_underdetermination,
    'Is the contested kernel best framed as a policy commitment system grounded in normative identity, or as an empirical hypothesis about energy systems?',
    'Discourse analysis tracing whether disagreement turns on falsifiable empirical claims (decoupling feasibility, renewable costs) or on non-falsifiable normative commitments (scale/legitimacy, anti-industrial identity).',
    'If the kernel is empirical, empirically_contingent axioms route to engine-computed foreclosure under contrary evidence; if normative, deontological axioms persist regardless and the dispute is identity-based.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel disagreement is empirical or normative in nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel, decomposed from the colloquial label 'climate mitigation' which conflates four structurally distinct policy framings. This reading instantiates the degrowth/sufficiency position; siblings instantiate baseload necessity, renewable primacy, and portfolio pragmatism. Each has distinct epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
