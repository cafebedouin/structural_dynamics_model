% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Growth-Dependent Climate Response Arrangement (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the contested
 *   climate-response-imperative kernel: the claim that mitigation and
 *   adaptation both require structural economic transformation in the Global
 *   North — reduced consumption, redistribution, and post-growth institutions
 *   — rather than being achievable through continued growth plus technology
 *   and markets (the mitigation-priority reading) or through
 *   resilience-building alone (the adaptation-priority reading). The referent
 *   for extraction here is the STANDING growth-preserving arrangement as this
 *   reading sees it: policy architecture that treats Global North consumption
 *   levels as fixed and defers structural change in favor of technological
 *   offsets, particularly unproven carbon dioxide removal at scale. This is
 *   not a story about the degrowth reading's own endorsed post-growth
 *   institutions (which would have near-zero extraction by construction) — it
 *   is a story about the arrangement this reading holds is currently in place
 *   and is being defended against transformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.61).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Growth-Dependent Climate Response Arrangement (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '3b8a5fdb-1128-41a9-81c6-cb882ab39fac').
narrative_ontology:cs_kernel_codification('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', distributed).
narrative_ontology:cs_authority_grounding('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', distributed).
narrative_ontology:cs_reading_relation('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', foundational, growth_and_climate_stabilization_are_structurally_incompatible_at_required_speed).
narrative_ontology:cs_axiom_status(growth_and_climate_stabilization_are_structurally_incompatible_at_required_speed, holdable).
narrative_ontology:cs_axiom_grounding('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', growth_and_climate_stabilization_are_structurally_incompatible_at_required_speed, empirically_contingent).
narrative_ontology:cs_axiom('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', foundational, present_generation_consumption_reduction_is_a_legitimate_and_necessary_transfer_mechanism).
narrative_ontology:cs_axiom_status(present_generation_consumption_reduction_is_a_legitimate_and_necessary_transfer_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', present_generation_consumption_reduction_is_a_legitimate_and_necessary_transfer_mechanism, deontological).
narrative_ontology:cs_reference_frame('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', unconstrained_growth_baseline).
narrative_ontology:cs_drift_state('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', post_paris_agreement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b8a5fdb-1128-41a9-81c6-cb882ab39fac', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_north_consumer_class).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, growth_dependent_financial_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_south_frontline_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_working_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, carbon_dioxide_removal_industry).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumer_class).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, growth_compatible_decarbonization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently sustains a high-consumption lifestyle underwritten by continued fossil-fuel-intensive growth and cheap imported goods. Under the standing arrangement, this population is shielded from the near-term costs of transformation (reduced consumption, shorter working hours, redistribution) that a degrowth transition would impose on them directly. Their political preferences for continued growth are treated as fixed, and policy is designed around not disturbing them, which is itself the extraction mechanism from this reading's standpoint: their present comfort is purchased by displacing costs onto the Global South and the future.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumer_class, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, global_north_consumer_class, payer).

% Sets the terms of climate policy debate by funding research, lobbying against consumption limits, and promoting technological/market solutions that preserve growth trajectories and their asset base. Actively works to keep structural transformation off the table, framing it as politically impossible or economically catastrophic, while continuing to extract value from continued extraction and combustion.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_incumbents, agenda_setter,
    institutional, biographical, arbitrage, global).

% Pension funds, banks, and investment vehicles whose return models assume continued GDP growth and asset appreciation. A post-growth transition threatens the mathematics underlying their solvency and mandate; they lobby against redistribution and consumption-reduction policy through capital allocation decisions and political influence, benefiting from the arrangement's inertia.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the immediate physical costs of climate change — floods, droughts, displacement, crop failure — produced overwhelmingly by historical and current Global North consumption they did not generate and cannot exit. Under the growth-preserving arrangement, they receive adaptation finance framed as charity rather than restitution, while the emissions driving their harm continue because the Global North's consumption base is treated as non-negotiable.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_frontline_populations, payer,
    powerless, immediate, trapped, global).

% Inherit a degraded climate system and depleted carbon budget because the present arrangement defers structural transformation in favor of unproven technological fixes and continued consumption. They have no seat at any negotiating table and no capacity to renegotiate the terms set now; their exit option does not exist because they do not yet exist as political actors.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Would bear the direct costs of a degrowth transition — reduced working hours redistributed without full wage compensation absent strong policy design, transition away from carbon-intensive jobs, restructured consumption norms — while historically having captured a smaller share of the gains from growth than capital owners. This reading names them as victims of the TRANSFORMATION ITSELF unless redistribution is genuinely built in, distinguishing them from the consumer class that captures disproportionate benefit from the status quo.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_working_class, payer,
    moderate, biographical, constrained, national).

% Economists, ecological researchers, and social movements advocating for planned reduction in material and energy throughput, work-time redistribution, and post-growth institutions. Structurally marginalized from mainstream climate policy venues (UNFCCC negotiations, national climate plans) that are built around green-growth and market-mechanism assumptions; their proposals are treated as politically infeasible rather than substantively evaluated.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, degrowth_policy_advocates, excluded,
    moderate, generational, constrained, global).

% Firms and researchers developing direct air capture and other negative-emissions technologies whose business models depend on continued high-emissions trajectories being offset later rather than avoided now. This reading treats reliance on their unproven, unscaled technology as part of the extractive cover story that permits deferral of structural transformation, though they are excluded from this story's stakeholder conversation about redistribution.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_dioxide_removal_industry, excluded,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, carbon_dioxide_removal_industry, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing arrangement coordinates continued economic activity, employment, and consumption patterns in the Global North around a shared assumption that GDP growth and per-capita material throughput are non-negotiable, letting institutions plan investment and policy without confronting whether the underlying growth trajectory is itself compatible with climate stabilization.
% TRANSFER_FUNCTION: Physical and temporal risk is transferred from the present-day Global North consumer class to Global South frontline populations (via emissions-driven climate damage) and to future generations (via carbon budget depletion), while the economic burden of any eventual adjustment is transferred within the Global North from capital owners toward the working class unless redistribution is deliberately engineered.
% ABSENT_VOICES: Degrowth policy advocates and ecological economists are structurally excluded from the mainstream negotiating architecture, which is built on green-growth premises; Global South delegations participate but with vastly less negotiating leverage and are routinely out-resourced in technical and legal capacity relative to Global North blocs.
% DISAPPEARANCE_RATIONALE: If the growth-preserving assumption underlying current climate policy were abandoned overnight in favor of structural post-growth transformation, Global North consumption patterns, labor markets, financial return models, and international climate finance obligations would all have to be fundamentally reorganized — this is precisely the transformation the degrowth reading holds is being deferred.
% FOUNDING_PROBLEM: Climate policy was built to reduce greenhouse gas emissions fast enough to avoid catastrophic warming while preserving the economic stability and living standards of the populations and institutions designing the policy.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working group reports and ecological economists outside the fossil fuel and finance sectors attest that continued growth-compatible pathways are increasingly incompatible with remaining carbon budgets absent large-scale unproven CDR deployment; mainstream policymakers and growth-dependent institutions attest the founding problem remains adequately addressed through market mechanisms and technological innovation without structural transformation. No source affiliated with fossil fuel incumbents or growth-dependent finance is treated as independent corroboration here.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.42 to 0.68) reflecting the accumulating carbon budget consumed while growth-preserving policy persists — each year of deferral compounds the transfer to future generations and the Global South. Theater ratio rises in parallel (0.20 to 0.42) as climate summitry, net-zero pledges, and voluntary corporate commitments increasingly substitute performative activity for the structural transformation this reading holds is required; net-zero-by-2050 pledges without near-term consumption reduction are read as theater under this framing. Suppression is substantial but not maximal (0.61) — it operates less through direct coercion than through agenda-control: growth-dependent financial institutions and fossil fuel incumbents shape which policy options are treated as serious, marginalizing degrowth proposals as unserious rather than banning them outright. Accessibility collapse is moderate (0.50) because degrowth alternatives are documented and available in the literature but structurally excluded from mainstream negotiation venues rather than logically foreclosed. Resistance is high (0.78) reflecting the active political and academic contestation this reading generates.
 *
 * DIRECTIONALITY LOGIC:
 *   The global north consumer class and growth-dependent financial institutions sit near the beneficiary end: the arrangement's defining feature, from this reading, is that it shields their present consumption and return expectations from the costs of transformation. Fossil fuel incumbents are the clearest beneficiary and simultaneously the agenda-setter, actively defending the arrangement's persistence. Global south frontline populations and future generations sit at the full-target end — trapped, powerless, bearing costs they did not generate and cannot renegotiate. The global north working class is deliberately placed in the payer set for THIS transformation's likely costs (absent built-in redistribution) rather than lumped with the consumer class as a monolithic beneficiary — this is the structural delta that distinguishes the degrowth reading from readings that treat 'the Global North' as an undifferentiated beneficiary block.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoid catastrophic warming while preserving economic stability) is marked contested rather than dead: it has not disappeared, but this reading holds that the arrangement built to solve it has been captured by an additional, unstated mandate — preserving growth and consumption patterns — that has come to dominate the original mandate. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (climate negotiation venues, emissions accounting, finance mechanisms do solve real coordination problems) while registering the asymmetric extraction this reading identifies riding on top of that coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decarbonization_decoupling_ambiguity,
    'Can Global North economies achieve sufficient absolute emissions reduction while maintaining GDP growth (absolute decoupling at the pace required), or does the empirical decoupling record support the degrowth reading''s claim that structural consumption reduction is necessary?',
    'Longitudinal empirical tracking of absolute decoupling rates across OECD economies against required emissions trajectories consistent with 1.5C/2C carbon budgets; comparison of decoupling-dependent scenarios'' technology assumptions against realized deployment rates.',
    'If sufficient absolute decoupling is empirically achieved at required pace, the degrowth reading''s core premise weakens substantially and the arrangement it critiques may be closer to a genuine (if imperfect) coordination mechanism; if decoupling continues to fall short, the extraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decarbonization_decoupling_ambiguity, empirical, 'Whether decoupling evidence supports or undermines the necessity claim at the core of the degrowth reading.').

omega_variable(
    cdr_scalability_uncertainty,
    'Will carbon dioxide removal technologies scale to the gigaton-per-year levels assumed in growth-compatible net-zero pathways, or is reliance on CDR at that scale a cover story for deferring structural transformation?',
    'Track realized CDR deployment (direct air capture capacity, verified sequestration) against IPCC pathway assumptions over the next 15-20 years.',
    'Persistent large gaps between assumed and realized CDR capacity would corroborate this reading''s claim that CDR reliance functions as extractive deferral; successful scaling would undermine the degrowth reading''s necessity claim specifically on this point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability_uncertainty, empirical, 'Whether CDR technology can deliver what growth-compatible pathways assume of it.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among the degrowth, mitigation-priority, and adaptation-priority readings a matter of differing empirical predictions about what will work, or a matter of differing values about who should bear transition costs?',
    'None fully resolves this — it is partly empirical (what technologies and policies can deliver required emissions reductions) and partly a distributional/values question (who bears adjustment costs) that empirical resolution alone cannot settle.',
    'If primarily empirical, evidence accumulation over the coming decade could shift consensus toward one reading; if primarily a values question, the readings will persist as coexisting positions regardless of evidence, consistent with their coexists_with relationship in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading selection is empirically resolvable or is an irreducible values contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_response_imperative__degrowth_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(clim_tr_t16, climate_response_imperative__degrowth_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__degrowth_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(clim_tr_t32, climate_response_imperative__degrowth_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__degrowth_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t8, climate_response_imperative__degrowth_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(clim_be_t16, climate_response_imperative__degrowth_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__degrowth_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(clim_be_t32, climate_response_imperative__degrowth_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__degrowth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(clim_su_t8, climate_response_imperative__degrowth_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(clim_su_t16, climate_response_imperative__degrowth_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__degrowth_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(clim_su_t32, climate_response_imperative__degrowth_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__degrowth_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_imperative kernel. mitigation_priority_reading treats emissions reduction via technology/markets as sufficient with growth intact; adaptation_priority_reading treats resilience-building in exposed regions as primary. All three readings share the same underlying kernel (what climate response fundamentally requires) but instantiate structurally distinct constraints with different beneficiary/victim sets and different epsilon values, per the epsilon-invariance principle. This reading is linked to both siblings because its core claim — that the growth-preserving status quo is extractive — creates direct legitimacy pressure on both: it argues the mitigation-priority reading's market mechanisms function partly as deferral cover, and it argues the adaptation-priority reading's resilience focus, absent mitigation ambition, effectively ratifies continued harm transfer to exposed populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
