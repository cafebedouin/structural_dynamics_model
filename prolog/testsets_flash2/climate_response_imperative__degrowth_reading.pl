% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative: Degrowth Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the climate response
 *   imperative, which posits that structural economic transformation in the
 *   Global North (reduced consumption, redistribution, post-growth
 *   institutions) is essential for both climate change mitigation and
 *   adaptation. It explicitly rejects reliance on unproven carbon dioxide
 *   removal (CDR) technologies. This reading places present-day Global North
 *   populations into the victim set due to the required lifestyle and
 *   economic changes, while future generations and Global South populations
 *   are the primary beneficiaries. The constraint is claimed as a Snare
 *   because its persistence depends on actively suppressing alternative, less
 *   disruptive climate narratives and economic models, and it imposes
 *   significant, non-consensual costs on identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, snare).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative: Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'bd42c84c-a405-4f99-8d72-a88956a86c2e').
narrative_ontology:cs_kernel_codification('bd42c84c-a405-4f99-8d72-a88956a86c2e', distributed).
narrative_ontology:cs_authority_grounding('bd42c84c-a405-4f99-8d72-a88956a86c2e', diffuse_epistemic).
narrative_ontology:cs_reading_relation('bd42c84c-a405-4f99-8d72-a88956a86c2e', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('bd42c84c-a405-4f99-8d72-a88956a86c2e', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('bd42c84c-a405-4f99-8d72-a88956a86c2e', foundational, ecological_limits_to_growth).
narrative_ontology:cs_axiom_status(ecological_limits_to_growth, holdable).
narrative_ontology:cs_axiom_grounding('bd42c84c-a405-4f99-8d72-a88956a86c2e', ecological_limits_to_growth, empirically_contingent).
narrative_ontology:cs_axiom('bd42c84c-a405-4f99-8d72-a88956a86c2e', foundational, global_north_historical_responsibility).
narrative_ontology:cs_axiom_status(global_north_historical_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('bd42c84c-a405-4f99-8d72-a88956a86c2e', global_north_historical_responsibility, deontological).
narrative_ontology:cs_reference_frame('bd42c84c-a405-4f99-8d72-a88956a86c2e', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('bd42c84c-a405-4f99-8d72-a88956a86c2e', contemporary_policy_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bd42c84c-a405-4f99-8d72-a88956a86c2e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, ecosystems).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, growth_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Required to reduce consumption, accept redistribution, and adapt to post-growth economic models, leading to a perceived reduction in living standards and economic freedom. Their current consumption patterns are directly targeted by this imperative.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, global).

% Face existential threat due to the imperative for rapid decarbonization and a shift away from fossil-fuel-dependent economic structures. Their business model is directly incompatible with the degrowth reading.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_industries, payer,
    institutional, immediate, trapped, global).

% Must fundamentally restructure their economic systems away from continuous growth, impacting traditional metrics of prosperity and requiring significant policy shifts. This represents a deep challenge to their foundational operating principles.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_economies, payer,
    institutional, generational, constrained, global).

% Benefit from a stabilized climate, preserved ecosystems, and a more equitable distribution of resources, avoiding the catastrophic impacts of unchecked climate change. Their well-being is the primary long-term goal of this imperative.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from reduced climate impacts, increased adaptive capacity, and a more equitable share of global resources, addressing historical injustices and enabling sustainable development paths without replicating Global North's extractive model.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Benefit from reduced anthropogenic pressure, biodiversity preservation, and a more stable climate, allowing for recovery and resilience. Their health is intrinsically linked to the degrowth imperative's success.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__degrowth_reading, ecosystems).

% Advocate for technological solutions (e.g., carbon capture, geoengineering) to address climate change without requiring fundamental economic restructuring or reduced consumption. Their preferred solutions are sidelined by the degrowth reading.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, technological_optimists, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global shift in economic priorities and resource allocation, moving away from growth-centric models towards ecological sustainability and social equity, ensuring a liveable planet for all.
% TRANSFER_FUNCTION: Transfers wealth, resources, and ecological space from present-day Global North populations and growth-dependent industries to future generations, Global South populations, and ecosystems, via reduced consumption and redistribution.
% ABSENT_VOICES: Technological optimists and proponents of 'green growth' are excluded from the core framing, as their solutions are deemed insufficient or counterproductive to the structural transformation required by the degrowth reading. They would argue for less disruptive pathways.
% DISAPPEARANCE_RATIONALE: If the imperative for degrowth vanished, the world would revert to business-as-usual growth patterns, accelerating climate change, exacerbating inequality, and leading to ecological collapse. The current trajectory would continue, with severe long-term consequences.
% FOUNDING_PROBLEM: Unchecked economic growth in the Global North has led to ecological overshoot, climate breakdown, and global inequality, threatening the long-term habitability of the planet and the well-being of vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists, climate scientists (e.g., IPCC reports), and indigenous communities corroborate the founding problem's live status, citing ongoing ecological degradation, rising global temperatures, and persistent socio-economic disparities. This corroboration comes from outside the direct beneficiaries of degrowth policies.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the profound economic and social costs imposed on Global North consumers and industries, requiring a fundamental shift in their mode of existence. Suppression (0.70) is high because the degrowth imperative actively challenges and seeks to displace dominant narratives of continuous economic growth and technological salvation, which are deeply embedded in political and economic systems. Resistance (0.90) is also very high, as the proposed changes directly confront powerful vested interests and deeply ingrained societal norms. The low theater ratio (0.10) indicates that the degrowth movement is largely direct and functional in its demands, with little performative cover for other agendas. Accessibility collapse (0.40) is moderate, as alternative climate narratives (e.g., green growth, techno-fix) still exist and are actively promoted, though the degrowth reading seeks to collapse their legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Global North consumers and industries, this imperative is a severe Snare, imposing unacceptable costs and restrictions. From the perspective of future generations and Global South populations, it is a necessary, albeit challenging, Rope or even a Mountain, representing the only path to a just and sustainable future. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North consumers, fossil fuel industries, and growth-dependent economies are clear targets (high d) as they bear the direct costs of reduced consumption, decarbonization, and economic restructuring. Future generations, Global South populations, and ecosystems are the primary beneficiaries (low d), as they gain from a stable climate and equitable resource distribution. Technological optimists are excluded, as their preferred solutions are deemed insufficient or counterproductive by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading argues that the mandate for continuous economic growth has outlived its ecological function and become a source of planetary harm. This constraint aims to resolve that mandatrophy by replacing the growth mandate with a new imperative for ecological and social well-being. The high extractiveness and suppression are seen as necessary to overcome the inertia and resistance of the existing growth-dependent system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_ambiguity,
    'Is a rapid, managed degrowth transition politically and socially feasible in Global North economies without triggering severe social unrest or authoritarian responses?',
    'Empirical observation of pilot programs or national-level policy implementations in democratic contexts; comparative analysis of historical transitions away from growth-dependent models.',
    'If infeasible, the degrowth reading''s proposed solution is itself a Snare, leading to unintended and potentially more extractive outcomes. If feasible, it strengthens the claim that the current system is the Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_feasibility_ambiguity, empirical, 'Uncertainty regarding the practical implementability of degrowth policies.').

omega_variable(
    cdr_reliance_necessity,
    'Is the rejection of unproven carbon dioxide removal (CDR) technologies a foundational principle of degrowth, or a contingent position that could shift with technological advancements?',
    'Analysis of degrowth literature for the philosophical grounding of CDR rejection; expert consensus on the technical and ethical viability of future CDR technologies.',
    'If contingent, the degrowth reading might converge with mitigation-priority readings if CDR becomes viable, reducing the perceived extractiveness on Global North. If foundational, the divergence remains absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_reliance_necessity, conceptual, 'Whether the rejection of CDR is a core, immutable tenet of the degrowth reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative climate narratives structural (e.g., media ownership, political lobbying) or internalized (e.g., cognitive biases, cultural norms)?',
    'Post-intervention analysis: if suppression persists after structural barriers are removed, reclassify as partially internalized. Content analysis of media and policy discourse for framing effects.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as resistance to degrowth is self-perpetuating. If structural, targeted policy interventions could reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for climate narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__degrowth_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(clim_tr_t50, climate_response_imperative__degrowth_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__degrowth_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(clim_be_t50, climate_response_imperative__degrowth_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__degrowth_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(clim_su_t50, climate_response_imperative__degrowth_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, global_carbon_markets).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, fossil_fuel_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_response_imperative' kernel. This 'degrowth' reading emphasizes structural economic transformation and reduced consumption, contrasting with 'mitigation_priority_reading' (techno-fixes) and 'adaptation_priority_reading' (resilience-building). Each reading represents a distinct structural claim about the nature of climate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
