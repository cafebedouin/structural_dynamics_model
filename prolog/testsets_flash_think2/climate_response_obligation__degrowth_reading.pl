% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Obligation for Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint is the 'degrowth_reading' of the
 *   'climate_response_obligation' kernel. It posits that reducing material
 *   throughput is a non-negotiable obligation to stay within planetary
 *   boundaries, emphasizing sufficiency over efficiency. This contrasts with
 *   'mitigation_priority' (rapid decarbonization within a growth paradigm)
 *   and 'adaptation_priority' (investing in resilience to inevitable
 *   warming). The degrowth reading asserts that continued economic growth is
 *   fundamentally incompatible with ecological limits, necessitating a
 *   planned contraction of material and energy use in high-income nations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.9).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Obligation for Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '50d90de8-1824-4963-b325-79722f9a1098').
narrative_ontology:cs_kernel_codification('50d90de8-1824-4963-b325-79722f9a1098', implicit).
narrative_ontology:cs_authority_grounding('50d90de8-1824-4963-b325-79722f9a1098', expertise).
narrative_ontology:cs_interpretation_layer_present('50d90de8-1824-4963-b325-79722f9a1098').
narrative_ontology:cs_reading_relation('50d90de8-1824-4963-b325-79722f9a1098', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('50d90de8-1824-4963-b325-79722f9a1098', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('50d90de8-1824-4963-b325-79722f9a1098', foundational, material_throughput_must_decrease).
narrative_ontology:cs_axiom_status(material_throughput_must_decrease, holdable).
narrative_ontology:cs_axiom_grounding('50d90de8-1824-4963-b325-79722f9a1098', material_throughput_must_decrease, empirically_contingent).
narrative_ontology:cs_axiom('50d90de8-1824-4963-b325-79722f9a1098', foundational, sufficiency_over_efficiency_is_normative).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('50d90de8-1824-4963-b325-79722f9a1098', sufficiency_over_efficiency_is_normative, conventional).
narrative_ontology:cs_reference_frame('50d90de8-1824-4963-b325-79722f9a1098', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('50d90de8-1824-4963-b325-79722f9a1098', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('50d90de8-1824-4963-b325-79722f9a1098', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_oriented_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of reduced material throughput, experiencing less extraction pressure and greater stability. Lacks agency but is the foundational referent for the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% Will inherit a more stable and habitable planet if material throughput is reduced. Currently lack direct political voice but are represented by advocates.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the primary burden of lifestyle changes, reduced consumption, and shifts away from material-intensive goods and services. Their current consumption patterns are directly targeted for reduction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    powerful, biographical, constrained, global).

% Face existential threat as the constraint demands a rapid phase-out of their core business model. Their capital assets become stranded, and their political influence is challenged.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_fuel_industries, payer,
    institutional, immediate, trapped, global).

% Must fundamentally restructure their economic models away from continuous GDP growth, challenging deeply entrenched institutions, financial systems, and policy frameworks.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_oriented_economies, payer,
    institutional, biographical, constrained, global).

% Benefit from a more equitable distribution of ecological space and reduced climate impacts, but may also face constraints on their own development aspirations if degrowth is not carefully managed and differentiated.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_nations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_nations, payer).

% Propose and champion policies and cultural shifts necessary to implement degrowth, acting as intellectual and political drivers for the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, analytical, global).

% Are largely excluded from the degrowth framing, as their models prioritize efficiency and growth within existing paradigms, rather than absolute reduction in material throughput. They would argue for technological solutions and market mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mainstream_economists, excluded,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Realigns human economic activity with the biophysical limits of the planet, coordinating resource use and waste assimilation capacity across global populations and generations to ensure long-term ecological stability.
% TRANSFER_FUNCTION: Transfers ecological space, resource availability, and a stable climate from current high-consuming populations (primarily Global North) and growth-oriented industries to planetary systems, future generations, and potentially the Global South.
% ABSENT_VOICES: Mainstream economists and policymakers focused on GDP growth are structurally excluded by the degrowth framing's premise. They would argue for technological solutions and efficiency gains rather than absolute reduction in throughput, and for continued economic expansion.
% DISAPPEARANCE_RATIONALE: If the obligation to reduce material throughput vanished, current economic systems would continue to prioritize growth, leading to accelerated ecological collapse, resource depletion, and severe intergenerational inequity. The biophysical world would rearrange itself catastrophically, and human societies would face increasing instability.
% FOUNDING_PROBLEM: Unfettered economic growth and material consumption are exceeding planetary boundaries, leading to climate change, biodiversity loss, resource depletion, and ecological collapse, threatening the long-term habitability of Earth for all species, including humans.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on planetary boundaries (e.g., Stockholm Resilience Centre, IPCC reports) and ecological overshoot, corroborated by independent environmental organizations, indigenous communities, and a growing body of interdisciplinary research, provides strong evidence for the problem's continued existence and urgency.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (aligning human activity with planetary limits for collective survival) but involves substantial asymmetric extraction from current high-consuming populations and growth-oriented industries. Extractiveness is high (0.85) due to the radical restructuring of economic systems and lifestyles required. Suppression is very high (0.90) because the constraint directly challenges the dominant growth paradigm, requiring active suppression of growth-oriented policies and consumer demand. Resistance is also very high (0.95) given the profound societal changes implied. Theater ratio is low (0.10) as the core demand for reduction is functional and directly tied to biophysical realities, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of planetary systems and future generations, this obligation is a vital coordination mechanism for survival. From the perspective of current growth-oriented economies and high-consuming populations, it is a highly extractive and suppressive force that threatens their established way of life and economic models. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are the primary beneficiaries, as the constraint aims to reduce the extraction pressure on them. Global South nations are conditional beneficiaries, as their development space would increase if the Global North degrows first. Current Global North consumers, fossil fuel industries, and growth-oriented economies are the primary targets/payers, as the constraint demands significant reductions in their material consumption, production, and profit models. Degrowth advocates act as agenda-setters, pushing for the implementation of this obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as pure extraction (Snare) by acknowledging its genuine coordination function for planetary stability, while also preventing it from being seen as pure coordination (Rope) by highlighting the significant and asymmetric extraction from specific groups. It also avoids the false summit of a Mountain, as the 'obligation' is a human construct, albeit one derived from natural limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_vs_natural_law,
    'Is the ''degrowth obligation'' a human-constructed constraint (Tangled Rope) or an emergent property of planetary limits (Mountain)?',
    'Conceptual analysis of the ''is-ought'' gap: if the constraint is understood as a normative imperative derived from scientific observation, it remains a constructed obligation. If it is seen as an inevitable consequence of biophysical laws, it approaches a Mountain.',
    'If reclassified as a Mountain, the extractiveness and suppression would be seen as inherent to reality, not human policy, fundamentally altering the classification and the locus of agency for change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligation_vs_natural_law, conceptual, 'Distinguishing the normative obligation from the underlying biophysical limits.').

omega_variable(
    sufficiency_threshold_accuracy,
    'Is the proposed reduction in material throughput truly sufficient to stay within planetary boundaries, or is it an underestimation of the required change?',
    'Ongoing scientific research and monitoring of planetary boundary indicators (e.g., carbon budget, biodiversity loss, nitrogen cycle).',
    'If the current degrowth proposals are found to be insufficient, the effective extractiveness and suppression required would be even higher, potentially pushing the constraint closer to a Snare for current systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_threshold_accuracy, empirical, 'Accuracy of the sufficiency threshold for material throughput.').

omega_variable(
    equitable_distribution_of_burden,
    'How can the burden of degrowth be distributed equitably, particularly between the Global North (historical emitters/consumers) and the Global South (developing nations)?',
    'International policy negotiations, justice-oriented economic modeling, and social movements advocating for differentiated responsibilities and capabilities.',
    'Failure to achieve equitable distribution would increase resistance from the Global South, potentially leading to a breakdown of the coordination function and exacerbating the extractive nature of the constraint for vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equitable_distribution_of_burden, preference, 'Fairness in distributing the costs of degrowth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__degrowth_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__degrowth_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__degrowth_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__degrowth_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__degrowth_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__degrowth_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__degrowth_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__degrowth_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__degrowth_reading, base_extractiveness, 2040, 0.88).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__degrowth_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__degrowth_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__degrowth_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__degrowth_reading, suppression_requirement, 2030, 0.9).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__degrowth_reading, suppression_requirement, 2040, 0.92).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is the 'degrowth_reading' of the 'climate_response_obligation' kernel, which also includes 'mitigation_priority' and 'adaptation_priority' readings. Each reading represents a distinct structural claim about the nature of the climate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
