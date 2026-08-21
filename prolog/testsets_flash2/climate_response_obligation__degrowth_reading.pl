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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Reading of Climate Response Obligation: Sufficiency over Efficiency
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the broader climate
 *   response obligation. It posits that staying within planetary boundaries
 *   necessitates a reduction in material throughput and a shift from
 *   efficiency-driven growth to sufficiency-oriented well-being. This reading
 *   identifies planetary systems and future generations as primary
 *   beneficiaries, while current Global North consumption patterns and fossil
 *   capital industries are seen as victims due to the required reduction in
 *   extraction and consumption. Capital accumulation itself is reframed as an
 *   extractive mechanism.
 *
 * KEY AGENTS:
 *   - planetary_systems: Primary beneficiary (powerless/trapped)
 *   - future_generations: Primary beneficiary (powerless/trapped)
 *   - global_north_consumers: Primary target (moderate/constrained)
 *   - fossil_capital_industry: Primary target (institutional/constrained)
 *   - growth_oriented_economies: Primary target (institutional/constrained)
 *   - global_south_nations: Excluded voice (organized/constrained)
 *   - degrowth_advocates: Agenda setter (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, snare).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Reading of Climate Response Obligation: Sufficiency over Efficiency").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'c98d25fb-c178-4e27-b28a-fef7c2e767a1').
narrative_ontology:cs_kernel_codification('c98d25fb-c178-4e27-b28a-fef7c2e767a1', distributed).
narrative_ontology:cs_authority_grounding('c98d25fb-c178-4e27-b28a-fef7c2e767a1', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c98d25fb-c178-4e27-b28a-fef7c2e767a1', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('c98d25fb-c178-4e27-b28a-fef7c2e767a1', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_axiom('c98d25fb-c178-4e27-b28a-fef7c2e767a1', foundational, ecological_limits_are_absolute).
narrative_ontology:cs_axiom_status(ecological_limits_are_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c98d25fb-c178-4e27-b28a-fef7c2e767a1', ecological_limits_are_absolute, empirically_contingent).
narrative_ontology:cs_axiom('c98d25fb-c178-4e27-b28a-fef7c2e767a1', foundational, sufficiency_over_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('c98d25fb-c178-4e27-b28a-fef7c2e767a1', sufficiency_over_efficiency, deontological).
narrative_ontology:cs_reference_frame('c98d25fb-c178-4e27-b28a-fef7c2e767a1', pre_industrial_ecological_balance).
narrative_ontology:cs_drift_state('c98d25fb-c178-4e27-b28a-fef7c2e767a1', contemporary_overshoot_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c98d25fb-c178-4e27-b28a-fef7c2e767a1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_industry).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_oriented_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from reduced material throughput and extraction, leading to stabilization within ecological limits. Currently bears the brunt of overshoot.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% Inherits a stable climate and ecological systems, free from the burdens of ecological collapse. Currently has no voice in policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Required to significantly reduce consumption and material throughput, leading to lifestyle changes and potential economic contraction. Faces resistance to these changes due to ingrained habits and economic structures.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, global).

% Faces existential threat due to the imperative to cease extraction and production of fossil fuels. Its business model is directly targeted by the degrowth imperative, leading to strong resistance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_industry, payer,
    institutional, immediate, constrained, global).

% Must fundamentally restructure their economic models away from continuous growth, impacting GDP, employment, and traditional development metrics. This is a radical shift from current paradigms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_oriented_economies, payer,
    institutional, generational, constrained, global).

% Their development aspirations are constrained by the degrowth imperative, particularly if the Global North does not reduce consumption first. They seek equitable access to resources and development opportunities.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_nations, excluded,
    organized, generational, constrained, global).

% Actively promote and advocate for policies that prioritize ecological sustainability and human well-being over economic growth. They seek to reshape economic and social systems.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human economic activity with the biophysical limits of the planet, ensuring long-term ecological stability and equitable resource distribution for all species and future generations.
% TRANSFER_FUNCTION: Transfers material resources, energy, and ecological space from current high-consuming economies (Global North) to planetary systems and future generations, by reducing throughput and prioritizing sufficiency.
% ABSENT_VOICES: Future generations and non-human planetary systems are structurally absent from current decision-making, though degrowth advocates attempt to represent their interests. Global South nations are often excluded from setting the terms of degrowth, despite being disproportionately affected by both climate change and development constraints.
% DISAPPEARANCE_RATIONALE: If the degrowth obligation vanished, the world would continue on its current trajectory of increasing material throughput and ecological overshoot, leading to accelerated climate breakdown and biodiversity loss. Economic systems would continue to prioritize growth, and the planetary systems would face increasing pressure, eventually leading to collapse and a forced rearrangement.
% FOUNDING_PROBLEM: Humanity's economic activity, driven by continuous growth and material consumption, is exceeding the Earth's biophysical carrying capacity, leading to climate change, biodiversity loss, and resource depletion.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on planetary boundaries and ecological overshoot, as documented by IPCC reports and ecological footprint analyses, corroborates the live status of the founding problem. This is attested by a broad scientific community and international bodies, not just degrowth advocates.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.85) because this reading demands a radical restructuring of economic systems and a significant reduction in consumption, which is highly extractive from current growth-dependent economies and lifestyles. Suppression (0.70) is also high, reflecting the immense political and economic force required to overcome resistance from entrenched interests and societal norms. Theater ratio is low (0.10) as the degrowth agenda is direct and functional, with little room for performative gestures without genuine change. Resistance is very high (0.90) due to the fundamental challenge it poses to existing power structures and economic paradigms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of planetary systems and future generations, this is a necessary and beneficial constraint. However, from the perspective of Global North consumers and fossil capital industries, it is a highly extractive and suppressive demand that threatens their current way of life and economic viability. Degrowth advocates see it as a moral and ecological imperative, while growth-oriented economies view it as an impediment to development.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are full beneficiaries (d=0.0) as the constraint directly subsidizes their long-term viability. Global North consumers, fossil capital industry, and growth-oriented economies are full targets (d=1.0) as the constraint extracts directly from their current operations and consumption patterns. Global South nations are excluded, as their development is constrained by this framework unless the Global North reduces first, placing them in a complex position of both potential beneficiary (of a stable planet) and victim (of constrained development).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the 'climate response obligation' as a simple coordination problem (Rope) or a temporary support (Scaffold). By explicitly identifying the beneficiaries (planetary systems, future generations) and victims (current high-consuming economies, fossil capital), it highlights the inherent extraction required to shift away from the status quo. It clarifies that the mandate is not to 'grow green' but to 'degrow' in specific areas, making it a Snare for those who benefit from the current extractive system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_empirical,
    'Is a global degrowth transition empirically feasible without causing widespread social collapse or authoritarian enforcement?',
    'Longitudinal studies of degrowth-oriented policies in practice, analysis of historical precedents for large-scale economic contraction, and modeling of alternative economic systems.',
    'If empirically infeasible, the constraint''s suppression and extractiveness would be deemed unsustainable, potentially leading to reclassification as a Piton (unmaintainable) or a Snare (requiring extreme coercion). If feasible, it strengthens the claim of a viable alternative to growth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_feasibility_empirical, empirical, 'The empirical viability of a global degrowth transition.').

omega_variable(
    degrowth_equity_conceptual,
    'Does the degrowth imperative, as currently framed, adequately address historical injustices and ensure equitable development for the Global South?',
    'Analysis of degrowth policy proposals through a post-colonial and environmental justice lens, and direct engagement with Global South stakeholders to assess their perceived equity and agency within the framework.',
    'If inequitable, the constraint''s legitimacy would be undermined, potentially leading to reclassification as a Snare for the Global South, despite its ecological benefits. If equitable, it strengthens its claim as a just transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_equity_conceptual, conceptual, 'The equity implications of the degrowth framework for the Global South.').

omega_variable(
    degrowth_vs_growth_framing,
    'Is the ''degrowth'' framing itself a conceptual barrier to broader adoption, or is it a necessary and precise term for the required transformation?',
    'Sociological studies of public perception and political discourse around ''degrowth'' vs. alternative framings (e.g., ''post-growth'', ''well-being economy''), and analysis of policy uptake under different terminologies.',
    'If the framing is a barrier, the constraint''s effective suppression (due to public resistance) might be higher than necessary, suggesting a conceptual re-framing could reduce resistance without altering the underlying material imperative. If necessary, it reinforces the clarity of the call to action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_vs_growth_framing, conceptual, 'The impact of the ''degrowth'' terminology on its political and social acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1970, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1970, climate_response_obligation__degrowth_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(clim_tr_t1985, climate_response_obligation__degrowth_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__degrowth_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__degrowth_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.1).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t1970, climate_response_obligation__degrowth_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(clim_be_t1985, climate_response_obligation__degrowth_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__degrowth_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__degrowth_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__degrowth_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.85).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1970, climate_response_obligation__degrowth_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(clim_su_t1985, climate_response_obligation__degrowth_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__degrowth_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__degrowth_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__degrowth_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.7).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_obligation' kernel. It focuses on reducing material throughput, contrasting with 'mitigation_priority' (rapid decarbonization) and 'adaptation_priority' (resilience to warming).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
