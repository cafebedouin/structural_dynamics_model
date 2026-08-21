% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Intergenerational Climate Mitigation Obligation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the ethical and policy obligation to rapidly
 *   decarbonize the global economy to prevent future harm from climate
 *   change, grounded in principles of intergenerational justice. It is a
 *   reading of the broader 'climate_response_obligation' kernel, prioritizing
 *   prevention (mitigation) over adaptation or degrowth. The constraint
 *   demands significant structural changes and costs from the current
 *   generation, particularly high-emitting nations and industries, for the
 *   benefit of future generations and vulnerable ecosystems.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary beneficiaries, powerless in the present.
 *   - Current Generation Consumers: Bear transition costs, constrained exit.
 *   - Fossil Fuel Industry: Primary target for extraction, powerful but constrained.
 *   - High-Emitting Nations: Agenda-setters and payers, institutional power.
 *   - Low-Emitting Vulnerable Nations: Beneficiaries, but excluded from full agency.
 *   - Climate Scientists: Analytical observers, provide evidence.
 *   - Climate Activists: Payers (of advocacy), organized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.85).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.9).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Intergenerational Climate Mitigation Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'e890da9f-d0ec-42c4-936f-786c43bfb826').
narrative_ontology:cs_kernel_codification('e890da9f-d0ec-42c4-936f-786c43bfb826', formalized).
narrative_ontology:cs_authority_grounding('e890da9f-d0ec-42c4-936f-786c43bfb826', expertise).
narrative_ontology:cs_interpretation_layer_present('e890da9f-d0ec-42c4-936f-786c43bfb826').
narrative_ontology:cs_reading_relation('e890da9f-d0ec-42c4-936f-786c43bfb826', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e890da9f-d0ec-42c4-936f-786c43bfb826', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e890da9f-d0ec-42c4-936f-786c43bfb826', foundational, intergenerational_equity_principle).
narrative_ontology:cs_axiom_status(intergenerational_equity_principle, holdable).
narrative_ontology:cs_axiom_grounding('e890da9f-d0ec-42c4-936f-786c43bfb826', intergenerational_equity_principle, deontological).
narrative_ontology:cs_axiom('e890da9f-d0ec-42c4-936f-786c43bfb826', foundational, precautionary_principle).
narrative_ontology:cs_axiom_status(precautionary_principle, holdable).
narrative_ontology:cs_axiom_grounding('e890da9f-d0ec-42c4-936f-786c43bfb826', precautionary_principle, deontological).
narrative_ontology:cs_reference_frame('e890da9f-d0ec-42c4-936f-786c43bfb826', scientific_consensus_1990).
narrative_ontology:cs_drift_state('e890da9f-d0ec-42c4-936f-786c43bfb826', contemporary_climate_crisis, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e890da9f-d0ec-42c4-936f-786c43bfb826', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, vulnerable_ecosystems).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, high_emitting_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, low_emitting_vulnerable_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, climate_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will suffer the most severe and irreversible impacts of climate change if mitigation fails. They are the primary beneficiaries of rapid decarbonization, inheriting a more stable and habitable planet.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the direct and indirect costs of transitioning to a low-carbon economy, including higher energy prices, changes in consumption patterns, and investments in new infrastructure. Their choices are constrained by policy and market shifts.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_generation_consumers, payer,
    moderate, biographical, constrained, global).

% Faces significant economic disruption, including stranded assets, declining demand for products, and increased regulatory burdens, as the world decarbonizes. Their business model is directly targeted by mitigation policies.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_industry, payer,
    powerful, biographical, constrained, global).

% As historically high emitters, these nations (primarily in the Global North) are expected to lead mitigation efforts and bear a disproportionate share of the transition costs. They set international climate policy agendas but also face internal resistance to change.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, high_emitting_nations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, high_emitting_nations, payer).

% Suffer disproportionately from climate impacts despite low historical emissions. They benefit from global mitigation efforts but often lack the power to significantly influence the pace or equity of decarbonization, remaining largely excluded from agenda-setting.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, low_emitting_vulnerable_nations, beneficiary,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, low_emitting_vulnerable_nations, excluded).

% Provide the foundational scientific evidence and projections that underpin the urgency and targets of climate mitigation. They observe and analyze the system without directly bearing its costs or setting its rules.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists, observer,
    analytical, biographical, analytical, global).

% Actively advocate for rapid and equitable decarbonization, often bearing personal, social, and economic costs for their advocacy. They push for stronger enforcement of the mitigation obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_activists, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to rapidly reduce greenhouse gas emissions, preventing catastrophic warming and ensuring a habitable planet for future generations, while managing the transition costs.
% TRANSFER_FUNCTION: Transfers economic resources, technological innovation, and lifestyle adjustments from the current generation (especially high-emitting nations and industries) to future generations and vulnerable ecosystems, in exchange for a stable climate system.
% ABSENT_VOICES: Future generations cannot directly articulate their interests; their voices are represented by advocates. Vulnerable ecosystems also lack direct representation, relying on scientific and ethical proxies. Rival economic models that prioritize immediate growth over long-term sustainability are often marginalized in mitigation discourse.
% DISAPPEARANCE_RATIONALE: If the obligation to mitigate climate change vanished overnight, global emissions would likely accelerate, leading to severe and irreversible climate change. This would fundamentally reorganize human societies, economies, and natural systems, making the planet significantly less habitable.
% FOUNDING_PROBLEM: The scientific consensus on anthropogenic climate change and its severe, irreversible risks to future generations and planetary systems, necessitating urgent action to minimize warming.
% FOUNDING_PROBLEM_CORROBORATION: Overwhelming scientific consensus from the Intergovernmental Panel on Climate Change (IPCC), national academies of science worldwide, and independent research institutions. This corroboration is consistently reaffirmed by global scientific bodies, not just by benefiting parties.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because rapid decarbonization requires a fundamental restructuring of the global economy, imposing substantial costs on current economic activities and industries. Suppression is also high (0.90) as the constraint necessitates active policy enforcement (regulations, carbon pricing, bans) to overcome inertia and resistance from vested interests. Theater ratio is relatively low (0.20) because the scientific urgency demands genuine action, though some political rhetoric may outpace actual implementation. Accessibility collapse is moderate (0.60) as alternatives like 'doing nothing' or 'only adapting' are seen as morally unacceptable by this reading, but remain physically possible, leading to ongoing resistance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced very differently by its stakeholders. Future generations are pure beneficiaries, while the fossil fuel industry and high-emitting nations experience it as a severe extractive force. Current generation consumers face diffuse but significant costs. Low-emitting vulnerable nations are beneficiaries but also excluded from full agency, highlighting a justice gap. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable ecosystems are full beneficiaries (low d) as they receive the primary benefit of a stable climate without bearing current costs. The fossil fuel industry and high-emitting nations are full targets (high d) as they bear the brunt of decarbonization costs and regulatory pressure. Current generation consumers are targets (moderate d) due to diffuse costs. Low-emitting vulnerable nations are beneficiaries (low d) but their 'excluded' role means their agency is suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anthropogenic climate change risks) is unequivocally 'live', and the constraint's function (minimizing warming) is highly relevant. Therefore, mandatrophy is not resolved; the constraint's mandate is actively defended and its function is critical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct ''mitigation_priority'' reading, or is it an aspect of a broader ''climate_response_obligation'' that is better captured by a different primary framing?',
    'Analysis of policy documents, scientific consensus statements, and ethical frameworks to identify the dominant framing of climate action. If the primary focus shifts to resilience or material reduction, reclassify as a different reading.',
    'If reclassified, the beneficiary/victim sets and the primary coordination function would shift, altering the constraint''s type and effective extraction. For example, ''adaptation_priority'' would shift costs to vulnerable communities and emphasize resilience infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''mitigation_priority'' reading of the ''climate_response_obligation'' kernel. Sibling readings include ''adaptation_priority'' and ''degrowth_reading''. The ''mitigation_priority'' reading structurally changes the victim set to include fossil capital and high-emitting nations, and elevates future generations as primary beneficiaries, which differs from ''adaptation_priority'' (which shifts costs to vulnerable communities) and ''degrowth_reading'' (which shifts costs to current material consumption). The disagreement is located in the primary ethical obligation (prevention vs. resilience vs. sufficiency) and the distribution of costs and benefits across generations and nations.').

omega_variable(
    equitable_transition_feasibility,
    'Is rapid decarbonization feasible without imposing disproportionate and unjust burdens on vulnerable populations within the current generation?',
    'Empirical studies of ''just transition'' policies and their outcomes in different socio-economic contexts. Analysis of the distribution of costs and benefits of decarbonization across income levels and social groups.',
    'If an equitable transition is not feasible, the effective extraction from vulnerable segments of the current generation would be higher than currently estimated, potentially shifting their classification closer to ''trapped'' victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equitable_transition_feasibility, empirical, 'Uncertainty regarding the social equity of rapid decarbonization policies.').

omega_variable(
    political_will_vs_scientific_imperative,
    'To what extent does the observed ''practice_drift'' reflect a genuine lack of political will, versus inherent structural barriers to rapid global coordination?',
    'Comparative political analysis of national climate policies, lobbying efforts by vested interests, and public opinion surveys. Modeling of global coordination challenges versus national policy implementation failures.',
    'If primarily a lack of political will, the ''suppression'' metric might be understated, as the constraint is actively resisted by powerful actors. If primarily structural barriers, the ''accessibility_collapse'' for effective action might be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_vs_scientific_imperative, empirical, 'Distinguishing between political resistance and structural limits to climate action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__mitigation_priority, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__mitigation_priority, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__mitigation_priority, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__mitigation_priority, theater_ratio, 2030, 0.2).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__mitigation_priority, theater_ratio, 2040, 0.19).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__mitigation_priority, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__mitigation_priority, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__mitigation_priority, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__mitigation_priority, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__mitigation_priority, base_extractiveness, 2030, 0.82).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__mitigation_priority, base_extractiveness, 2040, 0.84).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__mitigation_priority, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__mitigation_priority, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__mitigation_priority, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__mitigation_priority, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__mitigation_priority, suppression_requirement, 2030, 0.88).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__mitigation_priority, suppression_requirement, 2040, 0.89).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__mitigation_priority, suppression_requirement, 2050, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, global_carbon_markets).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_divestment).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
