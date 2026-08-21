% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Dominant Nuclear Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents the 'comparative_risk_dominant' reading of
 *   nuclear energy acceptability, where nuclear risk is deemed acceptable
 *   only when weighed against the risks of competing energy sources,
 *   particularly fossil fuel emissions and climate catastrophe. This reading
 *   explicitly rejects an absolute threshold for nuclear safety, prioritizing
 *   temporal urgency for climate action over intergenerational waste
 *   concerns. It functions as a Tangled Rope, coordinating energy policy
 *   towards decarbonization while imposing asymmetric risks on specific
 *   populations and future generations.
 *
 * KEY AGENTS:
 *   - Governments seeking energy security: Agenda setter, beneficiary (institutional/constrained)
 *   - Nuclear energy proponents: Beneficiary (organized/mobile)
 *   - Climate vulnerable populations: Beneficiary/Payer (powerless/trapped)
 *   - Populations near nuclear facilities: Payer (moderate/constrained)
 *   - Future generations: Payer (powerless/trapped)
 *   - Anti-nuclear advocates: Excluded (organized/constrained)
 *   - Environmental regulators: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.6).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '83e66ed5-1f26-4b57-b687-59281c1b31d9').
narrative_ontology:cs_kernel_codification('83e66ed5-1f26-4b57-b687-59281c1b31d9', formalized).
narrative_ontology:cs_authority_grounding('83e66ed5-1f26-4b57-b687-59281c1b31d9', expertise).
narrative_ontology:cs_interpretation_layer_present('83e66ed5-1f26-4b57-b687-59281c1b31d9').
narrative_ontology:cs_reading_relation('83e66ed5-1f26-4b57-b687-59281c1b31d9', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('83e66ed5-1f26-4b57-b687-59281c1b31d9', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('83e66ed5-1f26-4b57-b687-59281c1b31d9', foundational, climate_catastrophe_is_imminent_threat).
narrative_ontology:cs_axiom_status(climate_catastrophe_is_imminent_threat, holdable).
narrative_ontology:cs_axiom_grounding('83e66ed5-1f26-4b57-b687-59281c1b31d9', climate_catastrophe_is_imminent_threat, empirically_contingent).
narrative_ontology:cs_axiom('83e66ed5-1f26-4b57-b687-59281c1b31d9', foundational, risk_is_always_relative).
narrative_ontology:cs_axiom_status(risk_is_always_relative, holdable).
narrative_ontology:cs_axiom_grounding('83e66ed5-1f26-4b57-b687-59281c1b31d9', risk_is_always_relative, conventional).
narrative_ontology:cs_reference_frame('83e66ed5-1f26-4b57-b687-59281c1b31d9', post_fukushima_risk_reassessment).
narrative_ontology:cs_drift_state('83e66ed5-1f26-4b57-b687-59281c1b31d9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('83e66ed5-1f26-4b57-b687-59281c1b31d9', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, governments_seeking_energy_security).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, populations_near_nuclear_facilities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritize energy sources based on national security and decarbonization goals, often framing nuclear power as a necessary component when compared to fossil fuels. They administer regulatory frameworks that implement this comparative risk approach.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, governments_seeking_energy_security, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for nuclear power by emphasizing its low carbon footprint and comparing its risks favorably against coal and climate change. They benefit from policies that adopt this comparative risk framework, enabling new projects and continued operation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents, beneficiary,
    organized, biographical, mobile, global).

% Benefit from policies that aggressively decarbonize the energy sector, including nuclear power, as they are disproportionately affected by climate change. However, they may also bear residual nuclear risks if they live near facilities or are part of future generations inheriting waste.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer).

% Bear the localized, immediate risks of nuclear power generation (e.g., potential accidents, waste storage) as a trade-off for broader climate benefits. Their concerns about absolute safety thresholds are often overridden by the comparative risk calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, populations_near_nuclear_facilities, payer,
    moderate, biographical, constrained, local).

% Inherit the long-term burden of nuclear waste, which remains hazardous for millennia. This reading's emphasis on temporal urgency for climate action tends to downplay or defer these intergenerational concerns.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Oppose nuclear power based on its catastrophic tail risks, waste burden, and proliferation concerns. Their arguments for absolute safety thresholds are often marginalized or reframed as less urgent than climate change within this comparative risk framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocates, excluded,
    organized, generational, constrained, national).

% Tasked with assessing and managing environmental and safety risks across the energy sector. They operate within the policy framework set by governments, applying comparative risk methodologies to nuclear projects.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, environmental_regulators, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy by establishing a framework for evaluating nuclear power's risks and benefits relative to other energy sources, particularly fossil fuels, to achieve decarbonization and energy security goals.
% TRANSFER_FUNCTION: Transfers the perceived urgency and magnitude of climate-related risks (from fossil fuels) onto the justification for accepting nuclear-specific risks (waste, accidents), effectively shifting the burden of risk from one population/timeframe to another.
% ABSENT_VOICES: Those who prioritize absolute safety thresholds for nuclear power, or who advocate for rapid, large-scale deployment of non-nuclear, non-fossil alternatives (e.g., renewables + storage) that are not yet fully scaled or politically viable. Their concerns are often deemed less urgent or practical within this framework.
% DISAPPEARANCE_RATIONALE: If this comparative risk framework vanished, nuclear power's acceptability would likely plummet, leading to slower decarbonization efforts, increased reliance on fossil fuels, or a scramble for alternative energy strategies. Energy policy would revert to other, potentially more conservative, risk calculi.
% FOUNDING_PROBLEM: The challenge of rapidly decarbonizing energy systems and ensuring energy security while managing the specific, long-term risks associated with nuclear power, especially when faced with the immediate and widespread threats of climate change from fossil fuels.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies, climate scientists, and national security strategists corroborate the ongoing nature of this problem. While anti-nuclear groups dispute the solution, they generally acknowledge the underlying energy and climate challenges.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is substantial because while the framework addresses a genuine coordination problem (climate change), it does so by imposing specific, long-term risks on identifiable victims (populations near facilities, future generations) whose concerns are systematically downplayed. Suppression (0.60) is moderate, as alternative energy pathways are not entirely foreclosed, but the policy discourse actively suppresses arguments for absolute nuclear safety. Theater ratio (0.25) is low to moderate; there is genuine risk assessment, but also a performative aspect in consistently framing nuclear risks as 'acceptable' relative to others. The rising extractiveness and suppression over time reflect the increasing urgency of climate action being used to justify greater acceptance of nuclear risks and to push back against opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of governments and nuclear proponents, this framework is a rational, necessary coordination mechanism for addressing climate change. From the perspective of populations near nuclear facilities or future generations, it is an extractive mechanism that externalizes their risks for broader societal benefit. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Governments and nuclear proponents are clear beneficiaries, as the framework enables their policy and industry goals. Climate-vulnerable populations are complex: beneficiaries of climate action, but potential payers of nuclear risk. Populations near facilities and future generations are primary targets, bearing direct and deferred costs. Anti-nuclear advocates are excluded, as their core arguments are structurally marginalized by the comparative framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (addressing climate change via nuclear) is still live, preventing a Piton classification. However, the 'contested' status of the founding problem (whether nuclear is truly the best or only solution) and the rising extractiveness suggest a potential for Mandatrophy if the comparative risk argument becomes a mere cover for rent-seeking or avoiding investment in safer alternatives. The framework's persistence is tied to the ongoing climate crisis, but its specific form (comparative risk dominant) is subject to contestation regarding its fairness and long-term sustainability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_vs_catastrophic_tail_risk,
    'Does the ''comparative risk dominant'' framework adequately account for the low-probability, high-consequence (catastrophic tail) risks of nuclear power, or does it systematically downplay them relative to aggregate climate risks?',
    'Development of integrated risk models that explicitly quantify and weight both catastrophic tail risks and aggregate comparative risks, followed by expert consensus on their relative importance in policy decisions.',
    'If catastrophic tail risks are found to be systematically undervalued, the constraint''s effective extractiveness would be higher for populations bearing those risks, potentially shifting its classification towards a Snare for those seats. This would align it more closely with the ''catastrophic_tail_dominant'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_vs_catastrophic_tail_risk, conceptual, 'Ambiguity in weighting different types of risk within the framework.').

omega_variable(
    objectivity_of_risk_quantification,
    'Are the quantitative comparisons of nuclear risk versus fossil fuel/climate risk truly objective, or are they influenced by advocacy and political agendas?',
    'Independent, transparent audits of risk assessment methodologies and data inputs, with sensitivity analyses to reveal the impact of different assumptions on the comparative outcomes.',
    'If significant bias is found, the constraint''s suppression and extractiveness would be higher, as the ''coordination'' function would be revealed as a cover for a predetermined outcome. This would strengthen the case for a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_risk_quantification, empirical, 'Bias in the quantification of comparative energy risks.').

omega_variable(
    temporal_urgency_vs_intergenerational_equity,
    'Is the prioritization of present climate urgency over intergenerational nuclear waste burden a justifiable ethical stance, or a deferral of responsibility?',
    'Philosophical and ethical discourse, potentially leading to new international norms or legal frameworks that explicitly balance intergenerational equity with present-day environmental imperatives.',
    'If deemed an unjustifiable deferral, the constraint''s effective extractiveness for future generations would be significantly higher, and the ''tangled_rope'' classification would lean more heavily towards extraction, potentially approaching a Snare for that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_urgency_vs_intergenerational_equity, preference, 'Ethical trade-off between present and future generations'' burdens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1980, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(acce_be_t1980, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1980, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
