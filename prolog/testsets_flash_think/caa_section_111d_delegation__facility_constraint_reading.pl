% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) 'Best System' Limited to Facility-Level Measures
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific legal reading of Section 111(d) of
 *   the Clean Air Act, primarily articulated by the Supreme Court in West
 *   Virginia v. EPA (2022). This reading limits the EPA's authority to
 *   regulate greenhouse gas emissions from existing power plants to 'measures
 *   implementable at and within the fence-line of individual facilities'
 *   (e.g., heat-rate improvements, carbon capture), explicitly excluding
 *   broader 'generation-shifting' strategies. This interpretation is
 *   presented by its proponents as a proper coordination of federal and state
 *   powers, but it effectively creates a regulatory ceiling that benefits the
 *   fossil fuel industry and states prioritizing them, while extracting from
 *   climate action and advocates. The claimed type is 'rope' (coordination of
 *   federalism), but the metrics reflect its highly extractive and
 *   suppressive operation from the perspective of climate action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.78).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.85).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) 'Best System' Limited to Facility-Level Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'd7fe11aa-1ba0-4d7b-8459-5df003edde54').
narrative_ontology:cs_kernel_codification('d7fe11aa-1ba0-4d7b-8459-5df003edde54', fixed_text).
narrative_ontology:cs_authority_grounding('d7fe11aa-1ba0-4d7b-8459-5df003edde54', lineage).
narrative_ontology:cs_interpretation_layer_present('d7fe11aa-1ba0-4d7b-8459-5df003edde54').
narrative_ontology:cs_reading_relation('d7fe11aa-1ba0-4d7b-8459-5df003edde54', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('d7fe11aa-1ba0-4d7b-8459-5df003edde54', foundational, epa_authority_limited_to_fence_line).
narrative_ontology:cs_axiom_status(epa_authority_limited_to_fence_line, holdable).
narrative_ontology:cs_axiom_grounding('d7fe11aa-1ba0-4d7b-8459-5df003edde54', epa_authority_limited_to_fence_line, conventional).
narrative_ontology:cs_axiom('d7fe11aa-1ba0-4d7b-8459-5df003edde54', foundational, states_retain_primary_energy_mix_control).
narrative_ontology:cs_axiom_status(states_retain_primary_energy_mix_control, holdable).
narrative_ontology:cs_axiom_grounding('d7fe11aa-1ba0-4d7b-8459-5df003edde54', states_retain_primary_energy_mix_control, deontological).
narrative_ontology:cs_reference_frame('d7fe11aa-1ba0-4d7b-8459-5df003edde54', traditional_federalism_regulatory_scope).
narrative_ontology:cs_drift_state('d7fe11aa-1ba0-4d7b-8459-5df003edde54', post_west_virginia_v_epa, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('d7fe11aa-1ba0-4d7b-8459-5df003edde54', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_prioritizing_fossil_fuels).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, states_prioritizing_renewables).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of statutory interpretation and constitutional limits on federal power. Its reading of Section 111(d) defines the scope of EPA's authority, actively enforcing the 'fence-line' limitation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% The federal agency tasked with implementing the Clean Air Act. This constraint severely limits its ability to address climate change through broad, generation-shifting strategies, forcing it to focus on less impactful facility-specific measures.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, payer,
    institutional, biographical, constrained, national).

% Benefits directly from the limitation on EPA's power, as it protects coal-fired power plants from mandates that would force their retirement or significant operational changes beyond individual facility improvements. Avoids substantial compliance costs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_sector, beneficiary,
    organized, biographical, mobile, national).

% States whose economies or political priorities favor continued reliance on fossil fuels. They benefit from the preservation of state autonomy over energy mix decisions, free from federal mandates for systemic energy transformation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_prioritizing_fossil_fuels, beneficiary,
    institutional, generational, constrained, regional).

% Bear the cost of delayed or insufficient climate action. This constraint creates a significant regulatory ceiling, making it harder to achieve ambitious emissions reductions necessary to mitigate climate change impacts.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, global).

% States actively pursuing renewable energy transitions. While they can continue their own policies, the lack of federal support for systemic change and the continued operation of fossil fuel plants in other states undermine their broader climate goals.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_prioritizing_renewables, payer,
    institutional, generational, constrained, regional).

% Indirectly bears the costs of climate change impacts (e.g., extreme weather, health effects) due to limited federal action, while also potentially benefiting from stable, affordable energy in the short term, depending on regional energy mix.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, general_public, payer,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, coal_sector).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the division of environmental regulatory authority between the federal EPA and individual states, ensuring EPA's actions remain within its statutorily defined 'best system of emission reduction' as limited to measures implementable at individual facilities.
% TRANSFER_FUNCTION: Transfers significant regulatory power and the associated economic burden/benefit of climate action away from the federal EPA's broader, grid-wide strategies, effectively protecting the coal sector and states reliant on fossil fuels from federal mandates for systemic energy transformation.
% ABSENT_VOICES: Future generations and non-human ecosystems, who bear the long-term costs of climate change exacerbated by limited federal action, are structurally excluded from the legal and political processes that define this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the EPA would immediately be empowered to pursue comprehensive, generation-shifting strategies to reduce emissions, fundamentally altering the national energy landscape, accelerating the transition to renewables, and significantly impacting the economic viability of the fossil fuel industry.
% FOUNDING_PROBLEM: To prevent federal administrative agencies from overstepping their statutory authority and encroaching on areas traditionally reserved for state control, particularly regarding energy policy and economic regulation.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court's majority opinion, supported by industry groups and states prioritizing fossil fuels, attests that the problem of federal overreach is live and requires judicial restraint. Environmental groups and dissenting justices argue the problem is climate change, not overreach, and that the Court's reading creates a new problem rather than solving an old one.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because this reading significantly curtails EPA's ability to impose cost-effective, systemic climate solutions, thereby preserving the economic viability of high-emitting sectors at the expense of climate goals. Suppression is very high (0.85) as it legally forecloses alternative, more impactful regulatory approaches, backed by the highest judicial authority. Theater ratio is low (0.10) because the constraint is functionally effective in limiting EPA's power; its enforcement is not primarily performative. Accessibility collapse is high (0.88) as the legal interpretation effectively removes broader regulatory alternatives. Resistance is moderate (0.70) from environmental groups and some states, who continue to challenge the interpretation and seek alternative avenues for climate action.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Supreme Court and its proponents, this constraint is a necessary 'rope' that coordinates federal and state powers, preventing administrative overreach and upholding constitutional principles. From the perspective of climate advocates and the EPA, it functions as a 'snare' or 'tangled rope,' extracting environmental progress and suppressing effective climate policy under the guise of legal interpretation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda-setter, defines and enforces this constraint. The coal sector and states prioritizing fossil fuels are clear beneficiaries, avoiding significant regulatory costs and preserving their energy mix. The EPA, climate advocates, and states prioritizing renewables are victims, as their ability to pursue comprehensive climate action is severely curtailed. The general public is an indirect victim, bearing the long-term costs of climate change.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from its proponents' view, is to uphold federalism and limit agency power, which they argue is a live problem. However, from the perspective of climate advocates, the constraint's effect is to prevent action on a far more pressing and evolving problem (climate change), suggesting a potential mandatrophy where the original 'problem' (federal overreach) is used to justify inaction on a critical contemporary issue. The rising extractiveness over time, especially post-2022, indicates an accumulation of costs on the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    best_system_definition_ambiguity,
    'Is the statutory phrase ''best system of emission reduction'' inherently limited to measures implementable at individual facilities, or does it encompass broader, grid-wide strategies?',
    'Congressional amendment to Section 111(d) explicitly defining the scope of ''best system,'' or a future Supreme Court ruling re-interpreting the statute based on new legal arguments or changed circumstances.',
    'If ''best system'' is interpreted broadly, EPA''s authority would expand, reducing extractiveness and suppression on climate action; if the narrow reading is affirmed, the current constraint persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(best_system_definition_ambiguity, conceptual, 'Ambiguity in the statutory definition of EPA''s regulatory scope.').

omega_variable(
    federalism_vs_climate_imperative,
    'To what extent does the principle of federalism, as interpreted by this reading, genuinely serve as a coordination function for environmental governance, versus acting as a cover for protecting specific economic interests from federal regulation?',
    'Empirical analysis of state-level climate action in the absence of federal mandates: if states effectively address climate change without federal intervention, the federalism argument is strengthened; if not, it suggests federalism is being leveraged for extraction.',
    'If federalism is primarily a cover, the constraint''s true nature is more extractive (snare-like); if it genuinely enables effective state-level coordination, it leans more towards a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_vs_climate_imperative, empirical, 'The true function of federalism in this context: coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(caa__tr_t2018, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2018, 0.07).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2021, 0.09).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(caa__tr_t2023, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(caa__be_t2018, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2022, 0.75).
narrative_ontology:measurement(caa__be_t2023, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2023, 0.77).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(caa__su_t2018, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2021, 0.78).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2022, 0.83).
narrative_ontology:measurement(caa__su_t2023, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2023, 0.84).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, us_climate_policy).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, epa_regulatory_authority).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the CAA Section 111(d) delegation kernel. This 'facility_constraint_reading' limits EPA's authority to individual facilities, while the 'systemic_transformation_reading' (a sibling constraint) would authorize broader, grid-wide strategies. Their ε values and stakeholder impacts differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
