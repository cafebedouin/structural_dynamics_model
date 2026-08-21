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
 *   human_readable: Clean Air Act Section 111(d) 'Best System' Limited to Facility-Specific Measures
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific, narrow reading of Section 111(d)
 *   of the Clean Air Act, limiting the EPA's authority to regulate greenhouse
 *   gas emissions from existing power plants to 'measures implementable at
 *   and by a source'. This interpretation, notably advanced by the Supreme
 *   Court in West Virginia v. EPA, prevents the EPA from mandating
 *   'generation-shifting' (e.g., replacing coal with renewables) and
 *   restricts it to facility-specific improvements like heat-rate upgrades or
 *   carbon capture. This reading protects the coal sector and states with
 *   fossil fuel interests, while climate advocates and the EPA itself become
 *   victims of a regulatory ceiling that prevents effective systemic change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.75).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) 'Best System' Limited to Facility-Specific Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '7a097c2b-b961-4d01-ba5d-f0d4ce535406').
narrative_ontology:cs_kernel_codification('7a097c2b-b961-4d01-ba5d-f0d4ce535406', fixed_text).
narrative_ontology:cs_authority_grounding('7a097c2b-b961-4d01-ba5d-f0d4ce535406', lineage).
narrative_ontology:cs_interpretation_layer_present('7a097c2b-b961-4d01-ba5d-f0d4ce535406').
narrative_ontology:cs_reading_relation('7a097c2b-b961-4d01-ba5d-f0d4ce535406', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('7a097c2b-b961-4d01-ba5d-f0d4ce535406', foundational, agency_authority_limited_to_source_specific_measures).
narrative_ontology:cs_axiom_status(agency_authority_limited_to_source_specific_measures, holdable).
narrative_ontology:cs_axiom_grounding('7a097c2b-b961-4d01-ba5d-f0d4ce535406', agency_authority_limited_to_source_specific_measures, conventional).
narrative_ontology:cs_axiom('7a097c2b-b961-4d01-ba5d-f0d4ce535406', foundational, major_questions_doctrine_applies_to_climate_regulation).
narrative_ontology:cs_axiom_status(major_questions_doctrine_applies_to_climate_regulation, holdable).
narrative_ontology:cs_axiom_grounding('7a097c2b-b961-4d01-ba5d-f0d4ce535406', major_questions_doctrine_applies_to_climate_regulation, conventional).
narrative_ontology:cs_reference_frame('7a097c2b-b961-4d01-ba5d-f0d4ce535406', original_congressional_intent_narrow_delegation).
narrative_ontology:cs_drift_state('7a097c2b-b961-4d01-ba5d-f0d4ce535406', west_virginia_v_epa_ruling, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7a097c2b-b961-4d01-ba5d-f0d4ce535406', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_interests).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with regulating greenhouse gas emissions under Section 111(d), but constrained by this reading to only mandate measures implementable at individual power plants. This limits its ability to achieve significant emissions reductions and forces it to enforce a less effective regulatory regime.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the regulatory ceiling imposed by this reading, which protects existing coal-fired power plants from mandates requiring generation-shifting or early retirement. This reduces compliance costs and extends the operational lifespan of their assets.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector, beneficiary,
    organized, biographical, mobile, national).

% Preserves their autonomy over state energy mix decisions, preventing federal mandates that would force a transition away from fossil fuels. This aligns with their economic and political interests in maintaining a fossil-fuel-dependent energy infrastructure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_interests, beneficiary,
    institutional, generational, mobile, national).

% Bear the costs of a less effective climate policy, as this reading limits the scope of emissions reductions. Their advocacy efforts are channeled into litigation and political pressure against a regulatory framework that prevents systemic change.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, global).

% Face a reduced market for large-scale renewable projects that would displace fossil fuels, as the regulatory framework does not incentivize generation-shifting. This limits their growth and investment opportunities.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% The ultimate arbiter of this interpretation, having issued rulings that shape the scope of EPA's authority under Section 111(d). Its decisions define the boundaries of this constraint.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit limited, framework for EPA to regulate greenhouse gas emissions from existing power plants, ensuring some level of federal action and consistency across states regarding facility-level improvements.
% TRANSFER_FUNCTION: Transfers regulatory authority and flexibility away from the EPA to states and the coal power sector, while transferring the burden of climate inaction onto climate advocates and future generations.
% ABSENT_VOICES: Future generations, who will bear the long-term costs of climate change exacerbated by limited regulatory action, are absent from the immediate legal and political discourse. Their interests are represented by climate advocates, but without direct voice.
% DISAPPEARANCE_RATIONALE: If this reading of Section 111(d) vanished, EPA's authority would expand to include generation-shifting, leading to significant changes in the energy sector, including accelerated coal plant retirements and increased investment in renewables. The regulatory landscape and energy mix would fundamentally reorganize.
% FOUNDING_PROBLEM: The Clean Air Act needed a mechanism to regulate emissions from existing sources, including those not covered by other sections, to protect public health and welfare from air pollution.
% FOUNDING_PROBLEM_CORROBORATION: EPA and climate advocates attest that the problem of regulating existing source emissions, particularly greenhouse gases, remains live and urgent. The coal power sector and states with fossil fuel interests acknowledge the problem but dispute the scope of federal authority to address it, corroborating the problem's existence but not the solution's breadth.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it performs a coordination function (providing a clear, if limited, regulatory framework) but also involves significant asymmetric extraction. Extractiveness is high (0.68) because it imposes a substantial cost on climate action and renewable energy development by limiting the most effective regulatory tools. Suppression (0.75) is high due to the judicial enforcement of this narrow interpretation, which actively suppresses broader regulatory alternatives. The theater ratio (0.20) is relatively low, as the facility-specific measures, while limited, are genuine attempts at emission reduction, not pure performance. Resistance is high (0.80) from climate advocates and the EPA, who actively challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and climate advocates experience this constraint as a significant limitation on necessary climate action, while the coal sector and states with fossil fuel interests perceive it as a legitimate defense of state sovereignty and economic interests. The engine's per-seat classification will reflect this divergence, showing the EPA and climate advocates as targets of extraction, and the coal sector/states as beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, despite being the nominal agenda-setter, is a victim of this reading, as its statutory authority is curtailed (d near 1.0). The coal power sector and states with fossil fuel interests are clear beneficiaries, as their economic models and energy policies are protected (d near 0.0). Climate advocates and renewable energy developers are victims, bearing the costs of limited climate action and reduced market opportunities (d near 1.0). The Supreme Court acts as an observer, defining the boundaries of the constraint through its rulings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_legislative_intent,
    'What was the original legislative intent behind ''best system of emission reduction'' in Section 111(d)?',
    'Historical legislative analysis, including committee reports, floor debates, and contemporaneous legal interpretations of the Clean Air Act''s delegation language.',
    'If original intent supports broader EPA authority, this reading is a judicial overreach; if it supports a narrow, facility-specific approach, this reading is a faithful interpretation. This would shift the legitimacy of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_legislative_intent, conceptual, 'Ambiguity regarding the original scope of EPA''s delegated authority.').

omega_variable(
    major_questions_doctrine_scope,
    'Is the ''major questions doctrine'' (which requires clear congressional authorization for agencies to decide issues of vast economic and political significance) being applied consistently, or is it selectively invoked to limit climate regulation?',
    'Comparative legal analysis of Supreme Court applications of the major questions doctrine across different regulatory domains (e.g., financial regulation, public health) to identify patterns of application.',
    'If selectively applied, the doctrine itself becomes a Snare for climate action, and this reading of 111(d) is a symptom of that broader extraction. If consistently applied, it reinforces the legitimacy of this constraint''s limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'The consistency and neutrality of the major questions doctrine''s application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, us_climate_policy_framework).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, epa_regulatory_authority).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_investment_incentives).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'caa_section_111d_delegation' kernel. Its sibling, 'caa_section_111d_delegation__systemic_transformation_reading', represents a broader interpretation of EPA's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
