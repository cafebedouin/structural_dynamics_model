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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Clean Air Act Section 111(d) Delegation (Facility-Specific Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific interpretation of Section 111(d) of
 *   the Clean Air Act, limiting the Environmental Protection Agency's (EPA)
 *   authority to regulate greenhouse gas emissions to measures implementable
 *   at individual facilities. This reading, often associated with a major
 *   questions doctrine approach, prevents the EPA from mandating broader,
 *   generation-shifting strategies. It is a 'tangled rope' because it
 *   provides a clear regulatory boundary (coordination) but does so in a way
 *   that disproportionately benefits fossil fuel industries and states
 *   reliant on them, while extracting costs from environmental and public
 *   health advocates (asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.65).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.7).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) Delegation (Facility-Specific Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '01ac3538-488b-46db-8a67-2709f06d1581').
narrative_ontology:cs_kernel_codification('01ac3538-488b-46db-8a67-2709f06d1581', fixed_text).
narrative_ontology:cs_authority_grounding('01ac3538-488b-46db-8a67-2709f06d1581', lineage).
narrative_ontology:cs_interpretation_layer_present('01ac3538-488b-46db-8a67-2709f06d1581').
narrative_ontology:cs_reading_relation('01ac3538-488b-46db-8a67-2709f06d1581', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('01ac3538-488b-46db-8a67-2709f06d1581', foundational, congressional_intent_limits_agency_power).
narrative_ontology:cs_axiom_status(congressional_intent_limits_agency_power, holdable).
narrative_ontology:cs_axiom_grounding('01ac3538-488b-46db-8a67-2709f06d1581', congressional_intent_limits_agency_power, deontological).
narrative_ontology:cs_axiom('01ac3538-488b-46db-8a67-2709f06d1581', foundational, major_questions_doctrine_applies_to_climate).
narrative_ontology:cs_axiom_status(major_questions_doctrine_applies_to_climate, holdable).
narrative_ontology:cs_axiom_grounding('01ac3538-488b-46db-8a67-2709f06d1581', major_questions_doctrine_applies_to_climate, conventional).
narrative_ontology:cs_reference_frame('01ac3538-488b-46db-8a67-2709f06d1581', limited_delegation_principle).
narrative_ontology:cs_drift_state('01ac3538-488b-46db-8a67-2709f06d1581', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('01ac3538-488b-46db-8a67-2709f06d1581', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_power_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_economies).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, public_health_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) of the Clean Air Act to limit EPA's authority to 'best system of emission reduction' to measures implementable at individual facilities, thereby preventing generation-shifting mandates. This interpretation constrains EPA's regulatory scope.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Is tasked with regulating greenhouse gas emissions but finds its authority significantly curtailed by this interpretation. It must develop regulations within the narrow confines of facility-specific measures, limiting its ability to address climate change effectively.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency, payer,
    institutional, biographical, constrained, national).

% Benefit from the constraint as it protects their existing business model from more stringent, systemic regulations that would force early retirement or significant investment in renewable energy. They face lower compliance costs than under a broader interpretation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_power_generators, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the preservation of their energy mix and economic structures tied to fossil fuels. This reading reinforces state autonomy over energy policy, shielding them from federal mandates that might disrupt their economies.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_economies, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of limited climate action, as their goals for rapid decarbonization are thwarted by the narrow regulatory scope. They must pursue more fragmented and less effective strategies to achieve environmental protection.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_advocacy_groups, payer,
    organized, generational, constrained, global).

% Experience the negative health impacts of continued fossil fuel emissions due to the limited regulatory authority. Their efforts to improve public health through cleaner air are hampered by the constraint.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, public_health_advocates, payer,
    moderate, biographical, constrained, local).

% Are disadvantaged as the regulatory landscape favors incumbent fossil fuel industries by limiting mandates for generation-shifting. This reduces market demand for their products and slows the transition to cleaner energy.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    powerful, biographical, mobile, national).

% Provide scientific consensus on climate change and the need for systemic decarbonization, but their findings are not directly incorporated into the regulatory framework under this interpretation. They observe the policy gap between scientific urgency and legal constraint.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates regulatory expectations by providing a clear, albeit narrow, boundary for EPA's authority under Section 111(d), ensuring that states and regulated entities understand the scope of federal intervention in energy policy.
% TRANSFER_FUNCTION: Transfers regulatory burden away from coal power generators and states with fossil fuel economies, effectively transferring the costs of climate inaction (environmental degradation, public health impacts) to environmental and public health advocates, and the broader public.
% ABSENT_VOICES: Future generations, who will bear the long-term costs of climate change exacerbated by limited regulatory action, are absent from the legal and political discourse shaping this interpretation. Their interests are represented by advocates whose influence is curtailed.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, EPA's authority would expand, likely leading to more aggressive, systemic regulations targeting greenhouse gas emissions. This would force significant changes in the energy sector, accelerating the retirement of fossil fuel plants and promoting renewable energy, fundamentally altering economic and environmental landscapes.
% FOUNDING_PROBLEM: The Clean Air Act aimed to protect and enhance air quality, with Section 111(d) specifically addressing emissions from existing sources where no other provision applied, allowing EPA to set standards based on the 'best system of emission reduction'. The problem was how to regulate a wide array of industrial emissions.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority attests that the founding problem was about limiting federal agency overreach and preserving state authority, which remains live. Environmental groups and dissenting justices argue the core problem was air pollution, which is still live but inadequately addressed by this narrow interpretation. Legal scholars and historical legislative records provide corroboration for both the broad environmental protection goals and the concerns about federalism and delegation limits.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because the interpretation imposes a significant ceiling on climate action, effectively allowing continued emissions that generate external costs. Suppression (0.70) is high due to the judicial enforcement of this narrow reading, which actively suppresses alternative, broader regulatory approaches. The theater ratio (0.20) is relatively low, as the EPA's efforts within this constrained framework are genuine, but their effectiveness is limited by the interpretation. Resistance (0.80) is high, reflecting ongoing legal challenges and advocacy from environmental groups against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coal power generators and states, this constraint is a legitimate defense of federalism and economic stability, a 'rope' that coordinates regulatory certainty. From the perspective of environmental and public health advocates, it is a 'snare' that actively thwarts necessary climate action and imposes significant external costs. The engine's classification as 'tangled_rope' captures this hybrid nature, acknowledging both the coordination function (setting clear regulatory boundaries) and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court majority acts as the agenda-setter, defining the constraint's scope. Coal power generators and states with fossil fuel economies are clear beneficiaries, as their operations are protected from more disruptive regulations. The EPA, environmental groups, public health advocates, and renewable energy developers are victims, bearing the costs of limited climate action and constrained regulatory options. Device users are not directly impacted by this specific constraint, as it concerns regulatory authority rather than direct consumer costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_scope,
    'Is the ''major questions doctrine'' (which underpins this interpretation) a genuine constitutional limit on agency power, or a judicial tool to block disfavored regulations?',
    'Analysis of future Supreme Court applications of the doctrine across diverse regulatory domains: consistent application suggests a genuine limit; selective application suggests a tool.',
    'If a genuine limit, the constraint''s ''naturalness'' increases, potentially shifting it closer to a Mountain (from the judicial seat); if a tool, its extractiveness and suppression are more clearly revealed as judicially imposed, reinforcing its Snare-like qualities for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Ambiguity of the major questions doctrine''s application.').

omega_variable(
    economic_impact_of_facility_measures,
    'What is the actual economic and environmental impact of facility-specific measures (e.g., carbon capture) compared to systemic, generation-shifting strategies?',
    'Empirical studies comparing the cost-effectiveness and emissions reductions of facility-level improvements versus grid-wide renewable energy transitions over a 10-20 year horizon.',
    'If facility-specific measures prove highly effective and cost-efficient, the constraint''s coordination function is strengthened; if they are demonstrably insufficient, the extractive nature of limiting EPA''s authority becomes more pronounced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_impact_of_facility_measures, empirical, 'Effectiveness of facility-specific vs. systemic climate measures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2022, 2032).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2024, 0.21).
narrative_ontology:measurement(caa__tr_t2026, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement(caa__tr_t2028, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2028, 0.23).
narrative_ontology:measurement(caa__tr_t2030, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2030, 0.24).
narrative_ontology:measurement(caa__tr_t2032, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2032, 0.25).

% Extraction over time
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement(caa__be_t2026, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2026, 0.67).
narrative_ontology:measurement(caa__be_t2028, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2028, 0.68).
narrative_ontology:measurement(caa__be_t2030, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2030, 0.69).
narrative_ontology:measurement(caa__be_t2032, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2032, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement(caa__su_t2026, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement(caa__su_t2028, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2028, 0.73).
narrative_ontology:measurement(caa__su_t2030, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2030, 0.74).
narrative_ontology:measurement(caa__su_t2032, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2032, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, epa_regulatory_authority_scope).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, us_climate_policy_trajectory).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Section 111(d) delegation kernel. Its sibling, 'caa_section_111d_delegation__systemic_transformation_reading', represents a broader interpretation of EPA's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
