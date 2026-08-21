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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint represents a specific legal interpretation of Section
 *   111(d) of the Clean Air Act, limiting the Environmental Protection
 *   Agency's (EPA) authority to regulate greenhouse gas emissions from
 *   existing power plants. Under this reading, the 'best system of emission
 *   reduction' is confined to measures implementable at individual facilities
 *   (e.g., heat-rate improvements, carbon capture), explicitly excluding
 *   broader 'generation-shifting' strategies that would compel a transition
 *   away from fossil fuels. This interpretation, notably reinforced by the
 *   Supreme Court's ruling in West Virginia v. EPA (2022), acts as a boundary
 *   on federal administrative power, protecting certain industries and state
 *   autonomy while significantly constraining climate action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.75).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) 'Best System' Limited to Facility-Specific Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '3909a5a9-7c24-433c-9d35-140fff4c9d0c').
narrative_ontology:cs_kernel_codification('3909a5a9-7c24-433c-9d35-140fff4c9d0c', fixed_text).
narrative_ontology:cs_authority_grounding('3909a5a9-7c24-433c-9d35-140fff4c9d0c', lineage).
narrative_ontology:cs_interpretation_layer_present('3909a5a9-7c24-433c-9d35-140fff4c9d0c').
narrative_ontology:cs_reading_relation('3909a5a9-7c24-433c-9d35-140fff4c9d0c', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('3909a5a9-7c24-433c-9d35-140fff4c9d0c', foundational, major_questions_doctrine_applies).
narrative_ontology:cs_axiom_status(major_questions_doctrine_applies, holdable).
narrative_ontology:cs_axiom_grounding('3909a5a9-7c24-433c-9d35-140fff4c9d0c', major_questions_doctrine_applies, conventional).
narrative_ontology:cs_axiom('3909a5a9-7c24-433c-9d35-140fff4c9d0c', foundational, epa_lacks_generation_shifting_authority).
narrative_ontology:cs_axiom_status(epa_lacks_generation_shifting_authority, holdable).
narrative_ontology:cs_axiom_grounding('3909a5a9-7c24-433c-9d35-140fff4c9d0c', epa_lacks_generation_shifting_authority, conventional).
narrative_ontology:cs_reference_frame('3909a5a9-7c24-433c-9d35-140fff4c9d0c', limited_agency_delegation_framework).
narrative_ontology:cs_drift_state('3909a5a9-7c24-433c-9d35-140fff4c9d0c', post_west_virginia_v_epa_ruling, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3909a5a9-7c24-433c-9d35-140fff4c9d0c', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_rights_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, regulated_facilities).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, federalism_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The EPA's authority to regulate greenhouse gas emissions from existing power plants under Section 111(d) is severely constrained to 'inside the fence-line' measures, limiting its ability to mandate broader generation-shifting strategies. It must enforce this narrower interpretation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency, payer,
    institutional, generational, constrained, national).

% Protected from federal mandates that would force early retirement of coal-fired power plants or require significant shifts in energy generation mix. They still face costs for facility-specific improvements but avoid more disruptive systemic changes.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector, beneficiary,
    organized, biographical, constrained, national).

% Their arguments for state autonomy over energy policy and against federal overreach are vindicated, preserving states' ability to determine their own energy mix without federal interference.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Their goals for rapid, systemic decarbonization are significantly hampered by the limitation of federal regulatory power, forcing them to pursue more fragmented or slower state-level actions.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, global).

% The ultimate arbiter of this interpretation, having issued rulings that define and enforce the limits of EPA's delegated authority under the Clean Air Act. Its decisions shape the regulatory landscape for decades.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Individual power plants must implement 'best system of emission reduction' measures that are implementable at the facility level (e.g., heat-rate improvements, carbon capture), but are shielded from mandates requiring them to shift to different energy sources.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, regulated_facilities, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of regulatory authority between the federal EPA and individual states regarding energy policy, and sets clear, albeit limited, expectations for regulated power facilities under Section 111(d).
% TRANSFER_FUNCTION: Transfers the primary burden of addressing climate change from broad federal mandates to facility-specific improvements and state-level policy decisions. It also transfers decision-making power from the EPA to states and the regulated industry regarding energy mix.
% ABSENT_VOICES: Future generations and ecosystems, who bear the long-term costs of delayed systemic climate action, are structurally absent from the legal interpretation process that defines the scope of federal environmental protection.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the EPA would likely reassert broader authority under Section 111(d), potentially mandating generation-shifting and accelerating the retirement of fossil fuel plants. This would fundamentally alter the U.S. energy landscape, state-federal relations, and the trajectory of climate policy.
% FOUNDING_PROBLEM: To define the appropriate scope of federal agency power, particularly when an agency seeks to regulate an issue of 'vast economic and political significance' without clear congressional authorization, as articulated by the Major Questions Doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, dissenting judicial opinions, and ongoing legislative debates consistently corroborate the live and contested nature of federal agency power and the Major Questions Doctrine. Independent constitutional law experts outside the directly benefiting parties attest to the ongoing nature of this foundational problem.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The `extractiveness` (0.68) is high because this interpretation extracts the possibility of systemic climate action from the EPA and climate advocates, effectively imposing a regulatory ceiling. It also extracts potential economic benefits from renewable energy sectors that would thrive under broader mandates. `Suppression` (0.75) is high due to the active judicial enforcement that prevents the EPA from pursuing alternative, more expansive regulatory strategies. `Theater_ratio` (0.15) is low because the legal interpretation is a direct, functional limitation, not primarily performative. The `resistance` (0.70) is high, reflecting ongoing efforts by climate advocates and the EPA to challenge or circumvent this limitation through other legal or legislative avenues.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the `coal_power_sector` and `states_rights_advocates`, this constraint is a necessary defense of federalism and limited government, potentially appearing as a 'rope' or even a 'mountain' (a fixed legal principle). For the `environmental_protection_agency` and `climate_advocates`, it operates as a 'snare' or 'tangled_rope,' actively extracting their capacity for effective climate action. The engine's computation will highlight this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The `coal_power_sector` and `states_rights_advocates` are clear beneficiaries, as the constraint protects their interests from more aggressive federal regulation. The `environmental_protection_agency` and `climate_advocates` are the primary victims, as their ability to achieve broader climate goals is curtailed. The `supreme_court` acts as the agenda-setter, defining and enforcing the boundaries of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reflection of congressional intent for Section 111(d), or a judicial construction that redefines the statute''s scope?',
    'Historical legislative analysis of the Clean Air Act''s drafting and subsequent amendments, and comparison with other statutory delegations of power to environmental agencies.',
    'If a judicial construction, the constraint''s legitimacy as a ''fixed'' boundary is weakened, potentially reclassifying it as a more actively enforced ''snare'' or ''tangled_rope'' rather than a ''mountain'' of administrative law. If it genuinely reflects intent, its ''mountain-like'' qualities for beneficiaries are reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the true origin and intent of the Section 111(d) ''best system'' limitation.').

omega_variable(
    major_questions_doctrine_scope,
    'What are the precise boundaries and applicability criteria of the Major Questions Doctrine, and how consistently is it applied across different administrative law contexts?',
    'Further Supreme Court rulings clarifying the doctrine''s scope, or comprehensive legal scholarship analyzing its application patterns and potential for selective enforcement.',
    'A clearer, more consistently applied doctrine would solidify this constraint''s legal foundation. Inconsistent or expansive application could reveal it as a flexible tool for judicial policy-making, increasing its ''extractiveness'' from agencies and its ''theater_ratio'' as a legal justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Uncertainty regarding the Major Questions Doctrine''s definition and consistent application.').

omega_variable(
    empirical_impact_of_limitation,
    'What is the measurable difference in greenhouse gas emissions reductions and climate impacts between facility-specific measures and systemic generation-shifting strategies over the next 10-20 years?',
    'Independent climate modeling and economic analysis comparing projected emissions trajectories under both regulatory approaches, accounting for technological advancements and market dynamics.',
    'If facility-specific measures prove significantly less effective at reducing emissions, the ''extractiveness'' from climate goals is empirically validated as severe, strengthening the ''snare'' aspect for climate advocates. If they prove surprisingly effective, the ''tangled_rope'' aspect might lean more towards coordination for regulated entities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_of_limitation, empirical, 'The actual environmental and climate impact of limiting EPA''s regulatory scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2016, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(caa__tr_t2018, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(caa__tr_t2020, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2024, 0.15).
narrative_ontology:measurement(caa__tr_t2026, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(caa__be_t2016, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(caa__be_t2018, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(caa__be_t2020, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement(caa__be_t2026, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2016, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(caa__su_t2018, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(caa__su_t2020, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2022, 0.75).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2024, 0.75).
narrative_ontology:measurement(caa__su_t2026, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, us_energy_policy_framework).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, epa_regulatory_authority).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'caa_section_111d_delegation' kernel, specifically the 'facility_constraint_reading'. It is structurally opposed to the 'systemic_transformation_reading', which posits a broader EPA authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
