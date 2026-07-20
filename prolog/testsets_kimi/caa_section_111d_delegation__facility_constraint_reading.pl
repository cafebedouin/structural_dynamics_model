% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) Facility-Constraint Reading
 *   domain: administrative_law/environmental_regulation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act requires EPA and states to set
 *   performance standards for existing stationary sources based on the 'best
 *   system of emission reduction.' This constraint story instantiates the
 *   facility-constraint reading: the judicial and administrative
 *   interpretation that limits 'best system' to measures implementable at
 *   individual facilitiesâprimarily heat-rate improvements and carbon
 *   captureâthereby prohibiting EPA from mandating generation-shifting,
 *   coal retirement, or renewable substitution. The reading protects
 *   coal-plant operators from forced retirement, preserves state autonomy
 *   over energy mix, and caps climate ambition, making climate advocates and
 *   affected communities the extraction victims of a regulatory ceiling. The
 *   constraint is actively enforced by federal courts applying textualist
 *   statutory interpretation and the major questions doctrine.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/analytical) â interprets and enforces the fenceline statutory limit
 *   - coal_plant_operators: primary beneficiary (powerful/constrained) â protected from generation-shifting mandates but subject to fenceline technology requirements
 *   - state_regulators: secondary beneficiary (institutional/constrained) â retain authority over resource planning and energy mix
 *   - fossil_fuel_dependent_utilities: tertiary beneficiary (powerful/constrained) â shielded from stranded-asset risk
 *   - epa_administrator: institutional payer (institutional/constrained) â regulatory authority capped by judicial interpretation
 *   - climate_advocates: primary payer (organized/constrained) â blocked from preferred regulatory pathway, climate ambition capped
 *   - renewable_energy_sector: secondary payer (organized/constrained) â denied regulatory-driven market expansion
 *   - affected_communities: tertiary payer (powerless/trapped) â exposed to continued local pollution without exit
 *   - administrative_law_scholars: observer (analytical/analytical) â analyze doctrinal evolution without direct cost-bearing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.7).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) Facility-Constraint Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '3ceffc56-7bbb-4f3f-aee2-952dacdcf322').
narrative_ontology:cs_kernel_codification('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', fixed_text).
narrative_ontology:cs_authority_grounding('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', lineage).
narrative_ontology:cs_interpretation_layer_present('3ceffc56-7bbb-4f3f-aee2-952dacdcf322').
narrative_ontology:cs_reading_relation('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', foundational, source_based_statutory_integrity).
narrative_ontology:cs_axiom_status(source_based_statutory_integrity, holdable).
narrative_ontology:cs_axiom_grounding('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', source_based_statutory_integrity, conventional).
narrative_ontology:cs_axiom('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', foundational, major_questions_clear_statement).
narrative_ontology:cs_axiom_status(major_questions_clear_statement, holdable).
narrative_ontology:cs_axiom_grounding('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', major_questions_clear_statement, conventional).
narrative_ontology:cs_reference_frame('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', facility_based_emission_control).
narrative_ontology:cs_drift_state('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', post_west_virginia_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3ceffc56-7bbb-4f3f-aee2-952dacdcf322', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_regulators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_dependent_utilities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, affected_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa_administrator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the statutory boundary of Section 111(d) through case law and the major questions doctrine; determines whether EPA rules exceed the fenceline limit and strikes down systemic transformation attempts.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Own and operate coal-fired generation assets; protected by the fenceline reading from mandates to retire or shift generation to renewables, but still required to install heat-rate improvements or carbon capture at individual facilities.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, payer).

% Develop and implement state implementation plans under Section 111(d); the facility-constraint reading preserves their traditional authority over resource planning and energy mix by prohibiting federal generation-shifting mandates.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_regulators, beneficiary,
    institutional, generational, constrained, national).

% Investor-owned and public utilities with significant coal generation portfolios; benefit from regulatory certainty that existing plants will not be stranded by beyond-the-fenceline compliance requirements.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_dependent_utilities, beneficiary,
    powerful, biographical, constrained, national).

% Federal agency charged with issuing Section 111(d) emission guidelines; its regulatory toolkit is judicially capped at fenceline measures, preventing it from pursuing generation-shifting or portfolio-based decarbonization strategies.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_administrator, payer,
    institutional, biographical, constrained, national).

% Environmental NGOs and climate-policy organizations seeking rapid power-sector decarbonization; the fenceline reading caps achievable emissions reductions and blocks their preferred regulatory tools of renewable substitution and coal retirement.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, national).

% Renewable energy developers and manufacturers who would expand market share under generation-shifting mandates; blocked from regulatory-driven coal displacement by the facility-constraint limitation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_sector, payer,
    organized, biographical, constrained, national).

% Residents living near coal plants who are exposed to ongoing local air pollution; the fenceline reading prevents regulatory strategies that would retire plants early, leaving these communities without exit from the exposure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, affected_communities, payer,
    powerless, generational, trapped, local).

% Legal academics analyzing statutory text, delegation doctrine, and the major questions rule; they observe the doctrinal contest and its federalism implications without bearing direct regulatory costs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, administrative_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, diffuse).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legally stable boundary for EPA's statutory authority under Section 111(d), preserving a cooperative federalism structure where states retain primary authority over resource planning and facility owners face predictable, technology-based compliance obligations confined to their existing assets.
% TRANSFER_FUNCTION: Transfers protection from federal generation-shifting mandates to incumbent coal-plant operators and state energy regulators; transfers the cost of foregone emissions reductions and continued local pollution to climate advocates, renewable competitors, and fence-line communities.
% ABSENT_VOICES: Future generations bearing climate damages; international climate partners relying on US grid decarbonization commitments; utility ratepayers who might prefer cheaper renewable power but are structurally excluded from state implementation plan processes.
% DISAPPEARANCE_RATIONALE: If the facility-constraint reading vanished, EPA could immediately propose generation-shifting and coal-retirement rules under Section 111(d); state energy markets would reorganize around federal grid-decarbonization mandates; the coal sector's capital planning and legal protections would collapse, and climate advocates would gain a major regulatory pathway.
% FOUNDING_PROBLEM: The original problem was to create a flexible, cooperative federalism mechanism for reducing emissions from existing sources not covered by NAAQS or hazardous air pollutant standards, using feasible technological improvements at the source level while respecting state implementation roles.
% FOUNDING_PROBLEM_CORROBORATION: Congressional drafters of the 1970 and 1990 CAA amendments left the phrase 'best system' ambiguous; state regulators and the coal industry attest the provision was never meant to authorize grid restructuring. Independent legal historians and textualists outside the benefiting parties note the ambiguity was deliberate, and the systemic-transformation reading has as much textual support as the facility-constraint reading.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial but not total extraction of regulatory decarbonization potential: the fenceline reading prevents the most cost-effective emissions reductions (generation shifting) and locks in continued coal operation. Suppression (0.70) is high because the constraint's persistence depends on active judicial enforcement striking down systemic rules; without the major questions doctrine and textualist interpretation, EPA would adopt generation-shifting. Accessibility collapse (0.78) is high because once the fenceline reading is established in Supreme Court precedent (West Virginia v. EPA), legal alternatives collapseâany EPA attempt at systemic transformation faces immediate judicial invalidation. Resistance (0.72) is high from climate advocates, the Biden EPA, and renewable interests. Theater ratio (0.35) captures the performative dimension: the legal formalism of 'source-based' textualism provides a veneer of neutral interpretation over a structurally protective outcome for fossil capital, though the legal reasoning is not wholly empty. The measurement series tracks the reading's consolidation from the ACE rule through West Virginia v. EPA, showing rising extractiveness and suppression as judicial enforcement hardened.
 *
 * PERSPECTIVAL GAP:
 *   The coal operator and state regulator seats experience the constraint as protective coordinationâlegal certainty, preserved autonomy, and predictable compliance costs. The climate advocate and affected community seats experience the same constraint as extractionâa judicially enforced ceiling that prevents meaningful emissions reductions and locks in pollution. The EPA seat experiences it as a structural incapacitation: the agency's statutory mission is partially disabled by the interpretation. The engine computes this divergence from the same structural data; no seat's perspective is authored as the true one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (coal_plant_operators, state_regulators, fossil_fuel_dependent_utilities) receive low directionality: the constraint subsidizes their economic and political position by capping regulatory demands. Victims (climate_advocates, renewable_energy_sector, affected_communities) receive high directionality: the constraint extracts from them by foreclosing cheaper and faster decarbonization pathways and by continuing local pollution exposure. EPA sits at moderate-high directionality as an institutional actor whose authority is directly curtailed. The federal judiciary sits near the agenda-setter pole with low extraction. The scholars are analytical with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a Scaffold because it carries no sunset clause and is designed as a permanent interpretive limit. It is not a Snare because there is a genuine coordination function: it solves a real federalism problem by preventing EPA from using a source-specific statutory provision to restructure the national grid, and it provides regulatory certainty to long-lived capital investments. It is not a Piton because it is not atrophied or theatricalâpowerful beneficiaries (coal operators, states) actively defend and benefit from it, and the legal doctrine is functionally central to current administrative practice. The Tangled Rope classification captures both the genuine federalism/coordination value and the asymmetric extraction of public health and climate benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    facility_reading_authenticity,
    'Does the facility-constraint reading reflect the enacted statutory meaning of Section 111(d), or is it a judicial construction that retroactively narrows delegation to protect incumbent fossil-fuel interests?',
    'Historical legislative record analysis, textual analysis of ''best system'' across the 1970 and 1990 CAA amendments, and comparison with other CAA sections authorizing beyond-the-fenceline measures.',
    'If authentic textual meaning, the constraint leans toward a Mountain-like legal fixedness; if constructed, it is a Tangled Rope or Snare benefiting identifiable incumbents and demands scrutiny of judicial capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facility_reading_authenticity, conceptual, 'Whether the fenceline limit is discovered statutory meaning or constructed protection.').

omega_variable(
    federalism_benefit_distribution,
    'Does the state autonomy preserved by this reading accrue to state governments as genuine sovereign flexibility, or to fossil-fuel incumbents who capture state implementation plans?',
    'Comparative analysis of state SIPs under ACE versus hypothetical systemic transformation scenarios; measurement of coal-sector lobbying influence in state energy regulation.',
    'If states genuinely exercise independent flexibility, the coordination function is stronger; if state autonomy is a pass-through for incumbent capture, extraction dominates and the reading functions more like a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_benefit_distribution, empirical, 'Whether preserved state autonomy is genuine federalism or regulatory capture conduit.').

omega_variable(
    major_questions_selectivity,
    'Does the major questions doctrine deployed in this reading impose a generalizable procedural safeguard, or does it selectively extract regulatory capacity from environmental and public-health domains?',
    'Cross-domain comparison of major-questions invocation rates and outcomes across regulatory areas (financial, environmental, labor, health).',
    'If selectively deployed against environmental rules, the reading''s authority grounding is extraction rather than neutral legal principle; if generalizable, it reflects a conventional interpretive norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_selectivity, empirical, 'Whether major questions doctrine is neutral procedural safeguard or selective extraction tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111dfc_tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(caa111dfc_tr_t2, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement(caa111dfc_tr_t4, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(caa111dfc_tr_t6, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(caa111dfc_tr_t8, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(caa111dfc_tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(caa111dfc_be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(caa111dfc_be_t2, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(caa111dfc_be_t4, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(caa111dfc_be_t6, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(caa111dfc_be_t8, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(caa111dfc_be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(caa111dfc_su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(caa111dfc_su_t2, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(caa111dfc_su_t4, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(caa111dfc_su_t6, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(caa111dfc_su_t8, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(caa111dfc_su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Section 111(d) delegation' conflates two structurally distinct constraints. The facility-constraint reading (this file) has low coordination overhead for grid restructuring but high extraction from climate ambition. The systemic-transformation reading (sibling file) has high coordination overhead and extracts from incumbent fossil capital. They share the same statutory text but have different epsilon values, different beneficiary/victim structures, and different failure modes. Decomposition per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
