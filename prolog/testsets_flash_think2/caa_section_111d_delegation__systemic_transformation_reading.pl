% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA Section 111(d) 'Best System' Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'systemic transformation' reading of
 *   Section 111(d) of the Clean Air Act, which interprets the EPA's authority
 *   to regulate existing power plants as extending to grid-wide,
 *   generation-shifting strategies, including mandating renewable
 *   substitution and early coal retirement. This reading is highly contested,
 *   particularly by the fossil fuel industry and states reliant on it, who
 *   advocate for a narrower interpretation. The constraint is claimed as a
 *   Tangled Rope due to its genuine coordination function (decarbonization)
 *   coupled with significant asymmetric extraction from specific sectors.
 *
 * KEY AGENTS:
 *   - epa: Agenda setter (institutional/analytical) — mandates and enforces decarbonization pathways.
 *   - coal_power_sector: Primary payer (powerful/constrained) — bears costs of early retirement and transition.
 *   - fossil_fuel_reliant_states: Payer (institutional/constrained) — must implement costly energy transitions.
 *   - renewable_energy_sector: Primary beneficiary (powerful/arbitrage) — gains market share and investment.
 *   - fossil_fuel_workers: Payer (powerless/trapped) — faces job displacement with limited alternatives.
 *   - supreme_court: Observer (institutional/analytical) — arbitrates the legal scope of EPA's authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.8).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.75).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) 'Best System' Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'b4f63a0c-3f64-40ec-a564-92dbb2191fe0').
narrative_ontology:cs_kernel_codification('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', fixed_text).
narrative_ontology:cs_authority_grounding('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', lineage).
narrative_ontology:cs_interpretation_layer_present('b4f63a0c-3f64-40ec-a564-92dbb2191fe0').
narrative_ontology:cs_reading_relation('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', foundational, epa_has_broad_delegation_for_public_welfare).
narrative_ontology:cs_axiom_status(epa_has_broad_delegation_for_public_welfare, holdable).
narrative_ontology:cs_axiom_grounding('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', epa_has_broad_delegation_for_public_welfare, deontological).
narrative_ontology:cs_axiom('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', foundational, best_system_means_grid_wide_optimization).
narrative_ontology:cs_axiom_status(best_system_means_grid_wide_optimization, holdable).
narrative_ontology:cs_axiom_grounding('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', best_system_means_grid_wide_optimization, conventional).
narrative_ontology:cs_reference_frame('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', broad_delegation_for_environmental_protection).
narrative_ontology:cs_drift_state('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', west_virginia_v_epa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b4f63a0c-3f64-40ec-a564-92dbb2191fe0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, states_investing_in_renewables).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, public_health_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_power_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_reliant_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary federal environmental regulator, EPA interprets Section 111(d) to authorize broad, grid-wide strategies for carbon emission reduction, including mandating shifts from coal to renewables. They enforce these mandates through state implementation plans.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, generational, analytical, national).

% Faces significant economic costs and early retirement mandates under this interpretation. Their options are to comply, litigate, or attempt to influence legislative changes, but direct exit from the market is often economically unfeasible.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_power_sector, payer,
    powerful, biographical, constrained, national).

% States with economies heavily dependent on fossil fuels bear the burden of transitioning their energy infrastructure and workforce. They must develop and implement state plans that align with EPA's broad decarbonization goals, facing high compliance costs and political resistance.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_reliant_states, payer,
    institutional, biographical, constrained, national).

% Benefits significantly from policies that mandate generation-shifting towards renewables. This interpretation creates a strong market signal and regulatory push for their technologies, leading to increased demand and investment.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector, beneficiary,
    powerful, biographical, arbitrage, national).

% States already committed to or investing in renewable energy benefit from federal mandates that align with their existing policy goals, potentially receiving federal support or market advantages for their early adoption.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, states_investing_in_renewables, beneficiary,
    institutional, biographical, mobile, national).

% Advocate for stronger environmental regulations and benefit from policies that reduce air pollution and mitigate climate change, aligning with their mission to protect public health and welfare.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, public_health_advocates, beneficiary,
    organized, generational, analytical, national).

% Face job displacement and economic insecurity as coal plants retire early and the fossil fuel industry contracts. Their options for retraining and relocation are often limited, making them highly vulnerable to the systemic transformation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_workers, payer,
    powerless, immediate, trapped, local).

% Serves as the ultimate arbiter of constitutional and statutory interpretation, reviewing EPA's authority under Section 111(d). Its rulings can significantly alter the scope and enforceability of this reading, as demonstrated by West Virginia v. EPA.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a national strategy for reducing carbon emissions from existing power plants, leveraging state-level implementation to achieve grid-wide decarbonization and address the collective action problem of climate change.
% TRANSFER_FUNCTION: Transfers economic burden and operational control from the fossil fuel-based electricity generation sector to the renewable energy sector and states willing to adopt decarbonization pathways, facilitated by EPA mandates and compliance mechanisms.
% ABSENT_VOICES: Future generations, who are the primary beneficiaries of climate action, are not directly represented in current regulatory debates. Additionally, communities disproportionately affected by fossil fuel pollution, while often represented by advocates, may not have direct voices in the policy formation process.
% DISAPPEARANCE_RATIONALE: If this interpretation of Section 111(d) vanished, EPA's authority to mandate broad decarbonization strategies would cease. This would lead to a fragmented, slower, or non-existent national climate strategy for the power sector, with states reverting to less ambitious or no generation-shifting policies. The energy transition trajectory would be significantly altered, likely slowing down decarbonization efforts across the grid.
% FOUNDING_PROBLEM: The Clean Air Act was established to address air pollution, and Section 111(d) specifically aimed to regulate emissions from existing sources not covered by other sections. The problem was how to effectively control these emissions in a way that allowed for state flexibility and technological innovation while achieving significant environmental and public health benefits.
% FOUNDING_PROBLEM_CORROBORATION: Environmental scientists, international climate bodies, and a broad coalition of public health organizations corroborate the ongoing problem of carbon emissions from existing power plants and the need for effective regulation. The scientific consensus on climate change and its impacts underscores the continued relevance of this regulatory challenge.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.80) reflects the substantial economic costs imposed on the coal sector and fossil-fuel-reliant states, including asset stranding and job losses. Suppression (0.75) is high because states and industries have limited legal and economic exit options once EPA mandates are in place, requiring active enforcement to overcome resistance. The theater ratio is low (0.10) because the EPA's actions are intended to be genuinely effective in achieving decarbonization, not merely performative. Resistance is high (0.80) due to the significant economic and political stakes involved, leading to extensive litigation and political opposition.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and public health advocates perceive this interpretation as a necessary and effective coordination mechanism for addressing climate change, with justified costs for polluters. Conversely, the coal power sector and fossil-fuel-reliant states experience it as an extractive mandate that unfairly targets their industries and economies, with limited legitimate coordination benefits for them. The Supreme Court's role highlights the deep legal and conceptual divide in how this authority is perceived.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, renewable energy sector, and public health advocates are beneficiaries, as the constraint aligns with their missions and creates favorable market conditions or environmental outcomes. The coal power sector, fossil-fuel-reliant states, and fossil fuel workers are targets, bearing the direct economic and social costs of the mandated transition. The Supreme Court acts as an analytical observer, assessing the legality and constitutional bounds of the constraint without directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Snare, acknowledging its genuine coordination function in addressing climate change. However, it also highlights the significant asymmetric extraction, ensuring that the costs borne by the fossil fuel sector and its workers are not dismissed as mere 'coordination costs.' The contestation around the 'founding problem status' (live vs. solved) is central to whether the coordination function is still genuinely needed or if the constraint has drifted into primarily extractive territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_best_system_ambiguity,
    'What is the true statutory scope of ''best system of emission reduction'' under Section 111(d)? Does it authorize generation-shifting or only facility-level improvements?',
    'Definitive Supreme Court ruling on the ''major questions doctrine'' as applied to EPA''s authority under 111(d), or clear legislative amendment to the Clean Air Act.',
    'If limited to facility-level measures, the constraint''s extractiveness and suppression would significantly decrease for the coal sector, and its classification would shift towards a Rope or even Piton for the broader decarbonization goal. If affirmed, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_best_system_ambiguity, conceptual, 'Ambiguity regarding the statutory scope of EPA''s authority under Section 111(d).').

omega_variable(
    economic_impact_on_fossil_communities,
    'What is the full economic and social cost of early coal retirement and generation-shifting on fossil-fuel-reliant communities and workers, and are adequate transition mechanisms in place?',
    'Comprehensive, independent economic and sociological studies assessing job losses, tax base erosion, and the effectiveness of federal/state retraining and relocation programs.',
    'If costs are higher than currently estimated and transition support is inadequate, the constraint''s effective extraction for these communities is amplified, potentially pushing it closer to a Snare from their perspective. If transition mechanisms are effective, the extraction is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_fossil_communities, empirical, 'Assessment of the just transition for communities affected by decarbonization mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(caa__tr_t2017, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(caa__tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(caa__tr_t2023, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(caa__tr_t2025, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(caa__be_t2017, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement(caa__be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(caa__be_t2023, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2023, 0.8).
narrative_ontology:measurement(caa__be_t2025, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(caa__su_t2017, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(caa__su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.73).
narrative_ontology:measurement(caa__su_t2023, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2023, 0.75).
narrative_ontology:measurement(caa__su_t2025, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, coal_power_plant_operations).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_subsidies).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_energy_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
