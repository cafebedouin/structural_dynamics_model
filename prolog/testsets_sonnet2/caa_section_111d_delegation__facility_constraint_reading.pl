% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Section 111(d) 'Best System' Confined to Facility-Level Measures (Major Questions Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint captures the facility-constraint reading of Section
 *   111(d)'s 'best system of emission reduction' language, as adopted by the
 *   Supreme Court's major questions doctrine ruling. Under this reading,
 *   EPA's authority to regulate carbon emissions from existing power plants
 *   is limited to measures implementable at an individual facility —
 *   efficiency upgrades, carbon capture and sequestration, co-firing — and
 *   cannot extend to grid-wide generation-shifting strategies (dispatching
 *   away from coal toward gas or renewables). This reading was actively
 *   sought through litigation by coal generators, coal-dependent states, and
 *   the fossil fuel extraction sector, and it converts what those parties
 *   experienced as an existential regulatory threat into a bounded compliance
 *   obligation. Climate advocates, downwind communities, renewable
 *   developers, and future generations bear the cost of a slower
 *   decarbonization pathway as a direct structural consequence. This is a
 *   single reading of a contested kernel; the sibling
 *   systemic_transformation_reading is authored as a separate constraint with
 *   its own ε, its own beneficiary/victim structure, and its own
 *   classification — the two are not the same constraint measured
 *   differently, they are different constraints sharing an unresolved
 *   statutory text.
 *
 * KEY AGENTS:
 *   - incumbent_coal_generators: Primary beneficiary (organized/mobile) — insulated from forced retirement
 *   - coal_dependent_state_governments: Beneficiary and agenda-setter (institutional/arbitrage) — retains energy-mix autonomy
 *   - environmental_protection_agency: Agenda-setter operating under a narrowed mandate (institutional/constrained)
 *   - climate_policy_advocates: Primary target (organized/trapped) — loses the largest-magnitude reduction pathway
 *   - downwind_pollution_affected_communities: Diffuse victim (powerless/trapped) — bears continued co-pollutant exposure
 *   - supreme_court: Analytical enforcer (institutional/analytical) — the ruling that forecloses the sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.52).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Section 111(d) 'Best System' Confined to Facility-Level Measures (Major Questions Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'eca60043-75f5-442c-a16a-2ff833d7a7bc').
narrative_ontology:cs_kernel_codification('eca60043-75f5-442c-a16a-2ff833d7a7bc', fixed_text).
narrative_ontology:cs_authority_grounding('eca60043-75f5-442c-a16a-2ff833d7a7bc', lineage).
narrative_ontology:cs_interpretation_layer_present('eca60043-75f5-442c-a16a-2ff833d7a7bc').
narrative_ontology:cs_reading_relation('eca60043-75f5-442c-a16a-2ff833d7a7bc', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('eca60043-75f5-442c-a16a-2ff833d7a7bc', foundational, agency_delegation_requires_clear_statement_for_major_economic_action).
narrative_ontology:cs_axiom_status(agency_delegation_requires_clear_statement_for_major_economic_action, holdable).
narrative_ontology:cs_axiom_grounding('eca60043-75f5-442c-a16a-2ff833d7a7bc', agency_delegation_requires_clear_statement_for_major_economic_action, conventional).
narrative_ontology:cs_axiom('eca60043-75f5-442c-a16a-2ff833d7a7bc', secondary, best_system_confined_to_source_specific_technology).
narrative_ontology:cs_axiom_status(best_system_confined_to_source_specific_technology, holdable).
narrative_ontology:cs_axiom_grounding('eca60043-75f5-442c-a16a-2ff833d7a7bc', best_system_confined_to_source_specific_technology, conventional).
narrative_ontology:cs_reference_frame('eca60043-75f5-442c-a16a-2ff833d7a7bc', pre_clean_power_plan_facility_specific_practice).
narrative_ontology:cs_drift_state('eca60043-75f5-442c-a16a-2ff833d7a7bc', post_west_virginia_v_epa_ruling, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('eca60043-75f5-442c-a16a-2ff833d7a7bc', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_extraction_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_policy_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, downwind_pollution_affected_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_generations_bearing_climate_costs).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, federalism_energy_mix_deference).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, narrow_agency_delegation_construction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own coal-fired plants whose economic life depends on avoiding forced retirement or output curtailment. Under this reading, EPA can only demand heat-rate efficiency upgrades achievable on-site, not fleet-wide substitution toward gas or renewables. This converts what would otherwise be an existential regulatory threat into a manageable capital expenditure, and the litigation strategy that produced this reading was substantially funded and pursued by this group.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators, beneficiary,
    organized, biographical, mobile, national).

% Retain full authority to set their own generation mix, tax base, and employment policy around coal without EPA compelling a shift toward renewables or gas. They litigated for and administer implementation of this reading through state implementation plans, and benefit from insulation of a politically important industry from federal displacement mandates.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, agenda_setter).

% Coal mining operations and associated supply chains depend on continued utility demand for coal. A facility-constrained reading of 111(d) preserves that demand by preventing EPA from engineering a demand-side phase-out through the power sector, extending the runway for extraction-dependent revenue and employment.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_extraction_sector, beneficiary,
    organized, generational, mobile, national).

% Administers Section 111(d) rulemaking but under this reading is confined to identifying the 'best system of emission reduction' as technology installable at or applicable to a single existing facility — heat-rate improvements, carbon capture and sequestration, co-firing — rather than grid-level dispatch changes. Its regulatory toolkit for the power sector's largest source category is substantially narrowed relative to what agency technical staff assessed as achievable and cost-effective at scale.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency, agenda_setter,
    institutional, generational, constrained, national).

% Sought a Clean Power Plan-style rule using generation-shifting (dispatching from coal to gas to renewables) as the most cost-effective and largest-magnitude emissions reduction pathway available under the statute. This reading forecloses that pathway entirely, forcing reliance on marginal facility-level upgrades that produce a fraction of the emissions reduction, with no comparable statutory hook elsewhere to recapture the lost reduction at the same speed.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_policy_advocates, payer,
    organized, civilizational, trapped, global).

% Live near or downwind of coal facilities that continue operating longer than they would under a generation-shifting regime, bearing particulate and criteria-pollutant co-benefits foregone as well as continued carbon emissions. They have no seat in the litigation or rulemaking process and cannot relocate the facility or shift its output on their own.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, downwind_pollution_affected_communities, payer,
    powerless, biographical, trapped, regional).

% Bear the accumulated atmospheric cost of a slower decarbonization trajectory in the power sector, the single largest stationary source category, without any voice in the interpretive dispute that determines the pace of reduction available under existing statutory authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations_bearing_climate_costs, payer,
    powerless, civilizational, trapped, global).

% Would have captured demand created by EPA-mandated generation-shifting away from coal. Under the facility-constrained reading, that demand-pull mechanism does not exist at the federal regulatory level, leaving market entry dependent on state policy, tax incentives, and cost competition alone rather than a federal emissions-reduction backstop.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% Articulated and enforces this reading through the major questions doctrine, holding that a decision of such economic and political significance (restructuring the nation's electricity generation mix) requires clear congressional authorization that Section 111(d)'s general 'best system of emission reduction' language does not supply. Its ruling is the enforcement mechanism that forecloses the systemic transformation reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, supreme_court, agenda_setter).

% Could resolve the interpretive dispute definitively by amending the Clean Air Act to explicitly authorize or foreclose generation-shifting measures, but has not done so amid partisan gridlock, leaving the question to agency rulemaking and judicial interpretation instead of legislative clarity.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, congress, excluded,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, diffuse).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confines EPA's facility-level emissions authority to measures a single plant operator can implement on-site, coordinating expectations across utilities, states, and regulators about the outer bound of federal power-sector rulemaking without requiring new legislation for every technological category.
% TRANSFER_FUNCTION: Moves regulatory risk and emissions-reduction burden away from incumbent coal generators, coal-producing states, and the extraction sector, and onto downwind communities, future generations, and renewable developers who lose the demand-shifting mechanism that would have accelerated displacement of coal generation.
% ABSENT_VOICES: Congress, which could settle the statutory question through amendment, has not acted. Downwind communities and future generations bear the practical consequences of the interpretive choice but have no formal party status in the litigation that produced it; their interests are represented only derivatively through amicus participation by advocacy organizations.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by the systemic transformation reading, EPA would regain authority to design generation-shifting rules, coal retirement timelines would likely accelerate under federal pressure, state implementation plans would need to account for grid-wide dispatch changes, and the coal and extraction sectors would lose the insulation this reading currently provides — a substantial rearrangement of power-sector investment and closure decisions.
% FOUNDING_PROBLEM: The interpretive dispute was built to resolve genuine textual ambiguity in Section 111(d)'s instruction that EPA identify the 'best system of emission reduction... adequately demonstrated' for existing sources — a phrase written in 1970 and 1977 without contemplation of grid-wide dispatch optimization as an emissions strategy, leaving open whether 'system' means a technology applied at a facility or a strategy applied across a fleet.
% FOUNDING_PROBLEM_CORROBORATION: Industry petitioners and the coal-state coalition attest that the facility-constrained reading is the only textually faithful one and that the ambiguity is resolved. Independent administrative law scholars outside both the industry and environmental advocacy coalitions have noted the statutory text does not unambiguously resolve the question either way, and that the major questions doctrine's application here reflects a judicially-selected background principle rather than a textually compelled outcome — corroboration that the underlying ambiguity remains live rather than settled by the text itself.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 — substantial but not maximal — because the facility-constrained reading does not eliminate emissions regulation entirely; it narrows the toolkit to a subset of measures with materially smaller reduction potential, transferring the difference in ambition onto parties who cannot capture it through any other statutory hook. Suppression sits at 0.52, reflecting that the constraint operates through judicial doctrine (the major questions doctrine) rather than through direct coercive enforcement against advocates — the suppression is structural (a closed interpretive door) rather than punitive. Theater ratio (0.4) reflects that facility-level measures like partial carbon capture retrofits carry real but limited functional value, alongside a growing share of compliance activity that exists primarily to demonstrate technical feasibility for future litigation rather than to achieve deep decarbonization. Accessibility collapse (0.62) and resistance (0.72) reflect that this reading, while operative, remains actively contested — advocates continue to litigate around its edges and push state-level alternatives, so alternatives have not fully collapsed the way they would under a settled natural-law-type constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status flows directly from who sought and administers this reading: incumbent coal generators and coal-dependent states litigated for it and now operate freely within its boundary, giving them low d. Climate advocates, downwind communities, and future generations bear the foreclosed-pathway cost with no comparable statutory recourse, giving them high d and trapped exit options — the interpretive door is closed by judicial doctrine, not by any choice available to them. Renewable developers sit at moderate-high d: they lose a federal demand-pull mechanism but retain state-level and market-based exit options, hence 'constrained' rather than 'trapped.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine ambiguity in 1970s statutory text about what 'system' means for a category of emissions control not contemplated at drafting — is genuinely contested rather than resolved, which is why founding_problem_status is authored as 'contested' rather than 'dead.' This prevents mislabeling the constraint as either pure coordination (settled textual clarity) or pure extraction (a purely engineered outcome with no textual basis): the ambiguity is real, but the major questions doctrine's application to resolve it in the facility-constrained direction was an active choice among available interpretive methods, one whose beneficiaries substantially funded and pursued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determinacy_of_best_system,
    'Does the phrase ''best system of emission reduction... adequately demonstrated'' in Section 111(d) unambiguously mean facility-level technology, or is the facility/systemic distinction itself a judicially-imported gloss not compelled by the statutory text?',
    'Close textual and legislative-history analysis of the 1970/1977/1990 Clean Air Act amendments, cross-referenced against contemporaneous EPA practice under parallel provisions (e.g., Section 111(b) new-source standards) that were not read as facility-limited in the same period.',
    'If the facility-limited reading is textually compelled, this constraint is closer to a genuine interpretive mountain (an intrinsic limit of the statute). If the distinction is a doctrine-imported gloss, the constraint is better characterized as an actively engineered extraction structure dressed in textualist language — a tangled rope with real coordination value (regulatory predictability) riding on top of asymmetric benefit capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_determinacy_of_best_system, conceptual, 'Whether the facility/systemic distinction is textually compelled or judicially constructed.').

omega_variable(
    major_questions_doctrine_neutrality,
    'Is the major questions doctrine a neutral interpretive canon applied consistently across regulatory domains, or a doctrine selectively invoked to block specific categories of ambitious agency action (particularly climate and health regulation)?',
    'Comparative analysis of major questions doctrine invocation across regulatory domains and administrations, tracking correlation between doctrine application and the political valence of the underlying regulation.',
    'If selectively invoked, the doctrine functions as an extraction mechanism providing textualist cover for outcome-driven judicial intervention; if applied neutrally, it functions as a genuine constitutional-structure safeguard limiting agency overreach regardless of substantive domain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_questions_doctrine_neutrality, empirical, 'Whether major questions doctrine application correlates with political valence of the regulated activity.').

omega_variable(
    reading_reversibility_via_congress,
    'Could Congress resolve this kernel dispute definitively through legislation, and if so, why has it not, and does the absence of legislative action itself constitute a form of tacit ratification of the facility-constrained reading?',
    'Track legislative proposals to amend Section 111(d) explicitly and analyze voting patterns and committee action to determine whether inaction reflects genuine gridlock versus revealed preference for the status quo reading.',
    'If Congress could act but chooses not to, the persistence of this reading shifts from purely judicial imposition toward a form of legislatively-endorsed equilibrium, which would weaken the case that this is an extraction structure imposed against the political branches'' will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_reversibility_via_congress, empirical, 'Whether congressional inaction reflects gridlock or tacit endorsement of the facility-constrained reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(caa__tr_t4, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(caa__tr_t8, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(caa__tr_t12, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(caa__tr_t16, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(caa__be_t4, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(caa__be_t8, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(caa__be_t12, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(caa__be_t16, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(caa__su_t4, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(caa__su_t8, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(caa__su_t12, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(caa__su_t16, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.1).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine_general_application).

% DUAL FORMULATION NOTE:
% This story is one of two decomposed readings of the caa_section_111d_delegation kernel. The facility_constraint_reading (this story) authors ε=0.58 with coal generators/coal states/extraction sector as beneficiaries and climate advocates/downwind communities/future generations as victims. The sibling systemic_transformation_reading (separate constraint) authors a different ε and an inverted beneficiary/victim structure, since under that reading EPA regains generation-shifting authority and the parties who benefit and pay reverse. Both stories share the same statutory kernel (Section 111(d)'s 'best system' language) but are structurally distinct constraints per the ε-invariance principle — they are linked here via affects_constraints rather than merged into a single story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
