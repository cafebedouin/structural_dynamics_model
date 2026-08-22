% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Section 111(d) 'Best System of Emission Reduction' Limited to Facility-Level Measures (West Virginia v. EPA Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Following West Virginia v. EPA, EPA's authority under Section 111(d) to
 *   regulate carbon dioxide emissions from existing power plants is read as
 *   bounded to technology and operational measures deployable at each
 *   individual facility. This forecloses standards whose stringency depends
 *   on shifting generation across the grid — the approach EPA had taken in
 *   its 2015 Clean Power Plan, which set state-level targets achievable
 *   partly through renewable buildout and coal-plant retirement. Under the
 *   facility-constraint reading, coal plants can be required to become
 *   somewhat more efficient or add carbon capture, but cannot be regulated
 *   out of existence through a standard that assumes their replacement by
 *   cleaner generation.
 *
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
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Section 111(d) 'Best System of Emission Reduction' Limited to Facility-Level Measures (West Virginia v. EPA Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'bcfc418a-bece-4830-91f5-d016cc7670be').
narrative_ontology:cs_kernel_codification('bcfc418a-bece-4830-91f5-d016cc7670be', fixed_text).
narrative_ontology:cs_authority_grounding('bcfc418a-bece-4830-91f5-d016cc7670be', lineage).
narrative_ontology:cs_interpretation_layer_present('bcfc418a-bece-4830-91f5-d016cc7670be').
narrative_ontology:cs_reading_relation('bcfc418a-bece-4830-91f5-d016cc7670be', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('bcfc418a-bece-4830-91f5-d016cc7670be', foundational, best_system_bounded_to_individual_source_application).
narrative_ontology:cs_axiom_status(best_system_bounded_to_individual_source_application, holdable).
narrative_ontology:cs_axiom_grounding('bcfc418a-bece-4830-91f5-d016cc7670be', best_system_bounded_to_individual_source_application, conventional).
narrative_ontology:cs_axiom('bcfc418a-bece-4830-91f5-d016cc7670be', foundational, clear_statement_required_for_generation_shifting_authority).
narrative_ontology:cs_axiom_status(clear_statement_required_for_generation_shifting_authority, holdable).
narrative_ontology:cs_axiom_grounding('bcfc418a-bece-4830-91f5-d016cc7670be', clear_statement_required_for_generation_shifting_authority, conventional).
narrative_ontology:cs_reference_frame('bcfc418a-bece-4830-91f5-d016cc7670be', textualist_facility_specific_delegation).
narrative_ontology:cs_drift_state('bcfc418a-bece-4830-91f5-d016cc7670be', post_west_virginia_v_epa, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bcfc418a-bece-4830-91f5-d016cc7670be', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_mining_labor_regions).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, downwind_frontline_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_climate_stabilization_interests).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, cooperative_federalism_in_energy_policy).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, nondelegation_adjacent_statutory_specificity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coal-fired power plant owners who would otherwise face compliance costs steep enough to force early retirement under a generation-shifting standard. Under the facility-constraint reading, their required 'best system' is limited to on-site heat-rate improvements and carbon capture retrofits — measures that keep the plants operating rather than requiring substitution with renewables or shifting generation off their fleet. They can lobby state regulators, litigate, and depreciate assets on their own schedule.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators, beneficiary,
    organized, biographical, arbitrage, national).

% States whose tax base, employment, and grid mix depend on continued coal generation. The facility-constraint reading preserves their authority to set state implementation plans without EPA dictating the state's generation portfolio. They can set compliance timelines, seek variances, and shape state energy policy largely free of federal generation-mix mandates.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, agenda_setter).

% The federal agency charged with setting the 'best system of emission reduction' under Section 111(d). Under this reading, EPA's authority is confined to identifying measures achievable at and applied to individual, existing facilities — it cannot set a standard whose achievement requires shifting generation to lower-emitting sources elsewhere in the grid. EPA can still write facility-level rules but cannot use the statute to reshape the national generation mix.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Populations living near coal plants and along pollution transport corridors who bear the health and environmental burden of continued coal operation. A facility-constraint ceiling means the plants nearest them keep running longer than a systemic approach would allow, since heat-rate improvements and unproven or costly carbon capture achieve far smaller reductions than generation-shifting would. They have no direct standing to compel a different 'best system' determination and cannot relocate the pollution source.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, downwind_frontline_communities, payer,
    powerless, biographical, trapped, regional).

% The diffuse, non-present interest in a stable climate trajectory, represented imperfectly by advocacy groups and future generations. Bounding 'best system' to marginal facility-level gains forecloses the largest available near-term decarbonization lever from this statutory pathway, pushing more of the burden onto future mitigation and adaptation costs. This entity cannot appear in court or lobby directly; it is carried only through proxies.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_climate_stabilization_interests, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(caa_section_111d_delegation__facility_constraint_reading, future_climate_stabilization_interests).

% Wind, solar, and storage developers who would benefit from a regulatory push toward generation-shifting under the systemic reading. Under the facility-constraint reading, the statute does not manufacture demand for their product by forcing coal retirements, so they compete only on market economics and separate incentive programs, slowing their addressable market growth relative to the alternative reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% The judicial body that in West Virginia v. EPA (2022) articulated the major questions doctrine basis for this reading, holding that Congress did not clearly authorize EPA to restructure the national electricity generation mix through Section 111(d). It adjudicates the boundary between facility-level and systemic authority but does not itself administer the statute day to day.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, supreme_court, agenda_setter).

% The body whose clear-statement authorization would resolve the ambiguity definitively. Under the facility-constraint reading, Congress has not spoken with the requisite clarity to authorize generation-shifting, and legislative gridlock means it is unlikely to clarify the statute in either direction soon — its silence is treated as a constraint on EPA rather than prompting new legislation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, congress, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confining 'best system' to facility-level, adequately demonstrated technological measures gives regulated utilities and states a predictable, judicially administrable compliance target and preserves state primacy over their own generation mix and grid reliability planning — a genuine coordination benefit for utilities needing capital-planning certainty and for states managing their own energy transitions on their own timelines.
% TRANSFER_FUNCTION: Moves the burden of unaddressed emissions from coal-fired generation away from incumbent generators and coal-dependent state budgets and onto downwind communities' health outcomes and the future costs of climate stabilization, by ruling out of the statutory 'best system' analysis the single largest available reduction lever (generation-shifting to lower- or zero-emission sources).
% ABSENT_VOICES: Downwind frontline communities and future generations bearing the deferred climate costs have no seat in the doctrinal argument, which was litigated primarily between the agency, coal-state governments, and industry petitioners on questions of statutory text and separation of powers; the health and intergenerational equity dimensions entered only as amicus argument, not as parties whose consent was sought.
% DISAPPEARANCE_RATIONALE: If this reading's ceiling disappeared and the systemic-transformation reading controlled instead, EPA could set standards contemplating generation-shifting, materially accelerating coal retirements, restructuring state implementation plans around portfolio-wide targets, and redirecting capital toward renewable buildout — a substantially different national electricity sector trajectory than the facility-constrained pathway permits.
% FOUNDING_PROBLEM: Section 111(d) was enacted to give EPA a mechanism to regulate emissions from existing stationary sources not covered by the primary new-source provisions, using state-implemented, facility-focused standards reflecting the 'best system of emission reduction' adequately demonstrated for that class of source.
% FOUNDING_PROBLEM_CORROBORATION: Coal-state attorneys general and utility petitioners attest the founding problem was always facility-level technology forcing, not economy-wide restructuring, and that the facility-constraint reading restores the statute's original design. Environmental law scholars, EPA's own Clean Power Plan technical record, and several dissenting justices attest — from outside the beneficiary set — that the 'best system' language was drafted broadly and that facility-level technology alone (particularly carbon capture at the time of enactment) was never adequately demonstrated at the scale needed, making the narrower reading a judicially imposed ceiling rather than a recovery of original statutory meaning.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high and rising (0.35 to 0.58) because the reading's practical effect compounds over time: as climate impacts accumulate and the achievable reduction under available facility-level technology remains marginal, the gap between what the statute could have achieved under the systemic reading and what it achieves under this reading widens, shifting increasing realized cost onto downwind communities and the climate trajectory. Suppression here operates through legal foreclosure rather than direct coercion — EPA affirmatively cannot promulgate the systemic alternative, which is a form of structural suppression of the coordination option itself, hence the moderate and rising suppression trajectory. Theater ratio rises because a growing share of EPA's rulemaking activity under this reading is now devoted to demonstrating facility-level technological feasibility (e.g., carbon capture cost and availability findings) that function partly to justify a narrower rule within the doctrinal ceiling rather than to achieve the deepest available reductions.
 *
 * PERSPECTIVAL GAP:
 *   From the coal-generator and coal-state seat, this reading is genuine coordination: it restores predictable, judicially bounded compliance obligations and protects legitimate state authority over energy mix against an agency overreach. From the downwind-community and climate-interest seat, the identical structure operates as extraction: a regulatory ceiling imposed by judicial doctrine that forecloses the policy tool most capable of addressing the harm they bear, with no compensating benefit flowing to them. The engine computing divergent per-seat classifications from these structural facts is expected and is the point — the claimed type (tangled_rope) already reflects that both a real coordination function and a real asymmetric cost exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent coal generators and coal-dependent states are structural beneficiaries: the ceiling removes the single most severe compliance pathway (forced retirement or replacement) and preserves capital and political flexibility, so their derived directionality sits near the beneficiary end. Downwind communities and future climate interests are targets: they bear the marginal difference between the two readings' achievable reductions as concentrated health harm and deferred mitigation cost, with no exit — communities cannot relocate a coal plant's pollution plume, and climate stabilization interests cannot be represented in the transaction at all. Renewable developers are moderate targets: their addressable market shrinks relative to the counterfactual, but they retain other market and policy levers (tax credits, state RPS programs), giving them constrained rather than trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating existing-source emissions where no new-source standard applies — remains live in the sense that existing coal plants still emit and are still uncontrolled by 111(b) new-source rules. What is contested is whether the facility-constraint reading is a recovery of the statute's original, narrower design or a judicially imposed narrowing that has drifted from a broader original delegation. Classifying this as tangled_rope rather than snare or mountain avoids two mislabeling errors: treating it as pure extraction would ignore the genuine coordination value of a stable, judicially administrable standard for regulated utilities and states; treating it as a natural-law mountain (the 'plain meaning' framing favored by its beneficiaries) would ignore that the reading was itself a contested judicial choice with identifiable winners and losers, not an inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_scope,
    'Is the major questions doctrine, as applied here, a principled constraint on agency overreach into questions of vast economic and political significance, or a judicially constructed device that happens to selectively immunize incumbent fossil-fuel interests from the most effective regulatory tool available under the statute?',
    'Track whether the doctrine is applied with comparable rigor and outcome-symmetry across analogous delegations that burden different interest groups (e.g., FDA, FCC, OSHA major-questions cases) versus consistently narrowing environmental delegations specifically.',
    'If the doctrine is applied asymmetrically to favor incumbent extraction industries across domains, this reading is better understood as a vehicle for extraction dressed in neutral interpretive principle; if applied symmetrically, it supports the reading''s claim to be a genuine, non-partisan check on delegation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Whether the major questions doctrine''s application here is principled or outcome-selective.').

omega_variable(
    facility_constraint_committer_choice,
    'This story is one of two readings of the caa_section_111d_delegation kernel — the facility_constraint_reading (this file) versus the systemic_transformation_reading (sibling, not authored here). Given identical statutory text, is the choice between them resolved by legal-interpretive method (textualism vs. purposivism), by institutional preference (judicial restraint vs. agency deference), or is it functionally a policy choice about acceptable pace of decarbonization dressed in doctrinal language?',
    'Compare judicial reasoning across similarly structured environmental delegations decided by the same interpretive coalition; if outcome consistently tracks industry burden rather than textual structure, the doctrinal framing is doing less work than the stated rationale claims.',
    'If the kernel choice is substantially policy-driven, both readings should be understood as contested policy instruments rather than one being the ''correct'' legal reading and the other an overreach — which reframes the entire tangled_rope classification of this reading as a live contest rather than a settled interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(facility_constraint_committer_choice, conceptual, 'Whether the choice between facility-constraint and systemic readings is legal-interpretive or policy-driven.').

omega_variable(
    carbon_capture_technological_adequacy,
    'Is carbon capture and storage ''adequately demonstrated'' at the scale and cost the facility-constraint reading requires it to be, such that the reading''s compliance pathway is real rather than largely theatrical?',
    'Track deployment rates, cost curves, and capture-rate performance of commercial-scale CCS retrofits on existing coal units over the next decade against EPA''s technical feasibility findings.',
    'If CCS remains commercially unviable at the scale needed, the facility-constraint reading''s compliance pathway is largely theater ratio — a ceiling that permits continued operation without a genuine emissions-reduction mechanism, pushing the classification closer to snare; if CCS matures, the coordination function is more genuinely served.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_capture_technological_adequacy, empirical, 'Whether carbon capture is a real compliance pathway or a theatrical one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(caa__tr_t4, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(caa__tr_t8, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(caa__tr_t12, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(caa__tr_t16, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(caa__be_t4, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(caa__be_t8, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(caa__be_t12, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(caa__be_t16, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(caa__su_t4, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(caa__su_t8, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(caa__su_t12, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(caa__su_t16, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint and caa_section_111d_delegation__systemic_transformation_reading are two readings of the same kernel (caa_section_111d_delegation): the same statutory text, the same 2022 West Virginia v. EPA litigation, evaluated under two structurally distinct interpretations of 'best system of emission reduction.' This reading (facility_constraint_reading) authors ε against the current, judicially controlling arrangement — the narrower ceiling — as experienced by its own stakeholders. The sibling story authors ε against the broader Clean Power Plan-style arrangement the systemic reading would authorize, with a different beneficiary/victim structure (renewable developers and downwind communities as beneficiaries; coal generators and coal-state budgets as victims of forced transition). Do not average or reconcile the two ε values — they describe different arrangements, and the systemic reading is not currently controlling law, which is itself part of why this reading's beneficiaries currently prevail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
