% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: Section 111(d) 'Best System of Emission Reduction' — Grid-Wide Generation-Shifting Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This story authors the systemic-transformation reading of the Section
 *   111(d) 'best system of emission reduction' kernel: EPA's Clean Power
 *   Plan-era and successor interpretation that the statutory phrase
 *   authorizes grid-wide, generation-shifting measures — mandating a shift in
 *   the overall generation mix toward renewables and away from coal, rather
 *   than confining 'best system' to technology retrofits at individual
 *   facilities. Under this reading, EPA sets state emission budgets
 *   calibrated to fleet-wide substitution and accelerated coal retirement,
 *   coal becomes a structural extraction target of the compliance
 *   architecture, renewable developers receive a federally mandated demand
 *   floor, and coal-dependent states and communities face high exit costs
 *   because the compliance pathway forecloses the facility-only alternative
 *   they would otherwise pursue. The sibling reading —
 *   facility_constraint_reading — is a separate constraint story with its own
 *   ε and its own stakeholder set; it is not blended into this one.
 *
 * KEY AGENTS:
 *   - epa_air_office
 *   - coal_sector_workers
 *   - coal_dependent_state_governments
 *   - fossil_locked_utility_ratepayers
 *   - renewable_energy_developers
 *   - grid_decarbonization_advocates
 *   - downwind_states_public_health
 *   - state_utility_regulators
 *   - federal_judiciary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Section 111(d) 'Best System of Emission Reduction' — Grid-Wide Generation-Shifting Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'bcc2c88e-f0d6-42a5-93b3-da1290a9c45e').
narrative_ontology:cs_kernel_codification('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', fixed_text).
narrative_ontology:cs_authority_grounding('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', extraction).
narrative_ontology:cs_interpretation_layer_present('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e').
narrative_ontology:cs_reading_relation('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', foundational, system_of_emission_reduction_is_grid_scale).
narrative_ontology:cs_axiom_status(system_of_emission_reduction_is_grid_scale, holdable).
narrative_ontology:cs_axiom_grounding('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', system_of_emission_reduction_is_grid_scale, conventional).
narrative_ontology:cs_axiom('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', secondary, generation_shifting_is_adequately_demonstrated_system).
narrative_ontology:cs_axiom_status(generation_shifting_is_adequately_demonstrated_system, holdable).
narrative_ontology:cs_axiom_grounding('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', generation_shifting_is_adequately_demonstrated_system, instrumental).
narrative_ontology:cs_reference_frame('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', clean_power_plan_era_systemic_authority).
narrative_ontology:cs_drift_state('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', post_west_virginia_v_epa, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('bcc2c88e-f0d6-42a5-93b3-da1290a9c45e', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, grid_decarbonization_advocates).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, downwind_states_public_health).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_sector_workers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_utility_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_state_governments).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, epa_systemic_authority_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, generation_shifting_is_best_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the 'best system of emission reduction' guideline and sets state-level emission budgets calibrated to grid-wide shifts toward renewables and away from coal generation. Administers the compliance pathway states must follow and enforces it through emission-guideline approval and federal plan backstops.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_air_office, agenda_setter,
    institutional, generational, analytical, national).

% Employed at coal plants scheduled for early retirement under state compliance plans built around generation-shifting. Have no comparable regional labor market to move into on the same timeline; retraining and transition-fund promises are downstream of a rulemaking they do not participate in drafting.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_sector_workers, payer,
    powerless, biographical, trapped, regional).

% Must submit state plans achieving emission targets premised on generation mix changes across their entire grid, not merely at individual plants. Bear stranded-asset costs, ratepayer disputes, and loss of tax base tied to coal facilities; litigating the rule's scope is their primary recourse, and success there is uncertain.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_state_governments, payer,
    organized, generational, constrained, regional).

% Live in utility territories where compliance costs of accelerated generation-shifting are passed through in rates. Cannot easily relocate to a jurisdiction with a different generation mix or opt out of the utility's compliance-driven rate structure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_utility_ratepayers, payer,
    powerless, biographical, trapped, regional).

% Gain a federally mandated demand floor for wind, solar, and storage capacity as states build compliance plans around generation-shifting. Can site projects wherever the compliance-driven market opens, moving capital freely between states with the most favorable interconnection and incentive terms.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, generational, arbitrage, national).

% Environmental and public-health organizations that pushed for the systemic reading precisely because it reaches beyond per-facility retrofits to the generation mix itself. Their policy goal is realized directly by this reading's authorization of substitution and retirement strategies.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, grid_decarbonization_advocates, beneficiary,
    organized, civilizational, analytical, national).

% Populations in states receiving interstate air pollution from upwind coal generation. Benefit from grid-wide emission reductions achieved through generation-shifting without having any voice in the interstate rulemaking or the compliance-plan negotiations that produce the benefit.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, downwind_states_public_health, beneficiary,
    powerless, generational, trapped, regional).

% Must translate the federal emission-guideline mandate into rate cases, resource-planning dockets, and utility compliance filings. Have discretion over implementation mechanics but not over whether the systemic mandate applies; their authority is subordinate to the federal 'best system' determination.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, state_utility_regulators, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, state_utility_regulators, excluded).

% Adjudicates challenges to EPA's statutory authority to read 'best system of emission reduction' as reaching grid-wide generation shifts rather than only facility-level measures. Reviews agency reasoning under administrative law doctrines including major questions analysis.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national decarbonization trajectory by setting a uniform federal floor on generation-mix emission intensity, preventing a race-to-the-bottom where individual states retain high-emission generation because neighbors have not moved and avoiding a patchwork of inconsistent facility-only rules that would leave interstate pollution transfer unaddressed.
% TRANSFER_FUNCTION: Moves capital demand from incumbent coal generation and its associated employment and tax base toward renewable generation and storage capacity; moves compliance cost from the federal government (which sets no funding mechanism) onto state governments, ratepayers, and displaced workers; moves public-health benefit from upwind emission reductions to downwind populations.
% ABSENT_VOICES: Coal sector workers and coal-community local governments have no formal seat in EPA's guideline-setting process beyond public comment; the rule is negotiated primarily among EPA, state regulators, utilities, and national advocacy organizations. Displaced workers' transition costs are addressed, if at all, through separate and non-binding programs outside the 111(d) rulemaking itself.
% DISAPPEARANCE_RATIONALE: If the systemic-transformation reading were vacated and only the facility-constraint reading survived, state compliance plans would revert to heat-rate improvements and carbon capture retrofits at existing coal plants; the federally mandated demand floor for renewable substitution and early retirement would disappear, materially slowing utility-scale renewable buildout timed to compliance deadlines and removing the primary near-term legal lever for closing coal plants on an accelerated schedule.
% FOUNDING_PROBLEM: Stationary-source greenhouse gas emissions from the power sector were not being reduced at a pace consistent with national climate commitments through voluntary state action or facility-level technology mandates alone; Congress had authorized EPA to determine the 'best system of emission reduction' for existing sources but left the phrase's scope — facility only, or grid-wide — undefined.
% FOUNDING_PROBLEM_CORROBORATION: EPA and grid-decarbonization advocates attest the founding problem (insufficient sector-wide decarbonization pace) remains fully live and requires exactly this systemic reading to address. Coal-dependent state governments and industry litigants attest, corroborated by dissenting circuit and Supreme Court opinions, that Congress's 1970/1990 delegation was never understood by legislators or regulated parties to authorize grid-wide generation-shifting, and that the 'problem' as EPA now defines it did not exist in the form the statute was written to solve — this corroboration comes from outside the coalition that benefits from the systemic reading.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62) reflects that under this reading the compliance architecture concentrates real economic cost on coal workers, coal-dependent state fiscal bases, and ratepayers in fossil-locked utility territories, while renewable developers and advocacy organizations capture a structural benefit without bearing a comparable cost — a genuine asymmetric transfer riding on a real coordination function (reducing sector-wide emissions and interstate pollution transfer). Suppression (0.58) is moderate-high because litigation over the rule's scope is the primary avenue of resistance and the rule persists through active EPA enforcement of state-plan approval and federal-plan backstops, not through unanimous buy-in; it has risen over the interval as the guideline framework matured into concrete state compliance obligations. Theater ratio stays low (0.2) because the coordination function — sector-wide emission reduction — is real and substantially achieved by the mechanism, not merely performed. Accessibility_collapse (0.4) is moderate: facility-only alternatives remain legally live (they are the sibling reading, actively litigated) so alternatives have not collapsed completely. Resistance (0.75) is high because coal states, utilities, and industry groups have mounted sustained litigation, including to the Supreme Court, specifically contesting this reading's scope.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA and state regulators sit as agenda-setters administering the mandate — analytical exit, institutional power. Coal sector workers and fossil-locked ratepayers are near-full targets: trapped exit, powerless standing, direct cost exposure with no seat in the guideline design. Coal-dependent state governments are organized payers with constrained (not trapped) exit — they can litigate and negotiate compliance timelines, which differentiates their directionality from individual workers despite shared victim status. Renewable developers sit near the full-beneficiary end: arbitrage-grade exit lets them move capital to wherever compliance-driven demand is strongest, and the reading's authorization of generation-shifting is the direct source of their federally created market. Downwind public health beneficiaries are powerless and trapped but genuinely benefit — this is a coordination benefit received without agency, distinct from the extraction relationship coal communities experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The systemic-transformation reading is not automatically pure extraction dressed as coordination: it does solve a genuine collective-action problem (interstate emission transfer, sector decarbonization pace) that facility-level retrofits alone cannot solve, which is why it is authored as tangled_rope rather than snare. But the coordination function does not erase the asymmetric cost borne by coal workers and coal-dependent state fiscal structures who had no voice in the rule's design and bear concentrated, front-loaded costs for a diffuse, delayed benefit. Declaring this tangled_rope rather than either mountain-of-necessity or snare-of-pretext preserves both halves: the emission-reduction coordination is real, and the extraction from a specific, identifiable, powerless population is also real and requires active federal enforcement to sustain against sustained litigation resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    best_system_textual_scope,
    'Does the statutory phrase ''best system of emission reduction... adequately demonstrated'' in CAA Section 111(a)(1) permit EPA to define ''system'' at the level of the regional grid and generation mix, or only at the level of an individual stationary source?',
    'Definitive appellate or Supreme Court resolution of the statutory-interpretation question, informed by legislative history of the 1970/1990 CAA amendments and any subsequent congressional clarification (including whether Congress ratifies or overrides the systemic reading through subsequent legislation).',
    'If courts confirm the systemic reading, the tangled_rope classification with its asymmetric coal-sector extraction stands as the durable structure; if courts confine ''best system'' to facility-level measures, this constraint dissolves and only the facility_constraint_reading (lower ε, narrower stakeholder set) survives as the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(best_system_textual_scope, conceptual, 'Whether ''best system'' textually reaches grid-wide generation-shifting or only individual-facility measures — the core kernel contest.').

omega_variable(
    major_questions_doctrine_application,
    'Does EPA''s assertion of authority to reshape the national electricity generation mix trigger the major questions doctrine, requiring clear congressional authorization that Section 111(d)''s general phrase ''best system'' does not provide?',
    'Tracking how courts apply major-questions reasoning to subsequent EPA 111(d) rulemakings and whether Congress enacts explicit statutory authorization or explicit statutory limitation in response.',
    'A major-questions finding against EPA would structurally favor the facility_constraint_reading and treat the systemic reading as an ultra vires extraction claim rather than a legitimate coordination mechanism, shifting this story''s classification toward snare from the judiciary''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_application, conceptual, 'Whether the systemic reading survives major-questions scrutiny or is judicially foreclosed.').

omega_variable(
    transition_funding_adequacy,
    'Is the cost borne by coal workers and coal-dependent communities genuinely uncompensated, or does it become adequately offset by separate federal transition-assistance programs enacted alongside or after the 111(d) guideline?',
    'Empirical tracking of transition-fund disbursement relative to documented job and tax-base losses in coal-dependent regions over the measurement interval.',
    'If transition funding proves adequate and durable, the victim-side extraction this story authors would be substantially mitigated, pushing the classification toward a genuine rope; if funding remains symbolic or underfunded, the tangled_rope classification''s extraction component is confirmed and may harden toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_funding_adequacy, empirical, 'Whether compensatory programs offset the extraction this reading imposes on coal communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(caa__tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.15).
narrative_ontology:measurement(caa__tr_t2023, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(caa__tr_t2027, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2027, 0.19).
narrative_ontology:measurement(caa__tr_t2031, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2031, 0.2).
narrative_ontology:measurement(caa__tr_t2035, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2035, 0.2).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(caa__be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(caa__be_t2023, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(caa__be_t2027, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2027, 0.6).
narrative_ontology:measurement(caa__be_t2031, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2031, 0.61).
narrative_ontology:measurement(caa__be_t2035, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2035, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(caa__su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(caa__su_t2023, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2023, 0.5).
narrative_ontology:measurement(caa__su_t2027, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2027, 0.55).
narrative_ontology:measurement(caa__su_t2031, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2031, 0.57).
narrative_ontology:measurement(caa__su_t2035, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2035, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% This story and caa_section_111d_delegation__facility_constraint_reading are the two readings of the caa_section_111d_delegation kernel. They share the same statutory text (CAA Section 111(d)) but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε: the systemic reading (this story) authorizes grid-wide generation-shifting and produces a coal-sector extraction relationship absent from the facility-constrained reading, which confines compliance cost to individual facility retrofits and involves no comparable renewable-sector demand transfer. Per the ε-invariance principle, these are not one constraint measured two ways — they are two constraints linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
