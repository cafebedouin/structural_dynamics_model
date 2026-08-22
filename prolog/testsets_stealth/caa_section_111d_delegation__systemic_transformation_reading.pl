% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [repudiated_judicially_2022]
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
 *   human_readable: CAA Section 111(d) 'Best System' Delegation — Systemic Transformation Reading
 *   domain: administrative law/environmental regulation/constitutional interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to determine the 'best
 *   system of emission reduction' adequately demonstrated for existing
 *   stationary sources and to compel state plans meeting it. Under the
 *   systemic-transformation instantiation, the best system is assessed at the
 *   level of the generating fleet: the degree of reduction achievable by
 *   shifting generation across the grid — renewable substitution, redispatch
 *   toward gas, accelerated retirement of high-carbon units — counts toward
 *   each state's target. Operationally this made EPA the architect of state
 *   decarbonization pathways: states translated a fleet-wide benchmark into
 *   retirement schedules, credit trading, and replacement-buildout
 *   obligations, backed by a federal plan for noncompliance. Costs
 *   concentrate sharply — stranded coal assets, occupational loss in coal
 *   regions, rate recovery on captive ratepayers — while benefits are diffuse
 *   (climate stabilization, downwind air quality) or flow to growth sectors
 *   (compliance-driven renewable and gas revenue). This story authors that
 *   arrangement as it operated from proposal through judicial termination.
 *   KEY AGENTS (by structural relationship): - epa_administrator: Agenda
 *   setter (institutional/mobile) — determines the best system and enforces
 *   it - state_environmental_agencies: Implementing agenda-setters
 *   (institutional/constrained) — administer the arrangement while bearing
 *   its costs - coal_generation_sector: Primary target (organized/trapped) —
 *   bears retirement and utilization extraction on sunk assets -
 *   coal_community_workers: Concentrated victims (powerless/identity_locked)
 *   — occupational and regional identity fused with the extracted activity -
 *   coal_state_ratepayers: Diffuse payers (powerless/trapped) — monopoly
 *   service, no supplier exit - renewable_energy_producers: Primary
 *   beneficiaries (organized/mobile) - natural_gas_generators: Secondary
 *   beneficiaries (organized/mobile) - downwind_health_communities: Diffuse
 *   beneficiaries (powerless/trapped) - federal_appellate_courts: Analytical
 *   observers (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) 'Best System' Delegation — Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative law/environmental regulation/constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'd49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b').
narrative_ontology:cs_kernel_codification('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', fixed_text).
narrative_ontology:cs_authority_grounding('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', lineage).
narrative_ontology:cs_interpretation_layer_present('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b').
narrative_ontology:cs_reading_relation('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', caa_section_111d_delegation__facility_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', foundational, best_system_is_industry_wide_term_of_art).
narrative_ontology:cs_axiom_status(best_system_is_industry_wide_term_of_art, holdable).
narrative_ontology:cs_axiom_grounding('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', best_system_is_industry_wide_term_of_art, conventional).
narrative_ontology:cs_axiom('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', secondary, emission_standard_target_may_be_generation_mix).
narrative_ontology:cs_axiom_status(emission_standard_target_may_be_generation_mix, holdable).
narrative_ontology:cs_axiom_grounding('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', emission_standard_target_may_be_generation_mix, conventional).
narrative_ontology:cs_reference_frame('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', best_system_gridwide_generation_shifting).
narrative_ontology:cs_drift_state('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', post_major_questions_ruling, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d49bbf9c-ef09-42d4-b9a8-b3ceeb89fd6b', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, downwind_health_communities).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_coalition).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_generation_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_community_workers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_state_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, coal_state_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, state_environmental_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines what the 'best system of emission reduction' consists of, converts that determination into emission guidelines for existing sources, and approves or displaces state plans, backing the whole structure with a federally implemented plan for noncompliant states. Can revise the determination, change the metric, or withdraw the rule; its discretion is bounded by judicial review and electoral turnover.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_administrator, agenda_setter,
    institutional, generational, mobile, national).

% Translate the federal fleet-wide benchmark into enforceable state plans: retirement schedules, credit trading, replacement-buildout obligations. They administer the arrangement and absorb its political blowback and administrative cost; declining to submit a plan hands their authority to the federal plan backstop, so exit means self-preemption.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, state_environmental_agencies, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, state_environmental_agencies, payer).

% Owns plants whose compliance value declines as the benchmark assumes their displacement. Assets are sunk and dedicated — mine-mouth siting, rail contracts, no alternative use — so the realistic responses are utilization decline, early retirement, and litigation. Trade associations and state attorneys general are its principal levers.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_generation_sector, payer,
    organized, biographical, trapped, national).

% Concentrated in Appalachia, Wyoming, and North Dakota mining and plant towns where occupation, family history, and municipal economy are fused with coal. Retraining programs have historically underdelivered; exit means leaving the region and abandoning an occupational and community identity carried across generations.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_community_workers, payer,
    powerless, biographical, identity_locked, local).

% Bear rate increases from stranded-asset recovery and replacement-investment cost through monopoly utilities they cannot choose among. They share marginally in the long-run climate and health benefit, but no mechanism routes compensation to them directly; just-transition funds rarely reach household bills.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_state_ratepayers, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, coal_state_ratepayers, beneficiary).

% Compliance benchmarks create a regulatory demand floor for wind and solar buildout independent of merchant economics. They sell into any market and benefit from any decarbonization instrument, so their position survives even if the particular rule changes form.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers, beneficiary,
    organized, biographical, mobile, national).

% Occupy the intermediate rung of the generation shift: redispatch toward gas is the cheapest first compliance step before renewables mature. They gain share under the same fleet-accounting logic and retain optionality across compliance designs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators, beneficiary,
    organized, biographical, mobile, national).

% Receive particulate and ozone reductions as coal units retire, with benefits arriving unbundled and without any lever they control over timing or magnitude. Exposure follows regional airsheds, so individual relocation does not reliably escape the harm.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, downwind_health_communities, beneficiary,
    powerless, generational, trapped, continental).

% Intervened throughout rulemaking and litigation to defend the fleet-wide reading. Collects policy outcomes rather than rents; its position is portable across legal instruments, so defeat in one forum redirects effort to the next.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_coalition, beneficiary,
    organized, generational, mobile, national).

% Adjudicate how far the statutory phrase reaches. Their doctrinal commitments — deference to agency readings versus major-questions caution — decide which account of the delegation governs, and their personnel composition shifted the outcome across the interval.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an interstate and global collective-action problem in electricity generation: carbon dioxide from any single state's fleet imposes costs far outside its borders, so no state internalizes the full benefit of reducing it. A uniform federal benchmark prevents free-riding and race-to-the-bottom among states competing for cheap generation.
% TRANSFER_FUNCTION: Moves compliance obligations and capital away from existing coal generation — retirement schedules, utilization limits, stranded assets — and redirects investment toward renewable and gas capacity; moves regulatory discretion upward from states to EPA; places costs on coal-sector owners, workers, and coal-state ratepayers while distributing climate and health benefits diffusely.
% ABSENT_VOICES: Coal-community workers and coal-state ratepayers were thinly represented in the rulemaking dockets relative to industry trade associations and environmental organizations; the largest beneficiaries — future generations and the global public — hold no seat in any administrative forum; grid-reliability planners entered the process late.
% DISAPPEARANCE_RATIONALE: If the delegation-as-read vanished overnight, state plans built around generation-shifting would lose their federal anchor, planned coal retirements would slow or reverse where replacement capacity was unbuilt, the renewable pipeline sized to compliance demand would contract, and downwind health gains would stall — the power sector's decarbonization trajectory would rearrange around whatever instruments remained.
% FOUNDING_PROBLEM: Existing-source air pollution whose harms cross state and national borders and which individual states systematically under-regulate; in its climate application, the fact that stationary-source CO2 imposes global damages priced by no market and controllable by no single state.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and successive U.S. National Climate Assessments attest the underlying harm independently of any rulemaking party; the public-health literature on particulate mortality corroborates the co-benefit claims; Massachusetts v. EPA (2007) established the endangerment predicate from outside the benefiting industries. No source outside the arrangement's opponents attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.68 at the mature operating phase) because compliance costs concentrate on actors with sunk, dedicated assets while the offsetting benefits are diffuse or captured by growth sectors; the extraction is purposeful rather than parasitic, which is why it stops short of snare territory. Suppression (0.62) is structural: the federal-plan backstop and litigation exposure coerce state participation, but multiple compliance pathways (trading, fuel switching, retirement timing) keep the coercion short of total. Theater is low-to-moderate (0.28) in the active phase — the regulatory function was substantively performed — rising sharply only when the rule existed on paper without operating. Accessibility_collapse (0.48) is moderate: facility-level retrofits, carbon capture, allowance trading, and alternative statutory instruments remained partly open after the reading was understood, so alternatives did not fully close. Resistance (0.82) is extreme and well documented: a multi-state attorney-general coalition, a congressional disapproval resolution, a Supreme Court stay, and ultimately a major-questions reversal. Base_properties characterize the constraint at its mature operating phase (roughly 2015–2016); the measurement series traces the full lifecycle on one shared time grid. The series is cyclical rather than monotonic — rise (proposal to finalization), suspension (stay and successor-rule dormancy), brief revival (appellate vacatur of the successor), termination (major-questions ruling). The cycle is driven by electoral and judicial personnel turnover, not by intermittent reinforcement as an extraction mechanism; the oscillation is a side effect of the constraint's dependence on political occupancy of the agenda-setter seat. Coordination type is resource_allocation: the arrangement's dominant function is allocating generation and compliance burden across many parties, with transaction costs inherent to that allocation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently. From EPA's position the arrangement is a coordination instrument it designed and can recalibrate; from the coal sector's position it is a retirement schedule wearing a benchmark's clothing; from the state agencies' position it is an unfunded mandate they must administer under threat of preemption — a genuinely dual seat that should compute as neither pure coordinator nor pure payer. Coal workers and ratepayers are numerous but geographically concentrated and politically outmatched: coalition potential exists on paper (ratepayers plus workers plus reliability concerns could have forced compensation-side design), but it was never realized against the organized opposition of the benefiting sectors and the advocacy coalition defending the rule's ambition. The courts' seat is distinctive: they experience the constraint as an interpretive-legitimacy question rather than a cost question, which is why the constraint's fate was decided in their forum rather than by any party's aggregate welfare.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable and gas producers sit near the beneficiary end: the compliance architecture subsidizes their buildout through regulatory demand, and their mobile exit means they lose little if the instrument changes form. Downwind health communities are beneficiaries in outcome but powerless and trapped — they cannot modulate the benefit they receive. Coal generation sits near the full-target end: trapped by sunk assets, its effective extraction is amplified beyond the nominal compliance cost. Coal workers are pushed to the full-target end by identity lock — exit requires abandoning region and occupation together. Ratepayers are trapped payers with a marginal beneficiary shadow. State agencies derive a middling directionality: they administer (agenda-setter pull toward beneficiary) but bear real compliance and political costs (payer pull toward target), which is why they are authored with a secondary role rather than an override. EPA derives low directionality as designer and enforcer, though its exposure is reputational and jurisdictional rather than pecuniary.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is declared: the founding problem — cross-border harm from existing-source emissions that states under-regulate — remains live, corroborated by sources outside the benefiting parties. The tangled-rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope would whitewash the sharp asymmetry: a genuine collective-action solution that simultaneously strands a regional economy and fuses a workforce's identity to the extracted activity is not costless coordination. Reading it as pure snare would erase the function: the interstate free-rider problem in carbon reduction is real, and no state acting alone solves it. The hybrid classification keeps both facts load-bearing, and the temporal series shows the mechanism by which such hybrids die — not mandate atrophy (the problem persisted) but authority revocation, which is a different lifecycle endpoint than the one the piton track detects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel caa_section_111d_delegation — the systemic_transformation_reading. What would the sibling facility_constraint_reading change structurally if it governed instead?',
    'Comparative analysis of the two instantiated stories: the facility reading restricts the best system to measures implementable at individual sources, shrinking the victim set to retrofit-facing plants, removing the regulatory demand driver for renewables, lowering epsilon, and eliminating the coal-region identity-lock dynamic.',
    'The disagreement is located entirely in the semantic scope of ''best system of emission reduction'' — fence line versus grid. Whichever reading governs determines whether the coal sector as a whole or only non-compliant units are targets, and therefore whether the arrangement computes as a hybrid with regional redistribution or as a narrow technology mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story instantiates one reading of the Section 111(d) delegation kernel; the sibling reading alters the victim set, the beneficiary set, and epsilon.').

omega_variable(
    major_questions_reversibility,
    'Is the 2022 major-questions repudiation of the systemic reading terminal, or is the reading revivable through new textual theories, doctrinal narrowing, or congressional amendment?',
    'Track subsequent jurisprudence and legislative proposals: revival signals include a Court majority treating major questions as case-specific rather than categorical, or a statutory amendment expressly authorizing fleet-wide systems.',
    'If revived, the full extraction profile returns with the same victim structure; if terminal, the constraint''s lifecycle closes as judicially repudiated rather than mandatorily atrophied, and downstream constraints built on compliance-driven renewable demand must re-anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_reversibility, empirical, 'Whether the judicial termination of this reading is stable or provisional.').

omega_variable(
    coal_exit_cost_persistence,
    'Are the high exit costs borne by fossil-locked regions transitional (stranded assets amortizing over a decade) or persistent (structural regional entrapment outlasting the asset cycle)?',
    'Longitudinal regional economic data on coal-county employment, tax base, and population following retirement waves, compared against retraining-program placement rates.',
    'If transitional, victims are better modeled as temporarily constrained and effective extraction falls as assets roll off; if persistent, the identity_locked and trapped classifications harden, effective extraction stays elevated, and the compensation-side design flaw becomes the constraint''s defining extraction feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_exit_cost_persistence, empirical, 'Durability of the exit costs that amplify extraction on coal-region victims.').

omega_variable(
    compliance_pathway_capture,
    'Does the compliance architecture function as a neutral decarbonization instrument or as a directed subsidy channel in which renewable producers capture the constraint''s gains?',
    'Counterfactual price analysis: compare renewable buildout attributable to compliance demand against buildout under equivalent non-regulatory subsidies, and trace whether compliance-flexibility design choices (trading, averaging, crediting) systematically route investment to particular producer classes.',
    'If capture is established, gain concentrates further in the renewable seat and the arrangement drifts toward the extractive end of the hybrid range; if the pathway is neutral, the measured extraction is better attributed to the coal-side cost imposition alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_pathway_capture, conceptual, 'Whether the beneficiary side of the hybrid reflects public-purpose coordination or sectoral capture of the compliance mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2014, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2014, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(caa__tr_t2016, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(caa__tr_t2018, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2018, 0.55).
narrative_ontology:measurement(caa__tr_t2020, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2022, 0.72).

% Extraction over time
narrative_ontology:measurement(caa__be_t2014, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(caa__be_t2016, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(caa__be_t2018, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(caa__be_t2020, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2022, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2014, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(caa__su_t2016, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(caa__su_t2018, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2018, 0.3).
narrative_ontology:measurement(caa__su_t2020, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2020, 0.26).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.55).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2022, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, resource_allocation).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Section 111(d) authority' covers two structurally distinct constraints instantiated by two readings of one statutory phrase. This story (systemic_transformation_reading) authors the fleet-wide arrangement: high epsilon, victims spanning the coal sector and its regions, beneficiaries including compliance-demand-fed producers. The sibling (facility_constraint_reading) authors the source-level arrangement: lower epsilon, victims limited to facilities facing retrofit mandates, no grid-wide demand driver. The upstream/downstream relation runs from this reading to the sibling: this reading's judicial repudiation changed the sibling's operating environment, promoting the facility-level measures from a contested subset to the operative constraint after 2022. Each story carries its own epsilon, stakeholders, and classification; neither hedges across the boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
