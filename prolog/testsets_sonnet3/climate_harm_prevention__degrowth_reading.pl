% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Growth-Bounded Mitigation Regime (Degrowth Reading of the Climate Harm Prevention Kernel)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_harm_prevention kernel: legitimate climate response requires
 *   planned economic contraction in the Global North because
 *   mitigation-within-growth is physically and politically impossible at the
 *   required speed and scale. The ε referent is the standing arrangement
 *   under contest — the growth-compatible mitigation regime that dominates
 *   international negotiation, AS SEEN by this reading — not the contraction
 *   program the reading endorses. From this reading's own lights, the
 *   growth-compatible regime substantially extracts: it preserves Global
 *   North throughput and consumption while transferring the resulting
 *   physical costs onto Global South states, future generations, and
 *   displaced populations, all while requiring active institutional
 *   enforcement (treaty architecture, target-setting bodies) to keep
 *   contraction off the table of 'legitimate' proposals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.71).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Growth-Bounded Mitigation Regime (Degrowth Reading of the Climate Harm Prevention Kernel)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '1d1a56b0-9d1f-480d-bf8a-770d9bcc7769').
narrative_ontology:cs_kernel_codification('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', distributed).
narrative_ontology:cs_authority_grounding('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', distributed).
narrative_ontology:cs_reading_relation('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', foundational, growth_boundary_illegitimate_as_policy_constraint).
narrative_ontology:cs_axiom_status(growth_boundary_illegitimate_as_policy_constraint, holdable).
narrative_ontology:cs_axiom_grounding('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', growth_boundary_illegitimate_as_policy_constraint, empirically_contingent).
narrative_ontology:cs_axiom('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', foundational, intergenerational_and_global_south_priority_over_present_north_consumption).
narrative_ontology:cs_axiom_status(intergenerational_and_global_south_priority_over_present_north_consumption, holdable).
narrative_ontology:cs_axiom_grounding('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', intergenerational_and_global_south_priority_over_present_north_consumption, deontological).
narrative_ontology:cs_reference_frame('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', unfccc_common_but_differentiated_responsibility_framework).
narrative_ontology:cs_drift_state('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', post_paris_agreement_ndc_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d1a56b0-9d1f-480d-bf8a-770d9bcc7769', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_north_incumbent_industry).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_north_consumer_classes).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, climate_displaced_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumer_classes).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, growth_compatible_mitigation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fossil-linked and growth-dependent sectors (energy majors, finance, heavy manufacturing) in wealthy states shape the boundary of 'legitimate' climate policy to exclude contraction, funding technology-transition narratives and lobbying against binding consumption limits. They continue extracting value from a high-throughput economy while the framing that contraction is 'infeasible' absorbs blame onto physics or politics rather than onto their own resistance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_incumbent_industry, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_incumbent_industry, agenda_setter).

% Enjoy consumption levels and material throughput that the degrowth reading identifies as the actual physical driver of overshoot. Some bear real costs under a contraction program (reduced consumption, restructured labor), but under the present growth-framework arrangement they are net beneficiaries of continued high-throughput life relative to the alternative this reading demands.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumer_classes, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_consumer_classes, payer).

% Bear the disproportionate physical harms of a warming trajectory driven overwhelmingly by cumulative Global North emissions, while having contributed the least. Cannot exit the climate system's physical consequences; their leverage in international negotiations is structurally weaker than the emitting blocs whose consumption the degrowth reading targets.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_frontline_states, payer,
    moderate, generational, trapped, global).

% Inherit whatever carbon budget and climate stability remain after present decisions. They have no seat in current negotiations, no capacity to bargain, and no exit from the physical trajectory locked in by present-day growth-bounded mitigation choices.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Already losing land, livelihoods, and homes to warming-driven disasters and slow-onset degradation. Have no standing in the international policy architecture that decides whether mitigation stays growth-compatible; migration and resettlement are the only 'exits' available, and both are heavily constrained by border regimes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_displaced_populations, payer,
    powerless, biographical, trapped, regional).

% Academics, movements, and some Global South delegations who argue planned contraction is the only physically coherent response are structurally marginalized in mainstream climate negotiation venues (UNFCCC, G7/G20 communiques), which are built around growth-compatible technological transition framings. They publish, protest, and submit alternative proposals but rarely get a binding seat at the table where targets are actually set.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_policy_coalitions, excluded,
    moderate, generational, constrained, national).

% UNFCCC bodies, IPCC working groups, and multilateral development institutions administer the negotiation architecture and set the boundaries of what counts as a legitimate proposal. They formally acknowledge both mitigation pathways but have institutionally embedded growth-compatibility as the default legitimacy test, which structurally disadvantages degrowth proposals regardless of their formal openness to considering them.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, international_climate_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, international_climate_institutions, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, global_north_incumbent_industry).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global response to a genuine collective-action problem — atmospheric carbon is a shared sink and no single state's restraint suffices without others' — by establishing what counts as a legitimate mitigation pathway that could, in principle, bind multiple parties to a common effort.
% TRANSFER_FUNCTION: Under the growth-compatible framing this reading contests, the arrangement transfers atmospheric budget space and ecological stability from the Global South and future generations to continued Global North consumption; the degrowth reading proposes reversing that transfer by moving material throughput reduction costs onto Global North present populations instead.
% ABSENT_VOICES: Degrowth policy coalitions and many Global South civil-society delegations argue contraction is the only physically coherent legitimate response, but are structurally outside the rooms (UNFCCC plenaries, G7/G20 communiques) where growth-compatible mitigation is treated as the default legitimate frame; future generations and climate-displaced populations have no seat at all.
% DISAPPEARANCE_RATIONALE: If the growth-bounded legitimacy test vanished overnight, planned-contraction proposals could be tabled as legitimate policy rather than fringe positions; carbon budgets would be renegotiated against throughput reduction rather than technological substitution, reallocating costs from the Global South and future generations onto present Global North consumption and industry.
% FOUNDING_PROBLEM: The founding problem was establishing SOME internationally legitimate basis for climate action given radically unequal historical emissions, present power, and physical urgency — a way to make binding claims about who must act and how much.
% FOUNDING_PROBLEM_CORROBORATION: IPCC physical science working groups (an institutional seat outside any single reading's beneficiary set) corroborate that remaining carbon budgets are incompatible with continued Global North growth trajectories under most scenarios; Global North industry associations and growth-compatible technology advocates dispute that contraction is necessary, asserting technological substitution suffices — the dispute is over whether the founding problem is solved by decoupling or requires throughput reduction, not over whether a founding problem existed.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 to 0.71) tracking the widening gap between what growth-compatible pledges (NDCs, net-zero-by-2050 commitments) actually deliver and what remaining carbon budgets require absent throughput reduction — from this reading's perspective, that gap IS the extraction, since it is measured in appropriated atmospheric budget space. Theater ratio also climbs (0.30 to 0.58) as summit diplomacy, voluntary pledges, and technology-transition rhetoric increasingly substitute for binding reduction commitments — this reading reads much of the COP architecture as increasingly performative relative to the physical requirement. Suppression (0.62 at present) reflects the institutional work required to keep degrowth proposals outside the legitimate negotiating frame — procedural exclusion, agenda-setting control, and the framing of contraction as inherently illegitimate or unthinkable within mainstream venues, rather than a debated option.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North incumbent industry and consumer classes sit near the beneficiary end: the growth-bounded frame preserves their present consumption and profit structures while displacing physical costs elsewhere and later. Global South frontline states, future generations, and climate-displaced populations sit near the full-target end: they are trapped (no exit from the physical trajectory), have civilizational or generational time horizons that cannot be renegotiated after the fact, and bear costs through a mechanism (atmospheric carbon budget) they did not create and cannot individually correct. Degrowth policy coalitions are excluded rather than coordinated — their marginalization from binding venues is the enforcement mechanism that keeps the growth-compatible frame dominant, not incidental friction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a legitimate binding basis for global climate action given unequal historical responsibility — remains live in the sense that no less-unequal alternative has replaced it; what has shifted is which specific commitments count as satisfying it. This reading holds that the founding problem has NOT been solved by decoupling-based technological transition, and treats the persistence of a growth-compatible legitimacy test as evidence the mandate has drifted from 'prevent harm' toward 'preserve growth while appearing to prevent harm' — a mandatrophy the theater_ratio trajectory is intended to track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is sufficiently rapid absolute decoupling of GDP growth from emissions and material throughput physically achievable at the pace required to meet remaining carbon budgets, or does physics require throughput contraction in high-consumption economies?',
    'Longitudinal empirical tracking of absolute decoupling rates in OECD economies against required emissions trajectories (IPCC remaining carbon budget updates); a sustained multi-decade record of absolute decoupling at the required rate would undermine the degrowth reading''s core empirical premise.',
    'If decoupling proves feasible at scale, this reading''s central claim collapses toward the mitigation_priority reading; if it proves infeasible, the mitigation_priority reading''s own framing becomes the object of critique this story identifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'Whether green growth can physically satisfy the same carbon budget constraint this reading says requires contraction.').

omega_variable(
    committer_kernel_reading_disagreement,
    'This constraint is one reading (degrowth_reading) of the climate_harm_prevention kernel; the sibling readings (mitigation_priority, adaptation_priority) would authorize different beneficiary/victim sets and different legitimate policy menus from the same underlying physical stakes. Where exactly does the disagreement sit — is it empirical (decoupling feasibility), political (which populations'' present welfare counts), or normative (whether growth itself is a legitimate constraint boundary)?',
    'Trace each sibling reading''s foundational axiom: mitigation_priority holds growth-compatibility as an unquestioned boundary condition; adaptation_priority holds mitigation itself as infeasible and reallocates to resilience; degrowth_reading rejects the growth boundary as illegitimate given the harms it generates. The disagreement is located in what each reading treats as a fixed constraint versus a variable to be adjusted.',
    'If the disagreement is purely empirical (resolved by growth_decoupling_feasibility), the readings could converge on evidence. If it is normative (whether Global North present consumption is a legitimate constraint on policy), no empirical resolution is possible and the readings remain permanently coexisting political positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_disagreement, conceptual, 'Documents where the kernel contest among the three readings is structurally located.').

omega_variable(
    global_north_beneficiary_boundary,
    'Are ''Global North consumer classes'' a coherent single beneficiary group, or does the degrowth reading''s own logic imply that lower-income populations within Global North states are misclassified as beneficiaries when they bear disproportionate costs of both climate impacts and any future contraction program?',
    'Distributional analysis of per-capita emissions and consumption within Global North states; if a large low-emitting subpopulation exists, the beneficiary declaration may need to be split into a separate constraint story disaggregating class within the Global North.',
    'A finer-grained beneficiary declaration would reduce the apparent extraction borne by lower-income Global North populations and could shift some of them toward payer status, changing the directionality computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_north_beneficiary_boundary, empirical, 'Whether the Global North beneficiary group is internally homogeneous enough for this story''s directionality claims to hold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__degrowth_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__degrowth_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__degrowth_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__degrowth_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__degrowth_reading, theater_ratio, 2020, 0.53).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__degrowth_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__degrowth_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__degrowth_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__degrowth_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__degrowth_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__degrowth_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__degrowth_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__degrowth_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__degrowth_reading, suppression_requirement, 1997, 0.45).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__degrowth_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__degrowth_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__degrowth_reading, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__degrowth_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_harm_prevention kernel. mitigation_priority holds growth-compatibility fixed and pursues technological decoupling; adaptation_priority treats mitigation as infeasible and reallocates to resilience-building under a higher accepted warming trajectory; degrowth_reading (this story) rejects growth-compatibility itself as a legitimate constraint boundary and requires planned Global North contraction. Each reading authors its own ε against the growth-bounded mitigation status quo as seen through that reading's own lights — the readings are not the same constraint measured three ways; they instantiate three structurally distinct constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
