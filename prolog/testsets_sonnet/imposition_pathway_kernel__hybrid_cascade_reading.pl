% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Meiji Calendar Reform as State-Manufactured-Fringe Cascade (Hybrid Reading)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story models the 1872 Meiji calendar decree as a hybrid mechanism: a
 *   genuine top-down override (mandatory adoption for state employees,
 *   military conscripts, and schools, enforced within weeks with real payroll
 *   and disciplinary consequences) that then functioned as an artificially
 *   manufactured fringe population. That fringe subsequently climbed outward
 *   through ordinary social diffusion channels — family transmission, local
 *   official contact, commercial necessity — into rural and ritual
 *   communities over the following four decades, without further top-down
 *   compulsion. The decree INITIATES the process (override); the diffusion
 *   COMPLETES it (organic climb). This is one of three linked readings of the
 *   imposition_pathway_kernel: the endogenous_climb_reading treats the entire
 *   sequence as a compressed climb with an invisible fringe stage; the
 *   exogenous_override_reading treats the decree as a wholly separate
 *   mechanism from climb dynamics; this hybrid_cascade_reading treats them as
 *   sequential and causally linked — imposition manufactures the fringe,
 *   climb dynamics then carry it the rest of the way. The
 *   suppression_requirement series falls over the interval because active
 *   enforcement was only ever needed against the state-linked fringe and
 *   became progressively less necessary once organic diffusion took over;
 *   extractiveness falls in parallel as the coordination benefit (temporal
 *   compatibility) increasingly outweighs the shrinking compliance-cost
 *   component.
 *
 * KEY AGENTS:
 *   - meiji_state_bureaucracy: agenda_setter (institutional/arbitrage) — issues and enforces the override decree
 *   - conscripted_state_employees: payer/beneficiary (moderate/trapped) — the manufactured fringe that becomes the climb vector
 *   - modernizing_elite_reformers: beneficiary (powerful/mobile) — gains diplomatic legibility without bearing compliance cost
 *   - rural_agricultural_communities: payer (powerless/constrained) — absorbs the multi-decade dual-calendar burden as climb reaches them
 *   - traditional_calendar_specialists: payer (powerless/trapped) — professional delegitimization without direct compensation
 *   - historical_sociologists_of_state_formation: observer (analytical) — adjudicates between the three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.58).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Meiji Calendar Reform as State-Manufactured-Fringe Cascade (Hybrid Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'e7d1b285-c4cc-4f95-88ff-5db7ab1be02b').
narrative_ontology:cs_kernel_codification('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', distributed).
narrative_ontology:cs_authority_grounding('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', distributed).
narrative_ontology:cs_reading_relation('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', foundational, override_and_climb_are_sequential_causally_linked_phases).
narrative_ontology:cs_axiom_status(override_and_climb_are_sequential_causally_linked_phases, holdable).
narrative_ontology:cs_axiom_grounding('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', override_and_climb_are_sequential_causally_linked_phases, empirically_contingent).
narrative_ontology:cs_axiom('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', secondary, manufactured_fringe_is_structurally_distinct_from_natural_fringe).
narrative_ontology:cs_axiom_status(manufactured_fringe_is_structurally_distinct_from_natural_fringe, holdable).
narrative_ontology:cs_axiom_grounding('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', manufactured_fringe_is_structurally_distinct_from_natural_fringe, empirically_contingent).
narrative_ontology:cs_reference_frame('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', lunisolar_administrative_calendar_tradition).
narrative_ontology:cs_drift_state('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', post_decree_diffusion_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e7d1b285-c4cc-4f95-88ff-5db7ab1be02b', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elite_reformers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rural_agricultural_communities).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, buddhist_temple_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_state_employees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_state_employees).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, state_capacity_climb_compatibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1872 decree mandating the Gregorian calendar for all government employees, military personnel, schools, and official transactions effective within weeks. Enforces via payroll, conscription rolls, and school curricula — anyone drawing a state salary or wearing a uniform must comply immediately. Frames the decree as fiscal necessity (eliminating a costly intercalary month) and civilizational catch-up with Western treaty partners.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Government clerks, teachers, and soldiers who must adopt the new calendar overnight as a condition of employment or conscription, with no transition period and often with lost wages from the collapsed intercalary month. Over subsequent years many become fluent in and dependent on the new system, transmitting it to family and community networks — the imposed fringe becomes the organic climb vector the reform needed to spread beyond the state payroll.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_state_employees, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_state_employees, beneficiary).

% Genro and Westernizing officials who gain diplomatic and commercial legibility with treaty-port partners once Japan's official calendar aligns with the Gregorian standard. They do not bear the compliance cost directly and can travel between the old and new reckoning as convenience allows.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elite_reformers, beneficiary,
    powerful, generational, mobile, national).

% Continue using the lunisolar calendar for planting, harvest, and festival timing for decades after the decree, since farming cycles and local ritual calendars are not directly policed the way state employment is. They absorb the cost of running two calendars — one for taxes and official interactions, one for actual agricultural and communal life — until organic diffusion from state-linked relatives and local officials eventually erodes the older system.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_agricultural_communities, payer,
    powerless, generational, constrained, local).

% Almanac compilers and ritual timekeepers whose professional function is delegitimized by state decree even though local demand for lunisolar reckoning persists for years. Their trade survives informally but loses official standing and eventually clientele as the climb from the state-employee fringe reaches their communities.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_specialists, payer,
    powerless, biographical, trapped, local).

% Maintain ritual calendars tied to lunar observances that the decree does not directly abolish but that lose state recognition and synchronization with civil administration, creating friction with a government now organized on a different temporal grid. They negotiate a long accommodation rather than outright resistance.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, buddhist_temple_networks, payer,
    moderate, generational, constrained, national).

% Western treaty powers and trading houses gain a Japan whose official commercial and diplomatic calendar now matches their own, reducing transaction friction, without bearing any part of the domestic compliance cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners, beneficiary,
    institutional, generational, analytical, global).

% Study the Meiji calendar decree as a test case for whether top-down state impositions and bottom-up organic climbs are the same underlying mechanism (endogenous reading), genuinely distinct mechanisms (exogenous reading), or a hybrid where imposition manufactures the fringe that then climbs (this reading).
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologists_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_bureaucracy).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes Japan's official administrative, fiscal, and diplomatic temporal reckoning with its major trading and treaty partners, and eliminates the periodic thirteenth-month payroll cost of the lunisolar calendar for the state itself.
% TRANSFER_FUNCTION: Moves compliance cost from the state (which saves on payroll and gains diplomatic legibility) onto conscripted employees and, over the following decades, onto rural and ritual communities who must eventually abandon or dual-track their calendar systems as the state-employee fringe diffuses outward.
% ABSENT_VOICES: Rural farmers, ritual specialists, and temple networks were not consulted on the decree's timing or manner; their objections surface only in local records of confusion and resistance, and in the slow, multi-decade persistence of dual calendar use documented well after the official transition.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement machinery had not existed, Japan's calendar transition would likely have followed a slower, contact-driven diffusion path through merchants, missionaries, and returning students — the state's forced fringe compressed a multi-generation organic climb into a single administrative act, and removing that act would restore the longer, uncompressed diffusion timeline.
% FOUNDING_PROBLEM: Japan needed administrative and commercial temporal compatibility with Western treaty powers, and the Meiji state needed to eliminate the intercalary-month payroll cost that was straining a young treasury.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians studying Meiji fiscal records corroborate the payroll-cost motive from outside the bureaucracy's own justifications; ethnographic and folklore studies of rural Japan (not produced by the state) corroborate that lunisolar practice persisted organically for decades after the decree, indicating the founding problem was solved for the state fringe well before it was solved for the wider population — the persistence of enforcement machinery past the state's own need is attested by administrative record, not merely bureaucratic self-report.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).
:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-high (0.55) reflecting the real compliance cost imposed on the state-employee fringe at the moment of override, then declines steadily as the coordination benefit (calendar compatibility, payroll savings) increasingly dominates once the transition completes via organic diffusion rather than continued coercion. Suppression starts high (0.78) because the initial override required genuine enforcement machinery — payroll penalties, conscription discipline — and falls to 0.35 by 1920 because the climb phase requires no comparable coercive apparatus; social diffusion through the manufactured fringe does the remaining work. Theater ratio rises modestly (0.15 to 0.28) as the state's ongoing calendar-related administrative activity increasingly serves symbolic modernization narratives rather than functional need, once the practical transition is largely complete in the population centers.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and modernizing elites sit near the beneficiary end: they set the terms, capture the coordination gains (diplomatic legibility, payroll savings), and bear minimal compliance cost themselves. Conscripted state employees occupy a genuinely dual position — victims of the initial override (trapped, immediate cost) who become the climb's transmission vector and thereby partial beneficiaries of the eventual widespread adoption they helped cause. Rural communities and calendar specialists are the clearest targets: high d, since the cost lands on them with a multi-decade lag and without their having any say in the original override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fiscal cost of the intercalary month, diplomatic incompatibility) was substantially resolved for the state itself within a decade of the decree, yet the broader societal transition and associated administrative apparatus persisted for another three decades. This is not classic mandatrophy (an institution persisting after its mandate is fully dead) but a staged obsolescence: the override's mandate died early for the fringe population that was its direct target, while the climb mechanism it triggered continued to do necessary work for populations the override never directly reached. Classifying this as tangled_rope rather than pure snare captures that: there is a genuine coordination function (temporal standardization with real diplomatic and administrative benefit) alongside asymmetric extraction (compliance costs concentrated on politically powerless populations who had no voice in the original override).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_manufacture_vs_natural_fringe_indistinguishability,
    'Is there an empirical test that distinguishes a state-manufactured fringe (this reading) from a naturally-occurring early-adopter fringe that the state merely accelerated (endogenous reading)?',
    'Compare diffusion curve shape and timing against comparable calendar transitions that lacked any top-down decree (e.g., gradual Gregorian adoption in societies without a comparable state mandate) — if the post-decree diffusion rate among non-state populations matches undecreed cases, the fringe was arguably not doing distinct causal work beyond what organic diffusion would have produced anyway.',
    'If diffusion rates are indistinguishable from undecreed cases, this hybrid reading collapses toward the endogenous_climb_reading — the decree would have accelerated timing but not created a structurally distinct mechanism. If diffusion rates are measurably faster or follow a different curve shape specifically tied to state-employee contact networks, the hybrid reading''s causal claim (manufactured fringe as distinct climb vector) is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_manufacture_vs_natural_fringe_indistinguishability, empirical, 'Whether the state-manufactured fringe is empirically distinguishable from an accelerated natural fringe.').

omega_variable(
    override_climb_boundary_location,
    'Where precisely does ''override'' end and ''organic climb'' begin in this cascade — is the boundary the moment of decree issuance, the moment fringe compliance is achieved, or some later diffusion threshold?',
    'Historical record of enforcement intensity over time (arrests, penalties, administrative pressure) mapped against the suppression_requirement trajectory — the point where enforcement activity drops to background levels marks the empirical override/climb boundary.',
    'If the boundary is sharp and enforcement drops rapidly to near-zero, the hybrid reading''s two-phase structure is well-supported. If enforcement remains elevated for decades without a clear drop-off, the exogenous_override_reading''s claim of a wholly distinct, sustained imposition mechanism becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_climb_boundary_location, conceptual, 'Whether the override-to-climb transition has a locatable empirical boundary or is itself a matter of interpretive framing.').

omega_variable(
    vindicated_proposition_naturalness_ambiguity,
    'Does the state_capacity_climb_compatibility_thesis (that state capacity and organic climb dynamics are compatible, sequential mechanisms rather than substitutes) represent a genuine structural finding, or is it a framing convenience that makes the hybrid reading appear more parsimonious than the alternatives?',
    'Test the thesis against other historical cases of state-imposed standardization (metric system adoption, national language standardization) to see if the manufactured-fringe-then-climb pattern replicates or whether each case requires ad hoc adjustment to fit the hybrid frame.',
    'If the pattern replicates cleanly across cases, the hybrid reading''s vindicated proposition has independent support. If it requires substantial ad hoc fitting per case, the vindication is closer to a post hoc rationalization of whichever reading the analyst prefers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vindicated_proposition_naturalness_ambiguity, conceptual, 'Whether the compatibility thesis this reading vindicates is a genuine cross-case finding or a framing artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 1872, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1872, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1872, 0.15).
narrative_ontology:measurement(impo_tr_t1882, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1882, 0.2).
narrative_ontology:measurement(impo_tr_t1892, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1892, 0.24).
narrative_ontology:measurement(impo_tr_t1902, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1902, 0.26).
narrative_ontology:measurement(impo_tr_t1912, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1912, 0.27).
narrative_ontology:measurement(impo_tr_t1920, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1920, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t1872, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1872, 0.55).
narrative_ontology:measurement(impo_be_t1882, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1882, 0.5).
narrative_ontology:measurement(impo_be_t1892, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1892, 0.46).
narrative_ontology:measurement(impo_be_t1902, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1902, 0.43).
narrative_ontology:measurement(impo_be_t1912, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1912, 0.43).
narrative_ontology:measurement(impo_be_t1920, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1920, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1872, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1872, 0.78).
narrative_ontology:measurement(impo_su_t1882, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1882, 0.68).
narrative_ontology:measurement(impo_su_t1892, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1892, 0.58).
narrative_ontology:measurement(impo_su_t1902, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1902, 0.48).
narrative_ontology:measurement(impo_su_t1912, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1912, 0.4).
narrative_ontology:measurement(impo_su_t1920, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1920, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_pathway_kernel, each with independently authored ε and stakeholder structure per the ε-invariance principle. The endogenous_climb_reading (ε lower, framed as pure organic diffusion with no distinct imposition mechanism) and the exogenous_override_reading (ε likely higher and more concentrated, framed as pure state-capacity imposition disjoint from climb dynamics) are NOT alternative measurements of this same constraint — they are structurally distinct claims about the mechanism, each instantiating its own constraint with its own beneficiary/victim structure. This hybrid_cascade_reading is the only one of the three that authors a two-phase temporal structure (high initial suppression declining as climb dynamics take over), which is the structural signature that would distinguish it empirically from its siblings if the omega variables above were resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
