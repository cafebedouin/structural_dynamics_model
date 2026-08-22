% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Meiji-Era State-Imposed Calendar and Dress Commitment Override
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This constraint models the 1872-73 Meiji government decrees replacing the
 *   lunisolar calendar with the Gregorian calendar and mandating Western
 *   dress/haircuts for officials, students, and military personnel. The
 *   distinguishing structural claim of this reading is negative: there was no
 *   meaningful pre-decree fringe-adoption pathway among the rural populace or
 *   traditional professional classes. A narrow urban/official elite had
 *   already begun adopting Western practices for independent reasons, but the
 *   mass of the population moved from non-adoption to compliance in the span
 *   of the decree's enforcement rollout, not through a gradual S-curve of
 *   voluntary imitation. State capacity — a centralized registry, salaried
 *   bureaucracy, compulsory schooling, and conscript military — is the
 *   mechanism that manufactured universal compliance directly, without
 *   needing (or producing, prior to imposition) a critical mass of early
 *   adopters. This is one reading of the imposition_pathway_kernel; sibling
 *   readings (endogenous_climb_reading, hybrid_cascade_reading) dispute
 *   whether this apparent absence of fringe adoption is real or merely a
 *   matter of unobserved compression/artificial-fringe generation. This story
 *   authors ONLY the override reading: it does not average across readings,
 *   and its epsilon (0.42) reflects moderate extraction from populations for
 *   whom compliance imposed real costs without proportionate voice, layered
 *   onto a genuine coordination function (administrative and diplomatic
 *   synchronization) that this reading holds was achieved by fiat rather than
 *   climbed to.
 *
 * KEY AGENTS:
 *   - meiji_state_apparatus: agenda_setter (institutional/analytical) — issues and enforces the decree
 *   - westernizing_elite_bureaucracy: beneficiary (organized/mobile) — ratified by decree, already partially compliant
 *   - treaty_revision_diplomats: beneficiary (institutional/arbitrage) — uses compliance as diplomatic leverage
 *   - rural_agricultural_populace: payer (powerless/trapped) — bears compliance costs with no prior fringe stage
 *   - traditional_calendar_ritual_specialists: payer (moderate/constrained) — professional function eliminated by fiat
 *   - local_dress_and_textile_producers: payer (powerless/constrained) — lost markets with no preceding voluntary shift
 *   - historical_sociologists_of_diffusion: observer (analytical/analytical) — tests M-set completeness against this case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Meiji-Era State-Imposed Calendar and Dress Commitment Override").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2').
narrative_ontology:cs_kernel_codification('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', formalized).
narrative_ontology:cs_authority_grounding('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', extraction).
narrative_ontology:cs_interpretation_layer_present('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2').
narrative_ontology:cs_reading_relation('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', foundational, state_capacity_sufficient_for_direct_displacement).
narrative_ontology:cs_axiom_status(state_capacity_sufficient_for_direct_displacement, holdable).
narrative_ontology:cs_axiom_grounding('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', state_capacity_sufficient_for_direct_displacement, empirically_contingent).
narrative_ontology:cs_axiom('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', secondary, fringe_adoption_not_necessary_precondition).
narrative_ontology:cs_axiom_status(fringe_adoption_not_necessary_precondition, holdable).
narrative_ontology:cs_axiom_grounding('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', fringe_adoption_not_necessary_precondition, empirically_contingent).
narrative_ontology:cs_reference_frame('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', pre_meiji_lunisolar_ritual_order).
narrative_ontology:cs_drift_state('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', post_treaty_revision_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc1e6ff9-0a9c-4eaa-9318-1f30bbbb94c2', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, westernizing_elite_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, treaty_revision_diplomats).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_populace).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditional_calendar_ritual_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, local_dress_and_textile_producers).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, state_capacity_sufficiency_for_commitment_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1872 calendar decree and associated dress/haircut edicts by administrative fiat, backdated to take effect within weeks, and enforces compliance through schools, civil registries, the military, and salaried officialdom. Does not wait for or measure any prior grassroots adoption; the decree itself is the mechanism of change.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Government officials and urban professionals who had already personally adopted or were poised to adopt Western dress and the Gregorian calendar for status and diplomatic reasons. The decree ratifies and accelerates what was, for this narrow group, a genuine preference, and it consolidates their social position relative to non-adopters.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, westernizing_elite_bureaucracy, beneficiary,
    organized, biographical, mobile, national).

% Use the visible adoption of Western calendrical and dress norms as evidence of 'civilization' in unequal-treaty renegotiation with Western powers. Their leverage in international standing is the direct payoff of universal, state-enforced compliance rather than partial or optional adoption.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, treaty_revision_diplomats, beneficiary,
    institutional, generational, arbitrage, global).

% Had the lunisolar calendar embedded in planting cycles, festivals, and debt/rent schedules with no prior movement toward the Gregorian calendar. Compliance was imposed by administrative registration, tax dates, and school calendars; noncompliance risked fines, social marking, and exclusion from official transactions. There was no fringe-adopter stage they could point to before the decree — the change simply arrived enforced.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_populace, payer,
    powerless, biographical, trapped, regional).

% Diviners, almanac publishers, and shrine calendrical officials whose professional and ritual authority depended on the old calendar. State decree eliminated their official function overnight rather than through gradual displacement by a competing practice gaining voluntary adherents; some continued underground almanac production at legal risk.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditional_calendar_ritual_specialists, payer,
    moderate, biographical, constrained, regional).

% Producers of traditional garments for officials and students lost mandated-use markets when uniforms and Western dress were decreed for schools, military, and civil service, with no preceding period in which Western dress spread through voluntary imitation in these markets.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, local_dress_and_textile_producers, payer,
    powerless, biographical, constrained, local).

% Study the Meiji case as a test of whether commitment-displacement models built on fringe-adoption-then-climb (S-curve diffusion) can account for cases with no detectable pre-decree fringe stage. Their analysis is what surfaces the need for a distinct override cell in the M-set framework.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists_of_diffusion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rapidly synchronizes Japan's administrative, educational, and diplomatic calendar and dress conventions with those of the Western powers whose recognition and treaty cooperation the new state urgently needed, avoiding the transaction costs of a mixed internal calendar system during state-building.
% TRANSFER_FUNCTION: Moves calendrical, ritual, and craft authority away from local ritual specialists and traditional producers and toward the centralized bureaucratic state and the urban elite whose existing practices the decree ratifies; moves compliance costs (lost festivals-linked income, re-registration burdens, sanction risk) onto the rural populace and traditional professionals.
% ABSENT_VOICES: Rural populations and calendar/ritual specialists were not consulted in the decree's drafting; regional administrators who anticipated implementation friction left records of concern that were overridden by the center. No fringe-adopter constituency existed to be consulted because none had formed.
% DISAPPEARANCE_RATIONALE: Had the decree and its enforcement apparatus not existed, Japan's civil calendar would very plausibly have remained lunisolar for a materially longer period, treaty-revision diplomacy would have lacked a key 'civilizational' credential, and traditional calendar specialists and dress producers would have retained their markets and social function considerably longer — the state's administrative machinery is doing causal work, not merely certifying a change already underway.
% FOUNDING_PROBLEM: The new Meiji state needed to demonstrate rapid, legible 'civilizational' convergence with Western powers to support unequal-treaty renegotiation, and needed a unified administrative time standard to run a modern bureaucracy, school system, and conscript military — problems for which waiting on organic cultural diffusion was judged too slow.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians studying the unequal-treaty revision process (an audience external to the Meiji state and to the beneficiary bureaucracy) attest that treaty parity was substantially achieved by the 1900s and that the international-legitimacy problem the decree targeted no longer exists; the enforced calendar and dress norms nonetheless persist as ordinary administrative fact rather than as an active solution to a live problem, which the state apparatus itself does not contest.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).
:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is authored as moderate rather than high because the decree does carry a genuine coordination payoff (administrative synchronization, treaty leverage) that this reading credits as real, not merely cover — this is what makes tangled_rope rather than snare the structurally correct claim. Suppression (0.78 at story-scalar level; falling from 0.90 to 0.55 across the measured interval) is authored high at the outset because the override reading's defining claim is that compliance was produced by enforcement capacity, not by voluntary movement toward a preferred alternative — there was no accumulating fringe whose growing size reduced the marginal coercion needed. Suppression declines over the interval as compliance normalizes into habit and the enforcement apparatus can rely on internalization and generational replacement rather than active sanction; this is normalization, not softening of the original imposition. Accessibility collapse (0.62) is moderate-high: within a generation, alternatives to the Gregorian calendar for official purposes had essentially disappeared, but folk/ritual calendars persisted informally in private life, so collapse is not total. Resistance (0.55) reflects documented friction — underground almanac production, regional foot-dragging, agricultural-calendar persistence in practice — without amounting to sustained organized opposition capable of reversing the decree.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus sits at the full beneficiary/agenda-setter end: it authored and enforces the constraint and captures the coordination gains (administrative legibility, diplomatic credibility) without bearing the compliance costs. The westernizing elite are beneficiaries because the decree ratifies pre-existing practice at negligible cost to them — their d sits low. Treaty diplomats are indirect beneficiaries whose entire payoff depends on the compliance being total and visible, which is why they have structural interest in maximal enforcement rather than gradual adoption. Rural populace, ritual specialists, and dress/textile producers are targets: their d sits high because the constraint imposed direct costs (registration burdens, loss of professional function, market loss) with no corresponding benefit captured, and their exit options were trapped or constrained — geographic and economic conditions of Meiji Japan gave rural populations essentially no realistic option to remain outside the new administrative calendar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgent need for internationally legible 'civilizational' markers to support treaty revision) is dead by the reading's own corroboration — treaty parity was achieved by the early 1900s — yet the calendar and dress norms persist as ordinary administrative fact rather than as a live solution to a live problem. This is not authored as pure mandatrophy/piton, however, because a second, still-live coordination function (a single administrative calendar for a functioning modern bureaucracy) survives independently of the original diplomatic motive; the tangled_rope classification captures a constraint whose original extractive urgency has faded while a residual coordination function persists, distinguishing it from a piton where no one benefits enough to maintain it — here the state apparatus continues to benefit from calendrical uniformity as ordinary governance infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_stage_observability_ambiguity,
    'Did a genuine pre-decree fringe-adoption stage exist among some subpopulation (treaty-port merchants, returning students, coastal officials) that simply left insufficient historical record to detect, or did the change truly originate with zero prior voluntary adopters outside the narrow urban elite already counted as beneficiaries?',
    'Systematic archival search for private diaries, merchant account books, or local administrative records showing voluntary Gregorian-calendar or Western-dress use predating the 1872-73 decrees, disaggregated by region and social class, compared against a null model of decree-triggered adoption timing.',
    'If a genuine hidden fringe is found at meaningful scale, this reading''s core premise weakens and the case migrates toward the endogenous_climb_reading (compressed climb with invisible stages) rather than supporting a distinct override cell in the M-set framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_stage_observability_ambiguity, empirical, 'Whether the apparent absence of pre-decree fringe adoption is a real structural feature or an artifact of historical record gaps.').

omega_variable(
    artificial_fringe_vs_override_distinction,
    'Is the rapid compliance among conscripts, students, and civil servants best modeled as the state directly imposing a new commitment on a passive population (override), or as the state first creating a small compliant population through employment/conscription which then organically diffuses the norm outward (hybrid cascade)?',
    'Compare adoption-timing curves for populations with direct state employment/conscription ties versus populations with no such tie; if the two populations show statistically indistinguishable adoption timing (both moving in lockstep with decree enforcement rather than diffusion lag), override is favored; if non-tied populations show a diffusion-lag curve behind the tied populations, hybrid cascade is favored.',
    'Resolving this determines whether the imposition_pathway_kernel needs one additional M-set cell (pure override) or whether override collapses into a limiting case of hybrid cascade with zero diffusion lag, changing how many distinct mechanism-cells the framework requires.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artificial_fringe_vs_override_distinction, conceptual, 'Whether override and hybrid cascade are genuinely distinct mechanisms or points on one continuum.').

omega_variable(
    coordination_versus_extraction_weighting,
    'How much of the decree''s function was genuine administrative/diplomatic coordination necessity versus consolidation of elite and state power at rural/traditional-sector expense?',
    'Comparative analysis against contemporaneous state-formation cases (e.g., Ottoman Tanzimat calendar reforms, Siamese administrative modernization) to establish whether the extraction-to-coordination ratio observed here is typical of state-capacity-driven commitment displacement or unusually skewed toward extraction.',
    'A higher extraction weighting would push the classification toward snare; a higher coordination weighting would push it toward scaffold (if a sunset on enforcement intensity were demonstrable) or rope. The tangled_rope claim here rests on judging both components substantial and joint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_extraction_weighting, conceptual, 'Relative weight of coordination function versus extractive consolidation in the decree''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t4, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(impo_tr_t4, observed).
narrative_ontology:measurement(impo_tr_t8, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(impo_tr_t8, observed).
narrative_ontology:measurement(impo_tr_t16, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement_basis(impo_tr_t16, observed).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(impo_tr_t25, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t4, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(impo_be_t4, observed).
narrative_ontology:measurement(impo_be_t8, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(impo_be_t8, observed).
narrative_ontology:measurement(impo_be_t16, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement_basis(impo_be_t16, observed).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(impo_be_t25, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t4, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement_basis(impo_su_t4, observed).
narrative_ontology:measurement(impo_su_t8, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement_basis(impo_su_t8, observed).
narrative_ontology:measurement(impo_su_t16, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(impo_su_t16, observed).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(impo_su_t25, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_pathway_kernel, each instantiating a structurally distinct mechanism-claim about how the Meiji calendar/dress reforms produced universal commitment displacement. exogenous_override_reading claims a distinct override mechanism with no fringe pathway; endogenous_climb_reading claims all apparent overrides are compressed/invisible climbs; hybrid_cascade_reading claims override manufactures an artificial fringe that then climbs organically. Each reading authors its own epsilon and its own beneficiary/victim structure rather than averaging; they are linked here for contamination-propagation and cross-reading comparison, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
