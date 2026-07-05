% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Exogenous Override Reading of Commitment Displacement (Meiji Calendar/Dress Decrees)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   imposition_pathway_kernel: the claim that state capacity can displace an
 *   existing commitment structure (the lunisolar calendar, traditional dress)
 *   and install a new one directly, by decree and enforcement, with no
 *   meaningful antecedent fringe-adoption phase. The empirical anchor is the
 *   Meiji government's 1872 calendar decree and associated dress/hairstyle
 *   reforms: documentary evidence shows negligible pre-decree grassroots
 *   adoption of Gregorian dating or Western dress outside a thin diplomatic
 *   and mercantile elite, and the transition to full administrative
 *   compliance was compressed into weeks under threat of civil service
 *   dismissal and local enforcement. This reading treats that compression and
 *   coercion as evidence of a structurally distinct mechanism — not a
 *   fast-forwarded version of organic climb, but a different causal pathway
 *   that the general M-set (mechanisms of commitment displacement) framework
 *   must accommodate as its own cell. The sibling readings
 *   (endogenous_climb_reading, hybrid_cascade_reading) are NOT represented
 *   here; they are separate constraint files with their own ε values,
 *   beneficiary/victim structures, and classifications, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - meiji_state_apparatus: agenda_setter (institutional/analytical) — issues and enforces the decree
 *   - modernizing_elite_bureaucracy: beneficiary (organized/arbitrage) — gains legitimacy and administrative capacity
 *   - treaty_revision_negotiators: beneficiary/agenda_setter (institutional/analytical) — uses the reform as diplomatic instrument
 *   - rural_agricultural_populations: payer (powerless/trapped) — bears disrupted agricultural and ritual timing with zero prior adoption
 *   - traditionalist_religious_practitioners: payer (moderate/constrained) — loses state calendrical recognition
 *   - provincial_local_officials: payer/agenda_setter (moderate/constrained) — absorbs enforcement burden with no lead time
 *   - historical_sociologists: observer (analytical/analytical) — documents absence of pre-decree fringe adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.71).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Exogenous Override Reading of Commitment Displacement (Meiji Calendar/Dress Decrees)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, '66d98e0d-7eb6-4d64-a21b-876320269514').
narrative_ontology:cs_kernel_codification('66d98e0d-7eb6-4d64-a21b-876320269514', formalized).
narrative_ontology:cs_authority_grounding('66d98e0d-7eb6-4d64-a21b-876320269514', extraction).
narrative_ontology:cs_interpretation_layer_present('66d98e0d-7eb6-4d64-a21b-876320269514').
narrative_ontology:cs_reading_relation('66d98e0d-7eb6-4d64-a21b-876320269514', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('66d98e0d-7eb6-4d64-a21b-876320269514', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('66d98e0d-7eb6-4d64-a21b-876320269514', foundational, state_capacity_originates_commitment_without_fringe_precursor).
narrative_ontology:cs_axiom_status(state_capacity_originates_commitment_without_fringe_precursor, holdable).
narrative_ontology:cs_axiom_grounding('66d98e0d-7eb6-4d64-a21b-876320269514', state_capacity_originates_commitment_without_fringe_precursor, empirically_contingent).
narrative_ontology:cs_axiom('66d98e0d-7eb6-4d64-a21b-876320269514', foundational, coercive_compliance_is_structurally_distinct_from_emergent_adoption).
narrative_ontology:cs_axiom_status(coercive_compliance_is_structurally_distinct_from_emergent_adoption, holdable).
narrative_ontology:cs_axiom_grounding('66d98e0d-7eb6-4d64-a21b-876320269514', coercive_compliance_is_structurally_distinct_from_emergent_adoption, conventional).
narrative_ontology:cs_reference_frame('66d98e0d-7eb6-4d64-a21b-876320269514', state_decree_as_originating_commitment).
narrative_ontology:cs_drift_state('66d98e0d-7eb6-4d64-a21b-876320269514', post_diplomatic_urgency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66d98e0d-7eb6-4d64-a21b-876320269514', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, modernizing_elite_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, treaty_revision_negotiators).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditionalist_religious_practitioners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, provincial_local_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1872 calendar decree (switching to the Gregorian calendar with eleven days' notice) and subsequent dress and hairstyle edicts, backed by police enforcement, civil service dismissal threats, and school curricula. No meaningful prior grassroots adoption of the Gregorian calendar or Western dress existed among the general population before the decree; the state manufactures the new commitment directly through administrative fiat rather than ratifying an existing informal practice.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bureaucrats and reform-minded officials who had traveled or studied abroad benefit from the new commitment being imposed uniformly; it gives them a functioning administrative timekeeping and diplomatic-presentation apparatus without needing to wait for organic cultural diffusion. They largely already practiced Western customs informally and gain legitimacy and career advancement from the state's ratification of what were, for them, minority elite practices into universal law.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, modernizing_elite_bureaucracy, beneficiary,
    organized, generational, arbitrage, national).

% Diplomats seeking revision of the unequal treaties use rapid, visible Westernization (calendar, dress, calendar-based salary schedules) as evidence of civilizational parity to Western powers. The imposed commitment is a diplomatic instrument; its value to them depends precisely on its being state-wide and immediate rather than a slow fringe-driven climb, which would not present the same demonstrable proof of state capacity to foreign observers.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, treaty_revision_negotiators, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, treaty_revision_negotiators, agenda_setter).

% Farmers whose agricultural, festival, and ritual calendars were organized around the lunisolar system absorb the transition cost directly: planting and harvest timing, tax payment schedules, and religious observances are thrown into confusion by the eleven-day calendar jump with no transition period. They had no prior exposure to or informal adoption of the new calendar; the change arrives entirely as external imposition with no ability to exit the jurisdiction or the tax/administrative system that now runs on the new dates.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_populations, payer,
    powerless, biographical, trapped, regional).

% Shinto and Buddhist ritual calendars, calculated against the old system for centuries, are administratively decoupled from official state time. They can continue observances privately but lose state recognition and calendrical synchronization with civil life, bearing an ongoing cost of maintaining a shadow calendar with no official standing and no organic transition support.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditionalist_religious_practitioners, payer,
    moderate, generational, constrained, national).

% Charged with implementing the decree locally with almost no lead time and no local mandate-building process; they absorb the administrative burden of enforcement (explaining, recording, punishing noncompliance) without having participated in any prior gradual local adoption that would have made the change feel indigenous rather than externally dropped on them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, provincial_local_officials, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, provincial_local_officials, agenda_setter).

% Study the documentary record of the 1872-73 reforms to determine whether any meaningful pre-decree fringe adoption existed among elites, merchants, or urban populations. Their finding — negligible fringe penetration before the decree, near-total administrative compression of the transition into weeks — is the empirical basis for treating this as a distinct override mechanism rather than a compressed climb.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes Japan's administrative, fiscal, educational, and diplomatic calendar with the Gregorian standard used by the Western powers whose treaties and trade relations the state urgently needed to renegotiate, and standardizes dress/appearance codes to present a legible, modern state to foreign observers and to unify a fragmented set of regional and status-based sartorial customs under one national administrative logic.
% TRANSFER_FUNCTION: Moves the cost of calendrical and behavioral transition from the state (which could have staged a gradual, opt-in transition) onto the general population, particularly rural and traditionalist communities, who bear disrupted ritual, agricultural, and payment-cycle costs; moves diplomatic and administrative legitimacy benefits to the modernizing bureaucracy and treaty negotiators.
% ABSENT_VOICES: Rural communities and religious authorities whose calendars were most disrupted had no consultative role in the decree's design or timing; regional shrine and temple networks that might have offered a slower transition path were not brought into the reform process. Their objections surface only retrospectively in local records of confusion and resistance, not in the decision record itself.
% DISAPPEARANCE_RATIONALE: Had the decree not been imposed by state fiat, Japan's administrative calendar would very plausibly have continued on a much slower, contested trajectory shaped by whatever informal adoption patterns existed among merchants and treaty-port populations — the state's capacity to compel immediate, universal compliance is precisely what produced the compressed, uniform outcome; removing that capacity removes the mechanism, not merely its speed.
% FOUNDING_PROBLEM: Japan needed to demonstrate rapid civilizational and administrative parity with Western powers to support treaty revision negotiations, and needed a unified national administrative calendar to run a modern bureaucratic, military, and fiscal state — problems that gradual organic cultural diffusion could not solve on the diplomatic timeline required.
% FOUNDING_PROBLEM_CORROBORATION: Independent historical demographic and diplomatic-record analysis (used by historians outside the Meiji state's own self-narrative, including foreign legation correspondence noting the abruptness of the change) corroborates that the treaty-revision urgency which motivated the decree was resolved decades later by other means, and the calendar/dress commitment persisted as ordinary administrative practice long after the specific treaty-revision pressure that occasioned its imposition had passed — corroboration comes from outside the beneficiary bureaucracy, not merely its own commemorative histories.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts moderately high (0.61) reflecting the severe initial transition shock to populations with zero prior exposure to the new calendar/dress norms, then declines over the interval (to 0.42) as the imposed commitment naturalizes into ordinary administrative practice and the cost of noncompliance normalizes into simple habit rather than active loss. Suppression starts very high (0.88) — the decree relied on dismissal threats, police enforcement, and short notice — and declines moderately (to 0.71) as compliance becomes habitual and enforcement infrastructure can relax somewhat, though it never approaches low levels because status-legibility enforcement (dress codes for officials, calendar-based tax and school schedules) remains a permanent administrative feature. Theater ratio is low throughout (0.10 to 0.22): this is a functioning administrative synchronization mechanism, not primarily performative, though a slowly rising theatrical component appears as later generations comply out of habituated national-identity performance rather than functional administrative necessity. Accessibility collapse is high (0.68) — once the old calendar lost official standing, alternatives collapsed rapidly for anyone dealing with the state, though private/ritual calendars persisted informally. Resistance (0.55) reflects real documented friction — confused farmers, resistant shrine networks, local administrative complaints — without ever threatening the imposition's success, consistent with successful top-down override rather than negotiated coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and treaty negotiators sit at the full-beneficiary end: they designed the mechanism, control its timing and enforcement, and derive the diplomatic and administrative value it was built to produce — their exit option is properly analytical/arbitrage since they are not subject to the constraint's costs, they administer it. Rural populations sit at the full-target end: trapped, powerless, bearing disrupted timing with zero say in design or timing and no meaningful prior adoption that would have softened the transition. Provincial officials occupy a genuinely dual position (agenda_setter/payer secondary role) — they enforce the decree locally while also absorbing its administrative burden with no lead time, which the derivation chain alone would not fully capture without the dual role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgent treaty-revision diplomacy requiring rapid demonstrable Westernization) is dead — treaty revision was eventually achieved through other, later diplomatic mechanisms decades on — yet the calendar and dress commitments persisted indefinitely as ordinary administrative infrastructure. This is not read as capture/zombie mandatrophy, however, because the disappearance_verdict of world_rearranges reflects a mechanism that, once imposed, created its own genuine new coordination value (a unified national administrative calendar) independent of the diplomatic urgency that justified its initial imposition — the coordination function outlived and detached from its founding justification, which is exactly the pattern this reading's tangled_rope classification is meant to capture: genuine coordination value coexisting with asymmetric extraction that required (and continues to require, at declining intensity) active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_adoption_evidentiary_completeness,
    'Does the documentary record genuinely establish an absence of pre-decree fringe adoption, or does it merely reflect gaps in what was recorded about elite, merchant, and urban informal practice before 1872?',
    'Systematic archival review of merchant diaries, treaty-port community records, and missionary correspondence for evidence of informal Western calendar or dress use prior to the 1872 decree, compared against the density of such evidence in cases the endogenous_climb_reading treats as genuine organic climbs.',
    'If meaningful undocumented fringe adoption existed among treaty-port merchants or returned students, this reading''s core claim (no fringe pathway) weakens and the case shifts toward the hybrid_cascade_reading or even endogenous_climb_reading''s compressed-climb account. If the absence holds under scrutiny, the override reading''s distinct-mechanism claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_adoption_evidentiary_completeness, empirical, 'Whether the override reading''s key empirical premise (no meaningful pre-decree fringe adoption) survives closer archival scrutiny.').

omega_variable(
    state_capacity_as_precondition_or_substitute,
    'Is state capacity to impose a distinct causal mechanism for commitment displacement, or is it better understood as a resource that dramatically accelerates and disguises an underlying climb process that would otherwise be slow?',
    'Comparative case analysis: identify other historical top-down impositions with documented near-zero fringe adoption and low state capacity, versus high state capacity, to see whether imposition success correlates with capacity independent of any fringe precursor.',
    'If low-capacity states also achieve durable imposition without fringe precursors, this weakens state capacity as the operative mechanism and points toward a different variable (legitimacy, crisis conditions) doing the causal work. If high capacity is consistently necessary and sufficient, this reading''s identification of state capacity as the operative distinguishing factor is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_as_precondition_or_substitute, conceptual, 'Whether state capacity itself, versus some correlated variable, is the operative mechanism distinguishing override from climb.').

omega_variable(
    committer_framing_kernel_completeness,
    'Does the M-set framework genuinely require a distinct override cell, or can override cases be adequately modeled as a limiting case of the climb framework with fringe-stage duration approaching zero?',
    'Formal comparison of predictive/explanatory power: does a unified climb-only model with a compressibility parameter fit the Meiji case as well as a two-mechanism model? This is a conceptual/modeling question, not purely empirical, since both models can be fit to the same data with different assumptions about what counts as a ''fringe stage.''',
    'If a single compressible-climb model fits equally well, the distinct-mechanism claim central to this reading is unnecessary theoretical proliferation. If the two-mechanism model captures qualitative differences (coercion vs. voluntary adoption, enforcement infrastructure requirements) that the single model cannot, the separate M-set cell is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_kernel_completeness, conceptual, 'Whether the override/climb distinction is a genuine structural difference or a modeling choice about how to parameterize a single underlying process.').

omega_variable(
    coordination_extraction_persistence_ambiguity,
    'Given that the founding diplomatic problem is dead but the calendar/dress commitment persists as functioning coordination infrastructure, is the current extraction level (0.42) better understood as residual imposition cost or as the ordinary, non-extractive cost of any national standardization?',
    'Compare against extraction levels in national calendar/measurement standardizations achieved through non-coercive means (e.g., metric system adoption trajectories) to establish a baseline for ''ordinary standardization cost'' against which the Meiji case''s coercive origin can be assessed for lingering extractive residue.',
    'If comparable non-coercive standardizations show similarly low residual extraction after a century, this reading''s classification should shift toward rope (coordination with vestigial, non-extractive residue) rather than tangled_rope. If coercively-imposed standardizations show persistently higher extraction than voluntarily-adopted ones even after full naturalization, that supports tangled_rope as the durable correct classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_persistence_ambiguity, empirical, 'Whether the coercive origin leaves a lasting extractive signature distinguishable from ordinary standardization cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(impo_tr_t5, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(impo_be_t5, observed).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement_basis(impo_su_t5, observed).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of imposition_pathway_kernel, each a separate constraint file with its own ε, stakeholders, and classification, linked here rather than merged. endogenous_climb_reading holds all displacement occurs through fringe adoption with impositions as compressed/invisible climbs; hybrid_cascade_reading holds imposition creates an artificial fringe (state employees, military) that then climbs organically, with override initiating and climb completing the process; this file (exogenous_override_reading) holds override is a structurally distinct mechanism requiring its own M-set cell, with no meaningful fringe precursor. The three readings are not to be reconciled into a single ε or a single classification — the kernel contest is preserved by declaring three files with typed relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
