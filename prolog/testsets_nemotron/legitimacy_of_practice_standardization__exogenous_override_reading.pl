% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization Legitimacy (Exogenous Override Reading)
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   This constraint captures the exogenous override reading of practice
 *   standardization legitimacy: the claim that state authority can
 *   legitimately impose abrupt, comprehensive changes to calendars, dress,
 *   weights, and measures when justified by modernization, fiscal stability,
 *   or international alignment. The historical referent is the wave of such
 *   reforms (e.g., Meiji Japan, Republican Turkey, Revolutionary France,
 *   Pahlavi Iran) where the state legally mandated Gregorian calendar
 *   adoption, metrication, and Western dress, enforcing compliance through
 *   fines, school curricula, and public ceremony. The constraint presents as
 *   coordination (a single administrative surface) but operates as extraction
 *   (compliance costs pushed onto populations whose cosmological and economic
 *   life is organized around the old systems). The 'double life' — surface
 *   compliance masking persistent underground practice — is not a
 *   transitional phase but a stable equilibrium maintained by the state's
 *   inability to fully penetrate rural life and the populations'
 *   identity-locked refusal to internalize the change.
 *
 * KEY AGENTS:
 *   - modernizing_state_elite: Primary agenda setter (institutional/arbitrage) — imposes standardization, collects administrative gains
 *   - rural_traditional_populations: Primary payer (powerless/identity_locked) — bears cognitive and ritual costs of double life
 *   - local_religious_authorities: Payer/excluded (moderate/constrained) — loses public ritual authority, maintains underground adherence
 *   - administrative_bureaucracy: Beneficiary (organized/constrained) — gains legible rules, career tied to new system
 *   - international_alignment_institutions: Beneficiary (powerful/mobile) — gains legible counterpart, conditions engagement on compliance
 *   - traditional_craft_guilds: Payer (moderate/constrained) — bears retooling costs, maintains dual inventories
 *   - urban_professional_class: Beneficiary/observer (organized/arbitrage) — genuinely gains from standardization, mobile exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization Legitimacy (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political/historical/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'd3d49942-4f8e-4a7e-91aa-28c324f27b7f').
narrative_ontology:cs_kernel_codification('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', formalized).
narrative_ontology:cs_authority_grounding('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', extraction).
narrative_ontology:cs_interpretation_layer_present('d3d49942-4f8e-4a7e-91aa-28c324f27b7f').
narrative_ontology:cs_reading_relation('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', legitimacy_of_practice_standardization__endogenous_displacement_reading, influences).
narrative_ontology:cs_reading_relation('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', foundational, state_decree_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', state_decree_suffices_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', foundational, collective_benefit_justifies_coercive_standardization).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_coercive_standardization, holdable).
narrative_ontology:cs_axiom_grounding('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', collective_benefit_justifies_coercive_standardization, instrumental).
narrative_ontology:cs_reference_frame('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', pre_reform_fragmented_polity).
narrative_ontology:cs_drift_state('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', post_standardization_equilibrium, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3d49942-4f8e-4a7e-91aa-28c324f27b7f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_elite).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_institutions).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, administrative_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, local_religious_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_craft_guilds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_professional_class).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_mandate).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, collective_benefit_justification).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_stability_through_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues decrees standardizing calendar, dress, weights, and measures to align with international norms and simplify fiscal administration. Justifies changes as modernization for collective benefit. Controls enforcement machinery (police, courts, school curricula). Gains administrative legibility, tax efficiency, and diplomatic recognition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives clearer rules, standardized forms, and simplified inter-office coordination. Career advancement depends on implementing the new standards. Resists reversion because their expertise and authority are now tied to the new system.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, administrative_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Foreign advisors, treaty bodies, and financial institutions that condition loans and recognition on adoption of international standards. Gain a legible counterpart state and reduced transaction costs. Can shift engagement to other states if this one reverts.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_institutions, beneficiary,
    powerful, generational, mobile, global).

% Continue using the lunar calendar for agriculture, religious festivals, and life-cycle rituals while performing surface compliance (Gregorian dates on official documents). Bear the cognitive and social cost of maintaining a 'double life.' Exit means abandoning the cosmological framework that structures their world.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_populations, payer,
    powerless, biographical, identity_locked, local).

% Lose official recognition of their ritual calendar and authority over life-cycle rites. Retain underground adherence but cannot perform public functions. Some co-opt the new system (e.g., issuing dual-dated certificates) to survive. Exit means surrendering communal leadership.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, local_religious_authorities, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, local_religious_authorities, excluded).

% Must adopt metric weights and measures, invalidating apprenticeship knowledge and tooling calibrated to traditional units. Face fines for non-compliance in market inspections. Some maintain dual inventories for local vs. official trade.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_craft_guilds, payer,
    moderate, biographical, constrained, regional).

% Gains from standardized education, professional licensing, and international mobility. Sees the reform as progress. Can emigrate or shift sectors if the system becomes too extractive. Their situation is genuinely improved, not merely extracted from.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_professional_class, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, urban_professional_class, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single, legible administrative surface across the territory: one calendar for tax deadlines, one metric system for customs and contracts, one dress code for official identification. Solves the genuine coordination problem of a fragmented, multi-system polity trying to interface with a standardized international order.
% TRANSFER_FUNCTION: Moves compliance costs (cognitive load, material retooling, ritual disruption) from the state center and its international counterparts onto rural populations, traditional authorities, and craft guilds. Moves administrative efficiency, fiscal legibility, and diplomatic recognition to the modernizing elite and bureaucracy.
% ABSENT_VOICES: Nomadic and mountain communities beyond effective state reach — they would reject the standardization entirely but are not in the conversation because the state's enforcement does not penetrate their territory. Also absent: the dead — ancestors whose rites are now misaligned with the imposed calendar.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement vanished overnight, rural populations would immediately revert to the lunar calendar for all purposes; religious authorities would resume public ritual scheduling; guilds would return to traditional measures in local trade. The administrative surface would fracture, tax collection would become opaque, and international treaty obligations would be breached — the polity would reorganize around the traditional partition.
% FOUNDING_PROBLEM: The state faced fiscal collapse and diplomatic isolation under a fragmented system of calendars, weights, and legal customs that prevented reliable revenue collection, military conscription, and treaty compliance. Standardization was decreed to create a legible, governable administrative unit recognizable to foreign powers.
% FOUNDING_PROBLEM_CORROBORATION: State archives and reformist memoirs attest the founding problem was live and urgent. Rural oral histories and ethnographic records from outside the benefiting parties attest the problem was a state construction — the traditional system functioned for local coordination and the 'crisis' was manufactured by the state's desire for international recognition. No neutral arbiter corroborates either side.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the coordination function (administrative legibility) is real but the compliance burden is asymmetrically distributed — rural populations pay the cost of the state's international legibility. Suppression (0.72) is high because the constraint's persistence depends on active enforcement (calendar police, metric inspectors, dress codes in schools) and the exclusion of rival calendars from public space. Theater ratio (0.55) is elevated because the state performs 'modernization' through ceremonies and curricula while the actual coordination gains (tax collection, conscription) plateau early and the rest is symbolic. Accessibility collapse (0.45) is moderate — alternatives (lunar calendar, traditional measures) persist robustly underground. Resistance (0.58) is significant but channeled into the 'double life' equilibrium rather than open revolt.
 *
 * PERSPECTIVAL GAP:
 *   From the state elite seat, the constraint is a rope: they built a coordination mechanism that solves a genuine collective-action problem (interfacing with the international order). From the rural population seat, it is a snare: the coordination story is cover for extracting compliance costs from people whose identity is fused to the old calendar. From the religious authority seat, it is a tangled rope: they lose public authority (extraction) but retain underground ritual coordination (coordination). The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state elite and international institutions are structural beneficiaries (d near 0.0) — they collect administrative legibility and diplomatic recognition. The administrative bureaucracy is a secondary beneficiary (d ~0.2) — they gain coordination but are locked into maintaining it. Rural populations are full targets (d near 1.0) — identity-locked, no exit, bear the cognitive and ritual costs. Religious authorities and craft guilds are constrained targets (d ~0.7-0.8) — they lose public standing but retain some adaptive capacity. Urban professionals sit near symmetric (d ~0.5) — genuine gains, genuine costs, arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by exposing the dual structure: the coordination function (administrative standardization) is real and declared in beneficiaries (bureaucracy, international institutions), while the asymmetric extraction (compliance costs on identity-locked populations) is declared in victims (rural populations, religious authorities). The active enforcement requirement is met by the historical record of calendar police, metric inspectors, and school dress codes. Without the tangled_rope classification, this would be misread as either a pure rope (ignoring extraction) or a pure snare (ignoring the genuine coordination that keeps the bureaucracy and international partners invested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the administrative coordination function (tax, conscription, treaty compliance) structurally separable from the cultural standardization (calendar, dress, measures), or does the latter''s extraction ride necessarily on the former''s coordination?',
    'Counterfactual: if the state had standardized only fiscal/administrative units (tax calendar, metric customs weights) while leaving ritual calendar and dress untouched, would the coordination gains hold? Historical cases of partial standardization (e.g., Ottoman fiscal calendar vs. religious calendar) provide evidence.',
    'If separable, the cultural standardization is pure extraction riding on a real coordination core — the constraint decomposes into a rope (fiscal) and a snare (cultural). If inseparable, the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    identity_lock_mechanism,
    'Is the rural populations'' identity-locked exit (exit_options: identity_locked) driven by cosmological fusion (the calendar structures their relation to the divine/ancestors), epistemic closure (no exposure to alternatives), or material dependency (agricultural cycles require the lunar calendar)?',
    'Ethnographic comparison across reform cases: where the lunar calendar was purely ritual, identity lock weakened in 1-2 generations; where it governed agriculture, it persisted indefinitely. Oral history collection on the subjective experience of the ''double life.''',
    'If cosmological, the identity lock is structural and the constraint''s extraction is permanent. If epistemic, it decays with education/exposure. If material, it tracks agricultural modernization. Determines whether the tangled_rope equilibrium is stable or transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity lock for rural populations under exogenous standardization.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (exogenous_override_reading) of the contested kernel legitimacy_of_practice_standardization. How does the structural relationship between this reading and its siblings (endogenous_displacement_reading, dual_practice_equilibrium_reading) affect the classification of each?',
    'Map the three readings as a constraint family linked by network.affects_constraints. Author each reading as a separate constraint story with its own ε, beneficiaries, victims, and claimed_type. Compare computed per-seat classifications across the family.',
    'If the exogenous reading computes as tangled_rope while the dual_practice reading computes as rope (coordination without extraction) and the endogenous reading computes as mountain (emergent, no enforcement), the kernel''s contest is structural: different legitimacy claims instantiate different constraint types. The engine''s cross-constraint analysis would reveal this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural relationship between this kernel reading and its siblings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (fines, inspectors, school mandates) or does it include internalized suppression (populations believing the old ways are ''backward'' and self-policing)?',
    'Post-reform trajectory analysis: if suppression persists after enforcement machinery is dismantled (e.g., post-Soviet calendar reversion in Central Asia), the internalized component is significant. Compare regions with high vs. low state penetration.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after formal exit. This would increase effective extraction for identity-locked agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in exogenous standardization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(legitimacy_of_practice_standardization__exogenous_override_reading_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint (exogenous_override_reading) and its two siblings form the legitimacy_of_practice_standardization kernel family. The exogenous reading claims state decree suffices for legitimacy; the endogenous reading claims legitimacy requires voluntary adoption; the dual_practice reading claims legitimacy is domain-partitioned. Their ε values differ: exogenous (0.68, active enforcement, identity-locked victims) vs. endogenous (low ε, no enforcement, mobile agents) vs. dual_practice (moderate ε, partial enforcement, domain-partitioned victims). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, institutional, 0.05).
constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, powerless, 0.95).
constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
