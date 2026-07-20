% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: Exogenous State Override of Practice Legitimacy
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous_override_reading of the
 *   legitimacy_of_practice_standardization kernel: the claim that practice
 *   change is legitimate when state authority decrees it for collective
 *   benefit (modernization, fiscal stability, international alignment). In
 *   this reading, the state imposes calendar, dress, and metrological
 *   standards through abrupt legal decree backed by enforcement mechanisms.
 *   Rural populations maintain traditional lunar calendars underground for
 *   decades, producing a stable 'dual life' equilibrium of surface compliance
 *   and persistent traditional practice. The state agenda-setter benefits
 *   from administrative legibility and international standing; urban
 *   commercial elites benefit from reduced transaction costs; rural
 *   traditional communities and local religious authorities bear the costs of
 *   compliance and symbolic subordination.
 *
 * KEY AGENTS:
 *   - state_modernizers: Agenda-setter (institutional/identity_locked/national) â enforces standardization, captures political legitimacy and administrative control.
 *   - urban_commercial_elites: Beneficiary (powerful/mobile/national) â gains trade and contract standardization without bearing enforcement costs.
 *   - rural_traditional_communities: Primary target (powerless/trapped/local) â bears cognitive and symbolic costs of dual practice.
 *   - local_religious_authorities: Secondary target (moderate/constrained/regional) â loses ritual jurisdiction to state time.
 *   - dissident_traditional_intellectuals: Excluded voice (moderate/constrained/national) â argues for endogenous legitimacy but excluded from policy forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Exogenous State Override of Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'e2002c58-024b-4a44-9aed-dca91effa23f').
narrative_ontology:cs_kernel_codification('e2002c58-024b-4a44-9aed-dca91effa23f', formalized).
narrative_ontology:cs_authority_grounding('e2002c58-024b-4a44-9aed-dca91effa23f', extraction).
narrative_ontology:cs_interpretation_layer_present('e2002c58-024b-4a44-9aed-dca91effa23f').
narrative_ontology:cs_reading_relation('e2002c58-024b-4a44-9aed-dca91effa23f', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('e2002c58-024b-4a44-9aed-dca91effa23f', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('e2002c58-024b-4a44-9aed-dca91effa23f', foundational, state_decree_confers_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e2002c58-024b-4a44-9aed-dca91effa23f', state_decree_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('e2002c58-024b-4a44-9aed-dca91effa23f', foundational, collective_benefit_trumps_local_custom).
narrative_ontology:cs_axiom_status(collective_benefit_trumps_local_custom, holdable).
narrative_ontology:cs_axiom_grounding('e2002c58-024b-4a44-9aed-dca91effa23f', collective_benefit_trumps_local_custom, instrumental).
narrative_ontology:cs_reference_frame('e2002c58-024b-4a44-9aed-dca91effa23f', modernizing_state_supremacy).
narrative_ontology:cs_drift_state('e2002c58-024b-4a44-9aed-dca91effa23f', decades_after_imposition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2002c58-024b-4a44-9aed-dca91effa23f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_commercial_elites).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, local_religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce decrees standardizing calendars, dress, weights, and measures. Their political legitimacy and institutional budgets depend on demonstrating successful modernization and international alignment. They view persistent traditional practice as a threat to national progress and actively suppress public non-compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from standardized administrative time, weights, and legal codes that reduce transaction costs in trade, banking, and contracts. They support state policy publicly and may privately accommodate traditional timings for social rituals without bearing enforcement costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_commercial_elites, beneficiary,
    powerful, biographical, mobile, national).

% Must display the new calendar and attire in official and school contexts while maintaining agricultural and religious life according to the old lunar calendar and custom in private. Non-compliance risks fines, exclusion from state services, or public shaming. The dual practice imposes heavy cognitive load and symbolic subordination.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_communities, payer,
    powerless, generational, trapped, local).

% Lose jurisdiction over ritual timing, dress codes, and the marking of sacred days. The state claims sole authority to legitimate practice change, forcing them to publicly endorse the new standards or face marginalization. Their moral authority erodes as congregations adopt state time for official life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, local_religious_authorities, payer,
    moderate, generational, constrained, regional).

% Would argue that practice legitimacy derives from endogenous cultural continuity and practical utility rather than state decree. They are excluded from legislative committees, state academies, and public policy forums where modernization is treated as self-evidently necessary.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, dissident_traditional_intellectuals, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes weights, measures, calendars, and dress across a territory to enable centralized taxation, military logistics, international treaty alignment, and market predictability.
% TRANSFER_FUNCTION: Moves authority over legitimate practice from local and religious institutions to the centralized state; moves compliance labor, symbolic adaptation costs, and cognitive burden from rural and traditional populations to the modernization project.
% ABSENT_VOICES: Traditional practitioners who hold that legitimacy derives from ancestral or divine sanction; rural communities who would prefer domain-partitioned legitimacy if consulted; endogenous reformers who argue for gradual voluntary adoption driven by perceived utility.
% DISAPPEARANCE_RATIONALE: State administration relies on uniform standards for taxation, conscription, and international diplomacy; without the constraint, fiscal and legal coordination would fragment, traditional practices would resurface openly, and the dual-life equilibrium would dissolve as local institutions reclaim authority over time and dress.
% FOUNDING_PROBLEM: Political fragmentation under diverse local customary practices impeded centralized taxation, military logistics, and participation in the international state system; the state needed uniform standards to function as a modern sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: State archives and diplomatic historians attest to administrative inefficiencies under plural practices. Anthropologists and rural chroniclers outside the benefiting parties attest that local systems were functionally coherent and that imposition created new disorders; no neutral party unambiguously corroborates the state's framing that the problem required override rather than partition.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the constraint generates genuine administrative coordination while imposing substantial asymmetric costs on rural populations. Suppression (0.78) is high because the persistence of underground practice requires active state enforcement to prevent open defiance. Theater_ratio (0.55) captures the performative nature of official compliance masking unchanged private practice. The temporal series show extraction and theater peaking mid-interval as enforcement machinery matures and the dual-life equilibrium stabilizes, then slightly declining as urban adoption becomes partially genuine while rural persistence continues.
 *
 * PERSPECTIVAL GAP:
 *   The state agenda-setter seat experiences the constraint as necessary coordination for sovereignty and progress; the rural payer seat experiences it as domination extracting compliance and symbolic submission. The engine will compute divergent per-seat classifications from these structural relationships: low directionality for the state and urban beneficiaries, high directionality for trapped rural communities.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernizers are declared beneficiaries with identity-locked exit, yielding low directionality (they are subsidized by the constraint's political surplus). Urban elites are mobile beneficiaries, also low directionality. Rural communities are declared victims with trapped exit, yielding high directionality. Religious authorities are constrained victims, yielding moderately high directionality. The receipt surface names state_modernizers as the seat receiving the extraction (political and administrative surplus), confirming the asymmetric flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfragmented practices impeding centralized state functionsâwas genuine but is contested in its severity and in the legitimacy of the solution. The constraint has not resolved into a pure rope because enforcement remains necessary and victims persist; it is not a pure snare because some genuine coordination benefit accrues to trade and administration. The R5 genealogy flags it as contested rather than dead, preventing piton misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the persistent dual-life equilibrium evidence that exogenous state override is the operative constraint, or evidence that the dual-practice equilibrium reading describes the true structural arrangement?',
    'Trace the causal mechanism of persistence: if removing state enforcement causes immediate reversion to traditional public practice, exogenous override is operative; if public practice remains hybrid even after enforcement lapses, dual-practice equilibrium may be dominant.',
    'If the dual-practice reading is operative, this constraint is largely theatrical and should be reclassified toward piton; if exogenous override drives the structure, tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the operative constraint is state override or domain-partitioned equilibrium.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditional practice sustained by state coercion (police, courts, exclusion from services) or by internalized modernization ideology and shame?',
    'Measure the visibility of traditional practice during periods of state weakness (fiscal crisis, war, revolution): if practices resurface immediately, suppression was structural; if they remain hidden, internalization dominates.',
    'Structural suppression supports the current classification; internalized suppression indicates higher effective extraction and may shift the seat classification for rural communities toward deeper identity-lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of traditional practice.').

omega_variable(
    coordination_benefit_genuineness,
    'What proportion of state-imposed standardization produces genuine collective benefit (trade, fiscal stability) versus regime consolidation and symbolic domination?',
    'Counterfactual economic and administrative modeling comparing state capacity and trade volumes under standardized versus domain-partitioned practice regimes.',
    'A high genuine coordination ratio would push classification toward rope; a low ratio toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_benefit_genuineness, empirical, 'Genuine coordination benefit versus extractive domination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'legitimacy of practice standardization' into three structurally distinct claims with different epsilon values and stakeholder arrangements. The exogenous_override reading is substantially extractive and enforcement-dependent; the endogenous_displacement reading would be less extractive if adopted; the dual_practice_equilibrium reading describes a descriptive outcome that may operate as a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
