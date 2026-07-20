% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Overdetermined Authority Composite
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   Vatican II (1962â1965) produced a body of conciliar documents that bind
 *   the Catholic Church but embed incompatible theological rationales forged
 *   through factional compromise. This constraint story models the
 *   overdetermined composite as a binding condition: the council's authority
 *   is asserted as univocal by the magisterial institution, yet the texts
 *   structurally resist such univocity. The result is a tangled rope in which
 *   genuine coordination (preventing schism, permitting diverse factions to
 *   remain in communion) is inseparable from asymmetric extraction (the
 *   magisterium pays in diminished teaching credibility; scholars and
 *   reformers gain interpretive leverage). The claim/metric independence is
 *   maintained: the constraint is claimed as tangled_rope while the metrics
 *   independently describe moderate-high extraction, substantial suppression
 *   of univocal alternatives, and rising theater as the institution performs
 *   interpretive unity.
 *
 * KEY AGENTS:
 *   - Magisterial Institution (Roman Curia / Papacy): agenda_setter and primary payer â bears the cost of defending conciliar unity while the text undermines it
 *   - Conciliar Scholars: beneficiary â gain professional authority from permanent interpretive demand
 *   - Progressive Reformers: beneficiary â exploit ambiguity to advance change without formal rupture
 *   - Radical Traditionalists: excluded â would resolve ambiguity by rejecting the council, structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Overdetermined Authority Composite").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theological/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '00ace724-ac37-4379-bb7d-2aa279d6df1a').
narrative_ontology:cs_kernel_codification('00ace724-ac37-4379-bb7d-2aa279d6df1a', fixed_text).
narrative_ontology:cs_authority_grounding('00ace724-ac37-4379-bb7d-2aa279d6df1a', lineage).
narrative_ontology:cs_interpretation_layer_present('00ace724-ac37-4379-bb7d-2aa279d6df1a').
narrative_ontology:cs_reading_relation('00ace724-ac37-4379-bb7d-2aa279d6df1a', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('00ace724-ac37-4379-bb7d-2aa279d6df1a', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('00ace724-ac37-4379-bb7d-2aa279d6df1a', foundational, vatican_ii_is_overdetermined_compromise).
narrative_ontology:cs_axiom_status(vatican_ii_is_overdetermined_compromise, holdable).
narrative_ontology:cs_axiom_grounding('00ace724-ac37-4379-bb7d-2aa279d6df1a', vatican_ii_is_overdetermined_compromise, empirically_contingent).
narrative_ontology:cs_axiom('00ace724-ac37-4379-bb7d-2aa279d6df1a', secondary, univocal_interpretation_is_unattainable_for_vatican_ii).
narrative_ontology:cs_axiom_status(univocal_interpretation_is_unattainable_for_vatican_ii, holdable).
narrative_ontology:cs_axiom_grounding('00ace724-ac37-4379-bb7d-2aa279d6df1a', univocal_interpretation_is_unattainable_for_vatican_ii, empirically_contingent).
narrative_ontology:cs_reference_frame('00ace724-ac37-4379-bb7d-2aa279d6df1a', overdetermined_compromise).
narrative_ontology:cs_drift_state('00ace724-ac37-4379-bb7d-2aa279d6df1a', post_conciliar_magisterial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('00ace724-ac37-4379-bb7d-2aa279d6df1a', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, progressive_reformers).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must simultaneously uphold Vatican II as a binding ecumenical council and impose coherent doctrinal interpretation on documents that embed incompatible theological rationales. Bears the cost in diminished teaching credibility whenever forced ambiguity surfaces in disputes. Cannot exit the constraint without repudiating the council itself, which would dissolve its own foundational legitimacy.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_institution, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, magisterial_institution, payer).

% Gain sustained professional relevance from the permanent interpretive demand created by the council's overdetermined texts; academic careers in theological faculties and conciliar studies depend on navigating the complexity that institutional authority cannot resolve.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Exploit conciliar ambiguity to advance liturgical and doctrinal innovations while claiming direct conciliar mandate; benefit from the absence of definitive resolution that would either authorize or forbid their reforms.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_reformers, beneficiary,
    moderate, biographical, constrained, global).

% Would resolve the ambiguity by rejecting Vatican II outright or declaring it non-binding; their position is structurally excluded from mainstream ecclesial discourse because acknowledging them would force a confrontation with the overdetermination that the institution avoids.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, radical_traditionalists, excluded,
    organized, generational, constrained, global).

narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global church across modernizing and traditionalist factions by producing conciliar documents with ambiguous formulations that permit shared assent without requiring theological unanimity, thereby preventing immediate schism during and after the council.
% TRANSFER_FUNCTION: Transfers hermeneutic authority and doctrinal credibility from the centralized magisterial teaching office to the interpretive community of scholars and reformers; transfers the political and theological costs of unresolved contradiction to the institution that must claim univocal teaching authority.
% ABSENT_VOICES: Radical traditionalists who reject the council's legitimacy and radical progressives who want explicit rupture with tradition are structurally excluded from the interpretive mainstream; their presence would force resolution of the ambiguity that the current arrangement manages but does not solve.
% DISAPPEARANCE_RATIONALE: If the overdetermined ambiguity were resolved or the conciliar composite were shown to be univocally coherent or univocally ruptured, the post-conciliar balance would collapse: progressive reforms would lose protective ambiguity, traditionalist dissent would be vindicated or refuted, the scholarly apparatus built on interpretive complexity would dissolve, and the magisterium would either regain clear authority or lose it entirely.
% FOUNDING_PROBLEM: How to reform Catholic practice and teaching for the modern world while preserving institutional unity among bishops, theologians, and laity who held genuinely incompatible theological premises; the council sought aggiornamento without schism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the documentary record of conciliar commissions, the Relatio texts, and periti memoirs, which explicitly reveal competing rationales for major documents; attested by non-Catholic historians of Christianity who have no stake in the authority outcomes. Contested by the Magisterium, which asserts a unified conciliar will from inside the beneficiary loop.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the persistent cost to magisterial credibility: every authoritative invocation of Vatican II risks surfacing the contradictions. Suppression (0.58) captures the institutional suppression of both rupture and radical traditionalist readings, as well as the text's own suppression of clear alternatives. Theater_ratio (0.55) registers the performative maintenance of hermeneutic continuity (e.g., Benedict XVI's hermeneutic of continuity) in the face of visible textual resistance. Accessibility_collapse is high (0.75) because once the overdetermination is analytically grasped, simple continuity or rupture readings become intellectually untenable even if institutionally enforced. Resistance (0.45) comes from traditionalist dissent on the right and progressive impatience on the left. The measurement series tracks rising extraction and theater from the council's close (T=0) to the present (T=60), reflecting the accumulating costs of unresolved ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (magisterial institution) experiences the constraint as a necessary but costly coordination mechanism it is duty-bound to defend. The beneficiary seats (scholars, reformers) experience the same structure as a generative source of authority and flexibility. The engine computes this divergence from the structural data: identical constraint, opposite directionalities. The computed per-seat classification will differ accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial institution is declared in the victim array because it bears the primary extraction: its teaching authority is fragmented by the very council it must uphold. Its identity_locked exit (it cannot repudiate Vatican II without self-annihilation) pushes its directionality toward the full-target end. Conciliar scholars and progressive reformers are declared beneficiaries: they collect hermeneutic authority and policy flexibility from the ambiguity, with mobile or constrained exit options that keep their directionality near the beneficiary end. The structural asymmetry is unusual because the high-power actor is the target and lower-power actors are beneficiaries of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the conciliar settlement as either pure coordination (rope) or pure extraction (snare). The council genuinely coordinated diverse factions and prevented schism â a real coordination function â but it did so by encoding contradictions that now extract from the institution's teaching coherence. Labeling it a snare would miss the genuine ecumenical achievement; labeling it a rope would miss the persistent cost to magisterial authority. The tangled_rope type captures the inseparability of these functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'Is the composite overdetermination reading a distinct constraint with a separate epsilon profile from the continuity and rupture readings, or merely a meta-description of the same underlying arrangement?',
    'Apply the epsilon-invariance test: evaluate whether the measurable extraction profile changes when the council is treated as overdetermined versus univocal; if epsilon is stable across framings, the readings should be merged.',
    'If the readings are not distinct constraints, the constraint family should be collapsed and the omegas absorbed into a single story with high conceptual load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the composite reading is a distinct constraint or a meta-frame').

omega_variable(
    extraction_seat_ambiguity,
    'Does the hermeneutic extraction from magisterial authority accrue primarily to academic scholars, to progressive reformers exploiting ambiguity, or is it genuinely diffuse with no single capturer?',
    'Trace institutional outcomes: identify which seats gain measurable authority, publication access, or policy influence when magisterial pronouncements on Vatican II are contested or deferred.',
    'If progressive reformers capture the extraction, the beneficiary set should expand and directionality shift; if no seat captures it, gain_flow should be set to diffuse or omitted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_seat_ambiguity, empirical, 'Whether extraction accrues to scholars, reformers, or no one').

omega_variable(
    suppression_internalization,
    'Is the suppression of univocal interpretation maintained by structural institutional enforcement alone, or have theologians and bishops internalized the overdetermined ambiguity as a normative good?',
    'Observe post-retirement or disciplinary-exit statements from theologians: do they continue to assert ambiguity as virtuous after leaving institutional constraints, or do they advocate for resolution?',
    'If internalized, effective suppression exceeds the structural measure and the constraint may compute as more extractive for theologians than the institutional reading suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression of univocal interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2_comp_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(v2_comp_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(v2_comp_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(v2_comp_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(v2_comp_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(v2_comp_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(v2_comp_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(v2_comp_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(v2_comp_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(v2_comp_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(v2_comp_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(v2_comp_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(v2_comp_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(v2_comp_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vatican_ii_authority__composite_overdetermination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vatican_ii_authority kernel, decomposed per the epsilon-invariance principle because the continuity, rupture, and composite readings have different epsilon profiles and different beneficiary/victim structures. The composite reading measures extraction arising from irreducible ambiguity; the continuity and rupture readings measure extraction arising from enforced univocal interpretation in opposite directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
