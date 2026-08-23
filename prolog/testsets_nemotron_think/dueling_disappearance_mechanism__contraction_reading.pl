% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dueling Disappearance via Dignity-Culture Displacement
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story examines the historical process by which dueling —
 *   once a central institution of elite honor culture — became culturally
 *   unthinkable in Western societies. The contraction_reading argues that the
 *   primary mechanism was the displacement of honor-culture axioms (personal
 *   reputation defended by violence) by dignity-culture axioms (inherent
 *   worth, legal equality, procedural conflict resolution). This cultural
 *   shift transformed the constraint from a rope (dueling as coordination on
 *   honor norms) into a mountain (dignity culture as an irreversible
 *   substrate that makes dueling appear as a natural impossibility). The
 *   reading identifies honor-culture practitioners (aristocrats, military
 *   officers, traditional elites) as victims whose framework became
 *   illegible, and dignity-culture participants (bourgeois citizens, legal
 *   professionals, state bureaucrats) as beneficiaries. The claim/metric gap
 *   is deliberate: the constraint is CLAIMED as mountain (the reading's
 *   structural thesis) while the authored metrics describe a low-extraction,
 *   low-suppression, high-accessibility-collapse end state — the engine
 *   measures that divergence; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Primary targets (organized/identity_locked) — their identity and status economy built on duelable honor
 *   - dignity_culture_participants: Primary beneficiaries (organized/mobile) — gain legal equality and procedural conflict resolution
 *   - bourgeois_citizens: Secondary beneficiaries (organized/mobile) — benefit from predictable legal order replacing volatile honor violence
 *   - state_legal_institutions: Agenda setters (institutional/analytical) — codify and enforce the new norms, but also ride the cultural wave
 *   - historians_cultural_theorists: Observers (analytical/analytical) — contest the mechanism but do not bear the constraint's costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.1).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.1).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling Disappearance via Dignity-Culture Displacement").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'c8175577-f610-404a-8641-79175692ce39').
narrative_ontology:cs_kernel_codification('c8175577-f610-404a-8641-79175692ce39', distributed).
narrative_ontology:cs_authority_grounding('c8175577-f610-404a-8641-79175692ce39', expertise).
narrative_ontology:cs_interpretation_layer_present('c8175577-f610-404a-8641-79175692ce39').
narrative_ontology:cs_reading_relation('c8175577-f610-404a-8641-79175692ce39', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('c8175577-f610-404a-8641-79175692ce39', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('c8175577-f610-404a-8641-79175692ce39', foundational, cultural_displacement_drives_institutional_change).
narrative_ontology:cs_axiom_status(cultural_displacement_drives_institutional_change, holdable).
narrative_ontology:cs_axiom_grounding('c8175577-f610-404a-8641-79175692ce39', cultural_displacement_drives_institutional_change, empirically_contingent).
narrative_ontology:cs_axiom('c8175577-f610-404a-8641-79175692ce39', foundational, dignity_culture_as_irreversible_substrate).
narrative_ontology:cs_axiom_status(dignity_culture_as_irreversible_substrate, holdable).
narrative_ontology:cs_axiom_grounding('c8175577-f610-404a-8641-79175692ce39', dignity_culture_as_irreversible_substrate, empirically_contingent).
narrative_ontology:cs_reference_frame('c8175577-f610-404a-8641-79175692ce39', honor_culture_hegemony).
narrative_ontology:cs_drift_state('c8175577-f610-404a-8641-79175692ce39', dignity_culture_ascendancy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8175577-f610-404a-8641-79175692ce39', '2026-08-27T12:00:00Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_participants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_citizens).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, dignity_culture_displaces_honor_culture).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, civilizing_process_irreversibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocrats, military officers, and traditional elites whose status, identity, and dispute-resolution system depended on duelable honor. They cannot exit the constraint without abandoning their self-concept; the cultural shift makes their framework illegible, not just illegal. They bear the cost of lost status and the violence of the transition.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    organized, biographical, identity_locked, national).

% Bourgeois professionals, merchants, and citizens who operate within a dignity-culture framework of inherent worth and legal equality. They gain a predictable, non-violent social order and procedural conflict resolution. Their exit options are high — they can migrate to other dignity-culture societies.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_participants, beneficiary,
    organized, generational, mobile, national).

% The broader middle classes who benefit from the stabilization of property rights and contract enforcement that accompanies the decline of honor violence. They are not direct participants in the cultural theory but reap the coordination gains.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_citizens, beneficiary,
    organized, biographical, mobile, national).

% Courts, police, and legislatures that criminalize dueling and promote legal dispute resolution. They set the agenda for the transition, enforce the new norms, and gain monopoly on legitimate violence. Their exit is analytical — they study the constraint but are not subject to it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Scholars who analyze the mechanism of dueling's disappearance. They hold the contraction_reading, institutional_displacement_reading, or overdetermined_composite_reading. They neither collect nor pay; they contest the genealogy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historians_cultural_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor culture provided a coordination mechanism for dispute resolution and status maintenance among elites through ritualized violence; dignity culture coordinates mutual respect and legal equality without violence, solving the problem of endemic elite feuding.
% TRANSFER_FUNCTION: The displacement transfers status and dispute-resolution authority from honor-bound elites to legal institutions and bourgeois citizens, moving the cost of conflict from physical risk and death to procedural delay and legal fees.
% ABSENT_VOICES: Honor-culture practitioners (aristocrats, military officers) who experienced the displacement as a loss of meaningful agency; their voices are absent because their framework became illegible in the emerging discourse. Also absent: non-elite participants in honor violence (seconds, servants) whose perspective is rarely recorded.
% DISAPPEARANCE_RATIONALE: If the cultural unthinkability of dueling vanished, honor-based dispute resolution could re-emerge, altering the status economy, legal culture, and the monopoly of state violence. The world would rearrange because the constraint organizes the legitimacy of state authority and the structure of elite competition.
% FOUNDING_PROBLEM: The problem of regulating elite violence and status competition in a society transitioning from aristocratic honor to bourgeois dignity, where the old coordination mechanism (dueling) became too costly and the new one (legal procedure) required cultural legitimation.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists like Norbert Elias and Pieter Spierenburg corroborate the long-term civilizing process; however, the specific centrality of dignity-culture displacement is contested by institutional historians (e.g., Robert Shoemaker) who emphasize the role of courts and policing. No single corroboration outside the beneficiary set is definitive.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.1, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.1) at interval end because the dignity-culture norm does not actively extract resources; it renders dueling unthinkable. Suppression is low (0.1) because the constraint persists without active enforcement — the cultural internalization does the work. Theater ratio is low (0.1) because there is little performative maintenance; the norm is genuinely believed. Accessibility collapse is high (0.9) because alternatives (dueling, honor violence) are not just illegal but culturally inaccessible. Resistance is low (0.1) because no organized resistance exists; the honor framework has dissolved. The measurement series show the transition: extractiveness and suppression peak mid-interval (legal enforcement phase) then collapse as cultural internalization completes; theater peaks during the performative enforcement era then falls.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (honor_culture_practitioners) experiences the constraint as a snare during the transition (active suppression, identity loss) but as a mountain at the end (no exit, no resistance possible). The beneficiary seats (dignity_culture_participants, bourgeois_citizens) experience it as a rope (coordination gain) that becomes a mountain (taken-for-granted substrate). The agenda-setter seat (state_legal_institutions) experiences it as a scaffold during the legal enforcement phase (temporary support for the cultural shift) that becomes a mountain. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners are declared victims: they bear the cost of losing their status economy and identity framework, and their exit is identity_locked (cannot abandon honor without abandoning self). Dignity-culture participants and bourgeois citizens are declared beneficiaries: they gain a more predictable, less violent social order without paying the transition costs. State legal institutions are agenda_setters: they enforce the transition but also benefit from expanded monopoly on violence. The directionality derivation from these declarations yields high d for honor practitioners (full targets) and low d for beneficiaries (full beneficiaries), matching the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling the cultural shift as pure extraction (snare) by showing the coordination function (dignity culture solves the problem of elite violence without dueling) and the natural-law appearance (mountain). The FSM omega captures the ambiguity: if the mountain claim is a cover for bourgeois class interest, the constraint is a false summit (tangled_rope). The founding problem (regulating elite violence) is contested — some argue it persists in new forms (litigation, reputation markets), others say it is solved. The disappearance verdict (world_rearranges) confirms the constraint organizes arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is the dignity-culture norm against dueling a genuine natural law of modern social order, or a constructed constraint that benefits identifiable agents?',
    'Cross-cultural comparison of societies that transitioned to dignity culture without state enforcement; if the norm emerges spontaneously, it leans natural; if it requires persistent institutional reinforcement, it leans constructed.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as tangled_rope (coordination + extraction). If natural, the mountain classification holds and beneficiaries are incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Natural-law vs. constructed status of the dignity-culture substrate').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dueling primarily structural (legal prohibition, police enforcement) or internalized (cultural unthinkability, shame)?',
    'Post-legalization suppression trajectory: in jurisdictions where anti-dueling laws were repealed but dueling did not return, measure residual suppression via cultural attitudes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after legal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling''s unthinkability').

omega_variable(
    coordination_extraction_boundary,
    'Was the honor-culture dueling system a pure coordination mechanism (rope) or did it already contain asymmetric extraction (tangled_rope) before displacement?',
    'Analyze historical records for whether dueling''s costs fell disproportionately on lower-status participants (seconds, non-elite challengers) while benefits accrued to elites.',
    'If honor culture was already extractive, the displacement is not a shift from rope to mountain but from tangled_rope to mountain — changing the mandatrophy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the pre-displacement honor system was purely coordinative or already extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_disp_contraction_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1775, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1775, 0.2).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1825, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1825, 0.3).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.25).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1875, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1875, 0.15).
narrative_ontology:measurement(dueling_disp_contraction_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(dueling_disp_contraction_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(dueling_disp_contraction_be_t1775, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1775, 0.3).
narrative_ontology:measurement(dueling_disp_contraction_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(dueling_disp_contraction_be_t1825, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1825, 0.2).
narrative_ontology:measurement(dueling_disp_contraction_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(dueling_disp_contraction_be_t1875, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1875, 0.12).
narrative_ontology:measurement(dueling_disp_contraction_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(dueling_disp_contraction_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.25).
narrative_ontology:measurement(dueling_disp_contraction_su_t1775, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1775, 0.35).
narrative_ontology:measurement(dueling_disp_contraction_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.45).
narrative_ontology:measurement(dueling_disp_contraction_su_t1825, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1825, 0.5).
narrative_ontology:measurement(dueling_disp_contraction_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(dueling_disp_contraction_su_t1875, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1875, 0.2).
narrative_ontology:measurement(dueling_disp_contraction_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.08).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_legal_prohibition).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, honor_culture_norms).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, civilizing_process).

% DUAL FORMULATION NOTE:
% This contraction_reading and the institutional_displacement_reading form a constraint family: cultural displacement creates the legitimacy conditions for institutional substitution. The overdetermined_composite_reading is a meta-constraint that binds them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, organized, 1.0).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, organized, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
