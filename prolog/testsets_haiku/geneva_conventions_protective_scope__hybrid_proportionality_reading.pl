% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Convention Protective Scope via Hybrid Proportionality Analysis
 *   domain: international_humanitarian_law/legal_theory
 *
 * SUMMARY:
 *   The Geneva Conventions and their Additional Protocols scale protective
 *   obligations by conflict type (international vs non-international armed
 *   conflict). This constraint instantiates the
 *   hybrid_proportionality_reading: the protective scope is determined by a
 *   combination of conflict classification (which party is a state, whether
 *   foreign intervention is present) and a proportionality analysis that
 *   contextually adjusts what 'protected status' means. This reading coexists
 *   with two contending siblings: the state_centric_reading (which treats
 *   only uniformed, state-commanded combatants as treaty beneficiaries,
 *   leaving non-international conflicts with minimal AP II/CA3 protections)
 *   and the universal_rights_reading (which treats Geneva protections as
 *   foundational human rights applying to all affected persons regardless of
 *   status). The hybrid reading stakes out a middle ground that preserves
 *   state flexibility through interpretive discretion while maintaining
 *   treaty language commitment to scaling. Structurally, this discretion
 *   concentrates protective authority in the hands of conflict participants
 *   (especially states) and international legal interpreters, creating
 *   asymmetric protective scope: stronger parties can credibly claim their
 *   proportionality analysis is the 'correct' one, shifting compliance
 *   burdens to weaker parties.
 *
 * KEY AGENTS:
 *   - state_military_authority: Sets conflict classification and applies proportionality analysis; benefits from interpretive ambiguity that allows operational flexibility
 *   - non_state_armed_groups: Face uncertain protection status; pay through classification ambiguity and proportionality discretion they cannot control
 *   - civilian_border_populations: Bear the cost of oscillating protection status; benefit theoretically but materially vulnerable to reclassification mid-conflict
 *   - ambiguously_combatant_persons: Identity-locked into the proportionality calculus; targetability determined post-hoc by the stronger party
 *   - international_legal_interpreters: Institutional beneficiaries of the constraint's interpretive framework; preserve interpretive authority through proportionality doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.74).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Convention Protective Scope via Hybrid Proportionality Analysis").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'f03ae1b9-0073-4e95-9b33-77df5cd6bc6c').
narrative_ontology:cs_kernel_codification('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', fixed_text).
narrative_ontology:cs_authority_grounding('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', lineage).
narrative_ontology:cs_interpretation_layer_present('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c').
narrative_ontology:cs_reading_relation('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', foundational, proportionality_contextual_scaling).
narrative_ontology:cs_axiom_status(proportionality_contextual_scaling, holdable).
narrative_ontology:cs_axiom_grounding('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', proportionality_contextual_scaling, conventional).
narrative_ontology:cs_axiom('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', foundational, conflict_classification_discretion).
narrative_ontology:cs_axiom_status(conflict_classification_discretion, holdable).
narrative_ontology:cs_axiom_grounding('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', conflict_classification_discretion, conventional).
narrative_ontology:cs_reference_frame('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', geneva_treaties_as_adaptive_framework).
narrative_ontology:cs_drift_state('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', contemporary_asymmetric_conflicts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f03ae1b9-0073-4e95-9b33-77df5cd6bc6c', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_military_authority).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_legal_interpreters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_border_populations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, ambiguously_combatant_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_border_populations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State command structures interpret conflict type (international vs non-international) for their operational context, determining which Geneva protocol applies to their conduct. The hybrid proportionality reading gives them discretion: they classify their own conflict, apply proportionality analysis to determine protective scope, and adjust targeting rules accordingly. They benefit from the ambiguity because conflict classification remains contestable until the conflict ends or external recognition intervenes.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_military_authority, agenda_setter,
    institutional, generational, analytical, national).

% Face uncertain protection status depending on how their conflict is classified and proportionality analyzed. If classified as non-international (AP II / Common Article 3 applies), they receive narrower protections; if the state contests their combatant status and proportionality calculus limits application, protections shrink further. They cannot exit the conflict easily and lack authority to reclassify it themselves.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    moderate, biographical, trapped, regional).

% Occupy territory where conflict-type classification is most contested (international if foreign state backing is detected; non-international if it remains internal). Their protection status oscillates with how proportionality is applied at each moment. They benefit theoretically from Geneva protection floors; they pay through uncertainty about whether and when those protections apply.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_border_populations, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_border_populations, beneficiary).

% Individuals whose combatant status is ambiguous under the proportionality standard (e.g., part-time fighters, support personnel, sympathizers in armed group areas). The hybrid reading's discretion in proportionality analysis determines whether they are targetable or protected; their identity as 'fighter' or 'civilian' becomes a judgment call made by the stronger party in real time, often post-hoc.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, ambiguously_combatant_persons, payer,
    powerless, biographical, identity_locked, local).

% International courts, treaty bodies, and legal academia interpret Geneva proportionality doctrine. They benefit from the constraint because it preserves interpretive authority: every conflict generates novel proportionality questions that require legal judgment, keeping international law as the arbiter of protection scope rather than shifting to bright-line mechanical rules.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_legal_interpreters, beneficiary,
    institutional, generational, analytical, global).

% States with limited military resources or facing powerful state adversaries. The proportionality analysis is nominally symmetric (both sides apply it), but in practice the stronger state's interpretation of proportionality and conflict classification is more credible to international observers, shifting the protective scope in their favor.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties, payer,
    moderate, biographical, constrained, national).

% International Criminal Court, fact-finding missions, human rights bodies that investigate IHL violations. They document the constraint's operation and can challenge parties' classification and proportionality judgments, but lack power to force reclassification mid-conflict without consensus among powerful state parties.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_enforcement, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_military_authority).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a scalable legal framework for armed conflict that can adapt to different conflict types (international vs non-international) and contexts, allowing parties to know which protection standards apply. Proportionality analysis coordinates parties' obligations to limit harm by making the relationship between military necessity and protective scope explicit and contestable through legal interpretation rather than leaving it to unilateral discretion.
% TRANSFER_FUNCTION: Shifts interpretive authority to the stronger party or to state military establishments: conflict-type classification and proportionality analysis are nominally neutral legal tasks, but control over these judgments transfers legal legitimacy and operational freedom from weaker parties. Weaker armed groups and civilian populations in disputed territories bear the cost of classification ambiguity; states that can credibly claim their interpretation is the 'correct' legal reading transfer compliance burdens to opponents.
% ABSENT_VOICES: Non-state armed groups, stateless persons, and civilians in disputed territories lack formal standing in treaty interpretation; they would argue for bright-line protections that do not depend on conflict classification or proportionality calculus, but are excluded from the process that determines Geneva scope. Their objection would be that discretionary legal interpretation is weaponized against them.
% DISAPPEARANCE_RATIONALE: If the hybrid proportionality reading vanished and protections were instead fixed by bright-line conflict classification (state_centric_reading) or universal application (universal_rights_reading), every state party would face different legal obligations, enforcement pressure would shift from interpretation to mechanical rule application, and non-state armed groups would gain certainty about their status. The current constraint's disappearance would collapse the interpretive flexibility that allows state parties to navigate between different protection regimes.
% FOUNDING_PROBLEM: Early armed conflicts were wholly international (state vs state) and Geneva I/II applied cleanly. By the mid-20th century, wars of national liberation and internal armed conflicts became the norm, but Geneva treaty language remained state-centric. The founding problem was: how do you extend protections to non-international conflicts without abandoning the state-centric framework? The proportionality/conflict-classification apparatus emerged as a compromise that could fit new conflict types without requiring states to rewrite the treaties.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross (outside the states executing the interpretation) and independent international legal scholars attest the founding problem was structurally real: expanding conflict types demanded legal adaptation. However, human rights organizations and non-state armed group advocates attest that the foundational problem is now SOLVED (proportionality analysis exists; protections have been extended), and the apparatus persists as a mechanism for selective interpretation. State parties and the ICJ attest the proportionality analysis is still necessary because each conflict remains contextually unique.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.45 → 0.68 over 50 time units, then plateau) models the learning curve: early in a conflict, proportionality analysis is genuinely contestable and protective scope is negotiated; over time, the stronger party's interpretation becomes entrenched and the extraction—the shift of compliance burdens to weaker parties—accumulates and stabilizes. Theater ratio rises steeply at first (0.18 → 0.42) as the proportionality framework is invoked rhetorically to justify operational decisions that were already made on military grounds, then plateaus as the interpretive legitimacy settles. Suppression requirement rises steadily (0.58 → 0.74) because maintaining the hybrid reading requires continuous active defense against the universal_rights_reading (which would remove state discretion) and the state_centric_reading (which would clarify that NSAGs fall outside scope entirely); the proportionality framework must be defended as the middle way. Accessibility collapse is high (0.71) because once a party's conflict classification is accepted by major powers, alternatives (claiming the conflict is different, demanding bright-line protections) become extremely difficult to advance. Resistance is moderate-high (0.58) because non-state armed groups, human rights bodies, and some legal scholars actively contest the hybrid reading's discretion, though they cannot override state interpretation mid-conflict. The claim/metric independence is intentional: this constraint is CLAIMED as tangled_rope (it genuinely solves a coordination problem—how to scale protections to new conflict types—while also extracting through discretion), and the metrics honestly reflect that it operationally functions as increasingly extractive as the conflict lengthens and stronger parties entrench their reading.
 *
 * PERSPECTIVAL GAP:
 *   State agenda-setters perceive this as proportionality-driven legal reasoning that adapts Geneva protections to complex conflicts. Weaker parties perceive it as selective interpretation that concentrates protective authority in the hands of the stronger party. The engine's per-seat computation should surface this: strong institutional seats compute one type (likely Rope or low-extraction Tangled Rope from their position); weak seats compute a higher-extraction type (Snare-adjacent or high-extraction Tangled Rope). This divergence is the measurement itself—it surfaces how the same legal framework operates asymmetrically across power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   State military authority: d ≈ 0.15 (beneficiary)—they set the interpretive terms, classify their own conflicts, apply proportionality in operational decisions, and experience low extraction because they control the classification. International legal interpreters: d ≈ 0.20 (beneficiary)—they hold interpretive authority; the constraint preserves their role as arbiters of proportionality. Non-state armed groups: d ≈ 0.85 (target)—they are the objects of proportionality analysis, classified by others, and cannot exit without ending their existence as organized entities; identity-locked to combatant status regardless of how proportionality is applied. Civilian border populations: d ≈ 0.68 (target)—they benefit from the existence of Geneva protections (genuine coordination gain) but bear the cost of oscillating protection status (extraction); constrained exit and powerless position. Ambiguously combatant persons: d ≈ 0.90 (target)—they are most vulnerable to proportionality discretion; their targetability is determined by legal analysis they do not control; identity-locked as 'fighter-or-civilian' depending on the analysis. Weaker state parties: d ≈ 0.55 (mixed)—they benefit from the flexibility of proportionality when they frame it, but suffer when they are the analyzed party. The directional overrides should not be necessary here because the beneficiary/victim declarations capture the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adapting Geneva protections to non-international conflicts without rewriting the treaties) remains LIVE but CONTESTED. The state_centric_reading asserts the problem is SOLVED because AP II and CA3 exist; the universal_rights_reading asserts the problem is now DEAD because human rights law provides universal protections. The hybrid_proportionality_reading straddles both: it asserts the founding problem remains live because proportionality analysis is genuinely needed for contextual application, but this claim is increasingly challenged by evidence that proportionality is applied inconsistently and often instrumentally to justify predetermined operational decisions. The constraint avoids misclassification as pure Rope (which would require stronger proof of genuine coordination benefit over time) because the theater_ratio trajectory and suppression requirement plainly show that the proportionality framework is being defended primarily against alternative readings, not against external coordination failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_instrument_vs_constraint,
    'Is proportionality analysis a genuine legal constraint on the stronger party''s conduct, or a post-hoc justification for decisions made on military grounds?',
    'Comparison of military objectives declared ex-ante vs. proportionality justifications offered post-incident; pattern analysis of whether proportionality rulings ever override military advantage; examination of whether weaker parties'' proportionality arguments receive equal weight in international adjudication.',
    'If proportionality is genuinely constraining, the hybrid reading is legitimate Tangled Rope (real coordination plus real constraint on extraction). If instrumentally deployed, it is Snare (extraction mechanism wearing legal justification). Classification hinges on whether the constraint actually limits stronger parties'' behavior or merely legitimizes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_instrument_vs_constraint, empirical, 'Whether proportionality analysis functions as legal constraint or justification mechanism').

omega_variable(
    conflict_classification_determinism,
    'Is conflict classification (international vs non-international) determinate from the facts of the conflict, or is it a contestable interpretation that different parties can credibly maintain simultaneously?',
    'Case study of conflicts where classification remained contested until resolution (e.g., Afghanistan, Yemen, Syria): if classification were deterministic, international consensus would emerge; if contestable, multiple readings would persist throughout the conflict.',
    'If deterministic, the hybrid reading is legitimate because proportionality analysis applies to a fixed conflict type. If contestable, protective scope oscillates with classification disputes, and weaker parties face moving targets for their protection status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_classification_determinism, conceptual, 'Whether conflict classification is fact-determined or interpretation-dependent').

omega_variable(
    state_discretion_vs_universal_principles,
    'Does the hybrid proportionality reading preserve legitimate state flexibility in tailoring protections to context, or does it institutionalize selective interpretation that benefits stronger parties?',
    'Analysis of whether non-state parties (armed groups, human rights bodies) ever successfully contest state proportionality rulings; whether international enforcement mechanisms have authority to override state classifications.',
    'If non-state contestation succeeds even occasionally, the constraint preserves genuine multi-party negotiation. If state interpretations are routinely uncontested, the flexibility is asymmetric and the constraint is extractive regardless of its stated purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_discretion_vs_universal_principles, preference, 'Whether state flexibility in proportionality is legitimate tailoring or asymmetric extraction').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the hybrid_proportionality_reading and the state_centric_reading genuinely coexist as live positions, or does the hybrid reading''s interpretive machinery foreclose the state_centric bright-line approach by making every conflict classification context-dependent?',
    'Examination of whether states that prefer the state_centric reading (strict combatant/civilian distinction, no proportionality override) can maintain that position consistently within the same treaty framework, or whether the proportionality requirement forces convergence on hybrid analysis.',
    'If they coexist, both readings are sustainable within the treaty framework. If hybrid forecloses state_centric, then the kernel has evolved toward the hybrid reading and the sibling relation should be updated to foreclosure rather than coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether hybrid_proportionality and state_centric readings logically coexist or foreclose each other').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the measured suppression (0.74) primarily structural (enforcement machinery defending Geneva interpretation against treaty review/amendment) or internalized (combatants and civilians have internalized the proportionality framework as legitimate and resist alternatives)?',
    'Comparison of suppression trajectories before and after major international rulings on proportionality (e.g., ICC decisions, UN General Assembly votes): if suppression drops after a ruling against the dominant reading, suppression is structural; if it persists, it is internalized.',
    'If structural, removing the enforcement machinery (treaty renegotiation, ICC delegitimation) would allow alternative readings to emerge. If internalized, the proportionality framework has become self-perpetuating even if formal enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether suppression of alternative readings is structural or internalized').

omega_variable(
    founding_problem_obsolescence_dispute,
    'Has the founding problem (adapting Geneva protections to non-international conflicts) genuinely been solved by the existence of AP II and CA3, or does the founding problem persist because AP II/CA3 remain under-enforced and proportionality discretion leaves non-international victims unprotected?',
    'Comparative analysis of protection outcomes in AP I conflicts (international armed conflicts) vs AP II/CA3 conflicts (non-international): if outcomes are equivalent, the founding problem is solved; if AP II/CA3 conflicts show consistently lower protection compliance and enforcement, the problem persists despite the treaties.',
    'If solved, the constraint persists as legitimate adaptation. If unsolved, the constraint is a zombie—maintaining the appearance of scaling while leaving non-international conflicts inadequately protected, suggesting the founding_problem_status should be ''dead'' and disappearance_verdict ''world_rearranges'' point toward mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_dispute, empirical, 'Whether the founding problem (adapting protections to non-international conflicts) is genuinely solved or persists despite AP II/CA3').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t7, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 7, 0.22).
narrative_ontology:measurement_basis(gene_tr_t7, observed).
narrative_ontology:measurement(gene_tr_t14, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(gene_tr_t14, observed).
narrative_ontology:measurement(gene_tr_t21, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 21, 0.35).
narrative_ontology:measurement_basis(gene_tr_t21, observed).
narrative_ontology:measurement(gene_tr_t28, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(gene_tr_t28, observed).
narrative_ontology:measurement(gene_tr_t35, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(gene_tr_t35, observed).
narrative_ontology:measurement(gene_tr_t42, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 42, 0.42).
narrative_ontology:measurement_basis(gene_tr_t42, observed).
narrative_ontology:measurement(gene_tr_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(gene_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t7, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement_basis(gene_be_t7, observed).
narrative_ontology:measurement(gene_be_t14, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement_basis(gene_be_t14, observed).
narrative_ontology:measurement(gene_be_t21, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 21, 0.64).
narrative_ontology:measurement_basis(gene_be_t21, observed).
narrative_ontology:measurement(gene_be_t28, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement_basis(gene_be_t28, observed).
narrative_ontology:measurement(gene_be_t35, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(gene_be_t35, observed).
narrative_ontology:measurement(gene_be_t42, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement_basis(gene_be_t42, observed).
narrative_ontology:measurement(gene_be_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(gene_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t7, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 7, 0.63).
narrative_ontology:measurement_basis(gene_su_t7, observed).
narrative_ontology:measurement(gene_su_t14, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement_basis(gene_su_t14, observed).
narrative_ontology:measurement(gene_su_t21, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement_basis(gene_su_t21, observed).
narrative_ontology:measurement(gene_su_t28, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 28, 0.73).
narrative_ontology:measurement_basis(gene_su_t28, observed).
narrative_ontology:measurement(gene_su_t35, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 35, 0.74).
narrative_ontology:measurement_basis(gene_su_t35, observed).
narrative_ontology:measurement(gene_su_t42, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 42, 0.74).
narrative_ontology:measurement_basis(gene_su_t42, observed).
narrative_ontology:measurement(gene_su_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement_basis(gene_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% The geneva_conventions_protective_scope kernel decomposes into three constraint stories representing three live readings of how Geneva protections scale by conflict type. The hybrid_proportionality_reading (this story) stakes proportionality analysis as the mechanism for scaling; the state_centric_reading treats Article 4 combatant criteria as the scaling gate; the universal_rights_reading treats human dignity as the non-negotiable floor regardless of combatant status. Each reading has a different ε (hybrid is moderate-high because proportionality discretion enables selective interpretation; state_centric is lower because bright-line rules reduce ambiguity; universal_rights is lower because universal application removes classification disputes). The three stories are linked via this network edge array so constraint-family analysis can track how empirical drift in one reading affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
