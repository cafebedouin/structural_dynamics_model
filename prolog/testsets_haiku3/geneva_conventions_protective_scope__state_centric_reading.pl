% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope — State-Centric Reading
 *   domain: international/legal/armed_conflict
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of the Geneva
 *   Conventions' protective scope: international humanitarian law protections
 *   apply to uniformed combatants operating under responsible command and
 *   meeting Article 4 criteria; non-uniformed armed actors and irregular
 *   belligerents fall outside the treaty's scope and may be targeted as
 *   unlawful combatants without combatant immunity. This reading narrows the
 *   victim set relative to universal-rights and hybrid-proportionality
 *   readings, benefiting conventional state militaries engaged in asymmetric
 *   conflicts while imposing extraction on non-uniformed actors and civilian
 *   populations in conflict zones. The constraint is claimed as tangled_rope
 *   (genuine coordination function + asymmetric extraction) because it solves
 *   a real problem — distinguishing lawful combatants under interstate law —
 *   while extracting by narrowing protections and permitting targeting of
 *   those excluded from the scope.
 *
 * KEY AGENTS:
 *   - Conventional state militaries: institutional power, operate globally, benefit from Article 4 distinction, set enforcement agenda
 *   - Non-uniformed armed actors (insurgents, militias, non-state forces): powerful locally, trapped exit, no POW status under this reading, face targeting without immunity
 *   - Civilian populations in asymmetric zones: powerless, immediate horizon, caught between state targeting and non-uniformed actor presence
 *   - ICRC and humanitarian bodies: institutional, analytical seat, interpret and monitor but do not enforce the state-centric reading
 *   - Human rights advocates and universal-rights scholars: excluded from treaty authority, contest the narrowing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope — State-Centric Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international/legal/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '328b6802-046f-448e-b624-7bb1d8bbdece').
narrative_ontology:cs_kernel_codification('328b6802-046f-448e-b624-7bb1d8bbdece', fixed_text).
narrative_ontology:cs_authority_grounding('328b6802-046f-448e-b624-7bb1d8bbdece', extraction).
narrative_ontology:cs_interpretation_layer_present('328b6802-046f-448e-b624-7bb1d8bbdece').
narrative_ontology:cs_reading_relation('328b6802-046f-448e-b624-7bb1d8bbdece', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('328b6802-046f-448e-b624-7bb1d8bbdece', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('328b6802-046f-448e-b624-7bb1d8bbdece', foundational, uniformed_combatant_status_reciprocal_protection).
narrative_ontology:cs_axiom_status(uniformed_combatant_status_reciprocal_protection, holdable).
narrative_ontology:cs_axiom_grounding('328b6802-046f-448e-b624-7bb1d8bbdece', uniformed_combatant_status_reciprocal_protection, conventional).
narrative_ontology:cs_axiom('328b6802-046f-448e-b624-7bb1d8bbdece', foundational, article_four_criteria_exclude_non_uniformed).
narrative_ontology:cs_axiom_status(article_four_criteria_exclude_non_uniformed, holdable).
narrative_ontology:cs_axiom_grounding('328b6802-046f-448e-b624-7bb1d8bbdece', article_four_criteria_exclude_non_uniformed, conventional).
narrative_ontology:cs_reference_frame('328b6802-046f-448e-b624-7bb1d8bbdece', interstate_reciprocal_protection_framework).
narrative_ontology:cs_drift_state('328b6802-046f-448e-b624-7bb1d8bbdece', post_asymmetric_conflict_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('328b6802-046f-448e-b624-7bb1d8bbdece', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_uniformed_armed_actors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, state_sovereignty_in_armed_conflict_law).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, lawful_combatant_distinction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State armed forces that operate under recognizable command structures, wear uniforms, and carry arms openly. They benefit from the protective scope because it grants their uniformed combatants prisoner-of-war status if captured, immunity from prosecution for lawful acts of war, and a clear distinction between lawful targeting and war crimes. They set the scope by advocating for strict Article 4 criteria in treaty interpretation and enforcement.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Non-state armed groups, militias, insurgents, and irregular fighters who do not meet the Article 4 criteria (lack uniform, lack centralized command, or lack visible insignia). Under this reading they fall outside Geneva protections and can be targeted as unlawful combatants without the right to prisoner status or combatant immunity. Their exit option is to dissolve or transform into state-aligned forces, but both are structurally trapped outcomes.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_uniformed_armed_actors, payer,
    powerful, biographical, trapped, regional).

% Ordinary people in territories where non-uniformed armed actors operate. They bear the direct cost: state military operations targeting non-uniformed combatants often produce civilian casualties and displacement; the scope reading permits this because the targeting is justified by the combatants' lack of lawful status. They nominally benefit from humanitarian law protections as civilians, but enforcement is weak where non-uniformed actors control territory.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_zones, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_zones, beneficiary).

% The International Committee of the Red Cross, treaty bodies, and UN human rights mechanisms that interpret and apply the Geneva Conventions. They monitor implementation, investigate violations, and publish guidance. They occupy an analytical seat because they interpret the constraint without directly enforcing it, though their interpretations influence state compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% NGOs, legal scholars, and advocates who argue for universal rights and broader protections for all persons affected by armed conflict. They would contest the narrowing of victim scope and argue for extending protections to non-uniformed actors, but they are structurally excluded from treaty interpretation authority — states control the Geneva Conventions and their readings.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocacy_organizations, excluded,
    organized, biographical, constrained, global).

% Civilians and resistance actors in occupied territories. They would argue for broader protections and recognition of resistance legitimacy, but the state-centric reading limits their voice and the protections available to those who resist without meeting Article 4 criteria. Their exclusion from the treaty interpretation conversation is structural.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, occupied_populations, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, mutually-recognized distinction between lawful and unlawful combatants, enabling state militaries to conduct targeting and maintain command discipline under international law. Creates a common legal framework that permits uniformed forces to operate under known rules, with protection for captives, and immunity for actions taken within the law of war.
% TRANSFER_FUNCTION: Moves protective status and immunities from non-uniformed actors to state-uniformed forces. Non-uniformed combatants are classified as unlawful belligerents and lose prisoner-of-war status, right to trial, and immunity from targeting — even civilians may be targeted if they directly participate in hostilities. States gain operational clarity and legitimacy for counter-insurgency operations.
% ABSENT_VOICES: Non-state actors, resistance movements, and civilian populations in asymmetric conflicts have no seat at the treaty-interpretation table. Human rights advocates and universal-rights scholars argue the scope is too narrow, but treaty authority rests with state parties, making this reading path-dependent on state preference.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and universal protections extended to all armed actors regardless of uniform or command structure, state counter-insurgency operations would face legal liability for targeting fighters in civilian dress, captured combatants would have POW status regardless of insignia, and asymmetric conflicts would be governed differently — the incentive structure for state military operations would shift fundamentally.
% FOUNDING_PROBLEM: The Geneva Conventions were drafted to regulate interstate warfare between uniformed state militaries. The original problem was preventing atrocities in conventional war and establishing reciprocal protections for captured soldiers. The Article 4 criteria codified the distinction that made this reciprocal protection possible: states protect uniformed soldiers of other states in exchange for reciprocal protection of their own.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and military-legal scholars attest the founding problem remains live: asymmetric threats justify careful distinction between lawful and unlawful combatants. Human rights bodies and universal-rights advocates attest the founding problem has been superseded by the rise of non-state armed conflict, and that extending protections to all affected persons is now required by international human rights law and Common Article 3 minimum standards. Legislative records and ICRC guidance documents support both readings; disagreement is genuine rather than evidentiary.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the scope reading narrows who receives Geneva protections, enabling states to conduct counter-insurgency operations against non-uniformed combatants with legal justification. Suppression (0.72) is higher because enforcing the Article 4 distinction requires actively excluding non-uniformed actors from protections — the enforcement machinery exists to maintain the boundary. Theater (0.41) is moderate: the distinction between lawful and unlawful combatants serves a real coordination function (state militaries operate more effectively under clear rules), but an increasing share of conflict involves asymmetric warfare where the distinction is contested and enforcement theater (military legal advisors, rules-of-engagement briefs) often precedes or substitutes for actual protection. The measurement series shows modest upward drift in both extractiveness and suppression over the interval: as asymmetric conflicts dominate state military operations (interval 0–40 corresponds to the rise of counter-insurgency post-2001), the state-centric reading is invoked more frequently to justify targeting of non-uniformed actors, and suppression of alternative readings intensifies. Theater ratio climbs steeper, indicating growing performance relative to coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The state military seat and the non-uniformed-actor seat should compute to different types: from the state military position (institutional power, global scope, arbitrage exit), the arrangement is largely coordination with acceptable asymmetry; from the trapped non-uniformed actor position (powerful locally, immediate horizon, no legal status), the same structure operates as pure extraction. The powered-organizational observer (human rights bodies) sits between: they see both coordination and extraction but lack enforcement authority to shift the reading. The engine computes these divergences from the structural data — the author's claim (tangled_rope) is independent of the computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries are beneficiaries (d ~ 0.15): they set the scope, collect operational and legal benefits, have arbitrage exit (can shift to other legal frameworks or alternative conflict narratives), and operate at global institutional scale. Non-uniformed actors are targets (d ~ 0.85): they bear the extraction (loss of POW status, lawful-targeting vulnerability, exclusion from protections), are trapped locally, and have no standing in treaty interpretation. Civilians in asymmetric zones carry mixed d (~0.50): they nominally benefit from civilian-protection status under humanitarian law, but the state-centric reading permits targeting of non-uniformed combatants in civilian-dense areas, creating diffuse costs. Human rights advocates have no d (role=excluded) — they contest the reading but do not have a structural relationship to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading avoids simple mandatrophy (founding problem dead, constraint persists as theater) by maintaining that the problem it was built to solve — distinguishing lawful combatants in interstate war — remains live. However, the contested founding_problem_status indicates that asymmetric conflicts and the rise of non-state actors have altered the landscape: the reading's core premise (reciprocal state protection) applies cleanly only in interstate scenarios, which are rare. Where non-uniformed actors dominate, the constraint persists by institutional inertia and state preference rather than by genuine operational necessity. The measurement series show theater_ratio climbing to 0.41 by interval end, indicating theater growth without extraction decline — a slow mandatrophy trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Does the founding problem — the need to distinguish lawful combatants under interstate law — remain live in a world dominated by asymmetric armed conflicts involving non-state actors?',
    'Empirical analysis of conflict types in the interval 0–40: proportion of interstate wars versus non-international armed conflicts; frequency of interstate reciprocal POW exchanges versus unilateral detention of non-uniformed actors; whether states invoke Article 4 criteria in practical enforcement or treat it as secondary to force-protection objectives.',
    'If asymmetric conflicts now dominate and states rarely rely on Article 4 criteria for reciprocal protection, the founding problem is substantially dead and the constraint becomes mandatrophic (persists by inertia, theater increases). If interstate conflicts remain significant and reciprocal protection remains operative, the founding problem is live and the constraint retains coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the constraint''s original founding problem remains operative in contemporary armed conflict.').

omega_variable(
    state_centric_vs_universal_axiom_contradiction,
    'Is the state-centric axiom (only uniformed combatants under responsible command receive protective status) logically foreclosed by the universal-rights axiom (all persons affected by armed conflict receive a minimum humanitarian floor), or do they coexist as different parties'' live commitments without contradiction?',
    'Legal analysis: do the Geneva Conventions'' Article 4 and Common Article 3 represent two compatible standards applied at different scopes (AP I, AP II, non-international), or are they fundamentally incompatible claims about who deserves protection? Examine whether a single legal framework can hold both axioms without paradox.',
    'If incompatible (foreclosure), the state-centric reading genuinely excludes the universal reading from any unified framework, and the constraint classification stands as tangled_rope. If compatible (coexistence), the readings are alternative positions held by different treaty parties/interpreters, and the constraint is more accurately described as a rope with asymmetric interpretation rather than pure tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_vs_universal_axiom_contradiction, conceptual, 'Whether state-centric and universal-rights axioms logically foreclose each other or coexist as alternative readings.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the high extractiveness (0.68) measuring the cost of necessary coordination (distinguishing lawful combatants, establishing reciprocal protection), or is it measuring the cost of state preference to exclude non-uniformed actors from protections?',
    'Counterfactual: if the same combatant distinction were applied but non-uniformed actors received minimum humanitarian protections (e.g., Common Article 3 without POW status), would extractiveness drop? If yes, the high value measures state preference for exclusion, not coordination cost. If no, the high value reflects coordination necessity.',
    'If preference, the constraint is closer to snare than tangled_rope, and the beneficiary set is narrower (only state militaries, not all who benefit from coordinated protection). If coordination necessity, the tangled_rope classification holds and the extraction is justified as the cost of clarity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether extractiveness measures coordination cost or state preference for exclusion.').

omega_variable(
    suppression_mechanism_structural,
    'Is the measured suppression (0.72) structural (legal barriers to alternative readings, excluding non-uniformed voices from treaty interpretation) or internalized (states and military establishments have absorbed the state-centric framing as natural/inevitable)?',
    'Post-suppression scenario: if treaty interpretation authority were distributed to non-state actors and asymmetric-conflict populations, would the state-centric reading lose adherents, or has it become entrenched as legitimate law? If the former, suppression is structural; if the latter, it is internalized.',
    'If structural, the constraint''s effective suppression is as authored (0.72) and depends on active exclusion of alternative voices — removal of legal barriers could shift the reading quickly. If internalized, the constraint carries suppression even when barriers are removed, requiring deeper institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether suppression of alternative readings is structural or internalized.').

omega_variable(
    article_four_operationality,
    'In practical military enforcement, do state forces actually distinguish uniformed versus non-uniformed combatants using Article 4 criteria, or is the distinction primarily a post-hoc legal rationalization for targeting decisions made on other grounds?',
    'Military legal review documents, rules-of-engagement briefs, after-action reports: frequency of Article 4 analysis in targeting decisions; correlation between combatant uniform/insignia and actual targeting; whether targeting decisions precede or follow legal classification.',
    'If operationally determinative, Article 4 distinction is a real constraint on state action. If post-hoc rationalization, the constraint''s coordination function is degraded and theater_ratio should be higher — the suppression and extractiveness are maintained by legal framing without corresponding operational discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_four_operationality, empirical, 'Whether Article 4 criteria drive or rationalize state targeting decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(gene_tr_t5, observed).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(gene_tr_t10, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(gene_tr_t25, observed).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(gene_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(gene_be_t5, observed).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(gene_be_t10, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(gene_be_t25, observed).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(gene_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(gene_su_t5, observed).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(gene_su_t10, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(gene_su_t25, observed).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(gene_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel: geneva_conventions_protective_scope. The kernel is the stabilized text and interpretive tradition of the Geneva Conventions concerning protective scope. Three structurally distinct constraints instantiate three readings: state_centric_reading (this story, narrows scope to Article 4 uniformed combatants), universal_rights_reading (extends protections to all affected persons), and hybrid_proportionality_reading (scope scales by conflict type). Each has different beneficiary/victim sets, different ε values, and different operative barriers. They form a constraint family linked by network.affects_constraints. The state-centric reading influences (but does not foreclose) the proportionality reading; it forecloses the universal-rights reading's core premise within a reciprocal-protection framework. See commentary.kernel_context and cs_structure for family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
