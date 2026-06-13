% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Keyboard Layout: Manufacturer Lock-in via Cartel Standardization
 *   domain: economic/technological/labor
 *
 * SUMMARY:
 *   QWERTY keyboard layout persistence is commonly explained as
 *   path-dependent accident: an early design choice that became too
 *   entrenched to displace. This reading instantiates the alternative: QWERTY
 *   persistence is manufacturer-engineered lock-in via coordinated cartel
 *   action, training-school partnerships, and deliberate suppression of
 *   superior alternatives. The constraint solves a real collective-action
 *   problem (typewriter interoperability, 1893) but persists long after that
 *   problem is solved, via cartel enforcement of the standard through
 *   supply-chain control and educational capture. By the 1930s–1950s, the
 *   founding coordination function had degraded; ergonomic evidence showed
 *   alternatives superior (Dvorak, 1936 onwards); but manufacturers and
 *   schools enforced QWERTY exclusivity, imposing retraining barriers on
 *   workers. The kernel contest: does QWERTY persistence emerge from
 *   path-dependent accident (sibling reading: path_dependency_reading) or
 *   from strategic manufacturer lock-in? This reading asserts strategic
 *   lock-in: the cartel's coordination was real in 1893, but the enforcement
 *   machinery persisted as extraction long after coordination was no longer
 *   needed. Victims are typists bearing ergonomic costs and identity-locked
 *   into retraining barriers; beneficiaries are the 1893–1950s cartel members
 *   extracting rents from design standardization control.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers_1893_cartel: organized institutional agenda-setter with arbitrage-level exit options—could switch standards but chose to enforce QWERTY for market control
 *   - typists_bearing_ergonomic_cost: powerless biographical-horizon payers, identity-locked into QWERTY through professional credential investment
 *   - workers_requiring_retraining: moderate-power moderate-horizon payers facing artificially raised retraining barriers
 *   - touch_typing_schools: organized dual-positioned agents (beneficiary + agenda-setter)—benefited from standardized curriculum and employer demand, enforced the standard by excluding alternatives
 *   - alternative_design_inventors: excluded moderate-power agents with trapped exit—technically superior designs but no market path
 *   - employers_of_typists: powerful beneficiaries locked into enforcement through hiring preferences and labor-pool standardization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Layout: Manufacturer Lock-in via Cartel Standardization").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "economic/technological/labor").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'f11c89c4-82cf-4f42-a417-76f3613be1d0').
narrative_ontology:cs_kernel_codification('f11c89c4-82cf-4f42-a417-76f3613be1d0', distributed).
narrative_ontology:cs_authority_grounding('f11c89c4-82cf-4f42-a417-76f3613be1d0', extraction).
narrative_ontology:cs_reading_relation('f11c89c4-82cf-4f42-a417-76f3613be1d0', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('f11c89c4-82cf-4f42-a417-76f3613be1d0', foundational, manufacturers_deliberate_standardization_strategy).
narrative_ontology:cs_axiom_status(manufacturers_deliberate_standardization_strategy, holdable).
narrative_ontology:cs_axiom_grounding('f11c89c4-82cf-4f42-a417-76f3613be1d0', manufacturers_deliberate_standardization_strategy, empirically_contingent).
narrative_ontology:cs_axiom('f11c89c4-82cf-4f42-a417-76f3613be1d0', secondary, cartel_suppressed_alternatives_after_coordination_solved).
narrative_ontology:cs_axiom_status(cartel_suppressed_alternatives_after_coordination_solved, holdable).
narrative_ontology:cs_axiom_grounding('f11c89c4-82cf-4f42-a417-76f3613be1d0', cartel_suppressed_alternatives_after_coordination_solved, empirically_contingent).
narrative_ontology:cs_reference_frame('f11c89c4-82cf-4f42-a417-76f3613be1d0', manufacturer_coordination_for_interoperability).
narrative_ontology:cs_drift_state('f11c89c4-82cf-4f42-a417-76f3613be1d0', post_dvorak_ergonomic_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f11c89c4-82cf-4f42-a417-76f3613be1d0', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_bearing_ergonomic_cost).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, workers_requiring_retraining).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (early coordination phase) to 0.68 (plateau by 1970) as the cartel shifts from solving coordination problems to collecting rents from design standardization control. The steepest rise occurs 1893–1930 (coordination → enforcement transition) and 1930–1950 (Dvorak alternative blocked → full cartel enforcement). By 1970, the constraint plateaus at 0.68: the cartel itself has dissolved but the institutional lock-in (school curricula, employer hiring norms, worker retraining costs) persists. Suppression rises in parallel: early suppression (0.25, 1893) reflects minimal competitive pressure; later suppression (0.72, 1980) reflects active enforcement against Dvorak and other alternatives—exclusion of alternative designs from school curricula, refusal of manufacturers to produce them, hiring market bias toward QWERTY. Theater ratio rising from 0.08 to 0.41 signals the constraint's function degrading: in 1893–1920, the 'security' and 'efficiency' frames were credible justifications; by 1950–1980, manufacturers' rhetoric about QWERTY's necessity persisted despite evidence of inferiority, indicating increasing performative maintenance. The measurement series tracks one shared time grid across all metrics to avoid the misalignment error (OQ-105). Theater and suppression_requirement rise together, indicating that as the founding coordination problem weakened, the cartel's enforcement activity had to compensate—more theater to justify what was no longer coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the cartel's seat (1893–1950), QWERTY is a legitimate coordination solution they built and maintain—they see rope. From the typist's seat, QWERTY is an imposed standard they cannot escape without career sacrifice—they see snare. The engine computes these divergent types from the per-seat directionality: cartel members have d ~ 0.1 (subsidy + control), deriving low extraction; typists have d ~ 0.9 (trapped cost-bearer), deriving high extraction. The same constraint produces different perceived types from different seats. This divergence is not error—it is the phenomenon the framework measures. The authored claimed_type (tangled_rope) sits between: rope-like in its genuine coordination phase (1893–1920), snare-like in its later enforcement phase (1930–1980). The engine's per-seat computation surfaces this evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   The cartel members (keyboard_manufacturers_1893_cartel, touch_typing_schools) sit at the beneficiary end (d ~ 0.1–0.2): they set the standard, enforced it, collected rents, and could exit to superior designs or new standards if alternatives were profitable. Their power and arbitrage options mean they are lightly affected by the constraint they control. Typists (typists_bearing_ergonomic_cost) sit at the target end (d ~ 0.85–0.95): they pay the ergonomic cost, cannot exit without retraining (identity-locked), and have no voice in standard-setting. Workers_requiring_retraining sit at moderate extraction (d ~ 0.65): they face retraining barriers but have more exit options than identity-locked typists. Employers_beneficiary sit near the beneficiary end despite being observers—they benefit from standardized labor but have no seat in standard-setting, so they don't actively defend the constraint. Alternative_design_inventors are excluded (trapped exit) but not targets—they bear no extraction, only the absence of opportunity. The directionality derivation is straightforward: beneficiaries have low d (arbitrage exit, power, no cost); victims have high d (identity-locked exit, power asymmetry, ergonomic cost). No overrides needed—the structural data generates the right directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status (dead) and disappearance_verdict (world_rearranges) mismatch signals mandate rot: the constraint was built to solve a problem (typewriter standardization) that was solved by ~1920, yet the constraint persists and enforces itself through cartel and institutional channels. The founding problem is dead but the constraint is not; instead, it has shifted function from coordination to rent extraction. This is the exact pattern mandatrophy flags: constraint persists after its mandate is fulfilled, enforced by the same machinery that once served coordination. The theater_ratio rise (0.08 to 0.41) corroborates: as the founding problem dies, the constraint's public justification becomes increasingly theatrical. The constraint shows piton characteristics (inertial enforcement, no dedicated beneficiary maintaining it against cost) beginning around 1950, when the cartel itself fragmented and QWERTY persisted via institutional habit rather than active cartel enforcement. However, the period 1893–1950 shows active tangled_rope enforcement—the beneficiary (cartel) is still defending the extraction. The claim of tangled_rope is accurate for the interval 1893–1950; by 1980 the constraint would reclassify as piton (theater high, extraction sustained by institutional inertia, no party actively profiting). The story captures the strategic lock-in reading's domain (1893–1950 when strategy was active); the full evolution to piton is documented in measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_coordination_vs_strategic_lock_in,
    'Did the manufacturers'' standardization activities in 1893–1920 constitute genuine coordination solving a real collective-action problem, or were they always strategically designed lock-in?',
    'Historical analysis of manufacturers'' internal documents, patent records, and correspondence. Distinguish between: (a) coordination justified by mechanical necessity (type-bar interference), and (b) strategic exclusion decisions (blocking alternative submissions to schools, refusing to license designs). The kernel contest: was the cartel''s enforcement machinery built to maintain coordination function or to extract rents after function was solved?',
    'If (a) dominates, the constraint''s early phase is genuine tangled_rope coordination with extraction cost. If (b) dominates from the beginning, the constraint is snare-with-cover. The terminal type (piton by ~1950) is stable either way, but the causal narrative differs: accident-driven drift (path_dependency_reading) vs. intentional capture (strategic_lock_in_reading, the reading you are instantiating).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_coordination_vs_strategic_lock_in, empirical, 'Whether standardization was coordination or strategic lock-in from inception').

omega_variable(
    ergonomic_cost_measurement,
    'What was the quantifiable ergonomic harm from QWERTY relative to available alternatives (Dvorak, etc.), and how much of that harm persisted because the cartel blocked alternatives?',
    'Occupational health literature (RSI studies, typing speed/error analysis), historical counterfactual: if Dvorak had been available to schools in 1930–1960, what injury rates and productivity gains would typists have experienced? Comparative studies on workers who learned alternatives despite cartel pressure.',
    'High measured ergonomic cost establishes the victim set''s extraction as substantial and involuntary. Low cost reduces the claim that typists bore genuine harm. The answer determines whether the constraint''s suppression is structural (workers cannot exit without retraining cost) or internalized (workers do not know alternatives exist or believe QWERTY inevitable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ergonomic_cost_measurement, empirical, 'Magnitude of ergonomic harm from QWERTY and its suppression via cartel lock-in').

omega_variable(
    path_dependency_vs_strategic_reading_boundary,
    'Can the same historical facts of QWERTY persistence be coherently described as both path-dependent accident (sibling reading) and strategic lock-in (this reading) within the same analytical framework, or do they require distinct frameworks?',
    'The kernel contest itself: if both readings can fit the same evidence, the kernel is genuinely ambiguous (distributed codification); if one reading''s evidence requires assumptions the other''s does not, the readings foreclose or influence each other. Committer structure: which reading is the ''real'' one depends on whether you privilege manufacturers'' stated intentions or workers'' actual experience.',
    'If they coexist, both readings remain live and the network links two constraint stories with identical beneficiary/victim sets but different causal frames. If one forecloses the other, the boundary lies at: intentional strategic design vs. unintended emergent pattern. This reading (strategic_lock_in) forecloses path_dependency if manufacturers'' documented coordination and deliberate exclusion of alternatives can be shown. Path_dependency forecloses this reading if the cartel''s decisions can be shown to follow market logic rather than strategic capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_reading_boundary, conceptual, 'Relationship between strategic lock-in and path-dependency readings of QWERTY persistence').

omega_variable(
    identity_locked_exit_mechanism,
    'Is the measured suppression on typists_bearing_ergonomic_cost (0.72 base) structural (economic retraining cost after job-switching) or identity-locked (professional identity fusion with typing skill), and does the distinction change the type classification?',
    'Post-departure trajectories: if workers who left typing or retired showed reduced RSI and no regret, the suppression was structural and reversible. If workers retained the constraint''s effect psychologically (belief in QWERTY inevitability, internalized deference to the standard), the suppression is internalized and identity-locked. Qualitative interviews with workers who attempted alternatives or left the field.',
    'Structural suppression: exit is materially costly but reversible; identity-locked suppression: exit carries psychological cost and the constraint''s effects persist post-exit. High identity-lock would elevate effective suppression above the 0.72 structural measure and support higher type severity (snare-ward). Low identity-lock keeps the measured suppression as structural and supports the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Structural vs. identity-locked suppression mechanism in typist dependency on QWERTY').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1893, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.08).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1930, 0.28).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1970, 0.41).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1980, 0.41).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.15).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1910, 0.35).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1980, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.25).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1910, 0.48).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1970, 0.71).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1980, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint (strategic_lock_in_reading) and qwerty_persistence_inevitability__path_dependency_reading are two readings of the same kernel: 'Why does QWERTY persist despite superior alternatives?' Both readings describe the same empirical outcome (QWERTY endurance from 1893–1980+) but diverge on causal mechanism: this reading privileges strategic manufacturer coordination and deliberate lock-in; the sibling reading privileges unintended path-dependent accumulation. The readings coexist (neither forecloses the other within current scholarship) and share identical beneficiary/victim sets and measured metrics. They are linked by kernel_id, not by mechanistic influence—the readings offer competing hypotheses about the same constraint, not sequential phases or causal dependencies. Consumers comparing them will find the dispute reducible to: evidence of manufacturers' intentional exclusion vs. evidence of emergent lock-in from market forces. Both readings have supportive historical evidence; the boundary between them is the question of strategic intent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
