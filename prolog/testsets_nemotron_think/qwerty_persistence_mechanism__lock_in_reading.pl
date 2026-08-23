% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Persistence via Path-Dependent Lock-In
 *   domain: economic/technological/historical
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, originally designed for mechanical
 *   typewriters to prevent jamming, persists as the dominant standard for
 *   digital keyboards despite the disappearance of its original functional
 *   rationale. The lock-in reading (Paul David, 1985) argues that QWERTY's
 *   persistence is a canonical case of path-dependent market failure: early
 *   historical accident gave QWERTY a head start, network effects and
 *   switching costs created a coordination equilibrium that no individual
 *   actor can profitably deviate from, and the resulting standard is
 *   collectively suboptimal — users bear typing inefficiency costs, but no
 *   single beneficiary extracts those costs. The constraint is the
 *   self-reinforcing coordination equilibrium itself, not an enforced rule.
 *   Its extractiveness is low because the costs are deadweight loss, not
 *   transfers; suppression is moderate because switching costs (retraining,
 *   compatibility) suppress alternatives without coercion; theater is low
 *   because there is no performative maintenance of a defunct function — the
 *   coordination function (interoperability) is real, just suboptimal.
 *
 * KEY AGENTS:
 *   - early_typists: Adopters of the first commercial typewriters; locked into QWERTY by training investment (powerless/identity_locked)
 *   - typewriter_manufacturers: Standardized on QWERTY early; benefited from installed base but did not actively suppress alternatives (organized/constrained)
 *   - touch_typists: Professionals with QWERTY-specific human capital; benefit individually from not retraining but collectively worse off (moderate/identity_locked)
 *   - dvorak_advocates: Proponents of technically superior alternative; excluded by network effects (excluded/trapped)
 *   - economic_historians: Analytical observers debating the lock-in hypothesis (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.3).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence via Path-Dependent Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, 'd3cf1ea9-8a40-4a70-b3c1-d7a79eb69198').
narrative_ontology:cs_kernel_codification('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', distributed).
narrative_ontology:cs_authority_grounding('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', expertise).
narrative_ontology:cs_reading_relation('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', foundational, path_dependence_causes_suboptimal_lock_in).
narrative_ontology:cs_axiom_status(path_dependence_causes_suboptimal_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', path_dependence_causes_suboptimal_lock_in, empirically_contingent).
narrative_ontology:cs_axiom('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', foundational, network_effects_override_technical_superiority).
narrative_ontology:cs_axiom_status(network_effects_override_technical_superiority, holdable).
narrative_ontology:cs_axiom_grounding('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', network_effects_override_technical_superiority, empirically_contingent).
narrative_ontology:cs_reference_frame('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', historical_accident_initial_condition).
narrative_ontology:cs_drift_state('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d3cf1ea9-8a40-4a70-b3c1-d7a79eb69198', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, touch_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, early_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, touch_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, path_dependence_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, network_effects_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First generation of professional typists; invested heavily in QWERTY motor skills. The layout became their professional identity; retraining would mean loss of livelihood and status. They bear the cost of the suboptimal layout daily but cannot exit without career disruption.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, early_typists, payer,
    powerless, biographical, identity_locked, global).

% Early manufacturers (Remington, etc.) adopted QWERTY as a de facto standard. They benefited from the installed base of typists and did not need to actively suppress alternatives — network effects did the work. They could have switched to a superior layout but would lose compatibility with the typist pool.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typewriter_manufacturers, agenda_setter,
    organized, biographical, constrained, global).

% Contemporary professional typists and knowledge workers. They bear the ongoing efficiency cost of QWERTY (slower speeds, higher error rates) but benefit from not having to retrain and from universal compatibility. Their QWERTY skill is fused to their professional identity; exit is personally costly.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, touch_typists, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, touch_typists, beneficiary).

% Proponents of the Dvorak Simplified Keyboard (and later alternatives like Colemak). They argue for technical superiority but face insurmountable switching costs: no employer will buy Dvorak hardware, no OS makes it default, no colleague can use their keyboard. They are structurally excluded from the coordination equilibrium.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, dvorak_advocates, excluded,
    powerless, biographical, trapped, global).

% Scholars (Paul David, Liebowitz & Margolis, etc.) who study QWERTY as a test case for path dependence vs. market efficiency. They have no material stake; their exit is analytical — they can change their interpretation without cost.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal key layout so that any typist can use any keyboard, any document can be typed on any machine, and training investments are portable across employers and devices.
% TRANSFER_FUNCTION: No transfer occurs. The constraint imposes a deadweight loss: all typists type slower and with more errors than they would on a superior layout, but no party collects the difference. The social cost is the gap between actual and potential typing productivity, summed across billions of users over decades.
% ABSENT_VOICES: Future generations of typists who will inherit the suboptimal standard; alternative layout designers who cannot gain market entry; ergonomists who see preventable repetitive strain injuries. They are absent because the coordination equilibrium has no mechanism to internalize their interests.
% DISAPPEARANCE_RATIONALE: If the QWERTY coordination equilibrium vanished overnight (e.g., a universal, costless switch to Dvorak), typing productivity would jump, retraining costs would be incurred once, and the keyboard market would reorganize around the new standard. The world would rearrange because the constraint currently structures the entire human-computer text interface.
% FOUNDING_PROBLEM: Mechanical typewriters jammed when adjacent keys were struck in rapid succession. The QWERTY layout separated common letter pairs to slow typing down and prevent jams. This was a genuine coordination problem: manufacturers needed a layout that worked reliably on mechanical machines.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jamming problem is undisputed historical fact (typewriter engineering literature). The lock-in reading's claim that the founding problem is dead while the arrangement persists is corroborated by the complete disappearance of mechanical typewriters from mainstream use (museums, collectors only). No beneficiary of the lock-in reading disputes this; the debate is about whether the persistence is efficient (naturalization) or extractive (beneficiary_extraction).
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15) reflects deadweight loss from suboptimal coordination, not transfer. Suppression (0.30) captures switching costs that raise the barrier to alternative adoption — not active enforcement. Theater (0.10) is low because the coordination function (universal key layout) is genuinely performed. Accessibility collapse (0.60) is moderate: alternatives exist (Dvorak, Colemak) but network effects make them inaccessible for most. Resistance (0.40) reflects persistent advocacy for alternatives and occasional organizational switching attempts. The claimed type 'rope' reflects the reading's core claim: a pure coordination equilibrium that solves interoperability but fails to reach the global optimum. The engine will compute per-seat types from the structural data; the lock-in reading predicts symmetric costs across users (no extraction), so all user seats should compute near rope or mountain, while the analytical seat sees the market failure.
 *
 * PERSPECTIVAL GAP:
 *   The lock-in reading and the naturalization_reading disagree on whether the coordination equilibrium is efficient. The lock-in reading sees collective suboptimality; the naturalization_reading sees revealed preference adequacy. The beneficiary_extraction_reading sees active rent extraction by manufacturers. These are three distinct constraints with different ε values, not perspectives on one constraint. The engine's per-seat computation will show: for lock_in_reading, all user seats have similar directionality (symmetric costs), so their effective extraction is uniformly low; for beneficiary_extraction_reading, manufacturers have low directionality (beneficiaries) and users high (targets), producing extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   No individual beneficiary or victim declared in base_properties — the lock-in reading posits symmetric costs across all users. Early typists and touch_typists are identity_locked (retraining cost is identity-fused skill loss). Manufacturers are constrained (they could switch standards but would lose compatibility). Dvorak advocates are trapped (no market entry). Economic historians are analytical. The engine derives directionality from these exit options and the absence of beneficiary/victim declarations: all non-analytical seats get d ≈ 0.5 (symmetric), so effective extraction ≈ base extractiveness for all. This matches the reading's claim of market failure without extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has no mandate — it is an emergent coordination equilibrium. Mandatrophy does not apply. The founding problem (interoperability of mechanical typewriters) is dead, but the arrangement persists because the coordination function (universal key layout) remains live. The mismatch (dead founding problem, live coordination function) is the essence of path dependence, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lock_in_empirical_validity,
    'Is the QWERTY lock-in effect empirically robust, or does historical evidence show that QWERTY was actually efficient and alternatives failed on merit?',
    'Comparative historical analysis of typing speed studies, market adoption data, and controlled experiments between QWERTY and Dvorak layouts; resolution of the David vs. Liebowitz-Margolis debate.',
    'If lock-in is empirically weak, the constraint reclassifies toward naturalization_reading; if robust, it supports the lock-in reading''s claim of market failure without extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_empirical_validity, empirical, 'Empirical status of the QWERTY lock-in claim.').

omega_variable(
    committer_structure_kernel_readings,
    'How do the three readings of the qwerty_persistence_mechanism kernel structurally relate?',
    'Structural mapping of each reading''s beneficiary/victim architecture, extraction profile, and coordination claims; the kernel is the persistence question, not a single constraint.',
    'Clarifies that lock_in_reading has no individual beneficiary/victim, while beneficiary_extraction_reading has manufacturers as beneficiaries and users as victims; naturalization_reading has neither. This drives distinct ε values and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_readings, conceptual, 'Commitment-system framing: one kernel, three distinct constraints.').

omega_variable(
    collective_suboptimality_measurement,
    'How to quantify the collective social cost of QWERTY persistence without a transfer recipient?',
    'Welfare economics estimation of deadweight loss from suboptimal standard: productivity loss, retraining costs, innovation foreclosure — summed across all users with no offsetting rent capture.',
    'If measurable and large, strengthens the market-failure claim; if negligible, weakens the lock-in reading''s divergence from naturalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_suboptimality_measurement, empirical, 'Quantifying collective cost in the absence of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t0, observed).
narrative_ontology:measurement(qwerty_lockin_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t20, observed).
narrative_ontology:measurement(qwerty_lockin_tr_t40, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t40, observed).
narrative_ontology:measurement(qwerty_lockin_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t60, observed).
narrative_ontology:measurement(qwerty_lockin_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t80, observed).
narrative_ontology:measurement(qwerty_lockin_tr_t100, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement_basis(qwerty_lockin_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(qwerty_lockin_be_t0, observed).
narrative_ontology:measurement(qwerty_lockin_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(qwerty_lockin_be_t20, observed).
narrative_ontology:measurement(qwerty_lockin_be_t40, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement_basis(qwerty_lockin_be_t40, observed).
narrative_ontology:measurement(qwerty_lockin_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement_basis(qwerty_lockin_be_t60, observed).
narrative_ontology:measurement(qwerty_lockin_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement_basis(qwerty_lockin_be_t80, observed).
narrative_ontology:measurement(qwerty_lockin_be_t100, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(qwerty_lockin_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lockin_su_t0, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(qwerty_lockin_su_t0, observed).
narrative_ontology:measurement(qwerty_lockin_su_t20, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement_basis(qwerty_lockin_su_t20, observed).
narrative_ontology:measurement(qwerty_lockin_su_t40, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(qwerty_lockin_su_t40, observed).
narrative_ontology:measurement(qwerty_lockin_su_t60, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(qwerty_lockin_su_t60, observed).
narrative_ontology:measurement(qwerty_lockin_su_t80, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(qwerty_lockin_su_t80, observed).
narrative_ontology:measurement(qwerty_lockin_su_t100, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement_basis(qwerty_lockin_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence_mechanism kernel decomposes into three constraint stories. Lock_in_reading: ε≈0.15, no beneficiaries/victims, claimed rope. Naturalization_reading: ε≈0.05, no beneficiaries/victims, claimed mountain. Beneficiary_extraction_reading: ε≈0.60, beneficiaries=[typewriter_manufacturers, incumbent_typists], victims=[users, alternative_layout_developers], claimed snare. The lock-in reading influences the beneficiary_extraction_reading by providing the coordination substrate that extraction could exploit; it coexists_with naturalization_reading as competing empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
