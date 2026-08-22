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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: QWERTY Lock-In: Path-Dependent Coordination Failure
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   QWERTY keyboard layout persists globally despite documented technical
 *   inferiority to alternatives like Dvorak. This lock-in reading frames the
 *   persistence as a MARKET FAILURE without beneficiary extraction: no
 *   individual actor benefits from QWERTY's suboptimality; instead, a
 *   coordination failure traps millions of typists in an equilibrium they
 *   would collectively prefer to exit but cannot exit individually. The
 *   founding problem (keyboard fragmentation in the 1870s) is completely
 *   solved, yet the constraint endures due to network effects and
 *   identity-locked learning. The reading asserts that QWERTY is not defended
 *   by any beneficiary with extractive intent, but rather perpetuated by the
 *   absence of any mechanism powerful enough to coordinate a universal
 *   transition away from it. This reading diverges sharply from the
 *   beneficiary-extraction reading (which attributes persistence to
 *   manufacturer rent-seeking) and the naturalization reading (which asserts
 *   QWERTY is genuinely adequate for practical purposes).
 *
 * KEY AGENTS:
 *   - typists_general_population: powerless, identity-locked via muscle memory and universal exposure; bear ergonomic costs with no individual exit
 *   - keyboard_manufacturers_incumbent: organized, mobile; passively benefit from lock-in through inaction, not active extraction
 *   - alternative_keyboard_designers: moderate power, constrained exit; technically superior product cannot reach market due to network effects
 *   - operating_system_vendors: institutional power, mobile; perpetuate QWERTY through inaction (shipping the default)
 *   - ergonomic_researchers: excluded from coordination mechanism; evidence is conclusive but institutionally powerless
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.41).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, snare).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Lock-In: Path-Dependent Coordination Failure").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '9db13ea9-a191-44cb-bc5d-1050ac155ca5').
narrative_ontology:cs_kernel_codification('9db13ea9-a191-44cb-bc5d-1050ac155ca5', distributed).
narrative_ontology:cs_authority_grounding('9db13ea9-a191-44cb-bc5d-1050ac155ca5', diffuse_epistemic).
narrative_ontology:cs_reading_relation('9db13ea9-a191-44cb-bc5d-1050ac155ca5', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9db13ea9-a191-44cb-bc5d-1050ac155ca5', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('9db13ea9-a191-44cb-bc5d-1050ac155ca5', foundational, coordination_failure_without_extraction).
narrative_ontology:cs_axiom_status(coordination_failure_without_extraction, holdable).
narrative_ontology:cs_axiom_grounding('9db13ea9-a191-44cb-bc5d-1050ac155ca5', coordination_failure_without_extraction, empirically_contingent).
narrative_ontology:cs_axiom('9db13ea9-a191-44cb-bc5d-1050ac155ca5', foundational, network_effects_lock_in_distributed_agents).
narrative_ontology:cs_axiom_status(network_effects_lock_in_distributed_agents, holdable).
narrative_ontology:cs_axiom_grounding('9db13ea9-a191-44cb-bc5d-1050ac155ca5', network_effects_lock_in_distributed_agents, empirically_contingent).
narrative_ontology:cs_reference_frame('9db13ea9-a191-44cb-bc5d-1050ac155ca5', universal_keyboard_standardization_equilibrium).
narrative_ontology:cs_drift_state('9db13ea9-a191-44cb-bc5d-1050ac155ca5', contemporary_ergonomic_research_era, gap(stable, severe, false)).
narrative_ontology:cs_created_at('9db13ea9-a191-44cb-bc5d-1050ac155ca5', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, typists_general_population).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_keyboard_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_keyboard_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learned to type on QWERTY; trained fingers carry the layout into every interaction with any keyboard. Switching would require months of painful retraining and would yield no individual benefit (every keyboard they encounter is QWERTY). They bear the ergonomic cost of the suboptimal layout — documented higher rates of RSI and typing fatigue compared to alternatives like Dvorak — but cannot exit individually because the layout is universal.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typists_general_population, payer,
    powerless, biographical, identity_locked, global).

% Manufactures QWERTY keyboards, which dominate every market segment because users are locked in. They do not extract monopoly rents (the competitive market keeps prices low) and do not maintain the layout through active enforcement (software and keyboard design simply reflect the universal standard). However, they benefit passively from the lock-in: switching to an alternative layout would require coordinating a simultaneous shift across the entire installed base, which no single manufacturer can impose. They have no incentive to break the coordination equilibrium, so they perpetuate it through inaction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent, agenda_setter,
    organized, biographical, mobile, global).

% Have designed and marketed ergonomically superior keyboards (Dvorak, Colemak, etc.) with measurable typing-speed and fatigue advantages. They bear the cost of a tiny market share despite technical superiority: users rationally refuse to learn a new layout when every computer they will ever touch is QWERTY. Network effects lock users into the incumbent even though individual typists would be better off with the alternative if everyone switched simultaneously.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_keyboard_designers, payer,
    moderate, biographical, constrained, global).

% Ship every OS with QWERTY as the default keyboard layout, supporting alternatives only as niche options. They do not enforce QWERTY through policy, but its universality makes switching costly for users (retraining) and invisible for vendors (they ship what users expect). The coordination equilibrium is self-perpetuating: no vendor has an incentive to break it because doing so unilaterally would alienate all users trained on QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, operating_system_vendors, agenda_setter,
    institutional, generational, mobile, global).

% Standardization bodies like ISO have codified QWERTY as the international standard keyboard layout. They observe the lock-in but lack the power to mandate a transition: any attempt to switch the standard would require simultaneous hardware, software, and user-training changes that no single institution can impose. Their standardization role perpetuates the lock-in by making alternatives non-interoperable.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, hardware_standardization_bodies, observer,
    institutional, generational, analytical, global).

% Have documented QWERTY's technical inferiority through decades of research: slower typing speeds, higher RSI rates, and measurable fatigue compared to alternatives. Their evidence is conclusive and published, but they are excluded from coordination decisions: they cannot mandate layout changes, and users will not abandon QWERTY individually despite knowing its disadvantages. The research has no institutional pathway to become practice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, ergonomic_researchers, excluded,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a universal keyboard layout standard, enabling all hardware and software to standardize on one physical key arrangement. This solved the early fragmentation problem: if keyboards varied by manufacturer, learning to type would require relearning the layout on every new device.
% TRANSFER_FUNCTION: Transfers ergonomic welfare (RSI risk, typing fatigue, learning inefficiency) from current typists to a diffuse collective. No single actor receives this transfer; it is a global welfare loss distributed across millions of users. Alternative keyboard designers forfeit market entry. The constraint moves nothing to anyone; it extracts from everyone while benefiting none.
% ABSENT_VOICES: Ergonomic researchers and alternative-keyboard manufacturers are structurally excluded from the coordination mechanism. They would argue for a collective transition to superior layouts, but they lack the institutional authority to mandate it and lack the individual incentive for users to act. Their evidence and alternative designs are available but institutionally powerless.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared overnight (imagine an OS vendor somehow shipping only Dvorak), every existing typist would be locked into a new layout they did not choose, at ruinous individual cost. The scenario is catastrophic, which illustrates why the lock-in persists: the only way to break it is a coordinated universal transition, which no actor has the power to enforce and no actor has the incentive to initiate individually. The constraint persists precisely because its dissolution requires a coordination mechanism no one has.
% FOUNDING_PROBLEM: Early mechanical typewriters varied in physical layout and key arrangement. This meant learning to type on one machine did not transfer to another, fragmenting the skill. A universal layout standard was adopted (QWERTY) to solve this fragmentation: once all machines standardized, typing skill became portable across devices.
% FOUNDING_PROBLEM_CORROBORATION: The fragmentation problem was empirically real in the 1870s–1890s, attested by contemporary manufacturer accounts and technology historians. The problem is now completely solved: every keyboard worldwide is QWERTY-compatible, and a typist trained on one machine can use any other without relearning. The founding problem no longer exists. Historians and economists outside the technology industry confirm this status. The constraint persists despite its founding problem being solved — a classic mandatrophy signature.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (early era when alternatives were still viable) to 0.62 (modern era when lock-in is absolute) as the installed base grows and switching costs compound. The trajectory models the accumulation of sunk learning costs: each new generation of typists learns QWERTY, raising the collective exit cost without any single actor deciding to maintain it. Suppression (0.41 at interval end) is moderate because the constraint is self-perpetuating via identity-locked learners, not through active coercion — typists are locked in by their own training, not by external force. Theater ratio (0.18) reflects that QWERTY's perpetuation requires little performative defense: vendors simply ship the default, researchers simply publish unheeded studies, alternative designers simply occupy a tiny niche. The constraint needs minimal theater because it needs no enforcement — the coordination failure is structural, not political. The measurement series runs on a single shared time grid: every metric is authored at six time points spanning 150 years (industrial typewriter era through modern computing), showing the lock-in strengthening over time as the installed base expands and switching costs accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The typist seat and the manufacturers seat compute radically differently. From the typist's perspective, the constraint extracts ergonomic welfare (RSI, fatigue, training inefficiency) at a rate proportional to the universality of QWERTY. From the manufacturer's perspective, QWERTY is simply the market standard — they have zero incentive to unilaterally deviate, and zero power to coordinate a collective transition. The manufacturer's beneficiary status is passive: they benefit from inaction, not from enforcing the layout. The engine should compute a snare from the typist's seat (high extraction, no alternative) and a rope or even mountain from the manufacturers' seat (they see natural coordination, they benefit from it, they have no reason to change it). This divergence reveals the lock-in: no party is extracting, yet extraction is occurring. A snare without an extractor.
 *
 * DIRECTIONALITY LOGIC:
 *   Typists are the target: d approaches 1.0. They are trapped (identity_locked via muscle memory) with zero individual exit options. The constraint extracts from them regardless of their preferences. Manufacturers are beneficiaries structurally (d approaches 0.0) but passively: they benefit from the universal standard through inaction and coordination failure, not through active expropriation. The key insight is that d is HIGH for typists not because any actor has power over them, but because the structural situation (network effects + sunk training costs) leaves them no alternative. Directionality is locked in by identity and coordination failure, not by coercion. No override needed: the derived directionality from beneficiary/victim declarations maps cleanly to the story's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This lock-in reading RESOLVES mandatrophy through market failure without extraction. The founding problem (keyboard fragmentation) is DEAD — attested by historians and economists outside the industry: every keyboard is now QWERTY-compatible. The constraint persists despite its founding function being solved. In the beneficiary-extraction reading, mandatrophy is resolved by naming manufacturers/incumbents as extractors who maintain the constraint for rent. In the lock-in reading, mandatrophy is resolved by showing that NO party is maintaining it — the constraint is self-perpetuating via coordination failure. The difference is structural: one reading has an agenda-setter defending the constraint for profit; the other reading has no agent defending it at all, only the absence of any agent powerful enough to break the lock-in. The lock-in reading is the weaker mandatrophy claim (harder to fix, no obvious beneficiary to sanction), but it is also the more parsimonious claim (requires fewer active choices to explain persistence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_structural_lock_in,
    'Is QWERTY''s persistence due to intentional coordination failure (manufacturers/standards bodies could switch but choose not to because they benefit passively) or structural coordination failure (no actor has sufficient authority or incentive to coordinate a transition, even if all would benefit from it)?',
    'Examine archives and interviews from keyboard manufacturers and OS vendors: did they ever seriously consider or attempt a universal transition to a superior layout? If yes, what stopped them? If no, why not?',
    'If intentional, the beneficiary-extraction reading becomes more plausible (manufacturers benefit from inaction, which is a form of passive extraction). If purely structural, the lock-in reading is correct (no party has extractive intent; the failure is mechanism-based). The two readings diverge on WHETHER QWERTY is defended versus WHETHER IT SIMPLY CANNOT BE UNFENCED.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_vs_structural_lock_in, empirical, 'Whether lock-in is defended by beneficiaries or is purely self-perpetuating coordination failure').

omega_variable(
    identity_lock_mechanism_stability,
    'Once typists are trained on QWERTY (identity_locked via muscle memory), does the lock persist because the identity is internalized and self-reinforcing, or does it persist because institutional factors (universal presence of QWERTY) actively prevent exit testing?',
    'Natural experiment: offer typists a period of immersion in an alternative layout with no penalty for failure. Measure how many achieve fluency and whether they prefer the alternative. If many succeed and prefer the alternative but do not switch in practice, the identity lock is internalized. If few even attempt the switch, external institutional factors dominate.',
    'If identity lock is internalized, suppression (0.41) understates the psychological barrier to exit. The constraint''s hold is deeper than the measurement suggests. If institutional factors dominate, suppression is correctly measured as moderate — the barrier is material (cost, ubiquity) not internalized (belief, identity fusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_stability, empirical, 'Whether typist lock-in is internalized identity or structural inability to practice alternatives').

omega_variable(
    reading_committer_boundary,
    'Could a beneficiary-extraction reading and a lock-in reading both be true of the same constraint, or do they foreclose each other?',
    'Examine the two readings'' core premises: (1) manufacturers benefit from QWERTY''s persistence (extraction reading); (2) no actor has incentive or authority to break the coordination failure (lock-in reading). Can both be true? Yes, if manufacturers benefit passively (inaction) while the structure remains unchanged (coordination failure). Both readings describe the same constraint but attribute its persistence to different mechanisms. They coexist, not foreclose.',
    'If both readings are structurally tenable, the constraint''s classification depends on which reading is adopted — the engine computes per-reading types. This reinforces the ε-invariance principle: the same constraint instantiates different types under different readings. The kernel-context framing is essential to keep the readings distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_boundary, conceptual, 'Whether extraction-reading and lock-in-reading are foreclosing or coexisting interpretations of QWERTY''s persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement_basis(qwer_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement_basis(qwer_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(qwer_su_t40, observed).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(qwer_su_t60, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 100, 0.41).
narrative_ontology:measurement_basis(qwer_su_t100, observed).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 150, 0.41).
narrative_ontology:measurement_basis(qwer_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% The QWERTY kernel decomposes into three distinct constraint readings: (1) lock-in_reading (this file) — market failure without extractive intent, path-dependent coordination failure; (2) beneficiary_extraction_reading — persistent rent-seeking by manufacturers and standards bodies; (3) naturalization_reading — QWERTY's adequacy and fair competitive elimination of alternatives. Each reading authors the same referent (global QWERTY universality) but instantiates different ε values, beneficiary/victim structures, and types. The three are not perspectives on one constraint; they are three different constraints read from the same kernel. The lock-in reading emphasizes structural coordination failure; the extraction reading emphasizes active rent defense; the naturalization reading emphasizes competitive adequacy. All three can coexist as live scholarly positions precisely because they foreclose each other only when forced into a single-reading frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
