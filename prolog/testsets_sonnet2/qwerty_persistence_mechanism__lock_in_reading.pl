% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: QWERTY Keyboard Layout Lock-In (Coordination-Failure Reading)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This story instantiates the LOCK-IN reading of the QWERTY persistence
 *   kernel: the layout persists not because any actor actively suppresses
 *   alternatives (the beneficiary-extraction reading) nor because it is
 *   genuinely competitively adequate (the naturalization reading), but
 *   because coordination on ANY shared standard is more valuable to each
 *   individual than switching alone, producing a stable but suboptimal
 *   equilibrium that no single decision-maker can unilaterally exit from. The
 *   efficiency gap (contested in the literature, but authored here as real
 *   for purposes of this reading) is a collective cost with no concentrated
 *   collector — a market failure, not extraction.
 *
 * KEY AGENTS:
 *   - typists_and_general_users: primary bearers of the diffuse efficiency cost, trapped by network effects
 *   - would_be_alternative_layout_adopters: rational individual holdouts whose exit would only help if coordinated at scale
 *   - keyboard_manufacturers_incumbent_scale: passive beneficiaries of manufacturing economy of scale, not active suppressors
 *   - software_and_device_ecosystem: administers the default, responding to expected demand rather than engineering lock-in
 *   - economic_historians: analytical observers of the underlying path-dependence debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.28).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.15).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Lock-In (Coordination-Failure Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '63328459-6822-402a-b1cd-d7fa93ed15ee').
narrative_ontology:cs_kernel_codification('63328459-6822-402a-b1cd-d7fa93ed15ee', distributed).
narrative_ontology:cs_authority_grounding('63328459-6822-402a-b1cd-d7fa93ed15ee', distributed).
narrative_ontology:cs_reading_relation('63328459-6822-402a-b1cd-d7fa93ed15ee', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('63328459-6822-402a-b1cd-d7fa93ed15ee', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('63328459-6822-402a-b1cd-d7fa93ed15ee', foundational, network_effects_produce_suboptimal_equilibria_without_design).
narrative_ontology:cs_axiom_status(network_effects_produce_suboptimal_equilibria_without_design, holdable).
narrative_ontology:cs_axiom_grounding('63328459-6822-402a-b1cd-d7fa93ed15ee', network_effects_produce_suboptimal_equilibria_without_design, empirically_contingent).
narrative_ontology:cs_axiom('63328459-6822-402a-b1cd-d7fa93ed15ee', secondary, no_concentrated_actor_need_exist_for_collective_harm).
narrative_ontology:cs_axiom_status(no_concentrated_actor_need_exist_for_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('63328459-6822-402a-b1cd-d7fa93ed15ee', no_concentrated_actor_need_exist_for_collective_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('63328459-6822-402a-b1cd-d7fa93ed15ee', coordination_equilibrium_under_switching_costs).
narrative_ontology:cs_drift_state('63328459-6822-402a-b1cd-d7fa93ed15ee', contemporary_digital_keyboard_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('63328459-6822-402a-b1cd-d7fa93ed15ee', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_scale).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typing_instruction_industry).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, typists_and_general_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, would_be_alternative_layout_adopters).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, network_effects_can_trap_inferior_standards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn QWERTY because everyone else uses it, every keyboard sold has it, and every job requires it. Individually would benefit from a more efficient layout but cannot unilaterally switch without abandoning years of trained muscle memory and losing compatibility with shared devices, workplaces, and each other.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typists_and_general_users, payer,
    powerless, biographical, trapped, global).

% Individuals or small groups who would prefer Dvorak or another layout bear the full retraining cost themselves while capturing almost none of the coordination benefit, since nearly everyone else remains on QWERTY. Their rational individual choice to stay locks in the collective suboptimum.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, would_be_alternative_layout_adopters, payer,
    powerless, biographical, constrained, global).

% Benefit passively from an existing standard that lets them manufacture one layout at massive economy of scale without needing to coordinate a switch. They did not engineer the lock-in and would not necessarily lose from a coordinated transition, but they have no incentive to bear first-mover cost of introducing a competing standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_scale, beneficiary,
    organized, generational, mobile, global).

% Schools, typing-course providers, and certification programs are built around teaching the existing standard. They benefit incidentally from continuity of curriculum, not from actively suppressing alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_instruction_industry, beneficiary,
    moderate, biographical, mobile, national).

% Operating system and device makers default every new device to QWERTY because that is what buyers expect and because deviating would require costly coordination with input-method standards, accessibility tooling, and international layouts. They administer the default but are responding to expected demand, not actively blocking alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, software_and_device_ecosystem, agenda_setter,
    institutional, generational, mobile, global).

% Designers and advocates of Dvorak, Colemak, and other layouts have no practical channel to coordinate a mass switch; there is no mechanism for aggregating dispersed individual preferences into a collective re-coordination, so their case is never seriously tested at scale.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_designers, excluded,
    powerless, biographical, trapped, global).

% Study the QWERTY case as a canonical (and contested) example of path dependence and network-effect lock-in, debating whether the empirical efficiency gap from alternative layouts is real or overstated.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, near-universal keyboard layout lets any typist use any keyboard, any manufacturer produce one standard product at scale, and any employer train and hire without layout-specific retraining — a genuine coordination good that saves enormous transaction costs across the whole typing population.
% TRANSFER_FUNCTION: Moves a diffuse efficiency cost — the gap between QWERTY's typing speed/ergonomics and a superior layout's — from no single collector to the entire population of typists collectively, in exchange for the coordination benefit of universal compatibility. No party captures the difference as rent; it is a deadweight loss of collective action failure, not an extraction.
% ABSENT_VOICES: Individuals and small organized groups who would prefer to coordinate a mass switch to a more efficient layout have no aggregation mechanism — there is no market process that lets millions of independently-rational 'stay on QWERTY' decisions be overridden by a collective decision to switch, even if switching would make almost everyone better off.
% DISAPPEARANCE_RATIONALE: If universal QWERTY conformity vanished overnight (say, a costless one-time global retraining and re-manufacturing event), users would plausibly reconverge on a more efficient layout within a generation, manufacturers would retool, and training curricula would update. The world does not need QWERTY specifically; it needs *a* shared standard, and the current one persists due to coordination inertia rather than necessity.
% FOUNDING_PROBLEM: In the 1870s, a shared, non-jamming, commercially manufacturable typewriter key arrangement was needed so that trained typists and commercially produced machines were mutually compatible — coordination on any-one-standard was more valuable than optimizing for typing speed at the time.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (David 1985, and subsequent QWERTY-efficiency-debate literature by Liebowitz & Margolis) attest from outside any beneficiary group that the original mechanical rationale (preventing typebar jams) is long obsolete on electronic keyboards, while the coordination problem it solved is now solved by inertia rather than by any continuing technical necessity — but the same literature is genuinely split on whether the efficiency gap of alternatives is large enough to justify switching costs, which is why the problem's current status is authored as contested rather than dead.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low-to-moderate (0.28 at interval end) because under this reading there is no rent collector — the cost is a pure efficiency loss distributed across the entire typing population, not a transfer to any beneficiary's pocket. Suppression is low (0.15): nothing coercively blocks an individual from learning Dvorak; the barrier is a coordination problem, not an enforcement mechanism. Accessibility collapse is high (0.72) because once the network effect locks in, no individual rational actor can unilaterally make the alternative accessible — the practical alternative space has collapsed even though nothing legally forbids it. Resistance is moderate (0.35): a persistent minority of alternative-layout advocates exists but never reaches critical mass.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical observer's seat, this looks like classic market failure: everyone would be better off switching, no one profits from the status quo, yet no one moves. From the trapped user's seat, the experience is indistinguishable in the moment from an enforced constraint — they cannot exit, and the fact that no one enforces the trap doesn't make it feel less binding. The engine's per-seat computation is expected to register this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, beneficiaries are declared cautiously and narrowly: manufacturers and the instruction industry gain from standardization inertia, but their benefit is a side effect of the coordination equilibrium, not something they built or defend. Victims (typists, would-be switchers) are declared because the efficiency loss is real and falls on them, but they are victims of collective-action failure, not of anyone's design — this is the key structural marker that separates this reading from the beneficiary-extraction reading, where the same victim group would be cast as targets of deliberate rent-protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typebar jamming) is dead on any electronic keyboard, yet the arrangement persists — this is a mandatrophy signature. But it does not resolve into extraction: under this reading the persistence mechanism is that the ORIGINAL coordination benefit (universal compatibility) is still live and valuable even though the ORIGINAL technical rationale (jam prevention) is gone. The mandatrophy is partial: the coordination function has migrated from solving a mechanical problem to solving a pure network-effect problem, while remaining a genuine (if now non-obligatory) coordination good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_gap_magnitude,
    'Is the empirical typing-speed/ergonomics gap between QWERTY and alternative layouts (e.g. Dvorak) large enough to constitute genuine collective suboptimality, or is it small/nonexistent as some studies (Liebowitz & Margolis) argue?',
    'Controlled, large-sample studies of expert typists trained from childhood on alternative layouts versus QWERTY, correcting for selection effects (people who switch to Dvorak are often already typing enthusiasts).',
    'If the gap is negligible, this reading collapses toward the naturalization reading — there would be no real suboptimality to explain via lock-in, only a standard that is adequate enough that switching costs dominate for good reason. If the gap is substantial, the lock-in story is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_gap_magnitude, empirical, 'Whether the alleged QWERTY inefficiency is empirically real and large.').

omega_variable(
    lock_in_vs_extraction_distinguishability,
    'Is it possible to structurally distinguish passive coordination-failure persistence (this reading) from active beneficiary maintenance (the sibling extraction reading) using observable evidence, or do both readings predict the same observable equilibrium?',
    'Historical evidence of manufacturer lobbying, patent behavior, or marketing specifically targeting suppression of alternative layouts (as opposed to mere non-adoption of alternatives) would support the extraction reading over this one.',
    'If no such active-suppression evidence exists, this lock-in reading is the best-supported account and the extraction reading should be read as a weaker, less-corroborated framing of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_vs_extraction_distinguishability, conceptual, 'Whether coordination-failure and active-extraction readings are empirically distinguishable given available historical evidence.').

omega_variable(
    beneficiary_declaration_appropriateness,
    'Is it appropriate under this reading to declare keyboard manufacturers and the typing-instruction industry as ''beneficiaries'' at all, given the reading''s core claim is that no one is actively extracting rent?',
    'Distinguish incidental benefit (manufacturing economy of scale from an existing standard) from active rent extraction (lobbying to block alternatives) — the former is compatible with a coordination-failure reading, the latter is not.',
    'If manufacturers are shown to derive an active, defended rent from the standard, the beneficiary declaration under this reading becomes structurally indistinguishable from the extraction reading, undermining the reading''s distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_declaration_appropriateness, conceptual, 'Whether declaring passive beneficiaries here risks collapsing this reading into the sibling extraction reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(qwer_tr_t110, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 110, 0.09).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 140, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement(qwer_be_t110, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 110, 0.25).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 140, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__lock_in_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the same kernel (qwerty_persistence_mechanism): this story (lock_in_reading) claims tangled_rope with diffuse, unextracted collective cost; naturalization_reading would claim rope or mountain with negligible extraction (the standard is genuinely adequate); beneficiary_extraction_reading would claim tangled_rope or snare with concentrated, actively-defended extraction. All three share the observable equilibrium (universal QWERTY adoption) but attribute radically different causal mechanisms and different ε referents to it, per the ε-invariance principle applied to committer readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
