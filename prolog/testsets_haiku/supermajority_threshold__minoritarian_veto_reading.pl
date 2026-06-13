% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto Mechanism
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The supermajority amendment threshold (typically requiring 66.7% of
 *   legislative votes to approve a constitutional amendment) is presented by
 *   institutional defenders as essential to constitutional stability—a
 *   mechanism ensuring that only changes backed by deep, broad consensus
 *   become permanent law. This minoritarian-veto reading contests that frame.
 *   It argues that the supermajority threshold functions primarily as a
 *   lock-in mechanism for historically entrenched power: those who benefit
 *   from the status quo (property-holders, regional majorities, sectional
 *   elites) can block any amendment by assembling 34% of votes, regardless of
 *   how overwhelming the popular and legislative majority for reform is. The
 *   threshold converts historical privilege into a permanent veto. As
 *   demographics and preferences shift, the threshold crystallizes obsolete
 *   constitutional law, widening the gap between formal constitution and
 *   lived reality. The constraint persists not because it solves a genuine
 *   coordination problem but because it protects entrenched beneficiaries
 *   from majoritarian redistribution. Contemporary majorities and
 *   historically excluded groups seeking recognition find their electoral
 *   mandates nullified by a barrier designed in a different era with
 *   different power distributions. The reading does NOT claim the threshold
 *   was created cynically; rather, it claims that the founding justification
 *   (preventing majoritarian tyranny) no longer matches the current
 *   structural function (protecting sectional power from majoritarian
 *   reform).
 *
 * KEY AGENTS:
 *   - Entrenched status quo beneficiaries (institutional/biographical, arbitrage exit): hold constitutional privileges the majority would remove; use the supermajority requirement as structural veto.
 *   - Minority blocking coalitions (organized/biographical, constrained exit): legislative minorities whose power exceeds their population share; use the threshold to preserve their overrepresentation.
 *   - Constitutional inertia guardians (institutional/generational, arbitrage exit): courts, law schools, constitutional commissions whose authority depends on constitutional stability; benefit from a high threshold that makes interpretation de facto constitutional law.
 *   - Contemporary reform majorities (moderate/biographical, constrained exit): electorally empowered but constitutionally blocked; their mandates die at the supermajority bar.
 *   - Historically excluded groups (powerless/generational, identity_locked exit): seek formal constitutional recognition but find their claims nullified by the same veto that excluded them originally. Identity fusion with the national system makes exit unthinkable.
 *   - Future generations (powerless/civilizational, trapped exit): inherit constitutional deadlock they did not create and cannot escape.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.81).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto Mechanism").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb').
narrative_ontology:cs_kernel_codification('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', fixed_text).
narrative_ontology:cs_authority_grounding('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', extraction).
narrative_ontology:cs_interpretation_layer_present('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb').
narrative_ontology:cs_reading_relation('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', foundational, supermajority_blocking_minority_veto_inevitable).
narrative_ontology:cs_axiom_status(supermajority_blocking_minority_veto_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', supermajority_blocking_minority_veto_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', foundational, constitutional_lock_in_becomes_privilege_entrenchment).
narrative_ontology:cs_axiom_status(constitutional_lock_in_becomes_privilege_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', constitutional_lock_in_becomes_privilege_entrenchment, deontological).
narrative_ontology:cs_reference_frame('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', majoritarian_constitutional_amendment).
narrative_ontology:cs_drift_state('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', contemporary_demographic_shift, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4de6bee6-81e8-4e1c-99cb-d9cdbc6749cb', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, minority_blocking_coalitions).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, constitutional_inertia_guardians).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, historically_excluded_groups_seeking_representation).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, future_generations_inheriting_obsolete_constraints).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers veto power from distributed majorities to concentrated blocking minorities, with no offsetting coordination benefit proportional to that extraction. The temporal series shows extractiveness rising from 0.62 to 0.78 over the interval: as demographics shift and the founding consensus that may have justified the threshold erodes, the constraint operates more purely as a lock-in mechanism for privilege, less as a consensus-safeguard. Theater is moderate (0.42): the constraint is performed as 'protecting constitutional stability,' and part of the enforcement activity is genuinely about constitutional interpretation. But a growing share is pure veto maintenance—defending the threshold itself against reform, using constitutional language to justify blocking majorities. The measurement grid shows suppression is highest at the class level (0.85 at endpoint)—entire classes of reform advocates find their majoritarian will systematically suppressed. Accessibility collapse is high across levels (0.75–0.80 at endpoint) because constitutional amendment is THE mechanism for large-scale institutional change; if it is locked, alternatives (statutory reform, executive action, judicial reinterpretation) are severely constrained. Resistance is lower (0.61–0.74) because the barrier is constitutional—opposing it requires either mounting sustained supermajority coalitions (high cost) or proposing extra-constitutional reform (higher reputational cost). Resistance exists but is structurally dampened by the threshold's own legitimacy narrative (people believe the constitution should be 'hard to change'). The claim/metric divergence is intentional: this reading CLAIMS snare (minoritarian veto lock-in) while acknowledging the institutional defenders' framing (coordination for stability). The engine computes which type the metrics support; the divergence is exactly the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The entrenched beneficiaries and constitutional inertia guardians perceive the threshold as genuine coordination—a mechanism ensuring constitutional changes rest on deep consensus. From their position, blocking majorities is protecting constitutional integrity. The contemporary reform majorities and historically excluded groups perceive the same structure as minoritarian veto and lock-in. From their position, the threshold is suppressing legitimate democratic claims. The engine computes per-seat types from the structural data. The beneficiary seats should compute as experiencing coordination (their veto power is genuinely coordinated and provides benefits without offsetting extraction). The payer seats should compute as experiencing extraction (they bear costs—blocked reforms, perpetuated exclusion—with no offsetting coordination benefit). This perspectival divergence is not an error in the authoring; it is structural reality. The same rule that coordinates for some (blocking minorities get stable veto) extracts from others (reform majorities get nullification). The threshold is a tangled rope FROM the beneficiary perspective and a snare FROM the payer perspective. This story instantiates the snare reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched beneficiaries: directionality~0.05–0.15 (full beneficiary). They collect the benefit of veto power without proportional cost. They have high power (institutional) and high exit options (arbitrage: if amendment reform threatened them in this jurisdiction, they could move wealth/influence elsewhere). The constraint subsidizes them. Minority blocking coalitions: directionality~0.1–0.25 (beneficiary-to-symmetric). They benefit from the threshold (it protects their overrepresentation) but bear some indirect cost (the legislature becomes gridlocked, making any legislation harder to pass, even for them). Constitutional inertia guardians: directionality~0.1–0.2 (beneficiary). Their authority depends on constitutional stability. Contemporary reform majorities: directionality~0.75–0.85 (target). They are blocked despite electoral mandate. They are identity-locked (they cannot 'leave' the constitutional system). Their exit options are severely constrained: statutory reform hits the judicial deference to the constitution; executive action is bounded by constitutional powers; extra-constitutional reform carries immense cost. The constraint extracts from them. Historically excluded groups: directionality~0.8–0.95 (full target). They seek constitutional recognition and face systematic veto from blocking minorities whose power structure depends on excluding them. They are identity-locked to the most extreme degree: their identity is constitutionally fused with the nation-state they seek to reform. Exit is unthinkable. The constraint extracts heavily from them. Future generations: directionality~0.85–0.95 (full target). They inherit the lock and cannot escape it except through extra-constitutional means. No overrides are needed; the derivation chain captures these directionalities accurately from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The supermajority threshold's founding problem was to prevent tyranny-of-the-majority and ensure constitutional changes reflect deep consensus. The problem is CONTESTED in status: institutional defenders argue it is still live (majorities can still threaten constitutional stability); majorities and excluded groups argue the problem is dead or inverted (constitutional deadlock, not majoritarian excess, is the pathology). The disappearance verdict is WORLD_REARRANGES: if the threshold vanished and were replaced with simple majority or citizens' initiative, constitutional reform would accelerate, blocking minorities would lose veto power, and the system would reorganize around new reform coalitions. This mismatch (founding problem contested-or-dead + world would rearrange) is the canonical mandatrophy signal: the constraint persists not because the founding problem is live and real but because entrenched beneficiaries maintain it for extraction. A piton would show similar metrics but would lack identifiable beneficiaries—the constraint would be mostly performance with distributed costs and no concentrated gain. This constraint has clear beneficiaries (entrenched elites, blocking minorities, inertia guardians), so it is Snare, not Piton. The classification prevents mislabeling the constraint as 'just constitutional tradition' or 'coordination we all depend on.' It names the extraction and identifies who collects it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_founding_intent_vs_lock_in_function,
    'Was the supermajority threshold designed primarily to prevent tyranny-of-the-majority and ensure deep consensus, or was its primary function from the outset to entrench the power of blocking minorities and property holders?',
    'Historical analysis of constitutional convention debates, recorded votes, and writings of framers regarding the threshold''s stated rationale vs. its actual structural effect on power distribution at the time of adoption. Separate the stated legitimacy claim from the empirical beneficiary distribution.',
    'If the threshold''s true founding function was lock-in for property-owning minorities (the minoritarian-veto reading), the constraint''s legitimacy narrative collapses and it reclassifies from ''coordination for stability'' to ''coordinated extraction for privilege.'' If genuinely dual-functional, the reading must account for degradation of the consensus-formation function over time (as social composition changed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_founding_intent_vs_lock_in_function, empirical, 'Whether the supermajority threshold was designed as a minoritarian-veto lock-in from inception or descended into that function as demographics shifted.').

omega_variable(
    consensus_formation_rate_empirical_calibration,
    'In the actual social system this constraint governs, what consensus-formation rate does the supermajority threshold achieve relative to real democratic consensus? Does it preserve genuine consensus or does it crystallize historical privilege as false consensus?',
    'Survey and polling data comparing the supermajority threshold (66.7% of legislature) to public support for successful and failed amendments. Track whether amendments that pass the supermajority test are more stable and legitimate than those that fail despite majority support. Compare to sister democracies with lower thresholds to assess whether they show greater instability.',
    'If the threshold enforces consensus better than lower alternatives (amendments that pass enjoy broad public durability), it retains coordination function. If it blocks amendments with supermajority public support and those amendments, once passed in other jurisdictions, prove stable, then the threshold is extractive — it enforces historical consensus, not present consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_formation_rate_empirical_calibration, empirical, 'Whether the supermajority threshold calibrates to actual consensus formation or crystallizes historical power distribution as false consensus.').

omega_variable(
    structural_representation_distortion_role,
    'To what degree does the supermajority requirement''s blocking power depend on pre-existing structural distortions in legislative representation (geographic overrepresentation, gerrymandering, voter-suppression effects) vs. operating as an independent veto mechanism?',
    'Decompose blocking coalitions by whether they would retain veto power under a proportional or corrected apportionment. Model amendment passage rates under current representation vs. reapportioned legislatures. Track whether blocking minorities are the same groups that benefit from structural representation distortions.',
    'If blocking minorities are the same groups that benefit from apportionment distortions, the supermajority threshold is compounding extraction via two mechanisms (over-representation + veto power). Reforming one without the other leaves the system extractive. If the threshold works independently of apportionment distortion, it is a separate lock-in mechanism requiring independent reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_representation_distortion_role, empirical, 'Whether the supermajority veto amplifies or operates independently of structural legislative representation distortions.').

omega_variable(
    kernel_reading_contestation_space,
    'This constraint is one reading of the supermajority_threshold kernel. What are the material and epistemic differences between this minoritarian-veto reading and the consensus-safeguard reading and adaptive-gradient reading?',
    'Each reading instantiates a different ε value, beneficiary set, and classification. This reading: high extraction (0.78), beneficiaries=entrenched elites/blocking minorities, type=snare. The consensus-safeguard reading: low extraction, beneficiaries=all participants in the consensus process, type=rope. The adaptive-gradient reading: medium extraction, beneficiaries=those calibrated to current consensus-rate, type=tangled-rope. The readings differ on what the threshold optimizes for (privilege preservation vs. consensus stability vs. evidence-based calibration) and on what counts as success. The network links show how this reading sits relative to siblings.',
    'If the minoritarian-veto reading''s ε (0.78) is correct, then the constraint is substantially extractive and should trigger Snare classification with recommendations for threshold reduction. If the consensus-safeguard reading is correct, then extraction is lower and the constraint is coordination. The corpus needs both readings present to allow empirical adjudication of which ε is more descriptively accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation_space, conceptual, 'The kernel contest itself — whether supermajority thresholds are primarily minoritarian-veto locks-in or genuine consensus-safeguards.').

omega_variable(
    identity_lock_mechanism_for_historically_excluded_groups,
    'For historically excluded groups seeking constitutional recognition, is the identity lock (exit_options: identity_locked) structural (they are legally constitutionally bound and cannot exit the system) or internalized (they have absorbed the belief that they should not exit, or cannot imagine exit, due to how identity has fused with national membership)?',
    'Distinguish structural exit barriers (legal penalties for secession, citizenship revocation, forcible reincorporation) from internalized barriers (identity fusion with the nation-state, belief in the possibility of eventual inclusion, fear of the unknown outside). Post-constitutional-reform movements in sister democracies provide natural experiments: when group identity shifts from ''locked-in member seeking recognition'' to ''excluded people entitled to self-determination,'' does exit become thinkable? Do constitutional reforms that grant formal recognition reduce identity lock?',
    'If lock is primarily structural, the constraint operates through legal coercion and constitutional reform is the primary lever for change. If lock is internalized, even constitutional reform may not dissolve exit barriers; the group may require identity-political work and solidaristic movement-building to recover the agency to exit. This affects the classification''s suppression component: internalized suppression persists after the formal barrier is removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_historically_excluded_groups, empirical, 'Whether identity lock is structural (legal barrier) or internalized (identity fusion) for historically excluded groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(supe_tr_t35, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(supe_be_t35, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(supe_su_t35, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel decomposes into three constraint stories corresponding to three incommensurable readings: (1) minoritarian_veto_reading (this story)—threshold as snare locking in privilege; (2) consensus_safeguard_reading—threshold as rope ensuring deep consensus; (3) adaptive_gradient_reading—threshold as tangled rope whose legitimacy depends on empirical calibration. Each reading has its own ε value, beneficiary/victim structure, and classification. They are linked via network.affects_constraints to enable corpus-level tracking of which reading's metrics more accurately describe the constraint's operation. The minoritarian-veto reading INFLUENCES both siblings by establishing high-extraction baseline; the consensus-safeguard reading COEXISTS_WITH this reading (different parties hold both simultaneously); the adaptive-gradient reading INFLUENCES both by reframing the question from 'is consensus-seeking legitimate?' to 'is the consensus rate correctly calibrated?'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
