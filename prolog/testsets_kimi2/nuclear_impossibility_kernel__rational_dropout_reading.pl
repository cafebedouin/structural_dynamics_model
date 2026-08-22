% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational Dropout Constraint
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   Nuclear weapons created a rational-choice constraint on great-power war:
 *   victory remains structurally possible but costs exceed any conceivable
 *   benefit, causing states to drop nuclear war from active consideration
 *   while retaining the option in the reachable set. This is the
 *   rational_dropout_reading of the nuclear_impossibility_kernel, distinct
 *   from structural_contraction (which claims physical impossibility) and
 *   credibility_paradox (which claims the threat is inherently incredible).
 *   The constraint coordinates by preventing nuclear war but extracts through
 *   nuclear coercion of non-nuclear adversaries and existential risk imposed
 *   on global populations. It requires active enforcement via arsenals, C3I,
 *   and non-proliferation regimes.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states (institutional/constrained): Maintain arsenals and doctrines; primary beneficiaries and agenda-setters
 *   - extended_deterrence_allies (powerful/constrained): Receive security guarantees; beneficiaries of extended deterrence
 *   - non_nuclear_adversaries (moderate/trapped): Strategic options constrained by nuclear threat; payers through coerced subordination
 *   - global_populations (powerless/trapped): Bear existential risk with no voice; payers through risk imposition
 *   - nuclear_strategists (analytical/analytical): Provide intellectual architecture; observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.75).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.7).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational Dropout Constraint").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'd23b0fa3-b443-4295-a929-c230bd2054a8').
narrative_ontology:cs_kernel_codification('d23b0fa3-b443-4295-a929-c230bd2054a8', distributed).
narrative_ontology:cs_authority_grounding('d23b0fa3-b443-4295-a929-c230bd2054a8', practice).
narrative_ontology:cs_interpretation_layer_present('d23b0fa3-b443-4295-a929-c230bd2054a8').
narrative_ontology:cs_reading_relation('d23b0fa3-b443-4295-a929-c230bd2054a8', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('d23b0fa3-b443-4295-a929-c230bd2054a8', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('d23b0fa3-b443-4295-a929-c230bd2054a8', foundational, nuclear_victory_remains_reachable).
narrative_ontology:cs_axiom_status(nuclear_victory_remains_reachable, holdable).
narrative_ontology:cs_axiom_grounding('d23b0fa3-b443-4295-a929-c230bd2054a8', nuclear_victory_remains_reachable, empirically_contingent).
narrative_ontology:cs_axiom('d23b0fa3-b443-4295-a929-c230bd2054a8', secondary, cost_benefit_governs_dropout).
narrative_ontology:cs_axiom_status(cost_benefit_governs_dropout, holdable).
narrative_ontology:cs_axiom_grounding('d23b0fa3-b443-4295-a929-c230bd2054a8', cost_benefit_governs_dropout, instrumental).
narrative_ontology:cs_reference_frame('d23b0fa3-b443-4295-a929-c230bd2054a8', rational_actor_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d23b0fa3-b443-4295-a929-c230bd2054a8', multipolar_nuclear_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d23b0fa3-b443-4295-a929-c230bd2054a8', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_adversaries).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, global_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals, command-and-control infrastructure, and deterrence doctrines. They set the strategic agenda for when and how nuclear threats are issued, and actively enforce the constraint through force posture and non-proliferation policy. Their security is enhanced by the constraint, but unilateral disarmament is treated as structurally unsafe because it would abandon the equilibrium.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, constrained, global).

% Receive formal security guarantees from nuclear patrons. Their conventional security is enhanced by the nuclear umbrella, which allows lower independent defense spending and deterrence of regional adversaries. They do not control the arsenal but benefit from the constraint on nuclear attack. Exit would mean acquiring independent nuclear capability or accepting strategic vulnerability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% Their strategic options are constrained by the implicit or explicit threat of nuclear retaliation. In crises their sovereignty is limited; they cannot challenge nuclear-armed opponents in existential ways without risking annihilation. They pay through coerced strategic subordination and the denial of full conventional escalation options.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_adversaries, payer,
    moderate, generational, trapped, national).

% Bear the existential risk of accidental launch, miscalculation, or deliberate use. They have no voice in targeting decisions, force posture, or doctrine, yet their survival depends entirely on the rationality and technical perfection of the nuclear-armed states. They pay through imposed catastrophic risk.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, global_populations, payer,
    powerless, civilizational, trapped, global).

% Analyze and model deterrence stability, escalation dynamics, and rational-choice frameworks. They produce the intellectual architecture that articulates the constraint as a rational equilibrium. Their professional identity and institutional funding are tied to the rational-actor deterrence paradigm.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_strategists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct major-power nuclear war by making the costs of nuclear exchange exceed any conceivable political benefit, creating a stable equilibrium of mutual restraint among armed rivals.
% TRANSFER_FUNCTION: Moves security and strategic stability from the ever-present possibility of nuclear annihilation to nuclear weapon states and their allies, while imposing existential risk and strategic subordination on adversaries and global populations.
% ABSENT_VOICES: Populations in potential target zones, future generations bearing long-term fallout and nuclear winter risks, and non-nuclear weapon states seeking abolition are structurally excluded from strategic deterrence debates; their objections are heard only in humanitarian initiative forums like the Treaty on the Prohibition of Nuclear Weapons, which nuclear states boycott.
% DISAPPEARANCE_RATIONALE: The constraint underwrites the entire post-1945 great-power peace. Its disappearance would remove the fundamental cost calculation that deters direct attack, triggering rapid conventional rearmament, alliance dissolution or independent proliferation, and possible great-power war.
% FOUNDING_PROBLEM: The prevention of total industrialized great-power war, which had produced catastrophic casualties in two world wars, and the deterrence of Soviet conventional aggression against Western Europe during the early Cold War.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and their strategists attest the problem remains live, citing renewed great-power competition. Humanitarian advocates, the majority of non-nuclear weapon states, and the UN General Assembly attest the founding problem is substantially transformed and the arrangement now poses greater risks than it solves; International Court of Justice advisory opinions from outside the beneficiary set corroborate the shifted-risk reading.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75 at interval end) is high because the constraint enables nuclear coercion and existential risk transfer, not merely mutual restraint. Suppression (0.70) reflects the active enforcement of non-proliferation, extended deterrence, and doctrinal taboos that suppress disarmament alternatives. Theater_ratio (0.45) captures the performative dimension of nuclear posturing and modernization, though the core coordination function remains real. Accessibility_collapse (0.50) indicates that while alternatives like nuclear disarmament are intellectually available, they are structurally blocked by the security dilemma and great-power competition. Resistance (0.50) comes from humanitarian movements, non-aligned states, and the TPNW, countered by nuclear-weapons-state intransigence.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear_weapon_states seat experiences the constraint as a prudent, self-enforcing rational equilibrium that guarantees survival. The non_nuclear_adversaries and global_populations seats experience the same structure as an imposed existential gamble and coercive limit on sovereignty. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_weapon_states and extended_deterrence_allies are structural beneficiaries (low d): the constraint subsidizes their security and extends their strategic advantage. Non_nuclear_adversaries and global_populations are structural targets (high d): they bear the costs of nuclear coercion and existential risk without accruing decision-making power. The asymmetry is driven by beneficiary/victim declarations and the trapped exit options of the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (Rope) by identifying the asymmetric extraction that runs through the same structure: the nuclear umbrella that coordinates allies also coerces adversaries. It also prevents mislabeling as pure extraction (Snare) by acknowledging the genuine coordination function (prevention of nuclear war). The founding problem (prevention of great-power total war) is contested in status, suggesting the constraint may have partially outlived its original function while generating new pathologies (proliferation risk, accidental war), but it has not atrophied into a Piton because the coordination function remains live and heavily contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_actor_assumption_validity,
    'Do nuclear-armed states and their leaders actually operate as rational cost-benefit calculators, or do organizational, psychological, and bureaucratic politics override rational dropout?',
    'Post-mortem analysis of nuclear crises (Cuban Missile Crisis, 1983 Able Archer, etc.) and organizational studies of nuclear command and control.',
    'If decision-making is not reliably rational, the constraint is not a stable rational-choice equilibrium but a precarious gamble on imperfect machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, empirical, 'Whether the rational dropout is produced by actual rationality or assumed rationality').

omega_variable(
    kernel_reading_empirical_distinguishability,
    'Can the rational dropout reading be empirically distinguished from the structural contraction reading in observable state behavior, or do they predict the same abstention from nuclear war?',
    'Analysis of strategic war-gaming, doctrinal documents, and force posture: structural contraction predicts no planning for nuclear victory; rational dropout predicts continued planning for limited nuclear options.',
    'If the readings are empirically indistinguishable, they may be one constraint described two ways; if distinguishable, the rational dropout reading carries higher extraction (active planning for war) and lower accessibility collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_empirical_distinguishability, conceptual, 'Whether the sibling readings of this kernel describe distinct constraints or one constraint').

omega_variable(
    suppression_of_disarmament_alternatives,
    'Is the suppression of nuclear disarmament alternatives maintained primarily by structural enforcement (NPT regime, sanctions, security dilemma) or by internalized cognitive frames (the nuclear taboo, unthinkability)?',
    'Comparative analysis of states that have disarmed (South Africa) versus states that maintain arsenals despite favorable security environments; measure whether structural or ideational factors better predict retention.',
    'If suppression is primarily internalized, effective suppression is higher than structural measures suggest and the constraint is more robust to institutional decay; if structural, dismantling enforcement regimes could rapidly collapse the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_disarmament_alternatives, empirical, 'Structural versus internalized suppression of disarmament alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nucl_tr_t16, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(nucl_tr_t32, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(nucl_tr_t48, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement(nucl_tr_t64, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 64, 0.42).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nucl_be_t16, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(nucl_be_t32, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(nucl_be_t48, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(nucl_be_t64, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 64, 0.7).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 80, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nucl_su_t16, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(nucl_su_t32, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(nucl_su_t48, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(nucl_su_t64, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 64, 0.72).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 80, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nuclear_impossibility_kernel. The rational_dropout reading (this file) posits that nuclear war remains reachable but is dropped from active consideration due to cost-benefit rationality. It is linked to structural_contraction_reading (physical impossibility) and credibility_paradox_reading (inherent incredibility of threats). Decomposition follows the Îµ-invariance principle: each reading has a distinct Îµ and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
