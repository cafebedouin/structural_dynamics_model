% ============================================================================
% CONSTRAINT STORY: conformity_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conformity_extraction, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conformity_extraction
 *   human_readable: Conformity Extraction Through Non-Itemized Judgment Transfer
 *   domain: philosophy_of_mind/social_psychology/ethics_of_relationship
 *
 * SUMMARY:
 *   Conformity extraction operates through the non-itemized transfer of
 *   individual judgment to collective norms via small, incremental
 *   concessions. Unlike explicit coercion (where the extraction mechanism is
 *   visible) or formal voting (where individual positions are itemized and
 *   aggregated), conformity extraction works through ambient social pressure
 *   that makes dissent costly while obscuring the cumulative magnitude of
 *   judgment transfer. The constraint exhibits genuine coordination function
 *   (groups with shared beliefs can act collectively more efficiently)
 *   alongside asymmetric extraction (the transfer is not reciprocal —
 *   high-status members shape norms while low-status members conform). The
 *   theater ratio (0.58) reflects that much of the consensus performance is
 *   ritualistic: participants perform agreement in group settings while
 *   maintaining private dissent, and the group's decision-making often
 *   proceeds as if consensus were genuine when it is not. The constraint's
 *   extractiveness has increased over the measurement interval (0.32 → 0.48)
 *   as initial voluntary alignment gives way to enforced conformity, and
 *   theater has increased (0.35 → 0.58) as the gap between private belief and
 *   public performance widens.
 *
 * KEY AGENTS:
 *   - Individual Epistemic Autonomy: Primary victim (powerless/identity_locked or moderate/constrained) — bears extraction through judgment transfer and belief suppression
 *   - Group Coherence Maintenance: Primary beneficiary (institutional/arbitrage) — abstract collective good that benefits from convergence but is often a cover story for high-status member interests
 *   - High-Status Norm Enforcers: Primary beneficiary (institutional/arbitrage) — shape group consensus and benefit from deference and alignment
 *   - Low-Status Dissenting Voices: Primary victim (powerless/identity_locked) — face maximum extraction with minimal exit options
 *   - Organized Subgroups: Mixed position (organized/mobile) — maintain partial autonomy through collective identity while performing sufficient conformity
 *   - Deliberative Democracy Coalition: Organized agents (organized/mobile) — building alternative consensus mechanisms with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conformity_extraction, 0.48).
domain_priors:suppression_score(conformity_extraction, 0.62).
domain_priors:theater_ratio(conformity_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conformity_extraction, extractiveness, 0.48).
narrative_ontology:constraint_metric(conformity_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(conformity_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conformity_extraction, tangled_rope).
narrative_ontology:human_readable(conformity_extraction, "Conformity Extraction Through Non-Itemized Judgment Transfer").
narrative_ontology:topic_domain(conformity_extraction, "philosophy_of_mind/social_psychology/ethics_of_relationship").

domain_priors:requires_active_enforcement(conformity_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conformity_extraction, group_coherence_maintenance).
narrative_ontology:constraint_beneficiary(conformity_extraction, norm_enforcers).
narrative_ontology:constraint_beneficiary(conformity_extraction, high_status_members).
narrative_ontology:constraint_victim(conformity_extraction, individual_epistemic_autonomy).
narrative_ontology:constraint_victim(conformity_extraction, dissenting_voices).
narrative_ontology:constraint_victim(conformity_extraction, low_status_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED DISSENTER (SNARE) — Individual whose self-concept is constituted through group membership but whose private beliefs diverge from group consensus. Structurally mobile (could leave the group) but identity-fused (cannot imagine self outside the group). Experiences maximum extraction: each small concession erodes epistemic autonomy while the identity lock prevents exit. The binding mechanism is cognitive rather than material — the agent has internalized the group's framing that dissent equals disloyalty.
constraint_indexing:constraint_classification(conformity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED PARTICIPANT (TANGLED ROPE) — Individual who faces real but surmountable costs to exit (social penalty, professional network loss, relocation burden). Benefits from group coordination (shared knowledge, collective action capacity, social support) while bearing extraction (judgment transfer, belief suppression, performative agreement). Genuine coordination function exists alongside asymmetric extraction. Can exit at a price but chooses to stay for the coordination benefits.
constraint_indexing:constraint_classification(conformity_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-STATUS NORM ENFORCER (ROPE) — Individual whose beliefs align with or shape group consensus. Experiences the constraint as pure coordination: the convergence mechanism enables collective action, maintains group boundaries, and reinforces their own epistemic authority. Net beneficiary — extraction runs toward this agent through deference and alignment. Can exit costlessly (arbitrage) because their status is portable across groups.
constraint_indexing:constraint_classification(conformity_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED SUBGROUP (TANGLED ROPE) — Coalition within the larger group that maintains partial epistemic autonomy through collective identity. Benefits from the larger group's coordination infrastructure while resisting full judgment transfer. Experiences mixed extraction: must perform sufficient conformity to remain in the group while maintaining internal dissent. Mobile exit options (can form splinter group or join alternative communities) reduce experienced extraction relative to isolated dissenters.
constraint_indexing:constraint_classification(conformity_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DELIBERATIVE DEMOCRACY COALITION (SCAFFOLD) — Organized agents building alternative consensus mechanisms (structured debate, anonymous voting, devil's advocate protocols, dissent protection norms). See conformity extraction as a temporary coordination failure with a sunset: as deliberative practices mature and spread, the non-itemized judgment transfer mechanism loses force. Groups adopting these practices create environments where private belief and public position can safely converge through reason rather than social pressure.
constraint_indexing:constraint_classification(conformity_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: VESTIGIAL CONSENSUS RITUAL (PITON) — In contexts where genuine coordination has atrophied but the consensus performance persists (corporate team-building exercises, mandatory town halls, performative unity statements), the constraint becomes pure theater. The ritual of agreement continues through institutional inertia despite providing minimal coordination function. Participants recognize the performance as hollow but maintain it because alternatives haven't replaced it.
constraint_indexing:constraint_classification(conformity_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, conformity mechanisms serve genuine coordination functions (enabling collective action, reducing decision costs, maintaining group boundaries) while simultaneously extracting from individual epistemic autonomy. The constraint is neither pure coordination nor pure extraction but an irreducible hybrid. The analytical classification matches the claimed type, confirming structural coherence.
constraint_indexing:constraint_classification(conformity_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conformity_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conformity_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conformity_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conformity_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conformity_extraction, TR),
    TR >= 0.70.

:- end_tests(conformity_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts individual epistemic autonomy through cumulative small concessions, but the extraction is not total — some agents maintain private dissent, organized subgroups resist full alignment, and exit is possible for some. The value reflects that the judgment transfer is substantial but not complete. Suppression (0.62): Moderate-high. Significant barriers to dissent include social penalty (ostracism, status loss), professional consequences (network exclusion, career damage), and internalized norms (belief that dissent equals disloyalty). But suppression is not total — some agents do dissent, and deliberative practices are reducing barriers in some contexts. Theater ratio (0.58): Moderate-high. Much of the consensus performance is ritualistic: participants perform agreement in group settings while maintaining private dissent. The theater has increased over the interval as the gap between private belief and public performance has widened. Initial voluntary alignment (theater_ratio 0.35) has given way to performative consensus (theater_ratio 0.58) as enforcement mechanisms matured.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full indexical range from identity-locked snare (powerless agents with cognitive binding) through tangled rope (moderate and organized agents experiencing mixed coordination and extraction) to rope (high-status beneficiaries experiencing pure coordination) to scaffold (organized agents building alternative mechanisms with sunset logic) to piton (vestigial consensus rituals in degraded contexts). The identity-locked dissenter sees pure extraction because their cognitive binding prevents them from accessing the coordination benefits or exit options that other agents perceive. The constrained participant sees tangled rope because they experience both genuine coordination (collective action capacity, social support) and extraction (judgment transfer, belief suppression). The high-status norm enforcer sees rope because they shape consensus rather than conform to it — extraction runs toward them. The organized subgroup sees tangled rope with lower effective extraction because collective identity provides partial autonomy. The deliberative democracy coalition sees scaffold because they are building alternative consensus mechanisms that will sunset the non-itemized judgment transfer mechanism. The vestigial consensus ritual perspective sees piton because the coordination function has atrophied while the performance persists. The analytical observer sees tangled rope because the constraint genuinely coordinates (enables collective action) while genuinely extracting (transfers judgment asymmetrically).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reflects asymmetric extraction from individual epistemic autonomy toward group coherence maintenance and high-status norm enforcers. Identity-locked dissenters (powerless/identity_locked) are victims with cognitive binding — they experience high d (≈0.89) because they are structurally mobile but functionally trapped by identity fusion. Constrained participants (moderate/constrained) are victims with real but surmountable exit costs — they experience moderate-high d (≈0.65) because they bear extraction but also receive coordination benefits. High-status norm enforcers (institutional/arbitrage) are beneficiaries with portable status — they experience low d (≈0.05) because extraction runs toward them through deference. Organized subgroups (organized/mobile) experience moderate d (≈0.55) because they maintain partial autonomy while performing conformity. The deliberative democracy coalition (organized/mobile) experiences moderate d (≈0.50) because they see the constraint as temporary and are building exit pathways. The analytical observer (analytical/analytical) uses the canonical analytical d (≈0.72) and classifies as tangled_rope, confirming the structural coherence of the mixed coordination-extraction hypothesis.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that conformity mechanisms serve genuine coordination functions (enabling collective action, reducing decision costs, maintaining group boundaries) while simultaneously extracting from individual epistemic autonomy through non-itemized judgment transfer. The coordination function is real: groups with shared beliefs can act collectively more efficiently than groups with persistent internal disagreement. The extraction is also real: the judgment transfer is not reciprocal (high-status members shape norms while low-status members conform), the mechanism is non-itemized (small concessions accumulate without explicit accounting), and the suppression is substantial (dissent carries social and professional costs). The constraint is neither a rope misclassified as extractive (the asymmetric judgment transfer is structural, not incidental) nor a snare misclassified as coordinative (the collective action benefits are genuine, not theatrical). The tangled rope classification captures the irreducible hybrid: you cannot remove the extraction without destroying the coordination function, and you cannot remove the coordination function without eliminating the extraction mechanism. The deliberative democracy coalition's scaffold perspective represents a structural bet that alternative consensus mechanisms (structured debate, anonymous voting, dissent protection) can preserve the coordination function while reducing the extraction — but this remains empirically unresolved (omega variable: deliberative_practice_effectiveness).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_belief_measurement,
    'Can private belief be measured without the measurement itself inducing conformity pressure?',
    'Comparison of anonymous survey responses, private diary entries, and revealed preference in low-stakes decisions vs stated positions in group settings. Longitudinal tracking of belief stability across social contexts.',
    'If measurement is contaminated: observed divergence between private and public position underestimates true extraction. If measurement is clean: observed divergence accurately reflects extraction magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_belief_measurement, empirical, 'Whether private belief can be measured without inducing conformity').

omega_variable(
    coordination_necessity_threshold,
    'What level of belief convergence is necessary for genuine coordination vs what level is extractive surplus?',
    'Experimental manipulation of consensus requirements in collective action tasks. Identification of minimum agreement thresholds for successful coordination across different task types.',
    'If threshold is low (e.g., 60% agreement sufficient): much observed conformity is extractive surplus. If threshold is high (e.g., 90% agreement necessary): most conformity serves coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Minimum consensus threshold for genuine coordination').

omega_variable(
    identity_lock_reversibility,
    'Is identity-based conformity extraction reversible through therapeutic intervention or does it require group exit?',
    'Longitudinal studies of individuals who underwent identity-focused therapy while remaining in conformity-extracting groups vs those who exited groups. Measurement of epistemic autonomy recovery trajectories.',
    'If reversible in situ: identity_locked classification overstates extraction severity. If exit-dependent: identity_locked classification accurately captures the binding mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock can be broken without group exit').

omega_variable(
    deliberative_practice_effectiveness,
    'Do deliberative democracy practices actually reduce conformity extraction or merely redistribute it to those who control the deliberative process?',
    'Comparison of private-public belief divergence in groups with vs without deliberative practices. Analysis of who speaks, whose arguments prevail, and whether dissent protection norms are enforced equitably.',
    'If effective: scaffold perspective confirmed — sunset is real. If redistributive: deliberative practices are themselves a tangled rope, and the scaffold perspective is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_practice_effectiveness, empirical, 'Whether deliberative practices reduce or redistribute extraction').

omega_variable(
    suppression_mechanism_partition,
    'What proportion of measured suppression is structural (social penalty, network loss) vs internalized (belief that dissent is wrong, identity fusion with group)?',
    'Post-exit suppression trajectory analysis: if suppression persists after leaving the group, it was partially internalized. Comparison of suppression levels in groups with identical social penalties but different ideological intensity.',
    'If primarily structural: exit reduces suppression immediately. If primarily internalized: suppression persists post-exit, and the constraint''s effective suppression is higher than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_partition, empirical, 'Structural vs internalized suppression partition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conformity_extraction, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conform_tr_t0, conformity_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(conform_tr_t3, conformity_extraction, theater_ratio, 3, 0.42).
narrative_ontology:measurement(conform_tr_t6, conformity_extraction, theater_ratio, 6, 0.5).
narrative_ontology:measurement(conform_tr_t9, conformity_extraction, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(conform_be_t0, conformity_extraction, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(conform_be_t3, conformity_extraction, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(conform_be_t6, conformity_extraction, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(conform_be_t9, conformity_extraction, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conformity_extraction, identity_coordination).
narrative_ontology:affects_constraint(conformity_extraction, epistemic_substitution).

% DUAL FORMULATION NOTE:
% Conformity extraction is downstream of epistemic substitution (the mountain constraint that individual judgment is constituted through social context). The upstream constraint establishes that some degree of social epistemic dependence is unavoidable; the downstream constraint measures how much of that dependence is coordinative vs extractive. The two constraints have different ε values because they measure different structural phenomena: epistemic_substitution measures the unavoidable baseline of social epistemic constitution (ε ≈ 0.08, mountain), while conformity_extraction measures the extractive surplus beyond that baseline (ε = 0.48, tangled_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
