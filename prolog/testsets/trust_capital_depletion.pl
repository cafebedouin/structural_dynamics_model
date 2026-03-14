% ============================================================================
% CONSTRAINT STORY: trust_capital_depletion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trust_capital_depletion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trust_capital_depletion
 *   human_readable: Trust Capital Depletion in Institutional Relationships
 *   domain: institutional_dynamics/relational_economics
 *
 * SUMMARY:
 *   Trust capital depletion describes a structural extraction mechanism where
 *   institutional actors accumulate short-term benefits by depleting the
 *   shared trust resource that enables all institutional function. The
 *   constraint operates at the intersection of individual incentives
 *   (extraction is profitable in the short term) and collective outcomes
 *   (excessive extraction degrades institutional function for all future
 *   actors). Trust capital is a commons with no individual owner and no
 *   enforcing agent — it accumulates through repeated cooperative
 *   interactions and depletes through betrayal, hypocrisy, or expectation
 *   violation. Once depleted below a threshold, institutional function
 *   collapses because voluntary cooperation becomes irrational: agents can no
 *   longer assume counterparties will honor agreements. The extractiveness
 *   value (0.58) reflects that immediate beneficiaries capture significant
 *   value from trust depletion while bearing none of the cost; suppression
 *   (0.65) reflects high barriers to exit and limited alternatives; theater
 *   ratio (0.55) reflects that much institutional response to trust crises is
 *   performative (apologies, reforms, reputational recovery) rather than
 *   functionally restorative.
 *
 * KEY AGENTS:
 *   - Trust Commons: Powerless/trapped victim (collective epistemic resource, no agent to defend it, absorbs all reputational cost)
 *   - Future Cooperators: Moderate/constrained victims (inherit depleted trust capital, face high cost of entry to alternative institutions)
 *   - Immediate Extractors: Institutional/arbitrage beneficiaries (capture short-term value, experience constraint as enabling through theatrical trust maintenance)
 *   - Accountability Movement: Organized/constrained secondary actors (must continuously enforce norms to prevent collapse, themselves subject to extraction)
 *   - Reputation Recovery Industry: Institutional/arbitrage actors (maintain theater through reform narratives despite low functional impact)
 *   - Alternative Trust Architecture Builders: Powerful/mobile agents (building exit pathways through decentralization and transparency structures)
 *   - Analytical Observer: Civilizational/analytical perspective (risks naturalizing contingent design as evolutionary necessity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trust_capital_depletion, 0.58).
domain_priors:suppression_score(trust_capital_depletion, 0.65).
domain_priors:theater_ratio(trust_capital_depletion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trust_capital_depletion, extractiveness, 0.58).
narrative_ontology:constraint_metric(trust_capital_depletion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trust_capital_depletion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trust_capital_depletion, snare).
narrative_ontology:human_readable(trust_capital_depletion, "Trust Capital Depletion in Institutional Relationships").
narrative_ontology:topic_domain(trust_capital_depletion, "institutional_dynamics/relational_economics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trust_capital_depletion, immediate_extractors).
narrative_ontology:constraint_victim(trust_capital_depletion, trust_commons).
narrative_ontology:constraint_victim(trust_capital_depletion, future_cooperators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUST COMMONS (SNARE) — The collective trust resource that enables all institutional function has no agent to defend it, no exit mechanism, and no capacity to refuse extraction. Absorbs all reputational cost of institutional betrayal. Maximally trapped, maximally exploited. The commons bears the structural extraction load.
constraint_indexing:constraint_classification(trust_capital_depletion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUTURE COOPERATOR (SNARE) — Inherits depleted trust capital and faces suppression: high cost of entry (must rebuild trust), limited alternative institutions, and sunk cost pressure. Constrained rather than trapped — can exit to other institutions if they exist, but the ecosystem is contaminated. High extraction, significant suppression.
constraint_indexing:constraint_classification(trust_capital_depletion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMMEDIATE EXTRACTOR (ROPE) — Extracts value from institutional trust in the short term. From their perspective, the constraint is coordination: maintaining just enough trust appearance to enable continued extraction. The theatrical maintenance of trust (apologies, reforms, reputational recovery) is their coordination function. They experience the constraint as enabling their activity, not as restricting it.
constraint_indexing:constraint_classification(trust_capital_depletion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ACCOUNTABILITY MOVEMENT (TANGLED ROPE) — Organized agents (civil society, media, regulators) see the trust depletion as a hybrid coordination-extraction problem. They coordinate enforcement of accountability norms while simultaneously experiencing extraction: constant mobilization required, burnout, regulatory capture risks. The constraint requires their active enforcement to prevent total collapse, but enforcement itself becomes extractive.
constraint_indexing:constraint_classification(trust_capital_depletion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUTATION RECOVERY INDUSTRY (PITON) — PR firms, consulting agencies, and institutional reform experts maintain the appearance that trust can be rebuilt through communication campaigns and procedural reforms. The theater ratio (0.55) reflects that much reputation recovery is performative ritual with limited functional impact on actual trustworthiness. The industry persists through institutional inertia — institutions keep hiring reputation managers despite low success rates because no alternative visibility strategy exists.
constraint_indexing:constraint_classification(trust_capital_depletion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE TRUST ARCHITECTURE (SCAFFOLD) — Agents building decentralized verification systems, blockchain-based transparency, and distributed governance structures see trust capital depletion as a temporary coordination failure with a sunset. Trust can be rebuilt through structural change (removing intermediaries, increasing transparency, distributed verification). This perspective has agency and exit — they can build alternative systems. Theater is low because alternatives are functional, not performative.
constraint_indexing:constraint_classification(trust_capital_depletion, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, trust depletion may appear as an immutable feature of human organization: incentive misalignment between short-term exploitation and long-term cooperation is a fundamental problem in evolutionary game theory. Trust inevitably depletes when extraction benefits exceed reputation costs. This perspective risks naturalizing what is actually a contingent feature of institutional design and enforcement capacity.
constraint_indexing:constraint_classification(trust_capital_depletion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trust_capital_depletion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trust_capital_depletion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trust_capital_depletion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trust_capital_depletion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trust_capital_depletion, TR),
    TR >= 0.70.

:- end_tests(trust_capital_depletion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The immediate extractors capture clear benefits — career advancement, financial gain, institutional power — during the period before trust collapse. The measurement trajectory (0.25 → 0.42 → 0.58) shows acceleration consistent with extractive accumulation: the more an institution violates trust expectations, the larger the short-term extraction becomes until collapse threshold. The value is not higher because some extraction appears justified as normal institutional overhead. Suppression (0.65): High. Barriers to exit include: (1) Specialized dependence on the institution (career investment, credentials, relational capital tied to institutional status), (2) Limited alternatives (other institutions in the same domain may be equally compromised), (3) Geographic/sectoral concentration (small number of competing institutions), (4) Sunk costs (exit requires abandoning accumulated position). Future cooperators bear the highest suppression — they enter an ecosystem already poisoned and have minimal leverage. Theater ratio (0.55): Moderate-high. Institutional responses to trust crises typically center on communication and procedural reform: apologies, investigations, personnel changes, new compliance procedures. Many of these actions are theater — they address appearance rather than root incentive misalignment. The trajectory (0.30 → 0.42 → 0.55) shows increasing theater as the institution depletes actual trust and relies more on performative recovery. Claimed type (Snare): The structure is pure extraction with suppressed alternatives. The immediate extractors have no incentive to cooperate (they benefit from defection), future cooperators cannot exit (trapped by dependency and limited alternatives), and the commons has no defense mechanism. This is the defining structure of a snare.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the immediate extractor's 'rope' classification and the powerless victim's 'snare' classification reveals the core asymmetry. To the extractor, the constraint is coordination — maintaining just enough trust appearance to enable continued extraction. To the victim, the constraint is pure extraction — absorbing cost with no benefit. This gap is precisely the asymmetry that defines a snare: one agent's coordination problem is another agent's extraction mechanism. The scaffold perspective (alternative architecture builder) shows that the snare is not inevitable — it depends on there being no functional exit. As soon as alternatives exist (decentralized systems, distributed verification), the constraint becomes temporary and solvable. The natural law perspective risks collapsing this gap by treating the snare as inherent to human nature rather than as a contingent feature of institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to the extraction flow. Immediate extractors benefit from the constraint (low d → negative effective extraction), while powerless victims bear the cost (high d → high effective extraction). The pipeline computes d from power level (affects absolute capacity) and exit options (affects relative cost of exploitation). Trapped agents with no alternatives experience the maximum derived d (approaching 1.0), producing the highest effective extraction. Agents with arbitrage options experience low d (near 0.0), experiencing the constraint as beneficial or neutral. Organized agents with some enforcement capacity and coordinated response options experience moderate d, producing moderate effective extraction. The analytical observer at civilizational scope occupies a unique position (d ≈ 0.72 by canonical) that may risk over-generalizing local institutional dynamics into universal principles.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the snare classification is structurally stable despite the appearance of coordination features. The 'coordination' that the immediate extractor perceives (maintaining institutional trust appearance through theater) is not genuine coordination — it is a confidence game that persists only as long as future cooperators believe the institution can be trusted. Once depletion reaches threshold, the coordination collapses catastrophically. The theater (0.55 value) indicates that institutions invest heavily in performative trust recovery precisely because true functional trust is depleted. This is the diagnostic signature of a snare: high-theater response to extraction crisis. A true rope constraint would show low theater because functional coordination reduces friction. A true scaffold would show declining theater as functional alternatives scale. The piton classification for the reputation recovery industry shows institutional inertia — the industry persists despite low functional impact because no alternative mechanism for institutional trust recovery exists. This inertia is distinct from the snare's active extraction; it represents degraded function maintained through performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_measurement_ambiguity,
    'What observable operational definition of ''trust capital'' permits measurement of its depletion rate?',
    'Operationalize trust through cooperation willingness (survey), transaction costs (economic), or reputation signals (behavioral). Measure depletion by comparing baseline cooperation rates to post-violation rates across populations.',
    'If trust is purely behavioral: depletion is measurable and the snare classification is empirically grounded. If trust is primarily psychological/emotional: depletion is interpretive and the classification risks over-reification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_measurement_ambiguity, empirical, 'Operational definition and measurement of trust capital depletion').

omega_variable(
    recovery_capacity_threshold,
    'What depletion threshold triggers irreversible institutional collapse versus recoverable partial depletion?',
    'Historical case studies of institutional trust collapse (banking crises, regime transitions, community breakdown). Identify threshold where voluntary cooperation no longer sustains the institution.',
    'If threshold exists and is identifiable: snare classification holds with predictive force. If threshold is indeterminate: trust depletion is gradient, not binary, and the mountain ''natural law'' framing becomes more credible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recovery_capacity_threshold, empirical, 'Irreversible collapse threshold for institutional trust').

omega_variable(
    extraction_vs_legitimate_cost,
    'How much institutional overhead (audit costs, transparency requirements, reform spending) represents legitimate institutional maintenance versus exploitative extraction disguised as accountability?',
    'Benchmark institutional spending on internal accountability (compliance, audit, investigation) against external service delivery. Compare ratio between high-trust and low-trust institutions in the same sector.',
    'If legitimate overhead is high: suppression (0.65) may overstate extraction (0.58), and the constraint is closer to tangled_rope than snare. If legitimate overhead is low: extraction is higher than measured, and snare classification is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimate_cost, empirical, 'Legitimate institutional overhead versus extractive overhead ratio').

omega_variable(
    collective_action_prisoner_dilemma,
    'Is trust capital depletion a tragedy-of-the-commons problem (individually rational extraction leads to collective ruin) or a structural asymmetry (extractors are different agents from those bearing costs)?',
    'Identify whether the same institutional actors who extract also bear depletion costs. If identical: tragedy-of-commons (coordination problem). If different: structural extraction (snare).',
    'If tragedy: the constraint is rope with coordination failure — all agents benefit from cooperation but incentives push defection. If structural: snare classification is confirmed — extraction is asymmetric and cannot be solved by cooperation among current actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_prisoner_dilemma, conceptual, 'Tragedy-of-commons versus structural extraction asymmetry').

omega_variable(
    exit_option_sufficiency,
    'Do alternative institutions (new firms, decentralized systems, informal networks) actually provide functional exit for agents trapped in depleted-trust institutions?',
    'Track migration of agents to alternative institutions post-trust-collapse. Measure whether alternatives actually deliver equivalent service or are merely perceived as ''less bad''.',
    'If exits are real: trapped agents can become constrained, classification shifts toward tangled_rope. If exits are illusory: agents are genuinely trapped, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_sufficiency, empirical, 'Functional versus illusory exit options for trapped agents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trust_capital_depletion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcd_tr_t0, trust_capital_depletion, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tcd_tr_t5, trust_capital_depletion, theater_ratio, 5, 0.42).
narrative_ontology:measurement(tcd_tr_t10, trust_capital_depletion, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(tcd_be_t0, trust_capital_depletion, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(tcd_be_t5, trust_capital_depletion, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tcd_be_t10, trust_capital_depletion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trust_capital_depletion, attachment_coordination).
narrative_ontology:affects_constraint(trust_capital_depletion, institutional_legitimacy_crisis).
narrative_ontology:affects_constraint(trust_capital_depletion, reputation_asymmetry_extraction).
narrative_ontology:affects_constraint(trust_capital_depletion, cooperation_willingness_decline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trust_capital_depletion, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
