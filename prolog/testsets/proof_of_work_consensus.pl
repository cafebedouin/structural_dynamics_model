% ============================================================================
% CONSTRAINT STORY: proof_of_work_consensus
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_proof_of_work_consensus, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proof_of_work_consensus
 *   human_readable: Hash-Based Proof-of-Work Consensus
 *   domain: technological/economic
 *
 * SUMMARY:
 *   A distributed consensus mechanism that replaces trusted third parties with
 *   computational proof. It timestamps transactions by hashing them into an
 *   ongoing chain, forming a record that is computationally impractical to
 *   change. This provides a coordination function (ordering transactions) but
 *   extracts significant value via energy consumption, paid for through
 *   protocol-defined inflation and transaction fees.
 *
 * KEY AGENTS (by structural relationship):
 *   - Non-Mining User: Primary target (powerless/trapped) — relies on the network's security but pays for it via inflation and fees.
 *   - Miners and Protocol Developers: Primary beneficiary (institutional/arbitrage) — benefit from block rewards, fees, and control over the protocol's direction.
 *   - Malicious Attacker: Secondary target (powerful/constrained) — actively suppressed by the computational difficulty and economic incentives.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_of_work_consensus, 0.50).
domain_priors:suppression_score(proof_of_work_consensus, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(proof_of_work_consensus, 0.11).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_of_work_consensus, extractiveness, 0.50).
narrative_ontology:constraint_metric(proof_of_work_consensus, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(proof_of_work_consensus, theater_ratio, 0.11).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(proof_of_work_consensus, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(proof_of_work_consensus). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, miners_and_protocol_developers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(proof_of_work_consensus, network_users).
narrative_ontology:constraint_victim(proof_of_work_consensus, malicious_attackers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE NON-MINING USER (MOUNTAIN)
% Agent who relies on the network's security. This is a subjective
% classification overriding the metrics; the user perceives an unchangeable
% foundation of trust, even though it is an actively maintained system.
constraint_indexing:constraint_classification(proof_of_work_consensus, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE MINER / PROTOCOL (ROPE)
% Agent who benefits from the system's incentives. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(proof_of_work_consensus, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MALICIOUS ATTACKER (SNARE)
% Agent seeking to subvert the system. The high suppression and extraction
% (cost to attack) make the constraint a Snare from this perspective.
constraint_indexing:constraint_classification(proof_of_work_consensus, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the vital coordination function
% (Rope) and the significant, asymmetric extraction (Snare component).
constraint_indexing:constraint_classification(proof_of_work_consensus, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proof_of_work_consensus_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between key agents.
    constraint_indexing:constraint_classification(proof_of_work_consensus, TypeUser, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, TypeMiner, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeUser == mountain,
    TypeMiner == rope,
    TypeAnalyst == tangled_rope.

test(tangled_rope_structural_properties) :-
    % Verify the structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(proof_of_work_consensus, _),
    narrative_ontology:constraint_victim(proof_of_work_consensus, _),
    domain_priors:requires_active_enforcement(proof_of_work_consensus).

:- end_tests(proof_of_work_consensus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.50): This value reflects the significant and continuous
 *     cost of securing the network. It is not a direct tax but an indirect one paid
 *     by all holders through protocol-level inflation (block rewards) and direct
 *     costs via transaction fees. The energy consumption is a real-world value
 *     extraction required to produce the intangible good of "security".
 *   - Suppression Score (0.90): The mechanism is explicitly designed to make
 *     alternative versions of the ledger computationally impractical to create,
 *     thus suppressing double-spending attacks with extreme prejudice.
 *   - requires_active_enforcement: The consensus rules are not passive; they
 *     require continuous, active enforcement by all nodes on the network through
 *     computational work and software validation.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is profound. A non-technical user (powerless) perceives the ledger's
 *     immutability as a law of nature (Mountain). The protocol's beneficiaries
 *     (institutional) see a pure coordination tool (Rope). An attacker sees a
 *     coercive trap (Snare). The analytical observer sees the synthesis: a system
 *     that performs a coordination function at the cost of high, asymmetric
 *     extraction (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Miners and protocol developers directly profit from the system's
 *     operation through block rewards and transaction fees.
 *   - Victims: All users of the network bear the cost of security through currency
 *     debasement (inflation) and fees. Malicious attackers are also victims in the
 *     sense that the system is designed to extract maximum cost from their attempts
 *     to subvert it.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification correctly identifies that Proof-of-Work is not
 *   a pure public good (Rope) nor a pure coercive trap (Snare). It has an
 *   essential coordination function (ordering transactions) that cannot be dismissed,
 *   but this function is bundled with a significant extractive process. Labeling it
 *   a pure Rope ignores the immense energy costs and wealth transfer to miners.
 *   Labeling it a pure Snare ignores its function in solving the Byzantine Generals Problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_pow_centralization,
    "Does the economic incentive for miners to consolidate into large pools inevitably centralize the network, undermining its core value proposition of decentralization?",
    "Continuous monitoring of hash rate distribution across mining pools and geographic locations; economic analysis of economies of scale in mining.",
    "If centralization occurs, the 'Tangled Rope' degrades into a 'Snare' controlled by a few powerful entities. If decentralization is maintained, it remains a functional (if costly) coordination mechanism.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_pow_centralization, empirical, "The long-term tendency of mining incentives toward centralization.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(proof_of_work_consensus, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (base_extractiveness > 0.46).
% Models the increasing energy cost and professionalization of mining over time.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(pow_tr_t0, proof_of_work_consensus, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pow_tr_t5, proof_of_work_consensus, theater_ratio, 5, 0.10).
narrative_ontology:measurement(pow_tr_t10, proof_of_work_consensus, theater_ratio, 10, 0.11).

% Extraction over time (increases as network value and competition grow):
narrative_ontology:measurement(pow_ex_t0, proof_of_work_consensus, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pow_ex_t5, proof_of_work_consensus, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pow_ex_t10, proof_of_work_consensus, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(proof_of_work_consensus, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */