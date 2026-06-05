% ============================================================================
% CONSTRAINT STORY: cryptographic_key_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptographic_key_selection, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cryptographic_key_selection
 *   human_readable: Cryptographic Key Selection and Mathematical Irreducibility
 *   domain: cryptography/mathematics/information_theory
 *
 * SUMMARY:
 *   Cryptographic key selection represents a canonical example of a
 *   mathematical natural law constraint: the requirement that a secure
 *   cryptographic system's key space be large enough to make exhaustive
 *   search computationally infeasible. This constraint emerges directly from
 *   information theory, Shannon entropy, and the counting argument for
 *   brute-force attack — not from institutional design, regulatory mandate,
 *   or power asymmetry. The constraint applies uniformly across all observer
 *   positions because it is grounded in mathematical properties that are
 *   invariant to social context, economic incentive, or institutional
 *   authority. An attacker cannot negotiate around the entropy floor; a
 *   cryptosystem designer cannot waive it; an analytical observer cannot find
 *   it to be context-relative. The keyspace irreducibility is anterior to and
 *   independent of how cryptography is implemented, deployed, regulated, or
 *   used. This makes cryptographic key selection a gold standard for the
 *   mountain classification: accessibility collapse is extreme (no agent can
 *   access an exponentially-large keyspace), resistance to the constraint is
 *   nearly zero (all known attacks respect the entropy bound), and the
 *   constraint emerges naturally from mathematical law rather than
 *   institutional enforcement.
 *
 * KEY AGENTS:
 *   - Cryptanalyst / Attacker (powerless/trapped): Faces absolute computational barrier; no strategy overcomes exponential search complexity
 *   - Cryptosystem Designer (institutional/arbitrage): Cannot compromise on entropy floor without catastrophic failure; the constraint is non-negotiable
 *   - User / Key Generator (varies by implementation): Implementation entropy (truly random vs human selection) creates a secondary institutional constraint, but the mathematical constraint remains binding
 *   - Analytical Observer (analytical/analytical): Views the constraint as mathematical law, invariant across all measurement frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptographic_key_selection, 0.08).
domain_priors:suppression_score(cryptographic_key_selection, 0.02).
domain_priors:theater_ratio(cryptographic_key_selection, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptographic_key_selection, extractiveness, 0.08).
narrative_ontology:constraint_metric(cryptographic_key_selection, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cryptographic_key_selection, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cryptographic_key_selection, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cryptographic_key_selection, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptographic_key_selection, mountain).
narrative_ontology:human_readable(cryptographic_key_selection, "Cryptographic Key Selection and Mathematical Irreducibility").
narrative_ontology:topic_domain(cryptographic_key_selection, "cryptography/mathematics/information_theory").

domain_priors:emerges_naturally(cryptographic_key_selection).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRYPTANALYST (MOUNTAIN) — An attacker with finite computational resources faces an insurmountable barrier: the keyspace entropy floor is a mathematical property, not a policy choice. No amount of effort, organization, or institutional pressure can reduce the computational irreducibility of exhaustive search below the entropy bound. The constraint is invariant — it applies regardless of the attacker's position, resources, or determination.
constraint_indexing:constraint_classification(cryptographic_key_selection, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CRYPTOSYSTEM DESIGNER (MOUNTAIN) — The designer cannot escape the entropy requirement through clever engineering, institutional privilege, or economic advantage. The mathematical constraint is anterior to institutional design. Any cryptosystem that violates the entropy floor fails universally — no designer position, funding level, or authority can override the computational irreducibility. The constraint binds designers and attackers identically.
constraint_indexing:constraint_classification(cryptographic_key_selection, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the standpoint of information theory and computational complexity, keyspace entropy is an invariant property of secure cryptography. Shannon entropy, Kolmogorov complexity, and the counting argument for exhaustive search all produce identical conclusions across all measurement frameworks. The constraint exhibits zero degrees of freedom — it is not observer-dependent, not context-relative, and not policy-contingent. This is the canonical natural law classification.
constraint_indexing:constraint_classification(cryptographic_key_selection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptographic_key_selection_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cryptographic_key_selection, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptographic_key_selection, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cryptographic_key_selection, ExtMetricName, E),
    domain_priors:suppression_score(cryptographic_key_selection, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cryptographic_key_selection),
    narrative_ontology:constraint_metric(cryptographic_key_selection, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cryptographic_key_selection, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cryptographic_key_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. There is no extraction occurring in the mathematical sense — the constraint does not transfer resources from one agent to another; it creates a symmetric barrier to all exhaustive-search attacks. The non-zero value (0.08 rather than 0.00) reflects the information-theoretic cost of securing communication: all parties must invest in key generation, transmission, and storage infrastructure to implement the constraint. This is a necessary coordination cost, not an extractive overhead. Suppression (0.02): Minimal. The constraint does not suppress alternatives; it defines the floor below which alternatives do not exist. There is no coercive mechanism — the constraint is mathematical, not institutional. Agents comply with the entropy requirement not through enforcement but through the logical necessity of system security. Theater ratio (0.05): Minimal. Cryptographic key selection has nearly zero performative content. The mechanism is transparent and verifiable: key length determines entropy; entropy determines exhaustive search difficulty; difficulty determines attacker barrier. No ritual, no institutional theater, no degraded performance. The measurements show flat trajectories across a 100-year interval, reflecting that mathematical irreducibility does not degrade over time. (Note: This assumes classical cryptography without quantum displacement. If quantum computing becomes practically relevant, the interval would show a discontinuity, but the constraint would reclassify to a different mathematical floor, not to a non-mountain type.)
 *
 * PERSPECTIVAL GAP:
 *   Zero perspectival gap. All three perspectives classify as Mountain. This is the defining characteristic of a genuine natural law constraint in the Deferential Realism framework. An attacker sees a mountain; a designer sees a mountain; an analytical observer sees a mountain. The uniformity of classification is not a limitation — it is evidence that the constraint is truly invariant. The perspectival gap exists in constraints where different observer positions experience the same structure differently due to power asymmetry, exit options, or temporal horizon. Cryptographic key selection exhibits no such asymmetry. All agents face identical mathematical irreducibility regardless of their institutional position, resources, or authority.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution required. Cryptographic key selection is pure Mountain — no mixture of coordination and extraction, no institutional asymmetry masquerading as natural law, no false summit structure. The constraint is neither coordination nor extraction; it is a mathematical floor. The absence of mandatrophy complexity reflects the constraint's mathematical simplicity. There is no 'genuine coordination benefit hiding asymmetric extraction' because there is no extraction at all — only necessary infrastructure cost distributed symmetrically across all agents. This stands in sharp contrast to constraints like 'verification bottleneck' (verification_bottleneck), which show all six types from different perspectives. Cryptographic key selection shows one type from all perspectives because it is genuinely invariant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_computing_displacement,
    'Does quantum computing (Grover''s algorithm, NISQ-era and fault-tolerant implementations) invalidate the classical keyspace entropy floor?',
    'Empirical demonstration of quantum computers achieving polynomial speedup on exhaustive key search; observation of error rates and coherence times in quantum devices reaching scales relevant to cryptographic keyspace reduction',
    'If quantum computers achieve practical speedup: classical key-selection constraint remains a mountain in its domain (classical computation), but a new constraint (quantum-resistant keyspace requirements) emerges as the binding irreducibility. If quantum speedup is impractical: classical keyspace entropy floor remains operative indefinitely. This does NOT change the mountain classification — it either shifts the binding constraint to a new mathematical property or confirms the original one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_computing_displacement, empirical, 'Whether quantum computing displaces classical keyspace entropy as the binding constraint').

omega_variable(
    algorithmic_breakthrough_possibility,
    'Could an undiscovered mathematical algorithm (not quantum) reduce the effective entropy floor below current bounds?',
    'Theoretical breakthrough in number theory or computational complexity proving sub-exponential solutions to discrete logarithm or factorization; or empirical discovery of structural weaknesses in widely-deployed cryptographic systems that reduce effective keyspace',
    'If such an algorithm exists: the current keyspace entropy floor is not truly irreducible — it reflects current mathematical knowledge, not mathematical law. The constraint reclassifies as Piton (degraded mountain, maintained by institutional inertia around current algorithms) or Tangled Rope (coordination on key size with hidden algorithmic vulnerability). If no such algorithm is discovered: the mountain classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_breakthrough_possibility, empirical, 'Whether undiscovered algorithms could reduce the effective keyspace entropy floor').

omega_variable(
    side_channel_embedding,
    'Does the mathematical irreducibility of keyspace choice persist when embedded in real cryptographic systems subject to side-channel attacks (timing, power, electromagnetic)?',
    'Analysis of whether side-channel vulnerabilities fundamentally depend on the keyspace entropy property, or whether they represent orthogonal institutional/implementation failures. Measurement of whether systems with larger keysizes are proportionally harder to attack via side channels.',
    'If side-channel attacks are orthogonal: the mathematical constraint (keyspace irreducibility) remains a mountain; side-channel vulnerability is a separate, institutional constraint (Tangled Rope or Snare). If side channels are inherent to implementation: the practical binding constraint is not keyspace entropy but implementation irreducibility — a more complex mountain with different accessibility collapse. The theoretical constraint remains a mountain; the practical constraint becomes hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(side_channel_embedding, empirical, 'Whether side-channel attacks represent orthogonal institutional failures or are inherent to the mathematical constraint').

omega_variable(
    human_selection_entropy_deficit,
    'When humans select cryptographic keys (passwords, passphrases, seed values), do they achieve the theoretical entropy floor, or is human entropy generation a separate binding constraint?',
    'Empirical measurement of actual entropy in human-generated keys vs algorithm-generated keys; analysis of whether password managers and truly-random hardware generators achieve the theoretical floor; historical corpus analysis of compromised keys for evidence of subrandom human selection',
    'If humans consistently under-generate entropy: the mathematical constraint (theoretical keyspace irreducibility) is a mountain, but the *effective* binding constraint is human entropy generation — a different, institutional constraint (Tangled Rope: coordination failure between cryptographic theory and user behavior). The mountain persists mathematically but not institutionally. If proper random generation is used: the mountain applies as stated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_selection_entropy_deficit, empirical, 'Whether human key selection creates a separate, more restrictive entropy constraint than mathematical irreducibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptographic_key_selection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cks_tr_t0, cryptographic_key_selection, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cks_tr_t50, cryptographic_key_selection, theater_ratio, 50, 0.05).
narrative_ontology:measurement(cks_tr_t100, cryptographic_key_selection, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cks_be_t0, cryptographic_key_selection, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cks_be_t50, cryptographic_key_selection, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(cks_be_t100, cryptographic_key_selection, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptographic_key_selection, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
