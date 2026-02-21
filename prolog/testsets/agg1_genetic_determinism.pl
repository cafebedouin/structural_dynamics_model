% ============================================================================
% CONSTRAINT STORY: agg1_genetic_determinism
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_agg1_genetic_determinism, []).

:- use_module(library(plunit)).
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: agg1_genetic_determinism
 *   human_readable: The institutional claim that the AGG-1 gene deterministically causes aggression.
 *   domain: social/technological/legal
 *
 * SUMMARY:
 *   Based on a fictional 2025 discovery, this constraint models the societal
 *   and legal framework built around the claim that a specific gene, AGG-1,
 *   is a direct cause of aggressive behavior. The constraint is not the gene
 *   itself, but the system of identification, prediction, and control that
 *   emerges from treating this scientific claim as fact. This system creates
 *   a powerful tool for risk management and social control, but at the cost of
 *   individual autonomy and rights for those identified as carriers.
 *
 * KEY AGENTS (by structural relationship):
 *   - individuals_with_agg1_gene: Primary target (powerless/trapped) — they face discrimination, loss of rights, and social stigma based on a biological marker they cannot change.
 *   - justice_and_insurance_sectors: Primary beneficiary (institutional/arbitrage) — they gain a seemingly objective tool to quantify risk, determine culpability, and justify policies, increasing efficiency and control.
 *   - biotech_pharma_companies: Secondary beneficiary (institutional/arbitrage) — profit from developing and selling AGG-1 tests and potential "therapies."
 *   - analytical_observer: Analytical observer — sees the full structure as a system of social control built on a scientific claim, with both coordination functions and extreme extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agg1_genetic_determinism, 0.75). % High extraction of rights, autonomy, and economic opportunity.
domain_priors:suppression_score(agg1_genetic_determinism, 0.80).   % Structural property (raw, unscaled). Suppresses concepts of free will and rehabilitation for carriers.
domain_priors:theater_ratio(agg1_genetic_determinism, 0.15).       % Not a Piton; the system is highly functional for its beneficiaries.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agg1_genetic_determinism, extractiveness, 0.75).
narrative_ontology:constraint_metric(agg1_genetic_determinism, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(agg1_genetic_determinism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a human-constructed constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(agg1_genetic_determinism, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(agg1_genetic_determinism). % Required for Tangled Rope. System needs testing, databases, and legal frameworks.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, justice_and_insurance_sectors).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, biotech_pharma_companies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(agg1_genetic_determinism, individuals_with_agg1_gene).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Individuals with the gene cannot escape their biology (trapped), making them
% vulnerable to the full extractive power of the constraint.
% victim + trapped -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Institutions gain a powerful coordination tool for risk assessment. They
% can choose which models to use (arbitrage).
% beneficiary + arbitrage -> d ≈ 0.05 -> negative χ.
constraint_indexing:constraint_classification(agg1_genetic_determinism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function for institutions and the
% asymmetric extraction from individuals, recognizing the complete structure.
% This requires active enforcement to maintain.
% analytical -> d ≈ 0.72 -> high χ.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agg1_genetic_determinism_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    constraint_indexing:constraint_classification(agg1_genetic_determinism, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(agg1_genetic_determinism, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met, [nondet]) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, _),
    narrative_ontology:constraint_victim(agg1_genetic_determinism, _),
    domain_priors:requires_active_enforcement(agg1_genetic_determinism).

:- end_tests(agg1_genetic_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The score is high because accepting the claim allows for the nearly total removal of an individual's agency, rights, and economic future. It's a justification for preemptive action, discrimination, and differential pricing of services like insurance.
 *   - Suppression (S=0.80): The constraint is powerful because it suppresses the alternative model of individual responsibility and free will. It frames behavior as biologically determined, making counterarguments seem "unscientific." The lack of an exit path (one cannot change their genes) makes suppression extremely effective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For an individual carrier (powerless, trapped), the system is a Snare that removes their future prospects without recourse. For an institution (institutional, arbitrage), it's a valuable Rope—a coordination mechanism that simplifies the messy problem of human behavior into a quantifiable, manageable risk variable, increasing profits and control. The institution can choose to ignore the test if it's inconvenient; the individual cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `justice_and_insurance_sectors` and `biotech_pharma_companies` directly benefit. The former gain a tool for social control and risk pricing; the latter gain a new market for tests and treatments. Their `arbitrage` exit option gives them low derived directionality (d), leading to negative effective extraction (χ).
 *   - Victims: `individuals_with_agg1_gene` bear the full cost. Their `trapped` status derives a high d, leading to maximum effective extraction (χ).
 *   This mapping of structural relationships to derived directionality is central to the model.
 *
 * MANDATROPHY ANALYSIS:
 *   is_mandatrophy_resolved: [RESOLVED MANDATROPHY]
 *   This model avoids mislabeling the system as pure coordination (Rope) or pure bad faith (Snare). The analytical view (Tangled Rope) correctly identifies that the constraint *does* have a genuine coordination function for the beneficiary institutions. Ignoring this would be a mistake. However, it also recognizes that this coordination is built upon severe, asymmetric extraction from a trapped population. The framework forces this dual nature into the open, preventing the beneficiaries' "Rope" narrative from obscuring the victims' "Snare" reality. The potential for a coalition of victims to organize and gain power could shift their classification over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_agg1_determinism,
    'Is the link between AGG-1 and aggression truly deterministic and causal, or is it a complex statistical correlation being oversimplified for social control?',
    'Independent, replicated, longitudinal studies controlling for socioeconomic and environmental factors. Falsification attempts.',
    'If true/deterministic, the constraint approaches a Mountain of biology. If false/statistical, it is a socially constructed Tangled Rope/Snare built on scientific misinterpretation.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_agg1_determinism, empirical, 'Whether the AGG-1 gene link is causal or a simplified statistical correlation.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(agg1_genetic_determinism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the constraint's institutionalization over a decade, showing
% how a scientific discovery evolves into a tool of extraction. This demonstrates
% the "extraction_accumulation" drift pattern.
% T=0: Discovery (low extraction, low theater)
% T=5: Early adoption, pilot programs
% T=10: Full institutionalization (high extraction)

% Theater ratio over time:
narrative_ontology:measurement(agg1_tr_t0, agg1_genetic_determinism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(agg1_tr_t5, agg1_genetic_determinism, theater_ratio, 5, 0.10).
narrative_ontology:measurement(agg1_tr_t10, agg1_genetic_determinism, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(agg1_ex_t0, agg1_genetic_determinism, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(agg1_ex_t5, agg1_genetic_determinism, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(agg1_ex_t10, agg1_genetic_determinism, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint provides a standard for assessing risk and culpability.
narrative_ontology:coordination_type(agg1_genetic_determinism, information_standard).

% This constraint would structurally influence legal and financial systems.
narrative_ontology:affects_constraint(agg1_genetic_determinism, criminal_sentencing_guidelines).
narrative_ontology:affects_constraint(agg1_genetic_determinism, insurance_risk_pooling).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% power dynamics between the trapped individuals and the institutions with
% arbitrage options.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */