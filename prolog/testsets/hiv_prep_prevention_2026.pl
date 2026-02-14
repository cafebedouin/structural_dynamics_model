% ============================================================================
% CONSTRAINT STORY: hiv_prep_prevention_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-01
% ============================================================================

:- module(constraint_hiv_prep_prevention_2026, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:base_extractiveness/2,
    domain_priors:emerges_naturally/1,
    domain_priors:requires_active_enforcement/1,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:boltzmann_floor_override/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hiv_prep_prevention_2026
 *   human_readable: PrEP-Mediated HIV Prevention
 *   domain: technological/social
 *
 * SUMMARY:
 *   Pre-exposure prophylaxis (PrEP) is a daily antiviral medication that reduces
 *   the risk of HIV transmission by over 90% when used correctly. It transforms
 *   HIV from an unmanageable risk into a preventable condition, enabling safer
 *   sexual practices and reducing public health burdens. The constraint is the
 *   biochemical efficacy of the drug, which creates new possibilities for
 *   coordination (safe sex) and public health management.
 *
 * KEY AGENTS (by structural relationship):
 *   - At-risk Individuals: Primary target of the intervention (powerless/trapped) —
 *     they use PrEP to mitigate a biological risk. Structurally, they are beneficiaries.
 *   - Public Health Institutions (WHO/FDA): Primary beneficiary (institutional/arbitrage) —
 *     they use PrEP as a tool to reduce transmission rates and manage the epidemic.
 *   - Biological Researcher: Analytical observer — sees the biochemical efficacy as a
 *     fixed, natural law-like fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: PrEP is highly generative, but the cost of daily medication and
% reliance on pharmaceutical supply chains introduces a minor extractive element.
domain_priors:base_extractiveness(hiv_prep_prevention_2026, 0.15).

% Rationale: The drug's efficacy is a biological fact that does not coercively
% suppress human alternatives (e.g., condoms). Its suppression score is low,
% reflecting the lack of active coercion against other prevention methods.
domain_priors:suppression_score(hiv_prep_prevention_2026, 0.05).
domain_priors:theater_ratio(hiv_prep_prevention_2026, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, extractiveness, 0.15).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain. The biochemical
% efficacy of PrEP is treated as a natural law-like fact.
% accessibility_collapse: High, as the biochemical pathway is fixed.
% resistance: Zero, as one cannot "resist" a chemical reaction's efficacy.
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hiv_prep_prevention_2026, mountain).

% --- Binary flags ---
% Rationale: Requires active adherence (daily pill) and medical monitoring.
domain_priors:requires_active_enforcement(hiv_prep_prevention_2026).

% --- Emergence flag (required for mountain constraints) ---
% The drug is human-designed, but its efficacy emerges from the natural
% laws of biochemistry once the molecule is introduced into the system.
domain_priors:emerges_naturally(hiv_prep_prevention_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% This constraint has a dual nature (Rope/Mountain). Beneficiary data is
% provided to support the Rope classification from user/institutional perspectives.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, at_risk_individuals).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, public_health_systems).
%
% Who bears disproportionate cost?
% No significant human victim group is identified; the primary target of the
% drug's suppressive action is the virus itself, not a human agent.

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

% PERSPECTIVE 1: THE PREP USER (ROPE)
% For an individual at risk, PrEP is a pure coordination tool (Rope). It
% allows them to engage in sexual activity safely, coordinating with partners
% to manage a shared biological risk. The 'trapped' exit refers to the
% inescapable reality of the epidemic before this tool was available.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PUBLIC HEALTH INSTITUTION (ROPE)
% For regulators like the WHO, PrEP is a Rope—a functional policy tool used to
% coordinate a global public health response, slash transmission rates, and
% stabilize health systems across nations.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BIOLOGICAL RESEARCHER (MOUNTAIN)
% From an analytical perspective, the >90% efficacy of antivirals against the
% HIV replication cycle is a Mountain—a fixed, unchangeable biochemical fact
% demonstrated by repeated trials. It is a law of modern medicine.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiv_prep_prevention_2026_tests).

test(perspectival_gap_rope_vs_mountain) :-
    % Verify the gap between the user (Rope) and analyst (Mountain).
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, mountain,
        context(agent_power(analytical), _, _, _)).

test(mountain_metric_thresholds) :-
    % Verify that the base metrics are consistent with a Mountain classification.
    domain_priors:base_extractiveness(hiv_prep_prevention_2026, E),
    domain_priors:suppression_score(hiv_prep_prevention_2026, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(hiv_prep_prevention_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core of this story is the dual nature of a technological breakthrough.
 *   The base extractiveness (0.15) reflects the minor economic cost (daily pills)
 *   of accessing an otherwise highly generative technology. The suppression score
 *   (0.05) is low because PrEP does not coercively eliminate other prevention
 *   methods like condoms. The file includes the full Natural Law profile
 *   (accessibility_collapse, resistance, emerges_naturally) to certify the
 *   Mountain classification from the analytical perspective, treating the drug's
 *   biochemical efficacy as a fixed law of nature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between function and foundation.
 *   - The User/Institution (Rope): They experience PrEP as a tool for coordination.
 *     It enables new, safer behaviors and policies.
 *   - The Analyst (Mountain): They see the underlying mechanism—the biochemical
 *     efficacy—as an unchangeable fact, a law of pharmacology. The Rope's
 *     existence is predicated on this Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint primarily benefits human agents. Beneficiaries are 'at_risk_individuals'
 *   and 'public_health_systems'. There is no clear human victim group bearing
 *   asymmetric costs; the entity being suppressed is the HIV pathogen. This lack
 *   of a human victim group is why Snare/Tangled Rope classifications are absent.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a technological fact (Mountain) that
 *   enables a coordination function (Rope). It avoids mislabeling the minor
 *   extractive cost as a Snare, because the suppression of alternatives is
 *   extremely low and the coordination benefit is immense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_prep_resistance,
    'Will long-term mass usage lead to HIV strains resistant to current PrEP drugs?',
    'Genetic sequencing of transmission breakthroughs in high-usage regions',
    'If yes, the Mountain erodes and the Rope snaps, requiring new research. If no, the Rope is permanent.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_access_equity,
    'Does unequal access to PrEP create a de facto Snare for underserved communities?',
    'Sub-national audit of PrEP prescriptions and costs in communities served vs. underserved',
    'If access is highly unequal, the *system of access* is a separate Snare constraint, even if the drug itself is a Rope/Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hiv_prep_prevention_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is low (0.15), so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% PrEP acts as a piece of global infrastructure for managing public health risk.
narrative_ontology:coordination_type(hiv_prep_prevention_2026, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary declarations
% is sufficient to model the directionality for user and institutional perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */