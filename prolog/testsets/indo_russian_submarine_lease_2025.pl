% ============================================================================
% CONSTRAINT STORY: indo_russian_submarine_lease_2025
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_indo_russian_submarine_lease_2025, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indo_russian_submarine_lease_2025
 *   human_readable: Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the ~US$2 billion agreement for India to lease
 *   an Akula-class nuclear-powered attack submarine from Russia, scheduled for
 *   delivery around 2025. The deal provides the Indian Navy with a critical
 *   strategic asset for power projection and deterrence. However, it also
 *   imposes a significant financial cost, creates long-term technological
 *   and maintenance dependency on Russia, and carries geopolitical risks,
 *   such as potential friction with the United States under CAATSA.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indian Taxpayers: Primary target (powerless/trapped) — bear the direct financial cost of the lease.
 *   - Russian Defense-Industrial Complex: Primary beneficiary (institutional/arbitrage) — receives revenue and cements a long-term strategic partnership.
 *   - Indian Defense Establishment: Secondary beneficiary / Constrained actor (institutional/constrained) — gains a crucial military capability but at the cost of dependency and financial resources.
 *   - Strategic Analyst: Analytical observer — sees the full structure of capability gain, financial extraction, and strategic dependency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, 0.65).
domain_priors:suppression_score(indo_russian_submarine_lease_2025, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(indo_russian_submarine_lease_2025, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, extractiveness, 0.65).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indo_russian_submarine_lease_2025, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(indo_russian_submarine_lease_2025). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, russian_defense_industrial_complex).
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, indian_defense_establishment).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, indian_taxpayers).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, indian_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIAN TAXPAYER (PRIMARY TARGET)
% Bears the direct financial cost without direct reciprocal benefit. The high
% base extraction combined with a victim/trapped status results in a very high
% effective extraction (χ), classifying the deal as a Snare.
% χ ≈ 0.65 * f(0.95) * σ(national) ≈ 0.65 * 1.42 * 1.0 ≈ 0.92
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE RUSSIAN DEFENSE-INDUSTRIAL COMPLEX (PRIMARY BENEFICIARY)
% Sees a highly beneficial coordination agreement. As a beneficiary with high
% exit/arbitrage capability (selling arms is their business), effective
% extraction is negative.
% χ ≈ 0.65 * f(0.05) * σ(national) ≈ 0.65 * -0.12 * 1.0 ≈ -0.08
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Perceives the full structure: a genuine coordination function (deterrence)
% intertwined with high asymmetric extraction (cost and dependency). This dual
% nature is the hallmark of a Tangled Rope.
% χ ≈ 0.65 * f(0.72) * σ(global) ≈ 0.65 * 1.15 * 1.2 ≈ 0.90
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE INDIAN DEFENSE ESTABLISHMENT
% This actor is both a beneficiary (gains capability) and a victim (pays costs,
% incurs dependency). Their exit is 'constrained' because sourcing a nuclear
% submarine from another nation is extremely difficult, if not impossible. This
% mixed role and constrained exit yields a classification of Tangled Rope,
% reflecting the trade-off they have made.
% d is derived higher than a pure beneficiary but lower than a pure victim.
% Let's estimate d ~0.60, f(d) ~0.85
% χ ≈ 0.65 * f(0.60) * σ(national) ≈ 0.65 * 0.85 * 1.0 ≈ 0.55
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_russian_submarine_lease_2025_tests).

test(perspectival_gap, [nondet]) :-
    % Verify the core perspectival gaps between the main actors.
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(threshold_validation_tangled_rope) :-
    % Verify that the base metrics support a Tangled Rope classification.
    domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, E),
    domain_priors:suppression_score(indo_russian_submarine_lease_2025, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(indo_russian_submarine_lease_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. Represents the significant ~$2B financial outlay from India to Russia, plus the long-term, non-negotiable costs of maintenance, training, and refits that create a sustained resource drain and dependency.
 *   - Suppression (0.75): High. India has virtually no alternative suppliers for leasing a nuclear-powered attack submarine. The US does not lease them, and other potential partners like France would represent a completely different, and likely unavailable, ecosystem. This lack of a competitive market gives Russia immense structural leverage.
 *   - Theater (0.15): Low. The submarine is a highly functional, potent military asset, not a symbolic gesture. Its purpose is concrete deterrence and power projection.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the constraint. The Russian defense industry sees a pure coordination win (Rope): a sale that enhances a strategic partnership. The Indian taxpayer sees a pure extraction (Snare): a massive cost for an intangible benefit they don't directly experience. The Indian military establishment sits in the middle, perceiving a Tangled Rope: a necessary tool for national security that comes with severe costs and binds them to a single supplier. This inter-institutional tension is key.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Russian defense complex profits directly. The Indian military gains capability. These are declared as beneficiaries.
 *   - Victims: The Indian taxpayers bear the cost. Indian strategic autonomy is also compromised by the dependency, making it a structural victim. These declarations drive the directionality `d` towards 1.0 for the powerless/trapped perspective.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the two institutional actors—Russia and India—by their exit options. Russia has `arbitrage` (it can sell weapons systems to many customers), leading to a low `d` and a Rope classification. The Indian military has `constrained` exit (it cannot easily find another SSN to lease), which, despite its beneficiary status, results in a higher `d` and a Tangled Rope classification. This correctly models the asymmetry in the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates the power of the Tangled Rope classification. A naive analysis might label the deal a "Rope" (focusing on the capability gain for India) or a "Snare" (focusing on the cost). Both are incomplete. The Tangled Rope classification correctly identifies that the constraint has a genuine coordination function (regional deterrence) that is inextricably coupled with a high degree of asymmetric extraction (financial cost and strategic dependency). It avoids mischaracterizing a complex strategic trade-off as either pure cooperation or pure exploitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_indo_russian_submarine_lease_2025,
    'Does the strategic deterrence value genuinely outweigh the long-term financial and autonomy costs, or is it a prestige acquisition that creates a critical vulnerability?',
    'Declassification of strategic planning documents in 30-50 years; analysis of its operational impact in a future regional crisis vs. the counterfactual of investing the funds in domestic defense R&D.',
    'If deterrence value is high, it is a necessary Tangled Rope. If low, it is a costly Snare verging on a state-level Piton (maintaining an expensive asset for prestige).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% The interval represents the 10-year lease period.
narrative_ontology:interval(indo_russian_submarine_lease_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.65 > 0.46), so temporal
% measurements are required to track potential lifecycle drift.

% Theater ratio over time: Models a potential shift from functional use to
% symbolic/prestige use as the asset ages or strategic needs change.
narrative_ontology:measurement(irsl25_tr_t0, indo_russian_submarine_lease_2025, theater_ratio, 0, 0.10).
narrative_ontology:measurement(irsl25_tr_t5, indo_russian_submarine_lease_2025, theater_ratio, 5, 0.15).
narrative_ontology:measurement(irsl25_tr_t10, indo_russian_submarine_lease_2025, theater_ratio, 10, 0.20).

% Extraction over time: The base cost is fixed, but dependency can be
% leveraged for additional extraction via maintenance, upgrades, and parts.
% We model a slight increase in effective extraction, but keep base ε constant.
% The ε=0.65 value already accounts for the expected long-term costs.
narrative_ontology:measurement(irsl25_ex_t0, indo_russian_submarine_lease_2025, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(irsl25_ex_t5, indo_russian_submarine_lease_2025, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(irsl25_ex_t10, indo_russian_submarine_lease_2025, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The lease is fundamentally an allocation of a scarce,
% high-value strategic resource.
narrative_ontology:coordination_type(indo_russian_submarine_lease_2025, resource_allocation).

% Network relationships: This deal does not exist in a vacuum. It directly
% interacts with US sanctions policy (CAATSA), which is its own constraint.
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, caatsa_sanctions_us_2017).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural declarations
% (beneficiary/victim) combined with the distinct exit_options (arbitrage vs.
% constrained vs. trapped) are sufficient for the engine to derive the correct
% directionality `d` for each perspective and capture the inter-institutional
% dynamics accurately.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */