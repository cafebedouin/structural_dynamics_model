% ============================================================================
% CONSTRAINT STORY: bor_tax_exemption_nl
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_bor_tax_exemption_nl, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bor_tax_exemption_nl
 *   human_readable: Dutch Business Succession Scheme (BOR)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Dutch "Bedrijfsopvolgingsregeling" (BOR) is a tax regulation that
 *   provides a substantial exemption on inheritance and gift tax when a family
 *   business is passed to the next generation. Ostensibly designed to ensure
 *   business continuity and prevent forced sales to cover tax bills, it has
 *   become a mechanism for the Netherlands' wealthiest families to transfer
 *   vast fortunes almost tax-free. The scheme costs the treasury over €2.3
 *   billion annually, shifting the tax burden to the general public, and its
 *   benefits are highly concentrated among the super-rich.
 *
 * KEY AGENTS (by structural relationship):
 *   - wealthy_business_owning_families: Primary beneficiary (institutional/arbitrage) — Receives massive tax breaks, preserving generational wealth.
 *   - general_dutch_taxpayers: Primary target (powerless/trapped) — Bears the cost of the €2.3 billion tax shortfall through higher taxes elsewhere or reduced public services.
 *   - dutch_government: Institutional actor (institutional/constrained) — Creates and maintains the regulation, balancing lobbying from beneficiaries against public criticism.
 *   - economic_policy_analysts: Analytical observer — Sees both the stated coordination goal and the severe extractive reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bor_tax_exemption_nl, 0.75). % Represents the massive tax revenue foregone, concentrated on a tiny group.
domain_priors:suppression_score(bor_tax_exemption_nl, 0.80).   % Structural property (raw, unscaled). Political lobbying and path dependency make alternatives (e.g., a flat inheritance tax) difficult to implement.
domain_priors:theater_ratio(bor_tax_exemption_nl, 0.40).       % The 'business continuity' argument has some merit but is theatrically used to justify a scheme that primarily facilitates tax-free wealth transfer for the ultra-rich.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bor_tax_exemption_nl, extractiveness, 0.75).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(bor_tax_exemption_nl, tangled_rope).
narrative_ontology:human_readable(bor_tax_exemption_nl, "Dutch Business Succession Scheme (BOR)").
narrative_ontology:topic_domain(bor_tax_exemption_nl, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(bor_tax_exemption_nl). % The Tax Authority (Belastingdienst) must administer this complex regulation.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, wealthy_business_owning_families).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(bor_tax_exemption_nl, general_dutch_taxpayers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (GENERAL TAXPAYERS)
% Experiences the constraint as a pure cost—a mechanism that forces them to
% cover a tax shortfall created for the benefit of others.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (WEALTHY FAMILIES)
% Experiences the constraint as a vital coordination tool for generational
% wealth transfer, enabling business continuity with minimal friction.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the stated coordination function (business continuity) and the
% massive, asymmetric extraction, classifying it as a hybrid.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Dutch Government maintains the rule. It benefits from political stability
% and lobbying support, but is constrained by public opinion and the rule's complexity.
% Its position is structurally different from the pure beneficiaries. The constrained
% exit option reflects its limited room to maneuver politically.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bor_tax_exemption_nl_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap confirmed: Snare (powerless) vs Rope (institutional).~n', []).

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural predicates for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, _),
    narrative_ontology:constraint_victim(bor_tax_exemption_nl, _),
    domain_priors:requires_active_enforcement(bor_tax_exemption_nl),
    format('Tangled Rope structural requirements met.~n', []).

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(bor_tax_exemption_nl, ClaimedType),
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(bor_tax_exemption_nl_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): This value reflects the scale of the tax
 *     revenue lost (€2.3 billion/year), which is a direct transfer of fiscal
 *     capacity from the public to a very small, wealthy group.
 *   - Suppression (0.80): The political power wielded by the beneficiaries
 *     has historically suppressed simpler, more equitable alternatives. This
 *     high score reflects the political difficulty of meaningful reform.
 *   - Theater (0.40): The "business continuity" argument is a genuine coordination
 *     narrative, but its role is secondary to the wealth preservation function,
 *     making it a significant but not dominant component of theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the `general_dutch_taxpayers` (powerless, trapped),
 *   the regulation is a Snare: a coercive system they cannot opt out of that
 *   extracts resources for others' benefit. For the `wealthy_business_owning_families`
 *   (institutional, arbitrage), it is a Rope: a pure coordination mechanism that
 *   smooths intergenerational transfers, with no perceived extraction (χ is negative).
 *   This gap is the hallmark of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `wealthy_business_owning_families`. They directly benefit from
 *     the tax exemption, which can be worth billions for a single family. Their
 *     `arbitrage` exit options (international tax planning) give them immense
 *     leverage, driving their directionality `d` close to 0.
 *   - Victim: `general_dutch_taxpayers`. They bear the cost indirectly through
 *     the national budget. Their `trapped` status within the tax system drives
 *     their directionality `d` close to 1.0.
 *   The engine correctly maps these structural roles to opposing directionalities,
 *   which explains the Rope vs. Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This is a classic case where a pure extraction
 *   mechanism (Snare) is disguised as a coordination mechanism (Rope). Without
 *   the Tangled Rope classification, one would be forced to choose between
 *   labeling it a "necessary evil for business" (Rope) or a "purely predatory
 *   loophole" (Snare). Tangled Rope correctly identifies its dual nature: it
 *   possesses a genuine (if overstated) coordination function *and* a massive,
 *   asymmetric extractive function, maintained by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bor_tax_exemption_nl,
    'Is the business continuity function essential or purely theatrical?',
    'A counterfactual economic analysis of business failures/sales that would occur under a standard inheritance tax regime, compared to the current system.',
    'If essential, the coordination aspect is stronger, making it a "purer" Tangled Rope. If theatrical, the constraint is functionally a Snare disguised with a thin narrative veneer, and ε is even higher than estimated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bor_tax_exemption_nl, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The BOR has existed for years, and its
% use for wealth preservation beyond simple business continuity has arguably grown.
% Required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time: The initial justification has become weaker as the scale of exploitation for pure wealth transfer has become public knowledge.
narrative_ontology:measurement(bor_tax_exemption_nl_tr_t0, bor_tax_exemption_nl, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bor_tax_exemption_nl_tr_t5, bor_tax_exemption_nl, theater_ratio, 5, 0.35).
narrative_ontology:measurement(bor_tax_exemption_nl_tr_t10, bor_tax_exemption_nl, theater_ratio, 10, 0.40).

% Extraction over time: As tax lawyers have optimized its use, the effective extraction from the public purse has increased.
narrative_ontology:measurement(bor_tax_exemption_nl_ex_t0, bor_tax_exemption_nl, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(bor_tax_exemption_nl_ex_t5, bor_tax_exemption_nl, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(bor_tax_exemption_nl_ex_t10, bor_tax_exemption_nl, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This regulation allocates the national tax burden.
narrative_ontology:coordination_type(bor_tax_exemption_nl, resource_allocation).

% Network relationships (structural influence edges)
% This tax policy is a major driver of wealth inequality.
narrative_ontology:affects_constraint(bor_tax_exemption_nl, wealth_inequality_nl).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (beneficiary/victim + exit_options) accurately captures the directionality
% for all key agents, including the distinct position of the constrained
% institutional government actor.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */