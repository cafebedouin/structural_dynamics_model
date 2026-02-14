% ============================================================================
% CONSTRAINT STORY: dutch_minority_govt_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_dutch_minority_govt_2026, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dutch_minority_govt_2026
 *   human_readable: Dutch Minority Government External Support Agreement (2026)
 *   domain: political
 *
 * SUMMARY:
 *   Following a fragmented 2025 election result in the Netherlands, the
 *   far-right PVV, despite being the largest party, cannot form a majority
 *   coalition. To avoid political deadlock and new elections, a novel
 *   arrangement is formed: a PVV-led minority government supported externally
 *   by three center-right parties (VVD, NSC, BBB). This agreement allows
 *   the government to pass key legislation (e.g., budget, immigration) while
 *   the supporting parties avoid the reputational cost of a formal coalition.
 *
 * KEY AGENTS (by structural relationship):
 *   - PVV (far-right party): Primary beneficiary (institutional/arbitrage) — forms a government it otherwise could not.
 *   - External Support Parties (VVD, NSC, BBB): Secondary beneficiaries (institutional/constrained) — achieve policy goals and stability without direct accountability.
 *   - Immigrant & Asylum Seeker Communities: Primary target (powerless/trapped) — bear the costs of restrictive policies enabled by the deal.
 *   - Parliamentary Opposition (Left/Green parties): Secondary target (organized/constrained) — systematically excluded from power and influence.
 *   - Analytical Observer: Sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dutch_minority_govt_2026, 0.48).
domain_priors:suppression_score(dutch_minority_govt_2026, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dutch_minority_govt_2026, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dutch_minority_govt_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dutch_minority_govt_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(dutch_minority_govt_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, pvv_party).
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, external_support_parties).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(dutch_minority_govt_2026, immigrant_communities).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, parliamentary_opposition).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (IMMIGRANT COMMUNITIES)
% Experiences only the extractive outcomes (restrictive policies) with no
% recourse or benefit from the "stability" coordination.
% Engine derives d from victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (PVV PARTY)
% The agreement is a pure coordination win, enabling them to govern. The
% extraction is seen as the legitimate function of government.
% Engine derives d from beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> low/negative χ
constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the genuine coordination function (avoiding deadlock) and
% the high, asymmetric extraction imposed on specific groups.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The supporting parties and the opposition have different institutional views.

% PERSPECTIVE 4A: EXTERNAL SUPPORT PARTIES (VVD, NSC, BBB)
% Also beneficiaries, but their position is more precarious. Their exit is
% constrained by the undesirable alternatives (new elections, left-wing govt).
% They see it as a necessary Rope, a coordination tool for stability.
% Engine derives d from beneficiary + constrained exit -> d ≈ 0.25 -> f(d) ≈ 0.15
constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: PARLIAMENTARY OPPOSITION
% Experiences the agreement as a mechanism of exclusion that suppresses their
% political mandate. They are victims of the coordination.
% Engine derives d from victim + constrained exit -> d ≈ 0.75 -> f(d) ≈ 1.18
constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dutch_minority_govt_2026_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated (Snare vs Rope)\n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_perspectives_differ) :-
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare, context(agent_power(organized), _, _, _)),
    format('... Inter-institutional gap validated (Rope vs Snare)\n').

:- end_tests(dutch_minority_govt_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.48): High. The agreement directly enables a political agenda focused on extraction from specific demographics (immigrants) and exclusion of political opponents, which would otherwise be blocked.
 *   - Suppression (0.65): High. The constraint's primary function is to suppress alternatives: a formal coalition, a different coalition, or new elections. It creates a stable but rigid political outcome.
 *   - The combination of a genuine coordination function with high, asymmetric extraction makes this a canonical Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiaries (PVV, support parties), the agreement is a Rope that solves the coordination problem of forming a government. For the primary targets (immigrants), it is a Snare that enables policies directly harmful to them; the "coordination benefit" is invisible and irrelevant. This difference is driven by directionality (d): beneficiaries have low d, victims have high d, leading to vastly different calculations of effective extraction (χ).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `pvv_party` and `external_support_parties` both benefit by achieving political goals (governing power, policy influence) and stability. Their membership in the beneficiary group correctly lowers their derived directionality `d`.
 *   - Victims: The `immigrant_communities` are the direct targets of the extracted value (in the form of rights, security, and social standing). The `parliamentary_opposition` is a victim of political exclusion. Their membership in the victim group correctly raises their `d`.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional dynamic. Both the PVV and the support parties are `institutional` actors and beneficiaries. However, their exit options differ. The PVV, as the government leader, has `arbitrage` (more negotiating power). The support parties are `constrained` because their alternatives are worse (e.g., losing seats in a new election). The Deferential Realism engine captures this nuance, deriving a slightly less favorable directionality for the constrained supporters than for the party with arbitrage, even though both classify the constraint as a Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure Snare, because the agreement genuinely solves a difficult coordination problem (avoiding governmental collapse), a function acknowledged by the beneficiary perspectives. It is not a pure Rope, because the solution imposes severe, asymmetric costs on identifiable groups, a fact captured by the high base extractiveness and the victim perspectives. The Tangled Rope classification reflects this dual nature, preventing the coordination function from masking the extractive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dutch_govt_2026,
    'Will the "external support" model prove stable, or is it a temporary phase before a full, formal coalition forms between the PVV and its supporters?',
    'Observation of voting patterns and cabinet stability over the next 2-3 fiscal years.',
    'If stable, it remains a Tangled Rope. If it collapses into a formal coalition, the constraint transforms into a more conventional (and likely more extractive) political coalition structure, requiring a new analysis.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dutch_minority_govt_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required as base_extractiveness (0.48) > 0.46.
% This models the constraint intensifying as the novel arrangement solidifies
% and begins to implement its agenda.

% Theater ratio over time (starts high to justify the deal, then drops as it becomes functional):
narrative_ontology:measurement(dutch_minority_govt_2026_tr_t0, dutch_minority_govt_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dutch_minority_govt_2026_tr_t5, dutch_minority_govt_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dutch_minority_govt_2026_tr_t10, dutch_minority_govt_2026, theater_ratio, 10, 0.30).

% Extraction over time (starts lower at the point of agreement, then rises as policies are enacted):
narrative_ontology:measurement(dutch_minority_govt_2026_ex_t0, dutch_minority_govt_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(dutch_minority_govt_2026_ex_t5, dutch_minority_govt_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dutch_minority_govt_2026_ex_t10, dutch_minority_govt_2026, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a mechanism for allocating political power and legislative priority.
narrative_ontology:coordination_type(dutch_minority_govt_2026, resource_allocation).

% Network relationships (structural influence edges)
% This political agreement would directly affect policy constraints.
narrative_ontology:affects_constraint(dutch_minority_govt_2026, dutch_asylum_policy_2027).
narrative_ontology:affects_constraint(dutch_minority_govt_2026, dutch_fiscal_budget_2027).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain, using
% the declared beneficiary/victim groups and the specified exit options
% (arbitrage vs. constrained), accurately captures the structural relationships
% and produces the correct directionality (d) values for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */