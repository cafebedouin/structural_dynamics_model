% ============================================================================
% CONSTRAINT STORY: cumbria_mine_rejection
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_cumbria_mine_rejection, []).

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
 *   constraint_id: cumbria_mine_rejection
 *   human_readable: UK government rejection of the Woodhouse Colliery coal mine
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint represents the UK's national planning and environmental
 *   regulatory framework, which was ultimately used by the central government
 *   to reject planning permission for a new coking coal mine in Cumbria. The
 *   decision prioritized national and international climate change commitments
 *   over local economic development and job creation, creating a sharp
 *   perspectival conflict.
 *
 * KEY AGENTS (by structural relationship):
 *   - West Cumbria Mining & pro-mine local community: Primary target (powerless/trapped) — bears the full economic extraction of the project's cancellation.
 *   - Environmental campaign groups & UK Climate Change Committee: Primary beneficiary (organized/arbitrage) — benefits from the enforcement of national climate policy.
 *   - UK Central Government (Levelling Up Secretary): Institutional beneficiary (institutional/arbitrage) — enforces the constraint, benefits from demonstrating policy alignment.
 *   - Cumbria County Council: Secondary actor (institutional/constrained) — an institutional actor that initially supported the project but was overruled, demonstrating a constrained exit.
 *   - Analytical observer: Sees the full structure of coordination (climate goals) and extraction (local economic loss).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cumbria_mine_rejection, 0.65).
domain_priors:suppression_score(cumbria_mine_rejection, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cumbria_mine_rejection, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cumbria_mine_rejection, extractiveness, 0.65).
narrative_ontology:constraint_metric(cumbria_mine_rejection, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(cumbria_mine_rejection, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cumbria_mine_rejection, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(cumbria_mine_rejection). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Not a naturally emerging constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, environmental_campaign_groups).
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, uk_climate_commitments_adherence).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cumbria_mine_rejection, west_cumbria_mining).
narrative_ontology:constraint_victim(cumbria_mine_rejection, pro_mine_local_community).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% West Cumbria Mining and pro-mine locals. They lose the entire potential
% economic value of the project. The coordination benefit is abstract and
% non-local, making the constraint appear as pure extraction.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(cumbria_mine_rejection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Environmental campaign groups and the government enforcing its climate policy.
% For them, the constraint is a pure coordination mechanism to align action
% with national goals, with the costs externalized onto the target.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(cumbria_mine_rejection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% This perspective sees both the genuine coordination function (meeting
% climate targets) and the severe, asymmetrically applied extraction (local
% economic loss). The high ε and suppression confirm the hybrid nature.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The local authority, Cumbria County Council, which initially approved the
% project but was ultimately overruled by the central government. They are an
% institutional actor, but their inability to enact their decision shows a
% constrained exit. They see both the coordination goal and the local pain.
% Engine derives d from: victim (local impact) + constrained exit → higher d than beneficiary.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cumbria_mine_rejection_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the project proponents and beneficiaries.
    constraint_indexing:constraint_classification(cumbria_mine_rejection, snare, context(agent_power(powerless), _, exit_options(trapped), spatial_scope(local))),
    constraint_indexing:constraint_classification(cumbria_mine_rejection, rope, context(agent_power(institutional), _, exit_options(arbitrage), spatial_scope(national))),
    writeln('Perspectival gap (Snare vs Rope) confirmed.').

test(tangled_rope_conditions_met) :-
    % Verify that the necessary structural facts for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, _),
    narrative_ontology:constraint_victim(cumbria_mine_rejection, _),
    domain_priors:requires_active_enforcement(cumbria_mine_rejection),
    writeln('Tangled Rope structural conditions (beneficiary, victim, enforcement) confirmed.').

test(analytical_claim_matches) :-
    narrative_ontology:constraint_claim(cumbria_mine_rejection, tangled_rope),
    constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope, context(agent_power(analytical), _, _, _)),
    writeln('Analytical perspective matches constraint_claim as Tangled Rope.').

:- end_tests(cumbria_mine_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High, representing the total loss of a major industrial project's economic value (jobs, investment) for a specific community.
 *   - Suppression (0.80): High, as the central government's authority provided a definitive and final block on the alternative (building the mine).
 *   - The combination of a legitimate coordination function (climate policy) and high, asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For West Cumbria Mining, this is a Snare: a system that trapped their investment and extracted its full value. For environmental groups and the central government, it's a Rope: a successful coordination mechanism that aligned national action with stated policy, with costs that are external to their own operations. The disagreement stems from the spatial and temporal scope of costs and benefits: costs are local and immediate, while benefits are global and generational.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The conflict between Cumbria County Council (institutional/constrained) and the UK Central Government (institutional/arbitrage) is a key feature. The local council, closer to the economic pain, approved the mine. The central government, accountable to national/international targets, overruled them. This demonstrates how two institutional actors can experience the same regulatory constraint with vastly different degrees of freedom and, therefore, different perspectives on its legitimacy and function. The `constrained` exit option for the council is critical to capturing this dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two errors. It is not a pure Snare, because the goal of climate change mitigation is a genuine, large-scale coordination problem. Mislabeling it as a Snare would ignore this function. Conversely, it is not a pure Rope, because the costs are not distributed symmetrically; they are borne almost entirely by one small community. The Tangled Rope classification captures this essential tension between function and cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cumbria_mine_rejection,
    'Is the net global carbon reduction from this specific decision significant, or is it a symbolic act whose main function is political theater, with emissions simply displaced elsewhere (e.g., via importing coking coal)?',
    'A full lifecycle analysis of global coking coal supply chains and steel manufacturing emissions, comparing the "mine vs. no-mine" scenarios over a 20-year period.',
    'If significant, it reinforces the Tangled Rope classification (real coordination). If symbolic/displaced, it would increase the theater_ratio and push the classification toward Piton from an analytical perspective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cumbria_mine_rejection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint (environmental regulation stringency) has intensified over the
% last decade. Initial stages had lower perceived extraction and less theater,
% while the final decision phase involved maximum extraction and high theater.
% Required because base_extractiveness (0.65) > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(cumbria_tr_t0, cumbria_mine_rejection, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cumbria_tr_t5, cumbria_mine_rejection, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cumbria_tr_t10, cumbria_mine_rejection, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(cumbria_ex_t0, cumbria_mine_rejection, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cumbria_ex_t5, cumbria_mine_rejection, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cumbria_ex_t10, cumbria_mine_rejection, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is a mechanism for enforcing national policy.
narrative_ontology:coordination_type(cumbria_mine_rejection, enforcement_mechanism).

% Network relationships (structural influence edges)
% This decision is structurally linked to policies on steel production (which
% needs coking coal) and broader energy security.
narrative_ontology:affects_constraint(cumbria_mine_rejection, uk_steel_production_viability).
narrative_ontology:affects_constraint(cumbria_mine_rejection, uk_energy_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using the declared beneficiary/victim groups and the specified exit_options
% for each perspective (trapped, arbitrage, constrained), is sufficient to
% generate accurate directionality (d) values that reflect the narrative.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */