% ============================================================================
% CONSTRAINT STORY: iran_guardian_council_vetting
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_iran_guardian_council_vetting, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iran_guardian_council_vetting
 *   human_readable: Iranian Guardian Council's Candidate Vetting System
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the power of Iran's Guardian Council to vet and
 *   disqualify political candidates for national office. While ostensibly a
 *   mechanism to ensure ideological conformity with the Islamic Republic, it
 *   has evolved into a powerful tool for political extraction, used by the
 *   hardline faction to systematically eliminate moderate, reformist, and
 *   even establishment-loyal but ideologically divergent politicians, thereby
 *   consolidating power and suppressing internal dissent.
 *
 * KEY AGENTS (by structural relationship):
 *   - Iranian Moderate/Reformist Politicians: Primary target (powerless/trapped) — bears the direct extraction of political opportunity and voice.
 *   - Iranian Hardline Faction: Primary beneficiary (institutional/arbitrage) — benefits from the consolidation of power and elimination of rivals.
 *   - Iranian Electorate: Secondary victim (powerless/trapped) — faces a severely restricted choice of candidates.
 *   - Analytical Observer: External analyst (analytical/analytical) — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_guardian_council_vetting, 0.75).
domain_priors:suppression_score(iran_guardian_council_vetting, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(iran_guardian_council_vetting, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_guardian_council_vetting, extractiveness, 0.75).
narrative_ontology:constraint_metric(iran_guardian_council_vetting, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(iran_guardian_council_vetting, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iran_guardian_council_vetting, tangled_rope).
narrative_ontology:human_readable(iran_guardian_council_vetting, "Iranian Guardian Council's Candidate Vetting System").

% --- Binary flags ---
domain_priors:requires_active_enforcement(iran_guardian_council_vetting). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iran_guardian_council_vetting, iranian_hardline_faction).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iran_guardian_council_vetting, iranian_moderate_politicians).

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
% For the moderate and reformist politicians being purged, the system offers
% no coordination benefit, only the extraction of their political careers.
% Engine derives d from: victim membership + trapped exit → d≈0.95 → f(d)≈1.42 → high χ
constraint_indexing:constraint_classification(iran_guardian_council_vetting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the hardline faction, this is a pure coordination tool to maintain
% ideological purity and regime stability, concentrating their power.
% Engine derives d from: beneficiary membership + arbitrage exit → d≈0.05 → f(d)≈-0.12 → negative χ
constraint_indexing:constraint_classification(iran_guardian_council_vetting, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An external observer sees both the stated coordination function (maintaining
% the Islamic Republic's principles) and the severe, asymmetric extraction of
% political power. This dual nature is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_guardian_council_vetting_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(iran_guardian_council_vetting, TypeTarget, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(iran_guardian_council_vetting, TypeBeneficiary, context(agent_power(institutional), _, arbitrage, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_detection) :-
    % The analytical view must resolve to Tangled Rope.
    constraint_indexing:constraint_classification(iran_guardian_council_vetting, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_gate_requirements) :-
    % Verify the structural predicates required for a Tangled Rope classification are present.
    narrative_ontology:constraint_beneficiary(iran_guardian_council_vetting, _),
    narrative_ontology:constraint_victim(iran_guardian_council_vetting, _),
    domain_priors:requires_active_enforcement(iran_guardian_council_vetting).

:- end_tests(iran_guardian_council_vetting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The constraint's primary function is now the extraction of political power from rival factions. It determines who can and cannot hold power.
 *   - Suppression Score (0.90): Also extremely high. The Guardian Council's decisions are effectively absolute and unappealable, completely suppressing alternative political representation.
 *   - Theater Ratio (0.40): The process maintains the form of constitutional oversight, but its function as a tool for factional consolidation is increasingly transparent. The theater is still present but wearing thin.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary (hardliners), the system is a Rope—a necessary tool for coordinating ideological purity and ensuring the regime's long-term stability. For the target (moderates), the system is a Snare—a trap that has ended their political careers with no recourse. The coordinating logic is invisible or irrelevant to those being extracted from; they only experience the coercive force.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `iranian_hardline_faction` directly benefits by having their political rivals eliminated and their own power consolidated. Their `arbitrage` exit status reflects their control over the rules of the system itself. This combination derives a very low `d` value, resulting in a negative effective extraction (χ) from their perspective.
 *   - Victim: The `iranian_moderate_politicians` bear the full cost. They are `trapped` within a system where the rules are turned against them, with no viable exit. This derives a very high `d` value, maximizing the perceived extraction (χ) and classifying the constraint as a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This is a classic case where a system could be mislabeled. A naive analysis might see it as pure coordination ("maintaining the state's ideology") or pure oppression. The Tangled Rope classification correctly identifies that it is *both*. It possesses a genuine, albeit perverse, coordination function for the ruling faction, but this function is achieved through highly extractive and suppressive means against other internal groups. This prevents mischaracterizing it as a simple Rope (ignoring the victims) or a simple Snare (ignoring the internal coordination logic that sustains it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_iran_vetting,
    'Will the systematic exclusion of all moderate factions lead to long-term regime stability through ideological purity, or will it create a brittle, hollowed-out state vulnerable to collapse from internal or external shocks?',
    'Observation of regime response to the next major economic or political crisis (e.g., post-succession) over a 5-10 year horizon.',
    'If it leads to stability, the hardliners perceived it correctly as a Rope. If it leads to brittleness and collapse, it was a Snare for the entire system, not just the moderates.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iran_guardian_council_vetting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This process has intensified over time, moving from vetting true outsiders
% to purging internal factions. This reflects an increase in both extraction
% and the theatricality of the process. This data is required as ε > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(igcv_tr_t0, iran_guardian_council_vetting, theater_ratio, 0, 0.20).
narrative_ontology:measurement(igcv_tr_t5, iran_guardian_council_vetting, theater_ratio, 5, 0.30).
narrative_ontology:measurement(igcv_tr_t10, iran_guardian_council_vetting, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(igcv_ex_t0, iran_guardian_council_vetting, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(igcv_ex_t5, iran_guardian_council_vetting, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(igcv_ex_t10, iran_guardian_council_vetting, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system is fundamentally an enforcement mechanism for ideological conformity.
narrative_ontology:coordination_type(iran_guardian_council_vetting, enforcement_mechanism).

% Network relationships (structural influence edges)
% The nature of the political vetting system directly influences foreign policy
% by ensuring only hardliners are in a position to make decisions.
narrative_ontology:affects_constraint(iran_guardian_council_vetting, iran_foreign_policy_rigidity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics between the hardline and moderate factions.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */