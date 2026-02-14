% ============================================================================
% CONSTRAINT STORY: algeria_france_colonial_legacy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% ============================================================================

:- module(constraint_algeria_france_colonial_legacy, []).

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
 *   constraint_id: algeria_france_colonial_legacy
 *   human_readable: The persistent structural legacy of French colonization in Algeria
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint represents the enduring economic, political, and cultural
 *   effects of France's 132-year colonization of Algeria (1830-1962). The
 *   Algerian parliament's recent vote to declare colonization a crime highlights
 *   the ongoing nature of this constraint, which manifests as demands for
 *   restitution, return of archives, and reparations for nuclear testing. The
 *   constraint is defined by a historical power asymmetry whose consequences
 *   persist in present-day international relations and economic structures.
 *
 * KEY AGENTS (by structural relationship):
 *   - Algerian State & People: Primary target (organized/constrained) — bears the historical and ongoing costs of extraction and suppressed development.
 *   - French State & Beneficiaries: Primary beneficiary (institutional/arbitrage) — benefits from historical resource extraction and maintains structural advantages.
 *   - Analytical Observer: A historian or international relations scholar who sees the full hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algeria_france_colonial_legacy, 0.75).
domain_priors:suppression_score(algeria_france_colonial_legacy, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(algeria_france_colonial_legacy, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, extractiveness, 0.75).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(algeria_france_colonial_legacy, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(algeria_france_colonial_legacy). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, french_colonial_state_and_beneficiaries).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_state_and_people).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDIVIDUAL)
% An individual Algerian citizen whose family history and economic opportunities
% are shaped by the colonial legacy.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (FRENCH STATE)
% The French state, which inherited the geopolitical and economic advantages
% of the colonial era.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Recognizes both the coordination function (imposing a state structure) and
% the massive asymmetric extraction, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Algerian state as an organized political actor. While organized, its options
% for escaping the historical legacy are highly constrained by established
% international power dynamics and economic path dependencies.
% Engine derives d from victim membership + constrained exit, yielding a high d.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, snare,
    context(agent_power(organized),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algeria_france_colonial_legacy_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    (TypeTarget == snare ; TypeTarget == tangled_rope),
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_inter_institutional) :-
    % Verify perspectival gap between the two nation-state actors.
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeAlgeria, context(agent_power(organized), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeFrance, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    (TypeAlgeria == snare ; TypeAlgeria == tangled_rope),
    TypeFrance == rope,
    TypeAlgeria \= TypeFrance.

test(analytical_claim_matches_type) :-
    % The analytical observer's classification must match the declared constraint_claim
    narrative_ontology:constraint_claim(algeria_france_colonial_legacy, ClaimedType),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

test(tangled_rope_gates_pass) :-
    narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, _),
    narrative_ontology:constraint_victim(algeria_france_colonial_legacy, _),
    domain_priors:requires_active_enforcement(algeria_france_colonial_legacy).

:- end_tests(algeria_france_colonial_legacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Set very high to reflect over a century of systematic resource transfer, land appropriation, and wealth extraction from Algeria to France.
 *   - Suppression Score (0.80): Colonialism is defined by the violent suppression of indigenous political, economic, and cultural alternatives. The Algerian War of Independence is testament to the extreme measures required to overcome this suppression.
 *   - Theater Ratio (0.20): While colonial powers employed rhetoric like the "civilizing mission," the primary function was brutally direct extraction, not performance. The theater was a justification, not the core mechanism.
 *   - This combination of high extraction, high suppression, and the presence of both beneficiaries and victims makes it a canonical Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The French state (beneficiary) can selectively focus on the infrastructure built or the state structure imposed, viewing it as a coordination mechanism (Rope). For Algerians (victims), the experience was one of overwhelming coercion, violence, and extraction with no meaningful exit, making it a clear Snare. This gap is the source of ongoing diplomatic friction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `french_colonial_state_and_beneficiaries`. The French state, its corporations, and settlers were the direct financial and political beneficiaries. Their institutional power and `arbitrage` exit options (they can choose how to engage with history) give them a very low directionality score (d), resulting in a low effective extraction (χ) and a Rope classification.
 *   - Victim: `algerian_state_and_people`. The native population bore the costs. Their status as victims with `trapped` or `constrained` exit options gives them a very high directionality score (d), maximizing effective extraction (χ) and leading to a Snare classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a core feature of the story. Both France and Algeria are institutional actors (nation-states), but their relationship to the historical constraint is asymmetric.
 *   - France (`institutional`/`arbitrage`): Has the power to set the terms of debate, delay reparations, and leverage its economic/diplomatic weight. Its exit is "arbitrage" because it can profit from the legacy while controlling the narrative.
 *   - Algeria (`organized`/`constrained`): Is an organized state actor, but it cannot simply "exit" its own history. Its development path, national identity, and international relations are fundamentally constrained by the colonial past. This difference in exit options is what drives the different classifications despite both being state actors.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] The Tangled Rope classification is crucial here. A simple Snare classification would miss the (perverse) coordination function that the colonial state imposed. A Rope classification would be a whitewashing of history, ignoring the immense extraction. Tangled Rope correctly identifies that a system of organization was created *for the purpose* of asymmetric extraction. It prevents the mislabeling of coercive extraction as benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_algeria_france_colonial_legacy,
    'What is the true, quantifiable net economic impact of colonization on Algeria, against a counterfactual of independent development?',
    'A comprehensive, multi-generational economic analysis modeling resource flows, suppressed industrialization, and human capital costs. This is likely impossible to calculate with high precision.',
    'A definitive high value would strengthen claims for restitution (Snare). A lower or ambiguous value would allow the beneficiary to frame the legacy as more coordinative (Rope).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents the colonial period.
narrative_ontology:interval(algeria_france_colonial_legacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the colonial period (1830s to 1960s, normalized to 0-10).
% Required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time: increased as "civilizing mission" rhetoric developed.
narrative_ontology:measurement(afcl_tr_t0, algeria_france_colonial_legacy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(afcl_tr_t5, algeria_france_colonial_legacy, theater_ratio, 5, 0.15).
narrative_ontology:measurement(afcl_tr_t10, algeria_france_colonial_legacy, theater_ratio, 10, 0.20).

% Extraction over time: intensified as colonial administration became more efficient.
narrative_ontology:measurement(afcl_ex_t0, algeria_france_colonial_legacy, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(afcl_ex_t5, algeria_france_colonial_legacy, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(afcl_ex_t10, algeria_france_colonial_legacy, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The colonial state was a mechanism for re-routing resources.
narrative_ontology:coordination_type(algeria_france_colonial_legacy, resource_allocation).

% Network relationships: The colonial legacy has direct structural impacts on
% modern geopolitical and economic constraints.
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, european_north_african_migration_policy).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, algerian_energy_exports_to_europe).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain
% correctly computes directionality (d) from the declared beneficiary/victim
% groups and their respective exit options (arbitrage vs. constrained),
% accurately capturing the asymmetric relationship between the French and
% Algerian states.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */