% ============================================================================
% CONSTRAINT STORY: unrwa_eviction_order
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-05
% ============================================================================

:- module(constraint_unrwa_eviction_order, []).

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
 *   constraint_id: unrwa_eviction_order
 *   human_readable: Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Israeli Land Authority (ILA) has issued an order for the United
 *   Nations Relief and Works Agency (UNRWA) to vacate its headquarters in
 *   East Jerusalem within 30 days, citing alleged contract violations and
 *   illegal construction. This administrative and legal act constrains
 *   UNRWA's ability to operate, serves a political goal of undermining the
 *   agency, and transfers control of valuable land.
 *
 * KEY AGENTS (by structural relationship):
 *   - Palestinian Refugees: Primary target (powerless/trapped) — bear extraction through disruption of essential services.
 *   - UNRWA: Primary institutional target (institutional/constrained) — loses a key operational hub and faces institutional delegitimization.
 *   - Israeli Land Authority / Israeli Government: Primary beneficiary (institutional/arbitrage) — reclaims sovereign control over land, achieves political goals.
 *   - Analytical Observer: Sees the full structure of coercive extraction under the guise of legal enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrwa_eviction_order, 0.75). % High extraction: loss of a major physical asset, operational disruption, delegitimization.
domain_priors:suppression_score(unrwa_eviction_order, 0.85).   % Structural property (raw, unscaled). Alternatives are heavily suppressed by state power.
domain_priors:theater_ratio(unrwa_eviction_order, 0.40).       % Piton detection (>= 0.70). The action is functional (eviction) but has a strong performative/political component.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrwa_eviction_order, extractiveness, 0.75).
narrative_ontology:constraint_metric(unrwa_eviction_order, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(unrwa_eviction_order, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(unrwa_eviction_order, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(unrwa_eviction_order). % Eviction and demolition require state force.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_land_authority).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(unrwa_eviction_order, unrwa).
narrative_ontology:constraint_victim(unrwa_eviction_order, palestinian_refugees).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (PALESTINIAN REFUGEES)
% Agent who bears the most extraction through service disruption. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ISRAELI LAND AUTHORITY)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% From this view, it's a Rope for enforcing property rights and sovereignty.
constraint_indexing:constraint_classification(unrwa_eviction_order, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees the high extraction and suppression.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. The classification matches the constraint_claim.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% This captures the dynamic between two institutional actors where one
% is subject to the coercive power of the other.

% PERSPECTIVE 4: THE INSTITUTIONAL VICTIM (UNRWA)
% UNRWA is an institution, but its exit options are constrained by the
% sovereign power of the state it operates in. The engine derives a high d
% from victim status + constrained exit, leading to a Snare classification.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrwa_eviction_order_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the powerless victim and institutional beneficiary.
    constraint_indexing:constraint_classification(unrwa_eviction_order, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(unrwa_eviction_order, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_perspectival_gap) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypeVictim, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeBeneficiary = rope,
    TypeVictim = snare.

test(analytical_claim_matches_snare) :-
    narrative_ontology:constraint_claim(unrwa_eviction_order, snare),
    constraint_indexing:constraint_classification(unrwa_eviction_order, snare, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_scores) :-
    domain_priors:base_extractiveness(unrwa_eviction_order, E), E >= 0.46,
    domain_priors:suppression_score(unrwa_eviction_order, S), S >= 0.60.

:- end_tests(unrwa_eviction_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The constraint's core function is the transfer of a valuable asset (land and a major operational hub) from one party (UNRWA) to another (ILA), while imposing severe operational costs and disruption on the victim and the population it serves. This is a highly extractive act.
 *   - Suppression (0.85): UNRWA's ability to resist or find an equivalent alternative in East Jerusalem is virtually nil. The order is backed by the full legal and physical force of the state, making non-compliance impossible without severe consequences.
 *   - Theater (0.40): While the eviction is a functional, material act, it is also framed politically as a strike against an organization Israel views as hostile. The action serves a signaling purpose to domestic and international audiences, but its primary nature is functional extraction, not performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Israeli Land Authority (beneficiary), this is a 'Rope'—a legitimate legal tool to enforce a contract and assert national sovereignty over land. The negative effective extraction (χ) from their perspective reflects that they see this as a net gain and a restoration of order. For UNRWA and the refugees it serves (victims), it is a 'Snare'—a coercive, high-extraction act that strips them of assets and critical functions, with no viable alternative. Their high χ reflects the immense, uncompensated costs imposed upon them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `israeli_land_authority` is the explicit beneficiary. With `arbitrage` exit options (they control the legal framework), the engine derives a very low directionality `d`, correctly classifying the constraint as a Rope from their viewpoint.
 *   - Victims: `unrwa` and `palestinian_refugees` are declared victims. For the refugees, `trapped` exit options generate the highest `d`, reflecting their complete inability to escape the consequences. For UNRWA, `constrained` exit options generate a slightly lower (but still high) `d`, reflecting its status as a powerful organization that is nevertheless subject to the authority of a sovereign state in this context.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional conflict. Both the ILA and UNRWA are institutional actors, but they have fundamentally different structural relationships to the constraint. The ILA is the author and enforcer, possessing `arbitrage` exit. UNRWA is the target, with only `constrained` exit. This asymmetry is the core of the dynamic and is captured by giving each a distinct perspective, resulting in a Rope vs. Snare classification even at the same `agent_power(institutional)` level. The difference in `exit_options` is the key that unlocks the correct directionality calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   By decomposing the perspectives, the framework avoids mislabeling the action. A simplistic analysis might call it "law enforcement" (a Rope) or "political persecution" (a Snare). Deferential Realism shows it is structurally *both*, depending on the index. The analytical perspective, which synthesizes the structure, correctly identifies it as a Snare because the high base extraction (ε) and suppression scores are objective properties, and the existence of a heavily extracted victim group is undeniable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_unrwa_eviction_order,
    'Is the legal claim of contract violation a legitimate, proportionate legal procedure or a pretext for a politically motivated eviction?',
    'Independent review of the original lease agreements, construction permits, and a history of ILA enforcement actions in similar cases against non-UN entities.',
    'If legitimate (pretext is false), the constraint might be a Tangled Rope, having a coordination function (contract enforcement) alongside high extraction. If pretext is true, the classification as a pure Snare is confirmed.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(unrwa_eviction_order, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data required because base_extractiveness (0.75) > 0.46.
% This models the degradation of UNRWA's position over a 10-year period,
% culminating in the final eviction order.

% Theater ratio over time:
narrative_ontology:measurement(unrwa_eviction_order_tr_t0, unrwa_eviction_order, theater_ratio, 0, 0.20).
narrative_ontology:measurement(unrwa_eviction_order_tr_t5, unrwa_eviction_order, theater_ratio, 5, 0.30).
narrative_ontology:measurement(unrwa_eviction_order_tr_t10, unrwa_eviction_order, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(unrwa_eviction_order_ex_t0, unrwa_eviction_order, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(unrwa_eviction_order_ex_t5, unrwa_eviction_order, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(unrwa_eviction_order_ex_t10, unrwa_eviction_order, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is primarily extractive and does not serve a broad coordination
% function, so coordination_type is omitted.

% Network relationships: This eviction order structurally impacts the ability
% to deliver humanitarian aid and is related to the broader political conflict.
narrative_ontology:affects_constraint(unrwa_eviction_order, humanitarian_aid_access_westbank).
narrative_ontology:affects_constraint(unrwa_eviction_order, israeli_palestinian_diplomatic_relations).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The automatic derivation from beneficiary/victim
% declarations combined with the distinct exit options (arbitrage, constrained,
% trapped) accurately captures the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */