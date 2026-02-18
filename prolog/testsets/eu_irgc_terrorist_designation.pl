% ============================================================================
% CONSTRAINT STORY: eu_irgc_terrorist_designation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_irgc_terrorist_designation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_irgc_terrorist_designation
 *   human_readable: EU Terrorist Designation of Iran's IRGC
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the European Union's potential designation of
 *   Iran's Islamic Revolutionary Guard Corps (IRGC) as a terrorist
 *   organization. Such a designation functions as a legal and economic
 *   framework that coordinates sanctions, asset freezes, and criminal
 *   proceedings across all EU member states, while imposing severe costs
 *   on the IRGC and its affiliates.
 *
 * KEY AGENTS (by structural relationship):
 *   - IRGC and Affiliates: Primary target (organized/trapped) — bears the full economic and political extraction of the designation.
 *   - EU Council: Primary beneficiary (institutional/arbitrage) — gains a powerful, coordinated tool for foreign policy enforcement.
 *   - EU External Action Service (EEAS): Secondary institutional actor (institutional/constrained) — part of the benefiting institution, but its diplomatic options are constrained by the designation.
 *   - Iranian Civilian Economy: Secondary victim (powerless/trapped) — suffers from the collateral economic damage of broad sanctions.
 *   - Analytical Observer: Geopolitical analyst (analytical/analytical) — sees the dual nature of the constraint as both coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_irgc_terrorist_designation, 0.60).
domain_priors:suppression_score(eu_irgc_terrorist_designation, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_irgc_terrorist_designation, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, extractiveness, 0.60).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_irgc_terrorist_designation, tangled_rope).
narrative_ontology:human_readable(eu_irgc_terrorist_designation, "EU Terrorist Designation of Iran's IRGC").
narrative_ontology:topic_domain(eu_irgc_terrorist_designation, "geopolitical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_irgc_terrorist_designation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, eu_member_states).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, irgc_and_affiliates).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, iranian_civilian_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1A: THE PRIMARY TARGET (IRGC)
% The IRGC is the direct target of coercive extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% From this viewpoint, the policy is a pure coercive weapon.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 1B: THE SECONDARY VICTIM (Iranian Civilian Economy)
% Civilians in Iran who are not part of the IRGC but suffer collateral damage
% from broad sanctions. They are powerless and trapped by the geopolitical
% conflict. For them, the designation is an indiscriminate Snare that harms
% them without offering any recourse or benefit.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (EU Council)
% The EU Council creates and wields the designation. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% For them, it's a low-cost mechanism to coordinate policy across 27 states.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function for the EU and the
% severe extraction imposed on Iran. This hybrid nature is the definition of
% a Tangled Rope.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The EU is not monolithic. Different bodies experience the constraint differently.

% Perspective 4: The EU External Action Service (EEAS) - The Diplomat
% The EEAS must maintain diplomatic channels, which are now constrained by the
% designation. Their exit options are limited. This reduces the perceived
% benefit, resulting in a Rope with higher (less negative) effective extraction.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_irgc_terrorist_designation_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap: Snare for the target, Rope for the beneficiary.
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint can only be a Tangled Rope if it has a beneficiary (coordination),
    % a victim (extraction), and requires active enforcement.
    narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, _),
    narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, _),
    domain_priors:requires_active_enforcement(eu_irgc_terrorist_designation).

:- end_tests(eu_irgc_terrorist_designation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.60): This value is set high to reflect the severe
 *     economic and political costs imposed by the designation. It is not higher
 *     because the designation's primary function is to enable coordinated
 *     action; the extraction is the *result* of that coordination.
 *   - Suppression (0.85): An FTO-style designation is extremely suppressive. It
 *     criminalizes a vast range of financial, logistical, and even communicative
 *     interactions, radically collapsing the alternative options for both the
 *     target and third parties wishing to engage with them.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the constraint. The EU Council, as the
 *   architect, sees a 'Rope'—a clean, effective tool for coordinating the
 *   disparate foreign policies of 27 member states into a single, enforceable
 *   position. The IRGC and the affected Iranian civilian economy, as the targets,
 *   experience a 'Snare'—a coercive trap with no exit that aims to cripple
 *   operations and inflict collateral damage. One agent's elegant coordination
 *   is another agent's existential threat.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `eu_member_states` benefit by solving a complex collective
 *     action problem. A single designation is far more efficient than 27
 *     separate national-level sanction regimes. This gives them low directionality (d).
 *   - Victim: `irgc_and_affiliates` and `iranian_civilian_economy` are the targets.
 *     The constraint is designed to extract resources, legitimacy, and operational
 *     freedom from them. This gives them high directionality (d).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key v6.0 feature. The EU Council (beneficiary, arbitrage exit)
 *   and the EU's diplomatic corps (EEAS; beneficiary, constrained exit) experience the
 *   same constraint differently. For the Council, it is an unalloyed policy win (Rope, χ < 0).
 *   For the EEAS, it is also a Rope, but a more costly one (χ is higher). The designation,
 *   while coordinating sanctions, simultaneously *constrains* diplomatic avenues,
 *   reducing the EEAS's operational flexibility. The perspectival gap *within* the
 *   institutional beneficiary measures the internal policy friction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid structure as a 'Tangled Rope'.
 *   A naive analysis might misclassify it as a pure 'Snare' by focusing only on
 *   the harm done to the target, thereby ignoring the genuine and complex
 *   coordination problem it solves for the EU. Conversely, a purely procedural
 *   analysis might label it a 'Rope' by focusing only on the coordination function,
 *   ignoring the highly extractive and coercive purpose. The Tangled Rope
 *   classification is essential for capturing this dual-function reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eu_irgc_terrorist_designation,
    'Is the primary function of this designation effective coercion (changing IRGC behavior) or performative political signaling (reassuring allies and domestic audiences)?',
    'Observing post-designation changes in IRGC funding, operations, and regional posture vs. measuring diplomatic and media signaling from EU capitals.',
    'If primarily effective, its ε=0.60 is justified. If primarily performative, its theater_ratio should be >0.70, reclassifying it towards a Piton from some perspectives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_irgc_terrorist_designation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is being created. T=0 represents the pre-designation phase where
% the threat exists, T=5 is early implementation, and T=10 is a mature, fully
% enforced regime.
% Theater ratio starts high (political signaling) and drops as functional
% enforcement takes over. Extraction starts low and ramps up as the legal
% machinery becomes effective.

narrative_ontology:measurement(eu_irgc_tr_t0, eu_irgc_terrorist_designation, theater_ratio, 0, 0.50).
narrative_ontology:measurement(eu_irgc_tr_t5, eu_irgc_terrorist_designation, theater_ratio, 5, 0.30).
narrative_ontology:measurement(eu_irgc_tr_t10, eu_irgc_terrorist_designation, theater_ratio, 10, 0.20).

narrative_ontology:measurement(eu_irgc_ex_t0, eu_irgc_terrorist_designation, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(eu_irgc_ex_t5, eu_irgc_terrorist_designation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eu_irgc_ex_t10, eu_irgc_terrorist_designation, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This designation is a legal framework for coordinating actions across states.
narrative_ontology:coordination_type(eu_irgc_terrorist_designation, enforcement_mechanism).

% This designation will structurally impact diplomatic efforts related to
% the Iran nuclear deal (JCPOA) by making negotiations more difficult.
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, iran_nuclear_deal_jcpoa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this model. The structural derivation from
% beneficiary/victim groups and exit options (trapped, arbitrage, constrained)
% correctly captures the directionality for all key perspectives. The
% distinction between the EU Council (arbitrage) and EEAS (constrained) is
% handled by the derivation chain without needing a manual override.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */