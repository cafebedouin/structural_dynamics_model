% ============================================================================
% CONSTRAINT STORY: fused_quartz_5d_archival
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_fused_quartz_5d_archival, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fused_quartz_5d_archival
 *   human_readable: Permanent Data Archival using 5D Fused Quartz Storage
 *   domain: technological
 *
 * SUMMARY:
 *   A new technology uses femtosecond lasers to write data into fused
 *   quartz wafers, creating a permanent archival medium capable of lasting
 *   billions of years. This technology solves the coordination problem of
 *   long-term data preservation, but its high initial cost, specialized
 *   equipment, and intellectual property create significant barriers to
 *   entry, concentrating power with a few institutional actors and
 *   displacing the legacy storage industry.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual Data Creators: Primary target (powerless/trapped) — Cannot afford the technology, trapping them in the cycle of data degradation on consumer-grade media.
 *   - Legacy Storage Industry: Secondary target (institutional/constrained) — Their business model of cyclical media replacement is threatened.
 *   - Archival Institutions (Libraries, Gov't Archives): Primary beneficiary (institutional/arbitrage) — Solves their core mandate of permanent data preservation, reducing long-term costs.
 *   - Technology IP Holders: Primary beneficiary (institutional/arbitrage) — Benefit from licensing and controlling the new standard.
 *   - Analytical Observer: Analytical observer — Sees both the immense coordination benefit and the extractive, market-disrupting dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fused_quartz_5d_archival, 0.47).
domain_priors:suppression_score(fused_quartz_5d_archival, 0.62).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fused_quartz_5d_archival, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fused_quartz_5d_archival, extractiveness, 0.47).
narrative_ontology:constraint_metric(fused_quartz_5d_archival, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fused_quartz_5d_archival, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fused_quartz_5d_archival, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fused_quartz_5d_archival). % Required for Tangled Rope (IP enforcement, standards maintenance)

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fused_quartz_5d_archival, archival_institutions).
narrative_ontology:constraint_beneficiary(fused_quartz_5d_archival, fused_quartz_ip_holders).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fused_quartz_5d_archival, individual_data_creators).
narrative_ontology:constraint_victim(fused_quartz_5d_archival, legacy_storage_industry).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (INDIVIDUAL DATA CREATOR)
% Cannot access the technology, trapped in the old system of data decay.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.47 * 1.42 * 1.0 (national scope) = 0.667, which meets the Snare threshold (>=0.66).
constraint_indexing:constraint_classification(fused_quartz_5d_archival, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ARCHIVAL INSTITUTION)
% Gains a permanent solution to its core mission.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(fused_quartz_5d_archival, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.47 * 1.15 * 1.2 (global scope) = 0.648. This is in the Tangled Rope range (0.40-0.90).
constraint_indexing:constraint_classification(fused_quartz_5d_archival, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The legacy storage industry, whose business model is disrupted.
% They are a victim, but an institutional actor with a constrained exit.
% Engine derives a moderately high d from victim + constrained exit.
constraint_indexing:constraint_classification(fused_quartz_5d_archival, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fused_quartz_5d_archival_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify perspectival gap between target (powerless) and beneficiary (institutional).
    constraint_indexing:constraint_classification(fused_quartz_5d_archival, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fused_quartz_5d_archival, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: Snare (powerless) vs Rope (institutional)').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(fused_quartz_5d_archival, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint is a Tangled Rope only if it has both coordination and asymmetric extraction,
    % and requires active enforcement.
    narrative_ontology:constraint_beneficiary(fused_quartz_5d_archival, _), % coordination
    narrative_ontology:constraint_victim(fused_quartz_5d_archival, _), % asymmetric extraction
    domain_priors:requires_active_enforcement(fused_quartz_5d_archival).

:- end_tests(fused_quartz_5d_archival_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.47) and suppression (0.62) are set to reflect a
 *   technology that is not directly coercive like a weapon, but whose IP and
 *   high capital costs create a powerful new center of economic gravity,
 *   effectively displacing an entire industry and creating a new class of
 *   data-haves and have-nots. This is a technology with a genuine, powerful
 *   coordination function, but also a significant extractive potential, making
 *   it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For archival institutions, it's a pure coordination
 *   tool (Rope) that solves a civilizational problem. For an individual or
 *   small business, the inaccessible promise of permanence acts as a Snare; it
 *   highlights the inadequacy of their available tools while offering no
 *   attainable alternative, trapping them in a cycle of cost and data loss.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'archival_institutions' and 'fused_quartz_ip_holders' directly
 *     benefit from the existence and control of this new standard. With arbitrage
 *     exit options, the engine derives a low 'd', resulting in a negative χ and a
 *     Rope classification.
 *   - Victims: 'individual_data_creators' are trapped, unable to access the
 *     technology, leading to a high 'd' and a Snare classification. The
 *     'legacy_storage_industry' is also a victim but as an institutional actor
 *     with a constrained exit, it perceives a Tangled Rope. This captures the
 *     nuance between different types of victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic Mandatrophy case. A naive analysis might see only the
 *   coordination function (preserving humanity's data) and label it a pure
 *   Rope. The Deferential Realism framework, by indexing to the powerless
 *   and the displaced industry, correctly identifies the hidden extraction
 *   and suppression. It is a Tangled Rope precisely because its coordination
 *   function is real, but its benefits are not symmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_fused_quartz_5d_archival,
    'Will fused quartz archival become a commoditized, accessible standard, or will it remain a centralized, high-cost technology controlled by a few institutions?',
    'Tracking manufacturing costs, open-sourcing of patents, and adoption rates outside of major institutions over the next 20-30 years.',
    'If commoditized, the constraint may evolve into a pure Rope. If it remains centralized, its Snare-like properties for powerless agents will intensify.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fused_quartz_5d_archival, empirical, 'The degree to which the technology will be democratized versus centralized over time.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fused_quartz_5d_archival, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a new technology. At T=0, extraction is low (R&D phase). As it is
% commercialized and IP is enforced (T=10), extraction rises to its modeled value.
% Theater remains low as it's a highly functional technology.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fq5da_tr_t0, fused_quartz_5d_archival, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fq5da_tr_t5, fused_quartz_5d_archival, theater_ratio, 5, 0.08).
narrative_ontology:measurement(fq5da_tr_t10, fused_quartz_5d_archival, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fq5da_ex_t0, fused_quartz_5d_archival, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(fq5da_ex_t5, fused_quartz_5d_archival, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(fq5da_ex_t10, fused_quartz_5d_archival, base_extractiveness, 10, 0.47).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(fused_quartz_5d_archival, information_standard).

% Network relationships (structural influence edges)
% This technology could render current data center models, which rely on
% constant power and cooling for volatile storage, less efficient for
% archival purposes.
narrative_ontology:affects_constraint(fused_quartz_5d_archival, data_center_energy_consumption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately captures
% the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */