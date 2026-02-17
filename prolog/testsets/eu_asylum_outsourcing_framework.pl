% ============================================================================
% CONSTRAINT STORY: eu_asylum_outsourcing_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_asylum_outsourcing_framework, []).

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
 *   constraint_id: eu_asylum_outsourcing_framework
 *   human_readable: EU framework for outsourcing asylum procedures to third countries
 *   domain: political
 *
 * SUMMARY:
 *   Following a vote in the EU Parliament, a new legal framework allows for
 *   asylum applications to be processed in non-EU countries. This policy,
 *   part of a broader migration pact, aims to reduce the number of asylum
 *   seekers arriving and residing in the EU during their application process.
 *   It is presented as a coordination mechanism for member states but is heavily
 *   criticized by human rights groups for externalizing humanitarian
 *   responsibilities and potentially violating international law.
 *
 * KEY AGENTS (by structural relationship):
 *   - Asylum Seekers: Primary target (powerless/trapped) — bear the extraction of their right to claim asylum on EU soil.
 *   - EU Member State Governments: Primary beneficiary (institutional/arbitrage) — benefit by externalizing the political and financial costs of processing asylum claims.
 *   - Third-Country Regimes: Inter-institutional partner (institutional/constrained) — receive financial aid but bear the logistical and social burden of hosting processing centers.
 *   - Human Rights Observers: Analytical observer (analytical/analytical) — see the full structure, including the coordination claims and the extractive reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_asylum_outsourcing_framework, 0.75). % Extraction of the fundamental right to seek asylum on EU territory.
domain_priors:suppression_score(eu_asylum_outsourcing_framework, 0.80).   % Structurally closes off the primary avenue for seeking asylum, suppressing alternatives.
domain_priors:theater_ratio(eu_asylum_outsourcing_framework, 0.20).       % While politically performative, the mechanism is highly functional, not a Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, extractiveness, 0.75).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this human-constructed political constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_asylum_outsourcing_framework, tangled_rope).
narrative_ontology:human_readable(eu_asylum_outsourcing_framework, "EU framework for outsourcing asylum procedures to third countries").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_asylum_outsourcing_framework). % Requires diplomatic deals, border enforcement, and transfer logistics.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, eu_member_state_governments).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, asylum_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ASYLUM SEEKER (SNARE)
% Victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.75 * 1.42 * σ(regional=0.9) ≈ 0.96.
% High base extraction, high suppression, and high effective extraction.
% From their perspective, this is a coercive trap that removes their agency.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE EU MEMBER STATE GOVERNMENT (ROPE)
% Beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.75 * -0.12 * σ(continental=1.1) ≈ -0.10.
% Negative effective extraction. They see a pure coordination tool that solves
% a collective action problem (managing migration) to their benefit.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.75 * 1.15 * σ(global=1.2) ≈ 1.03.
% The analytical observer sees the valid coordination function for member states
% AND the severe asymmetric extraction from asylum seekers, which are the
% definitional components of a Tangled Rope. The high χ confirms this.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE THIRD-COUNTRY REGIME (TANGLED ROPE)
% This actor is both a beneficiary (of EU funds) and a victim (of the
% political/social instability). The 'constrained' exit option reflects their
% dependency on the EU, yielding a higher directionality (d) than the EU's
% 'arbitrage' exit. The engine derives a mid-range d, producing a χ that
% reflects the mixed nature of the arrangement.
% Assuming derived d ≈ 0.6 -> f(d) ≈ 0.88.
% χ = 0.75 * 0.88 * σ(regional=0.9) ≈ 0.59.
% They experience it as a Tangled Rope: a coordination deal with significant extractive burdens.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_asylum_outsourcing_framework_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, _), % Has coordination function
    narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, _),     % Has asymmetric extraction
    domain_priors:requires_active_enforcement(eu_asylum_outsourcing_framework). % Has enforcement

:- end_tests(eu_asylum_outsourcing_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.75): The value is high because the constraint extracts a
 *     fundamental right established under international law: the ability to seek
 *     asylum in the territory of a signatory. This is a non-financial but
 *     profoundly severe form of extraction.
 *   - Suppression (0.80): The policy's explicit goal is to suppress the primary
 *     alternative (arriving at an EU border and applying directly), making it
 *     a structurally coercive system.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For asylum seekers (powerless/trapped), the system is a
 *   Snare, removing agency and trapping them in a legal limbo outside the
 *   territory where they seek protection. For EU member state governments
 *   (institutional/arbitrage), it's a Rope that solves a political coordination
 *   problem, allowing them to collectively manage migration pressures in a way
 *   that benefits their domestic political goals. The directionality engine
 *   captures this perfectly: victim+trapped yields d≈0.95 (high χ), while
 *   beneficiary+arbitrage yields d≈0.05 (negative χ).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `eu_member_state_governments`. They benefit by offloading the
 *     logistical, financial, and political costs of hosting asylum seekers.
 *   - Victim: `asylum_seekers`. They bear the direct cost through the loss of
 *     legal protections, access to counsel, and the fundamental right to have
 *     their claim heard on EU soil.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The perspective of the `third_country_regimes` is crucial. They are not
 *   pure beneficiaries. Their `constrained` exit status reflects their dependence
 *   on EU partnership, giving them less leverage than the EU's `arbitrage`
 *   position. They receive funds (a benefit) but also import significant social
 *   and political risks (an extraction). The resulting classification as a
 *   Tangled Rope accurately models this complex, semi-coercive partnership.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a textbook example of mandatrophy risk. The public narrative
 *   from proponents frames the policy as a pure Rope ("orderly and humane
 *   management of migration"). Deferential Realism pierces this narrative by
 *   insisting on an indexical analysis. By identifying the victims and the
 *   high base extraction (ε), the system correctly classifies the structure as a
 *   Tangled Rope from an analytical view and a Snare from the victim's view,
 *   preventing the coordination claim from masking the severe coercive extraction.
 *   [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eu_asylum_outsourcing_framework,
    'Is the legal designation of a "safe third country" a robust, enforceable safeguard or a political fiction to enable refoulement?',
    'Long-term, independent auditing of legal outcomes, human rights conditions, and appeal success rates in the designated third countries.',
    'If it is a robust safeguard, base extractiveness (ε) might be lower (e.g., 0.50). If it is a fiction, ε is correctly estimated or potentially higher.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_asylum_outsourcing_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This high-extraction constraint (ε=0.75) requires temporal tracking.
% The model shows the idea evolving from cooperative agreements (lower ε)
% to a formalized, highly extractive legal framework.
% T=0: Early bilateral border cooperation agreements.
% T=5: "Turkey deal" model, more formalized outsourcing.
% T=10: Pan-EU legal framework for third-country processing.

% Theater ratio over time (declines as policy becomes more functional):
narrative_ontology:measurement(eu_asylum_outsourcing_framework_tr_t0, eu_asylum_outsourcing_framework, theater_ratio, 0, 0.40).
narrative_ontology:measurement(eu_asylum_outsourcing_framework_tr_t5, eu_asylum_outsourcing_framework, theater_ratio, 5, 0.30).
narrative_ontology:measurement(eu_asylum_outsourcing_framework_tr_t10, eu_asylum_outsourcing_framework, theater_ratio, 10, 0.20).

% Extraction over time (increases as policy formalizes and intensifies):
narrative_ontology:measurement(eu_asylum_outsourcing_framework_ex_t0, eu_asylum_outsourcing_framework, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(eu_asylum_outsourcing_framework_ex_t5, eu_asylum_outsourcing_framework, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(eu_asylum_outsourcing_framework_ex_t10, eu_asylum_outsourcing_framework, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It enforces a common external border policy.
narrative_ontology:coordination_type(eu_asylum_outsourcing_framework, enforcement_mechanism).

% Network relationship: This policy directly impacts the functioning and
% meaning of internal free movement within the Schengen Area by hardening
% the bloc's external border policies.
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, schengen_area_free_movement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain
% (beneficiary/victim declarations + exit_options) accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */