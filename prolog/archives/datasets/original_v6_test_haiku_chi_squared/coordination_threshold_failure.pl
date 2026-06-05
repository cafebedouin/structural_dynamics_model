% ============================================================================
% CONSTRAINT STORY: coordination_threshold_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_threshold_failure, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coordination_threshold_failure
 *   human_readable: The Critical Mass Chasm
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Critical Mass Chasm is a structural coordination failure where a
 *   network or protocol provides zero utility to participants until a
 *   specific adoption threshold is crossed. Below the threshold, the
 *   constraint manifests as pure extraction: late joiners and marginal users
 *   bear all the switching costs and friction of participation while
 *   receiving no benefit (dead network utility). Early adopters and network
 *   operators benefit from first-mover advantage and subsidy mechanisms that
 *   lower the apparent threshold. The suppression mechanisms (data
 *   portability friction, social graph lock-in, switching costs, learning
 *   curve, app ecosystem gaps) are both technical requirements and
 *   artificially inflated by platform design. This creates a hybrid
 *   constraint: genuine coordination problem (bootstrapping a network to
 *   viability) combined with asymmetric extraction (those who join late pay
 *   switching costs those who joined early never faced). The threshold itself
 *   is neither immutable nor arbitrary — it is a design-contingent function
 *   that can be moved by UI improvements, subsidies, interoperability
 *   standards, or regulatory mandates. The theater ratio captures the
 *   performative element: mandates and subsidies often persist as enforcement
 *   theater even after organic critical mass has been achieved, because the
 *   institutional apparatus becomes inertial.
 *
 * KEY AGENTS:
 *   - Early Adopters / Network Operators: Primary beneficiaries (institutional/arbitrage) — capture network growth premium and first-mover advantages; can arbitrage to alternatives; face negligible switching costs
 *   - Late Joiners: Primary victims (moderate/constrained) — face binary payoff structure (below threshold = useless) and asymmetric switching costs relative to early adopters
 *   - Marginal Users (below threshold): Secondary victims (powerless/trapped) — receive zero utility and cannot exit; trapped in dead network by switching cost lock-in and platform fragmentation
 *   - Bootstrap Coalition: Organized agents (organized/constrained) — subsidy programs, user acquisition campaigns, corporate sponsorships, seed funding that lower the effective threshold through coercion rather than network value
 *   - Regulatory Mandate Apparatus: Institutional enforcement (institutional/arbitrage) — government mandates (e.g., digital ID adoption, payment system interoperability) bypass coordination problem through top-down enforcement; persist as inertial theater after organic threshold crossed
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing threshold as immutable network law rather than recognizing it as contingent on design, subsidies, and lock-in mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_threshold_failure, 0.52).
domain_priors:suppression_score(coordination_threshold_failure, 0.68).
domain_priors:theater_ratio(coordination_threshold_failure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_threshold_failure, extractiveness, 0.52).
narrative_ontology:constraint_metric(coordination_threshold_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coordination_threshold_failure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_threshold_failure, tangled_rope).
narrative_ontology:human_readable(coordination_threshold_failure, "The Critical Mass Chasm").
narrative_ontology:topic_domain(coordination_threshold_failure, "social/technological").

domain_priors:requires_active_enforcement(coordination_threshold_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_threshold_failure, early_adopters).
narrative_ontology:constraint_beneficiary(coordination_threshold_failure, network_operators).
narrative_ontology:constraint_victim(coordination_threshold_failure, late_joiners).
narrative_ontology:constraint_victim(coordination_threshold_failure, marginal_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL USER (SNARE) — Pre-threshold participant trapped in dead network with zero utility. Cannot exit because value proposition is binary (below threshold = useless). Network lock-in mechanisms (switching costs, data portability friction, social graph fragmentation) prevent migration. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(coordination_threshold_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATE JOINER (TANGLED ROPE) — Faces asymmetric payoff structure: benefits from coordination once threshold crossed, but bears disproportionate cost during accumulation phase (low utility, high switching friction, social pressure to conform). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY ADOPTER & NETWORK OPERATOR (ROPE) — Benefits from first-mover advantage and network growth before threshold. Experiences constraint as pure coordination problem: 'how do we bootstrap this network?' Switching costs are negligible for them; they can arbitrage to competing platforms. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(coordination_threshold_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BOOTSTRAP COALITION (SCAFFOLD) — Coordinated subsidy/incentive programs (user acquisition campaigns, seed funding, corporate sponsorships, regulatory mandates) temporarily lower the effective threshold. This is sunset logic: as organic adoption accelerates, subsidy can be withdrawn. d≈0.42, f(d)≈0.41, σ=1.2 → χ≈0.20. Low extraction because coalition has agency and sees an explicit exit path (self-sustaining adoption).
constraint_indexing:constraint_classification(coordination_threshold_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY MANDATE APPARATUS (PITON) — Government/institutional mandates (e.g., 'all merchants must accept digital ID') bypass the coordination problem through enforcement rather than network utility. Theater_ratio≈0.65: the mandate persists as theater even after critical mass is achieved organically, because enforcement infrastructure becomes institutionally inertial. d≈0.05, f(d)≈-0.11, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(coordination_threshold_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a universal/civilizational perspective, the threshold is a mathematical property: network effects are superlinear; utility scales as n² (or similar). Below critical mass, collective action fails. This looks immutable — the structure of network effects themselves. However, the structural data (ε=0.52, suppression=0.68) contradicts true mountain properties (ε≤0.25, suppression≤0.05). The engine will detect false summit: the threshold is a contingent function of platform design, subsidy levels, and switching costs, not a law of nature.
constraint_indexing:constraint_classification(coordination_threshold_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_threshold_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coordination_threshold_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coordination_threshold_failure, TR),
    TR >= 0.70.

:- end_tests(coordination_threshold_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Below threshold, marginal users receive zero utility while bearing full switching costs. This is extraction, not mere coordination cost. However, it is not maximal (0.66+) because: (1) the extraction is temporally bounded (ends once threshold is crossed organically), (2) the threshold can be lowered by improving network design or reducing switching costs, (3) subsidy mechanisms provide a real (if coercive) alternative to purely organic growth. The extractiveness has increased over the interval as platform lock-in mechanisms have been deployed more aggressively. Suppression (0.68): High. Significant barriers to exit include data portability friction, social graph lock-in (network effects mean staying in marginal platform is individually rational if peers are there), switching costs (learning UI, migrating data, rebuilding contacts), app ecosystem gaps, and incumbent advantage. Suppression is partly technical (inherent to network effects) and partly artificial (platform design choices to inflate lock-in). Theater ratio (0.48): Moderate. Initial theater is low because the chasm is a real structural problem—bootstrapping networks genuinely requires coordination. But theater increases as: (1) subsidies become performative after organic adoption begins, (2) regulatory mandates persist past the point of necessity, (3) enforcement theater substitutes for network utility. The interval shows this progression.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification divergence rooted in temporal position and structural role. The early adopter sees a coordination problem (Rope)—bootstrapping a valuable network. The late joiner sees extraction with a coordination function (Tangled Rope)—they are caught in the chasm between uselessness and critical mass. The marginal user sees pure extraction (Snare)—they receive zero utility and cannot exit. The bootstrap coalition sees a temporary problem (Scaffold)—subsidies and campaigns lower the threshold until organic adoption takes over. The regulatory apparatus sees institutional enforcement theater (Piton)—mandates persist through inertia after the problem is solved. The analytical observer risks seeing natural law (Mountain)—network effects and critical mass are immutable properties of network topology—but the structural data reveals this as a false summit: threshold location is design-contingent. The perspectival gap is the fundamental question: Is the chasm a law of networks, or a contingent institutional choice about where to set the threshold and how to fund the gap?
 *
 * DIRECTIONALITY LOGIC:
 *   Early Adopter/Network Operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. They capture network growth before threshold and have costless exit. Late Joiner: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction. They bear switching costs and chasm risk but also benefit once threshold is crossed (hence constrained, not trapped). Marginal User: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Below-threshold participants receive zero utility and cannot exit due to lock-in. Bootstrap Coalition: Organized + constrained → d≈0.42, f(d)≈0.41. Low effective extraction because coalition has agency and sees explicit sunset (self-sustaining adoption makes subsidies unnecessary). Regulatory Apparatus: Institutional + arbitrage → d≈0.05, f(d)≈-0.11. Piton classification comes from theater_ratio gate, not from high chi. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Natural law framing; engine detects false summit via ε and suppression metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: This constraint resolves mandatrophy by recognizing that the 'threshold' is not a natural law but a design parameter. The analytical observer's Mountain is a false summit. The true structure is Tangled Rope at the system level: the network provides genuine coordination value (beneficiary: early adopters get a working platform; victims: late joiners bear accumulation costs), asymmetric extraction (switching costs are inflated by design, not necessity), and active enforcement (subsidies and mandates coerce participation). The Scaffold perspective is empirically testable: if subsidies are withdrawn and adoption remains stable, the constraint has self-resolved (true scaffold). If adoption collapses, the constraint was never self-sustaining and remains Snare (pure extraction). The Piton perspective reveals that enforcement theater persists after the problem is solved—indicating institutional capture by the regulatory apparatus. The key mandatrophy insight: asking 'Is the chasm natural or designed?' disambiguates constraint from law. Network effects are immutable; the critical mass percentage is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_exact_threshold,
    'What is the true critical mass percentage for a given network topology? Is it fixed or design-dependent?',
    'Empirical testing across platforms (messaging, payments, social): measurement of adoption curves; identification of knee points; variation across UI design, subsidy levels, and switching cost regimes',
    'If threshold is fixed (e.g., always 15-20%): network effects are quasi-natural. If design-dependent (can be moved via UX, subsidies, mandates): threshold is contingent institutional choice. Classification shifts from mountain toward tangled_rope/snare across more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_exact_threshold, empirical, 'Whether critical mass threshold is a fixed function or design-contingent').

omega_variable(
    subsidy_withdrawal_stability,
    'When subsidies or regulatory mandates are withdrawn, does the network maintain adoption or collapse back to pre-threshold state?',
    'Historical case studies: subsidy withdrawal experiments (e.g., India digital payment rollback scenarios); longitudinal adoption tracking post-policy change; measurement of organic vs induced participation',
    'If collapses: threshold is fundamentally dependent on coercion (pure snare). If sustains: threshold was successfully bootstrapped and the constraint self-resolves (true scaffold sunset). Classification shifts the organized perspective from scaffold to piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_withdrawal_stability, empirical, 'Stability of adoption after subsidy or mandate withdrawal').

omega_variable(
    switching_cost_exogeneity,
    'Are switching costs (data portability, social graph replication, learning curve) intrinsic technical requirements or artificially inflated by platform design choices?',
    'Comparison of switching friction across platforms with equivalent functionality; audit of interoperability barriers (API access, data export formats); analysis of lock-in mechanisms as design vs necessity',
    'If intrinsic: suppression is quasi-immutable. If artificial: platform operators are actively extracting via lock-in design (increases snare classification for late joiners). Changes directionality of network_operators beneficiary status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_exogeneity, empirical, 'Whether switching costs are technical requirements or design choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_threshold_failure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crit_mass_tr_t0, coordination_threshold_failure, theater_ratio, 0, 0.22).
narrative_ontology:measurement(crit_mass_tr_t3, coordination_threshold_failure, theater_ratio, 3, 0.35).
narrative_ontology:measurement(crit_mass_tr_t6, coordination_threshold_failure, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(crit_mass_be_t0, coordination_threshold_failure, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(crit_mass_be_t3, coordination_threshold_failure, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(crit_mass_be_t6, coordination_threshold_failure, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_threshold_failure, resource_allocation).
narrative_ontology:affects_constraint(coordination_threshold_failure, network_effects_rent_extraction).
narrative_ontology:affects_constraint(coordination_threshold_failure, platform_ecosystem_lock_in).
narrative_ontology:affects_constraint(coordination_threshold_failure, user_acquisition_subsidy_decay).

% DUAL FORMULATION NOTE:
% The Critical Mass Chasm decomposes into two structural constraints: (1) the mathematical threshold property of network effects (quasi-mountain, ε≈0.08, universal), and (2) the institutional extraction of switching costs via lock-in design (tangled_rope, ε≈0.52, national/global scope). These are linked via design-contingency: moving the threshold via interoperability standards, data portability, or UI improvements reduces extraction in (2) without changing the mathematical property in (1). Separate stories for network_effects_threshold (mountain) and platform_lock_in_extraction (tangled_rope) are recommended for future corpus expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coordination_threshold_failure, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
