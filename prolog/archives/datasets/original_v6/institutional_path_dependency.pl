% ============================================================================
% CONSTRAINT STORY: institutional_path_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_path_dependency, []).

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
 *   constraint_id: institutional_path_dependency
 *   human_readable: Institutional Path Dependency and Lock-in
 *   domain: institutional_economics/organizational_theory
 *
 * SUMMARY:
 *   Institutional path dependency creates a structural constraint where
 *   historical decisions, sunk costs, and established procedures lock
 *   organizations into inefficient patterns that are difficult to change
 *   despite availability of superior alternatives. This constraint is not a
 *   bug in institutional design — it is a feature that provides coordination
 *   benefits through predictability and continuity. However, it also enables
 *   extraction: institutional incumbents benefit from the stability while
 *   agents locked into legacy systems bear the costs of inefficiency. The
 *   constraint demonstrates all six DR types from different observational
 *   positions. What appears as a natural law of complex systems (Mountain) to
 *   the analytical observer is experienced as predatory lock-in (Snare) by
 *   agents with no exit options, as mixed coordination-and-extraction
 *   (Tangled Rope) by reformers, as a temporary problem with visible
 *   alternatives (Scaffold) to organized challengers, and as a degraded
 *   ritual (Piton) to long-term institutional analysis. The theater ratio
 *   (0.68) reflects that institutional continuity is justified through
 *   elaborate narratives of necessity, tradition, and stability while actual
 *   functional benefits may be lower. The rising extractiveness and theater
 *   over the interval (0.35→0.58 and 0.52→0.68 respectively) indicates
 *   increasing rent-seeking layered onto coordination as the institution
 *   ages.
 *
 * KEY AGENTS:
 *   - Incumbent Decision Makers: Primary beneficiary (institutional/arbitrage) — retain power, resources, and predictability through institutional continuity; can exit to other leadership positions
 *   - Agents Locked into Legacy Systems: Primary victim (powerless/trapped) — career, credentials, and identity tied to existing institutional structure; face severe switching costs and suppression
 *   - Alternative Institutional Designs: Conceptual victim (moderate/constrained) — viable alternatives exist but cannot gain adoption due to coordination lock-in and entrenched power
 *   - Reform-Minded Mid-Level Actors: Secondary victim (moderate/constrained) — see inefficiencies and want change but face career risk from challenging institutional path
 *   - Institutional Reform Coalition: Organized agents (organized/constrained) — new entrants, disruptive movements, younger generations building alternative institutional forms; have partial exit option (create parallel institutions)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable properties of complex systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_path_dependency, 0.58).
domain_priors:suppression_score(institutional_path_dependency, 0.62).
domain_priors:theater_ratio(institutional_path_dependency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_path_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_path_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_path_dependency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_path_dependency, tangled_rope).
narrative_ontology:human_readable(institutional_path_dependency, "Institutional Path Dependency and Lock-in").
narrative_ontology:topic_domain(institutional_path_dependency, "institutional_economics/organizational_theory").

domain_priors:requires_active_enforcement(institutional_path_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_path_dependency, incumbent_decision_makers).
narrative_ontology:constraint_beneficiary(institutional_path_dependency, institutional_continuity_advocates).
narrative_ontology:constraint_victim(institutional_path_dependency, alternative_institutional_designs).
narrative_ontology:constraint_victim(institutional_path_dependency, agents_trapped_by_legacy_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN AGENT (SNARE) — Career, skills, and identity are constituted within the existing institutional structure. Cannot exit without losing accumulated investment. Suppression is severe: switching costs are material (retraining, credential mismatch, relocation) and psychological (identity loss). No perceived alternative path. Maximum extraction experienced.
constraint_indexing:constraint_classification(institutional_path_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-MINDED MID-LEVEL ACTOR (TANGLED ROPE) — Experiences both coordination function (the institution does solve collective action problems) and extraction (path dependency prevents modernization). Can theoretically exit but faces high career cost. Sees genuine institutional benefits alongside inefficient lock-in. Moderate extraction with real constraints.
constraint_indexing:constraint_classification(institutional_path_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Senior leadership, entrenched stakeholders, and legacy industries benefit from institutional continuation. Experience the constraint as pure coordination — 'This is how we do things; continuity enables planning.' Can exit at low cost (leadership mobility, portfolio diversification). Net beneficiary position.
constraint_indexing:constraint_classification(institutional_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (new entrants, disruptive startups, reform movements, younger generations) perceive path dependency as a temporary coordination failure with a sunset clause. They are building alternative institutional forms (platform economies, DAOs, distributed governance) that bypass legacy constraints. Low experienced extraction because coalition has agency and sees the path dependency as transitional.
constraint_indexing:constraint_classification(institutional_path_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL SUCCESSION MECHANISM (PITON) — From a civilizational view, institutional change mechanisms (generational succession, revolutionary overthrow, gradual norm shift) are themselves degraded and ritualized. The rhetoric of 'institutional reform' persists but with declining effectiveness — real change requires institutional collapse rather than path-correcting reform. Theater is high; functional change is low.
constraint_indexing:constraint_classification(institutional_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational analytical perspective, path dependency appears as an immutable mathematical property of complex adaptive systems: increasing returns, network effects, and coordination costs create natural barriers to institutional change. This perspective naturalizes what may be a contingent institutional arrangement. Engine's false summit detector identifies this as naturalization.
constraint_indexing:constraint_classification(institutional_path_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_path_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_path_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_path_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_path_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_path_dependency, TR),
    TR >= 0.70.

:- end_tests(institutional_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional path dependency does extract value from locked-in agents through suppressed innovation, limited career mobility, and constrained opportunities. However, the extraction is not total (χ ≈ 0.66 at powerless perspective) because coordination benefits are real and genuine — the institution does solve collective action problems. The increased value (from 0.35 to 0.58 over the interval) reflects rent-seeking layering: as the institution matures, more resources are devoted to defending the established path than to achieving the original coordination function. Suppression (0.62): High. Multiple barriers prevent exit: cognitive (agents cannot imagine alternatives within their institutional frame), material (credentials and skills transfer poorly), social (reputation and network effects), and organizational (formal and informal rules block deviation). But suppression is not total (0.62, not 0.95) because alternatives do exist and some agents do escape; suppression is the constraint that keeps escape rare, not impossible. Theater ratio (0.68): High. The rhetoric of institutional necessity, tradition, and stability often exceeds functional justification. Leadership narratives emphasize path dependency as natural law when it is actually a coordination equilibrium vulnerable to perturbation — but narratives persist because institutional change is genuinely costly. Theater increases over the interval as the original functional need becomes less salient and the narrative of 'this is how things are done' hardens into tradition.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence demonstrates the diagnostic power of indexed classification. From a powerless/trapped perspective, the constraint is a pure Snare (χ ≈ high). From an institutional/arbitrage perspective, it is a pure Rope (χ ≈ low). From moderate/constrained perspective at generational time horizon, it is Tangled Rope (mixed). From organized/constrained perspective at generational time horizon, it is Scaffold (temporary). From institutional/mobile perspective at civilizational time horizon, it is Piton (degraded). From analytical perspective at civilizational time horizon, it appears as Mountain but is detected as false summit. The perspectival gaps reveal that path dependency is socially constructed (different agents with different exit options perceive radically different constraints) but structurally real (the lock-in mechanisms are genuine, not perceptual illusions).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from the agent's structural position and exit options. Incumbents with arbitrage exit options (can move to leadership roles elsewhere) have low d (0.1-0.2): they are net beneficiaries. Locked-in agents with trapped exit have high d (0.90+): they bear maximum extraction. Mid-level reformers with constrained exit have moderate-high d (0.65-0.75): they could theoretically switch but face substantial career cost. Organized challengers with constrained exit (can build parallel institutions but at resource cost) have moderate d (0.50-0.60). The sigmoid f(d) applies these d values to base extractiveness to compute chi for each perspective. Beneficiary perspectives (low d) show low chi even when base extractiveness is moderate; victim perspectives (high d) show high chi. The directionality derivation captures why the same institutional structure is experienced as coordination by those who benefit and extraction by those locked in.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that institutional path dependency is genuinely both a coordination mechanism (hence Rope/Scaffold perspectives are legitimate) and an extraction mechanism (hence Snare/Tangled Rope perspectives are legitimate). The mandatrophy is not 'which is it really?' but 'who benefits and who bears costs, given their exit options?' The beneficiary's Rope is their genuine experience — the institution does coordinate. The victim's Snare is their genuine experience — they cannot escape. The reformer's Tangled Rope is the accurate middle view: institutional path dependency solves real coordination problems AND extracts value from those locked in. The scaffold perspective captures the temporal dimension: generational timescale may enable sunset as alternatives become available. The piton perspective captures the degradation: as original functional needs become less salient, the institution persists through institutional inertia rather than active coordination. The mountain perspective is a false summit that naturalizes what is actually a contingent institutional equilibrium vulnerable to sufficiently large perturbation (generational turnover, technological disruption, institutional collapse). No single type is 'the' answer — the presheaf of perspectives IS the answer, revealing that path dependency is neither a natural law nor pure extraction but a hybrid coordination-extraction mechanism whose balance shifts with the observer's structural position and time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurement,
    'Are switching costs primarily material (retraining, relocation, credential mismatch) or internalized (identity loss, cognitive reframing, psychological capital)?',
    'Longitudinal studies of agents who exit institutions: do post-exit suppression costs persist or decline? If costs persist after material barriers are removed, suppression is internalized.',
    'If material: suppression decreases with institutional alternatives. If internalized: path dependency may be identity-locked rather than structurally trapped. Classification may shift from Snare (trapped) to Snare (identity_locked) or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_measurement, empirical, 'Material vs internalized nature of institutional switching costs').

omega_variable(
    alternative_institution_viability,
    'Do institutional alternatives (DAOs, platform economies, distributed governance) actually reduce path dependency or merely displace it to new lock-in points?',
    'Longitudinal analysis of alternative institutions: do they show the same increasing-returns dynamics as legacy institutions? Do they resist change after maturation?',
    'If alternatives truly reduce lock-in: path dependency is contingent (Scaffold sunset is real). If alternatives recreate lock-in: path dependency is structural (Mountain from all perspectives, false summit classification is wrong).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_institution_viability, empirical, 'Whether institutional alternatives escape path dependency or recreate it').

omega_variable(
    generational_turnover_rate,
    'At what generational turnover rate does path dependency become unstable enough to permit institutional transition without collapse?',
    'Historical analysis of institutional transitions; correlation between generational replacement rates and successful path-correcting reforms vs institutional dissolution',
    'Determines whether Scaffold sunset is realistic (generational timescale) or aspirational (requires institutional collapse on Civilizational scale). Affects classification confidence for Reform Coalition perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_turnover_rate, empirical, 'Generational turnover rate threshold for institutional transition').

omega_variable(
    path_dependency_intentionality,
    'Is institutional path dependency maintained through active enforcement (decision makers deliberately blocking alternatives) or passive structural accumulation (alternatives simply cost more)?',
    'Documentary analysis of institutional decision-making; interviews with leadership about whether path dependency is viewed as problem vs feature; analysis of resources devoted to blocking vs supporting alternatives',
    'If active: path dependency is a Snare (deliberately extractive). If passive: path dependency is a Piton (degraded but not actively maintained). Affects whether requires_active_enforcement flag is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_intentionality, empirical, 'Active enforcement vs passive structural accumulation of path dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_path_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pathd_tr_t0, institutional_path_dependency, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pathd_tr_t3, institutional_path_dependency, theater_ratio, 3, 0.61).
narrative_ontology:measurement(pathd_tr_t6, institutional_path_dependency, theater_ratio, 6, 0.67).
narrative_ontology:measurement(pathd_tr_t10, institutional_path_dependency, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pathd_be_t0, institutional_path_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pathd_be_t3, institutional_path_dependency, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(pathd_be_t6, institutional_path_dependency, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(pathd_be_t10, institutional_path_dependency, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_path_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_path_dependency, coordination_equilibrium_stability).
narrative_ontology:affects_constraint(institutional_path_dependency, organizational_innovation_inhibition).
narrative_ontology:affects_constraint(institutional_path_dependency, generational_succession_mechanism).

% DUAL FORMULATION NOTE:
% Institutional path dependency operates at multiple scales (organizational, sectoral, civilizational) with different extractiveness values. The organizational-scale story (this JSON) shows ε ≈ 0.58 with active enforcement. Sectoral and civilizational scale stories would decompose the constraint family with different ε values reflecting increasing aggregation and institutional inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_path_dependency, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
