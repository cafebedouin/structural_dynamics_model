% ============================================================================
% CONSTRAINT STORY: steinmetz_valuation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_steinmetz_valuation_asymmetry, []).

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
 *   constraint_id: steinmetz_valuation_asymmetry
 *   human_readable: The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Steinmetz chalk mark story exemplifies a fundamental constraint in
 *   industrial capitalism: the extreme asymmetry between compensation for
 *   physical labor and compensation for specialized knowledge. The apocryphal
 *   tale — Steinmetz diagnosing a factory's broken electrical system with a
 *   single chalk mark, then billing Henry Ford $10,000 for the chalk mark
 *   ($5,000 for materials and $5,000 for knowing where to make it) — captures
 *   the paradox of knowledge valuation. The worker performs the same
 *   diagnostic task but is compensated as if labor is fungible, while the
 *   specialist is compensated for the accumulated knowledge embedded in the
 *   decision. This constraint creates a snare for workers without formal
 *   credentialing in specialized domains: they are trapped by artificial
 *   valuation scarcity even when they possess equal expertise. The constraint
 *   has intensified over the interval (1900-1950 in historical terms) as
 *   formalized credentialing systems emerged to gatekeep access to
 *   knowledge-based compensation. The theater ratio (0.55) reflects that both
 *   the credentialing system and the knowledge specialization themselves
 *   contain performative elements — a certification signals knowledge without
 *   proving capability, while some 'specialized knowledge' is actually
 *   codifiable or learnable but artificially restricted through institutional
 *   barriers.
 *
 * KEY AGENTS:
 *   - Specialized Knowledge Holders: Primary beneficiary (institutional/arbitrage) — extract rents from scarcity; can move between enterprises and leverage exclusive access
 *   - Industrial Workers: Primary victim (powerless/trapped) — compensation suppressed despite expertise; lack formal credentials or institutional mobility to access knowledge-based pricing
 *   - Factory Managers: Secondary victim (moderate/constrained) — aware of hidden cost externalization; cannot transparently acknowledge specialist dependency without disrupting labor market
 *   - Industrial Enterprise Leadership: Powerful coordinator (powerful/mobile) — intentionally maintains asymmetry to suppress labor costs; benefits from both specialist access and worker suppression
 *   - Labor Movement: Organized collective (organized/constrained) — pursuing education and credentialing democratization; sees sunset in broad-based technical training
 *   - Credentialing System: Institutional maintainer (institutional/constrained) — enforces gate-keeping through formal requirements; persists despite alternative pathways emerging
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing scarcity as immutable information asymmetry; engine detects false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, 0.58).
domain_priors:suppression_score(steinmetz_valuation_asymmetry, 0.68).
domain_priors:theater_ratio(steinmetz_valuation_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(steinmetz_valuation_asymmetry, snare).
narrative_ontology:human_readable(steinmetz_valuation_asymmetry, "The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)").
narrative_ontology:topic_domain(steinmetz_valuation_asymmetry, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, specialized_knowledge_holders).
narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, capital_intensive_enterprises).
narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, skilled_labor_without_specialized_knowledge).
narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, industrial_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDUSTRIAL WORKER (SNARE) — Cannot exit the valuation regime; labor market offers no alternative pricing for accumulated skill that lacks the formal 'specialist' credential. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Extraction is severe: the worker operates at expert level but receives wages calibrated to commodified labor.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FACTORY MANAGER (SNARE) — Constrained by dependency on specialized knowledge holders for crisis resolution; cannot be transparent about the true cost structure without raising worker compensation demands. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.67. Secondary victim: suffers from hidden cost externalization and labor market instability.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPECIALIZED KNOWLEDGE HOLDER (ROPE) — High arbitrage power: can move between enterprises, leverage scarcity, extract rents from coordination failure. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary. The specialist experiences the asymmetry as purely beneficial coordination: their scarce knowledge is properly priced, legitimately commanding premium compensation.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL ENTERPRISE LEADERSHIP (TANGLED ROPE) — Powerful + mobile exit. Experiences the constraint as both coordination benefit (access to specialist knowledge) and extractive mechanism (suppresses worker compensation through artificial valuation separation). d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.24. Low effective extraction because leadership can arbitrage and has agency; they maintain the asymmetry intentionally.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR MOVEMENT (SCAFFOLD) — Organized collective seeking to compress the valuation asymmetry through unionization, apprenticeship programs, and collective bargaining. d≈0.55, f(d)≈0.74, σ=0.9 → χ≈0.37. The coalition sees a sunset: broad-based technical education (technical colleges, credentialing reforms) are designed to eliminate the artificial knowledge scarcity that enables extraction.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CREDENTIALING SYSTEM (PITON) — Educational credentials (engineering degrees, professional certifications) perform gatekeeping theater while the actual knowledge transfer occurs through apprenticeship, on-the-job training, and accumulated experience. theater_ratio=0.55 reflects partial performativity: credentials do signal reliability but also artificially restrict access to knowledge valuations. The credentialing system persists through institutional inertia despite alternatives (bootcamps, open-source communities) emerging.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the constraint appears as an immutable information asymmetry: some knowledge is costly to acquire, therefore those with it can extract rents. Market mechanisms cannot eliminate this because knowledge scarcity is real. However, structural data (ε=0.58, suppression=0.68, theater=0.55) contradicts full naturalization — the engine detects a false summit. The asymmetry is amplified by artificial credentialing gatekeeping and suppression of alternative knowledge pathways, not purely by information scarcity.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(steinmetz_valuation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(steinmetz_valuation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(steinmetz_valuation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The specialist commands premium compensation far exceeding physical labor costs; this extraction is not absolute (specialists are paid legitimately higher wages) but is amplified by artificial credentialing barriers that restrict who can access knowledge-based pricing. The asymmetry increased from ~0.35 (when knowledge was more often transmitted through apprenticeship) to 0.58 (when formal credentialing became the primary gatekeeping mechanism). Suppression (0.68): High. Multiple suppression mechanisms operate: (1) Information asymmetry — workers often don't know their own expertise could command higher compensation; (2) Institutional gatekeeping — formal credentials are required to access specialist labor markets, even for workers with equivalent knowledge; (3) Mobility barriers — workers without credentials face regional and sectoral restrictions on knowledge-based roles; (4) Deliberate opacity — enterprises suppress transparency about specialist dependency and true cost structures. Theater ratio (0.55): Moderate. The credentialing system performs gatekeeping theater (formal degree proves competence but doesn't validate actual diagnostic ability), while some specialist knowledge is genuinely tacit and difficult to codify. However, open-source communities and alternative credentialing (bootcamps, apprenticeships) demonstrate that much 'specialized' knowledge is codifiable and teachable, indicating performative gatekeeping rather than pure necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power positions. The specialist sees a pure Rope (coordination benefit — their scarcity is properly rewarded). The enterprise leadership sees Tangled Rope (both coordination and extraction benefit). The labor movement sees Scaffold (temporary problem with educational sunset). The credentialing system sees Piton (degraded ritual maintained through inertia). The factory manager sees Snare as secondary victim. The worker sees maximum extraction (Snare). The civilizational observer risks seeing Mountain (immutable information scarcity) but the structural data reveals a false summit — artificial credentialing gatekeeping amplifies natural knowledge scarcity. The constraint cannot be resolved from any single perspective; it requires acknowledgment that the valuation asymmetry is neither fully natural nor fully artificial, but contingent on institutional choices about knowledge access.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized knowledge holders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. They experience the constraint as pure coordination benefit. Industrial workers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No alternative market for their expertise without formal credentials. Factory managers: Victim + constrained → d≈0.70, f(d)≈1.08. Secondary victim; constrained by knowledge dependency but complicit in labor suppression. Enterprise leadership: Beneficiary + mobile → d≈0.45, f(d)≈0.42. Intentional maintainers of asymmetry; low effective extraction because they have agency and profit from it. Labor movement: Victim + constrained → d≈0.55, f(d)≈0.74. Organized resistance; see a path forward (educational sunset). Credentialing system: Maintains asymmetry + constrained → d≈0.35. Piton classification comes from theater gate (0.55 ≥ 0.70 threshold would be high piton; 0.55 indicates partial performativity). Analytical observer: d≈0.72, f(d)≈1.15. Natural law perspective leads to false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is a Snare (confirmed by ε=0.58, suppression=0.68, χ=0.67) masked by a false Mountain (natural information scarcity). The resolution mechanism is NOT denying that knowledge scarcity is real (it is), but distinguishing between (1) the natural cost of acquiring expertise and (2) the artificial scarcity created by credentialing gatekeeping and institutional opacity. The labor movement's Scaffold perspective offers a concrete sunset mechanism: credentialing democratization (bootcamps, apprenticeships, open-source contributions gaining market recognition) can reduce the artificial component of scarcity without eliminating legitimate specialist compensation. The enterprise leadership maintains the Snare not because it's inevitable, but because it's profitable — they have chosen to suppress knowledge democratization and maintain labor cost suppression through artificial scarcity. The analytical observer's false summit is the key mandatrophy resolution: the constraint is NOT immutable. Educational policy, transparency requirements, and credentialing reform can reduce suppression from 0.68 to 0.40, shifting the constraint from Snare toward Tangled Rope (legitimate specialist premiums) or Scaffold (temporary transition with sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_vs_codifiable_knowledge,
    'What proportion of the specialist''s expertise is genuinely tacit (impossible to codify without years of experience) versus artificially restricted through credentialing and institutional gatekeeping?',
    'Longitudinal tracking of workers who acquire expertise outside credentialed pathways (bootcamps, apprenticeships, autodidacts); correlation between formal credentials and actual job performance in repair/diagnosis tasks',
    'If mostly codifiable: the valuation asymmetry is artificially maintained by credentialing monopoly (increases ε to 0.70+, Snare confirmed). If mostly tacit: some extraction is justified by real scarcity (decreases ε to 0.35, shifts toward Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_vs_codifiable_knowledge, empirical, 'Proportion of specialist expertise that is genuinely tacit versus artificially restricted').

omega_variable(
    substitution_feasibility,
    'Are there organizational or technological substitutes for individual specialist knowledge (documentation, AI diagnostics, distributed problem-solving) that could reduce dependency and enable worker escape from trapped exit?',
    'Case studies of repair/diagnostics outsourcing; technology adoption curves for knowledge-encoding systems; comparison of enterprises that invested in knowledge democratization versus those that maintained scarcity',
    'If substitutes feasible: suppression can be reduced; constraint becomes Scaffold with real sunset (open-source diagnostics, AI-assisted repair). If infeasible: suppression remains high; Snare classification is structural, not contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_feasibility, empirical, 'Whether organizational or technological substitutes can reduce specialist dependency').

omega_variable(
    knowledge_pricing_market_failure,
    'Does the labor market fail to price specialist knowledge because of persistent information asymmetry about what workers actually know, or because of deliberate institutional suppression (credentialing restrictions, deliberate opacity about compensation)?',
    'Audit studies comparing wage offers for workers with identical actual expertise but different credentials; market wage surveys before/after credentialing requirement changes; historical cases where knowledge became transparent (open-source communities, public documentation)',
    'If market failure is genuine: valuation asymmetry is coordination problem (Rope from worker view). If suppression is deliberate: asymmetry is extraction mechanism (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_pricing_market_failure, empirical, 'Market failure or deliberate institutional suppression in knowledge pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(steinmetz_valuation_asymmetry, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(steinmetz_tr_t0, steinmetz_valuation_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(steinmetz_tr_t25, steinmetz_valuation_asymmetry, theater_ratio, 25, 0.48).
narrative_ontology:measurement(steinmetz_tr_t50, steinmetz_valuation_asymmetry, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(steinmetz_be_t0, steinmetz_valuation_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(steinmetz_be_t25, steinmetz_valuation_asymmetry, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(steinmetz_be_t50, steinmetz_valuation_asymmetry, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(steinmetz_valuation_asymmetry, information_standard).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, credentialing_gatekeeping).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, labor_market_information_asymmetry).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, knowledge_commodification).

% DUAL FORMULATION NOTE:
% The Steinmetz constraint decomposes into three related mechanisms: (1) inherent knowledge scarcity (low ε, Mountain-like), (2) artificial credentialing gatekeeping (moderate ε, Piton-like), and (3) deliberate enterprise suppression of knowledge democratization (high ε, Snare). The unified constraint (ε=0.58) reflects the composite effect. Upstream constraints (knowledge_commodification) establish the framing; downstream constraints (labor_market_information_asymmetry) measure the impact on worker compensation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(steinmetz_valuation_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
