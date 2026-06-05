% ============================================================================
% CONSTRAINT STORY: complexity_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complexity_debt, []).

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
 *   constraint_id: complexity_debt
 *   human_readable: The Cumulative Fragility Surcharge
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   Complexity debt represents the compounding cost of architectural
 *   shortcuts, layered abstractions, and deferred refactoring within
 *   technological and organizational systems. It exhibits the core tension
 *   between short-term coordination benefits (rapid feature delivery, quick
 *   problem-solving) and long-term extraction (escalating maintenance burden,
 *   fragility, reduced adaptability). The constraint operates as a tangled
 *   rope: it enables quick-fix coordination that benefits short-term
 *   stakeholders and vendor ecosystems, while simultaneously extracting from
 *   maintenance engineers, organizational resilience, and long-term user
 *   value. The theater ratio (0.68) reflects that governance mechanisms
 *   (architecture reviews, technical documentation, debt tracking) become
 *   increasingly performative as complexity exceeds organizational capacity
 *   to actually verify system health or enforce refactoring discipline. The
 *   suppression component (0.58) is high because engineering teams face
 *   strong pressure to deliver features over refactoring, exit costs for
 *   adopting new architectures are substantial, and the benefits of
 *   refactoring (reduced future maintenance) are invisible to metrics-driven
 *   organizations. The constraint is not inevitable technical law but a
 *   structural consequence of misaligned incentives, short-term performance
 *   pressure, and vendor ecosystems that profit from customer lock-in.
 *
 * KEY AGENTS:
 *   - Maintenance Engineers: Primary victims (powerless/trapped) — bear cognitive overload and exponential debugging cost; cannot exit without career damage
 *   - Organizational Resilience: Abstract victim (powerless/trapped) — adaptive capacity degrades as complexity exceeds system comprehensibility; cannot be defended through individual action
 *   - Product Teams: Mixed victims/beneficiaries (moderate/constrained) — benefit from rapid feature delivery enabled by quick fixes; victims of architectural constraint that limits future autonomy
 *   - Vendor Ecosystem: Primary beneficiaries (institutional/arbitrage) — lock-in through complexity creates switching costs and sustained revenue; can exit to serve less-complex customers
 *   - Refactoring Initiatives: Organized response (organized/mobile) — systematic programs to modernize architecture; represent genuine sunset pathway if adequately resourced
 *   - Technical Governance: Institutional actor (institutional/arbitrage) — maintains documentation and review rituals that become performative as system complexity diverges from human comprehension capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing organizational short-termism and vendor strategy as inherent to software evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complexity_debt, 0.52).
domain_priors:suppression_score(complexity_debt, 0.58).
domain_priors:theater_ratio(complexity_debt, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complexity_debt, extractiveness, 0.52).
narrative_ontology:constraint_metric(complexity_debt, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(complexity_debt, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complexity_debt, tangled_rope).
narrative_ontology:human_readable(complexity_debt, "The Cumulative Fragility Surcharge").
narrative_ontology:topic_domain(complexity_debt, "technological/organizational").

domain_priors:requires_active_enforcement(complexity_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complexity_debt, short_term_stakeholders).
narrative_ontology:constraint_beneficiary(complexity_debt, vendor_ecosystem).
narrative_ontology:constraint_victim(complexity_debt, system_maintainers).
narrative_ontology:constraint_victim(complexity_debt, downstream_users).
narrative_ontology:constraint_victim(complexity_debt, organizational_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MAINTENANCE ENGINEER (SNARE) — Trapped within layers of legacy code, deprecated dependencies, and patched-over abstractions. Cannot exit without career cost; faces exponential cognitive load and debugging complexity that increases faster than system value. Maximum experienced extraction through forced mastery of non-functional layers that exist only for historical compatibility.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZATIONAL RESILIENCE (SNARE) — Abstract collective property that bears the full fragility cost. Cannot articulate demands, cannot exit, cannot organize. Systems optimized for short-term throughput accumulate dependencies that create catastrophic failure modes when complexity exceeds team capacity or when requirements change. The organization's adaptive capacity degrades as complexity debt compounds.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PRODUCT TEAM (TANGLED ROPE) — Benefits from rapid feature delivery and quick-fix solutions (coordination function: enables short-term value capture). Also victims of the constraint because technical debt compounds into architectural risk, reducing their autonomy over future features. Can partially exit via microservices or modularization but faces resource constraints and organizational pressure to prioritize new features over refactoring.
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: VENDOR ECOSYSTEM (ROPE) — Benefits from system complexity that locks in customers and creates dependency. Each layer of abstraction, each vendor API integration, each proprietary extension increases switching costs. Experiences the constraint as pure coordination benefit — the more entangled the system, the more valuable the integration points they control. Can exit at will to serve next-generation customers; uses fragility as moat.
constraint_indexing:constraint_classification(complexity_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFACTORING INITIATIVES (SCAFFOLD) — Organized response to complexity debt via systematic modernization programs, microservices migration, and modular redesign. Sees the constraint as temporary coordination failure solvable through engineering investment. Has sunset logic: as legacy systems are incrementally replaced and team practices mature (architectural discipline, automated testing, dependency management), the overhead ratio declines. Estimated 5-10 year transition horizon for established systems.
constraint_indexing:constraint_classification(complexity_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DOCUMENTATION THEATER (PITON) — Architecture review boards, technical documentation, and governance processes become performative as system complexity exceeds their capture capacity. Team members maintain documentation that quickly becomes obsolete; code reviews verify syntax but not architectural coherence. The governance ritual persists through institutional inertia despite low functional verification of system health. Theater ratio high; actual constraint enforcement low.
constraint_indexing:constraint_classification(complexity_debt, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN FRAMING) — From a civilizational/universal view, some technical debt is inherent to software evolution: all systems accumulate historical decisions, and the cost of maintaining backward compatibility is a structural property of long-lived systems. This perspective risks naturalizing what is contingent institutional choice (short-termism, vendor lock-in, insufficient refactoring investment) as immutable law. The engine's false summit detector will flag this as naturalization of avoidable complexity accumulation.
constraint_indexing:constraint_classification(complexity_debt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complexity_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complexity_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complexity_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(complexity_debt, TR),
    TR >= 0.70.

:- end_tests(complexity_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from maintenance engineers and organizational resilience through enforced mastery of non-functional complexity. However, extraction is not maximal because some of the complexity is legitimate architectural depth (genuine value creation requires some abstraction layers), not pure rent-seeking. The extraction coefficient reflects the excess complexity above what is functionally necessary. Suppression (0.58): Moderate-high. Teams face significant barriers to refactoring: capital constraints (refactoring produces no new features), organizational metrics that reward velocity over quality, vendor lock-in making migration costly, and coordination problems (partial refactors create worse fragility). But suppression is not total — some organizations do execute major modernizations and some teams do maintain architectural discipline. Theater ratio (0.68): High and increasing. Technical governance becomes performative as complexity diverges from human comprehension. Code reviews cannot verify architectural coherence; architecture reviews cannot track emergent fragility from layer interactions; documentation becomes obsolete faster than it can be maintained. The theater ratio has increased from 0.35 to 0.68 over the interval because governance overhead grows exponentially with complexity.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between short-term optimization (product team, vendor ecosystem) and long-term fragility (maintenance engineers, organizational resilience). Product teams experience quick fixes as coordination success (Rope/Scaffold); maintenance engineers experience the same decisions as extraction burden (Snare). Vendors profit from the lock-in that emerges from layered complexity; customers bear the cost of vendor switching as a hidden extraction mechanism. The theater gap is critical: governance rituals are experienced as meaningful control by leadership but as performative overhead by engineering teams. The false summit gap is the most revealing: the mountain perspective naturalizes organizational short-termism ('technical debt is inherent to software') while structural data shows it is institutional choice (refactoring initiatives prove modernization is tractable; different organizations with different incentive structures accumulate debt at different rates).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position in the extraction pipeline. Short-term stakeholders and vendors occupy low-d positions (beneficiary + arbitrage exit) — they experience negative or minimal effective extraction. Maintenance engineers and organizational resilience occupy high-d positions (victim + trapped exit) — they experience maximum f(d) conversion of base extraction into lived constraint. Product teams occupy intermediate-d positions (mixed beneficiary/victim + constrained exit) — they experience moderate effective extraction. Refactoring initiatives occupy low-d positions despite being victims in some sense (organized + mobile exit) — they have agency to shape the constraint through engineering practice changes. The piton perspective (institutional/arbitrage) occupies low-d but sees theater as the revealing metric because their governance role is symbolic: they maintain the ritual without capacity to enforce actual system health standards.
 *
 * MANDATROPHY ANALYSIS:
 *   COMPLEX CASE: Extractiveness (0.52) falls in the range where mandatrophy detection is essential but not automatic (0.46 < ε < 0.70). The constraint is correctly classified as Tangled Rope at the analytical level — it has genuine coordination benefits (quick fixes enable rapid feature delivery, solving legitimate speed/responsiveness problems) and genuine asymmetric extraction (maintenance cost, fragility, vendor lock-in). The mandatrophy false positives to check: (1) Is this really extraction or just the cost of coordination? Answer: Both. Coordination at low cost would not require quick fixes; the extraction lies in the speed premium extracted from future maintainability. (2) Is this really coordination or just entrenchment? Answer: The product team experiences coordination benefit (faster delivery) even though it creates future extraction. (3) Could this be a Snare misclassified as Tangled Rope? Answer: No — the constraint has genuine beneficiaries (vendors, product teams in short term) and genuine coordination function (enables rapid feature delivery). The Snare perspectives (maintenance engineer, organizational resilience) see high extraction precisely because they are outside the beneficiary coalition. The scaffold perspective confirms that the constraint is not inherent: refactoring programs and modernization initiatives prove the extraction is decoupling-able. STATUS: Mandatrophy is NOT fully resolved. The constraint requires explicit analysis of the coordination vs extraction boundary (rapid feature delivery coordination vs technical debt extraction) to prevent mislabeling rapid-development acceleration as pure coordination. The omegas (complexity threshold, refactoring ROI, vendor intentionality, short-termism driver) are designed to disambiguate this boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_threshold_phase_transition,
    'At what complexity metric (lines of code, cyclomatic complexity, dependency depth) does maintenance cost undergo a phase transition from linear growth to exponential growth?',
    'Empirical study across 20+ codebases: measure complexity metrics, maintenance time logs, and bug density; identify inflection point where marginal maintenance cost diverges from linear trend',
    'If threshold is well-defined and organization-independent: complexity debt is closer to natural law (mountain). If threshold is highly context-dependent (team size, domain expertise, architecture discipline): complexity debt is organizational choice (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_threshold_phase_transition, empirical, 'Complexity threshold for exponential maintenance cost growth').

omega_variable(
    refactoring_roi_visibility,
    'Can organizations reliably measure the return on investment of refactoring efforts in terms of reduced maintenance overhead and improved feature delivery velocity?',
    'Longitudinal study of teams conducting major refactors; measurement of maintenance time allocation, feature development time, and defect rates pre- and post-refactor over 12+ month periods',
    'If ROI is measurable: refactoring becomes budgetable, scaffold sunset is real structural feature. If ROI remains opaque: complexity debt persists because costs are invisible; snare and piton perspectives dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refactoring_roi_visibility, empirical, 'Measurability of refactoring ROI in organizational practice').

omega_variable(
    vendor_lock_in_intentionality,
    'To what degree do vendor ecosystems deliberately architect for complexity and dependency as a business strategy versus accepting it as an inevitable byproduct?',
    'Strategic analysis: interview vendor product teams; analyze API design decisions against modularity principles; compare vendor incentives (lock-in value) with public commitments to interoperability',
    'If intentional: vendor extraction is the primary beneficiary of complexity debt (rope perspective valid). If accidental: complexity is emergent property and vendors are secondary beneficiaries (tangled_rope perspective valid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_lock_in_intentionality, conceptual, 'Whether vendor lock-in architecture is deliberate strategy or emergent byproduct').

omega_variable(
    organizational_short_termism_driver,
    'Is organizational short-termism that prioritizes quick fixes over refactoring driven by capital markets pressure, leadership incentive misalignment, or structural inability to value long-term resilience?',
    'Comparative study of organizations with different ownership structures (public vs private vs cooperative), leadership compensation models, and strategic planning horizons; measurement of technical debt accumulation rates against ownership structure',
    'If markets drive it: complexity debt is system-level extraction (snare becomes civilizational). If organizational, refactoring is tractable through governance change (scaffold sunset is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_short_termism_driver, conceptual, 'Root cause of organizational short-termism in technical decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complexity_debt, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compdebt_tr_t0, complexity_debt, theater_ratio, 0, 0.35).
narrative_ontology:measurement(compdebt_tr_t3, complexity_debt, theater_ratio, 3, 0.52).
narrative_ontology:measurement(compdebt_tr_t7, complexity_debt, theater_ratio, 7, 0.68).

% Extraction over time
narrative_ontology:measurement(compdebt_be_t0, complexity_debt, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(compdebt_be_t3, complexity_debt, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(compdebt_be_t7, complexity_debt, base_extractiveness, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complexity_debt, resource_allocation).
narrative_ontology:affects_constraint(complexity_debt, organizational_fragility_cascade).
narrative_ontology:affects_constraint(complexity_debt, vendor_ecosystem_lock_in).
narrative_ontology:affects_constraint(complexity_debt, engineering_cognitive_load).

% DUAL FORMULATION NOTE:
% Complexity debt decomposes into three structurally distinct constraints: (1) technical_debt_accumulation (ε ≈ 0.35, Mountain-ish) — inherent cost of feature layering; (2) refactoring_investment_shortage (ε ≈ 0.55, Snare/Tangled Rope) — organizational failure to allocate capital to modernization; (3) vendor_complexity_lock_in (ε ≈ 0.48, Tangled Rope) — deliberate ecosystem strategy. This story models the aggregate constraint across all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(complexity_debt, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
