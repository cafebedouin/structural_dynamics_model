% ============================================================================
% CONSTRAINT STORY: platform_algorithmic_management
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_algorithmic_management, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: platform_algorithmic_management
 *   human_readable: Platform Algorithmic Management of Labor
 *   domain: political_economy/labor/digital_platforms
 *
 * SUMMARY:
 *   Platform algorithmic management represents a structural shift in labor
 *   control: the replacement of direct managerial authority with encoded
 *   decision criteria embedded in opaque software systems. The constraint
 *   coordinates task allocation, workforce scheduling, and quality assurance
 *   across distributed networks while simultaneously extracting labor value
 *   through asymmetric information control, wage suppression, and
 *   deactivation risk. This constraint exhibits six DR types from different
 *   perspectives, diagnosing the divergence between coordination and control
 *   functions. The same algorithmic system appears as pure extraction to
 *   trapped workers (Snare), mixed coordination-extraction to labor standard
 *   enforcers (Tangled Rope), pure coordination to platform operators (Rope),
 *   a temporary policy problem with a regulatory exit path (Scaffold), a
 *   degraded HR function maintained through compliance theater (Piton), and
 *   an inherent tension of all management systems (Mountain). The theater
 *   ratio (0.65) reflects that platform algorithmic governance includes
 *   significant performative elements: published fairness policies,
 *   deactivation appeal processes, and earnings transparency features that
 *   are framed as worker protections but function as legitimation narratives
 *   for asymmetric control. The extractiveness trajectory (0.35→0.58 over the
 *   interval) documents the accumulation of extraction mechanisms: from
 *   initial task-matching algorithms to present-day systems incorporating
 *   dynamic pricing, predictive deactivation, psychological nudging, and data
 *   commodification. The suppression level (0.68) reflects worker inability
 *   to negotiate, collectively organize, or exit without economic hardship,
 *   compounded by information asymmetry and algorithmic opacity.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures coordination benefits and extracts labor value through wage suppression and data exploitation; can arbitrage between regulatory regimes
 *   - Gig Workers: Primary victim (powerless/trapped) — lack alternative income sources, experience algorithmic control without transparency or negotiation capacity; cannot exit without economic devastation
 *   - Labor Standard Enforcement Regime: Secondary victim (moderate/constrained) — faces verification challenges, opacity barriers, and effective circumvention of traditional labor protections; requires coordination with platforms while extraction undermines enforcement
 *   - Consumer Ecosystem: Secondary beneficiary (moderate/constrained) — experiences coordination benefit (service availability, convenience) but also experiences extraction (dynamic pricing, data harvesting) with moderate suppression (network effects, switching costs)
 *   - Regulatory Coalition: Organized actor (organized/constrained) — labor advocates, worker centers, progressive regulators building policy alternatives (portable benefits, sectoral bargaining, transparency mandates) with perceived sunset timeline
 *   - Human Resources Function: Institutional actor (institutional/arbitrage) — persists as compliance theater while algorithmic management executes actual control; degraded function maintained through legitimacy narratives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing control asymmetry as inherent management necessity rather than contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_algorithmic_management, 0.58).
domain_priors:suppression_score(platform_algorithmic_management, 0.68).
domain_priors:theater_ratio(platform_algorithmic_management, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_algorithmic_management, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_algorithmic_management, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_algorithmic_management, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_algorithmic_management, tangled_rope).
narrative_ontology:human_readable(platform_algorithmic_management, "Platform Algorithmic Management of Labor").
narrative_ontology:topic_domain(platform_algorithmic_management, "political_economy/labor/digital_platforms").

domain_priors:requires_active_enforcement(platform_algorithmic_management).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_algorithmic_management, platform_operator).
narrative_ontology:constraint_beneficiary(platform_algorithmic_management, consumer_convenience).
narrative_ontology:constraint_victim(platform_algorithmic_management, gig_workers).
narrative_ontology:constraint_victim(platform_algorithmic_management, labor_standard_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Trapped by lack of alternative income sources and platform data lock-in. Algorithm controls task assignment, earnings visibility, deactivation decisions. No transparency into ranking criteria. Maximum suppression — worker cannot negotiate, organize, or exit without economic devastation. Pure extraction from the worker's perspective: no coordination benefit, only control mechanisms dressed as efficiency optimization.
constraint_indexing:constraint_classification(platform_algorithmic_management, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR STANDARDS REGIME (TANGLED ROPE) — Faces genuine coordination problem: algorithmic management creates verification challenges for traditional labor law (independent contractor vs employee, wage theft detection, working hours). But also faces extraction: platform opacity prevents inspection and enforcement. The regime needs coordination with platforms (data access, transparency) AND extraction is happening (standards being undermined). Active enforcement required but routinely subverted.
constraint_indexing:constraint_classification(platform_algorithmic_management, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — From the platform's view, the algorithm is a pure coordination mechanism: it solves the matching problem (who does which task), optimizes fleet efficiency, and reduces transaction costs. The platform experiences the constraint as coordination with minimal extraction relative to their benefits. They can exit or arbitrage by adjusting algorithm parameters, switching policy regimes, or relocating operations.
constraint_indexing:constraint_classification(platform_algorithmic_management, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized labor, worker centers, and progressive regulators see algorithmic management as a temporary coordination failure with a policy sunset: portable benefits, algorithmic transparency mandates, and sectoral bargaining are creating exit paths. The constraint has high suppression currently but is perceived as transitional — a decade-scale window before new labor frameworks (gig worker classification, algorithmic audit rights) mature. The coalition sees organized agency and a policy exit path.
constraint_indexing:constraint_classification(platform_algorithmic_management, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: HUMAN RESOURCES APPARATUS (PITON) — Traditional HR function is increasingly vestigial in platform firms. Algorithmic management replaces performance review, discipline, and promotion systems. HR persists as compliance theater (grievance procedures, anti-discrimination documentation) but has lost substantive control over labor conditions. Theater ratio (0.65+) reflects that many algorithmic management 'policies' are performative — they exist to present fairness narratives while the algorithm executes asymmetric control.
constraint_indexing:constraint_classification(platform_algorithmic_management, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSUMER ECOSYSTEM (TANGLED ROPE) — Experiences genuine coordination benefit (algorithmic matching ensures service availability, predictable pricing, convenience). But also experiences extraction hidden within pricing: consumer surplus is extracted through dynamic pricing, demand surge charges, and data harvesting. Suppression is moderate — consumers can switch platforms but face network effects and data stickiness. The constraint coordinates service delivery while extracting economic value.
constraint_indexing:constraint_classification(platform_algorithmic_management, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the tension between coordination and control is inherent to all management systems: you cannot have task allocation without some agent deciding allocation criteria. Algorithmic management just makes this transparent (or opaque, depending on design). This perspective risks naturalizing what is actually a contingent design choice — the 'extractiveness' comes from transparency absence and one-way information flow, not from the mere existence of management coordination.
constraint_indexing:constraint_classification(platform_algorithmic_management, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_algorithmic_management_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_algorithmic_management, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_algorithmic_management, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_algorithmic_management, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_algorithmic_management, TR),
    TR >= 0.70.

:- end_tests(platform_algorithmic_management_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The platform captures significant economic value through wage suppression (algorithmic task routing directs workers toward lower-paid tasks), dynamic pricing (surge pricing captures demand spikes), and data commodification (worker location, behavioral patterns sold to third parties). But extraction is not maximal (Snare threshold ≥ 0.66) because the platform genuinely solves the matching problem and workers do retain some agency in task selection. The trajectory from 0.35 to 0.58 documents the accumulation of extraction mechanisms beyond core coordination: initial algorithms focused on matching efficiency; later versions added predictive deactivation (removing high-cost workers), psychological nudging (task acceptance patterns), and surge pricing. Suppression (0.68): Moderately high. Multiple barriers prevent exit and negotiation: (1) structural—lack of alternative income sources, network effects making platform switching costly, data lock-in; (2) informational—algorithmic opacity prevents workers from understanding ranking criteria or challenging decisions; (3) legal—independent contractor classification prevents collective bargaining; (4) organizational—platform investment in union-busting and worker surveillance. Theater ratio (0.65): Moderately high. Platform governance includes substantial performative elements: published algorithmic fairness commitments that lack enforcement mechanisms; deactivation appeal processes that rarely overturn platform decisions; earnings transparency features that obscure hidden costs (platform fees, time required for acceptance/travel). The theater ratio has increased over the interval as regulatory pressure mounted — platforms added more legitimacy narratives without substantively changing control mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The worker (Snare) and platform operator (Rope) perceive fundamentally opposite constraint types from identical structural data. This gap reveals the extraction mechanism: the algorithm that the platform experiences as pure coordination (matching efficiency, scale optimization) is experienced by the worker as pure extraction (opaque control, wage suppression, deactivation risk). The labor standard enforcer (Tangled Rope) perceives both functions operating simultaneously — genuine coordination (the system does allocate tasks) AND genuine extraction (the system does suppress standards). The regulatory coalition (Scaffold) perceives the constraint as temporary because they see policy alternatives (transparency mandates, sectoral bargaining, portable benefits) maturing on a 10-20 year horizon. The HR function (Piton) perceives the constraint as degraded — algorithmic management has replaced HR's traditional role, leaving only compliance theater. The analytical observer risks perceiving a mountain (inherent management necessity) when the structural data reveals a design choice: opacity and control asymmetry are technically optional, not naturally necessary features of coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. Workers classified as (powerless/trapped) experience maximum d (≈0.95), producing maximum f(d) (≈1.42) and thus maximum experienced extractiveness χ. The platform (institutional/arbitrage) experiences minimum d (≈0.05), producing negative f(d) (≈-0.12), making their net χ negative—they see the constraint as beneficial coordination. Labor standard enforcers (moderate/constrained) experience medium-high d (≈0.65), producing f(d) (≈1.00) and moderate χ—they perceive mixed coordination and extraction. Consumers (moderate/constrained) experience medium d (≈0.55-0.60), producing f(d) (≈0.70-0.85) and moderate χ offset by coordination benefits. The regulatory coalition (organized/constrained) experiences lower d (≈0.40-0.45) because their organized power raises their structural capacity despite constrained exit—they perceive agency and a policy path. The scope modifier σ(S) = 1.2 for global scope, amplifying effective χ by 20% — the constraint's global reach means that platform arbitrage between regulatory regimes is feasible, forcing jurisdictions to choose between enforcement and service availability.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Platform algorithmic management resolves the mandatrophy by showing that the constraint's type depends entirely on the observer's structural position and exit capacity. The worker sees Snare because they are trapped with no alternatives. The platform sees Rope because they can exit or arbitrage regulatory constraints. The labor enforcer sees Tangled Rope because they experience both coordination necessity and extraction undermining. The regulatory coalition sees Scaffold because they perceive policy alternatives maturing. The HR function sees Piton because their traditional role has atrophied. There is no single 'correct' classification — the presheaf over the observation site IS the answer. The mandatrophy is resolved by recognizing that the analytical observer's mountain (inherent management necessity) is a false summit: it naturalizes the contingent choice to concentrate information and control asymmetrically. Alternative designs exist (transparent algorithms, worker data rights, collective bargaining) that would preserve coordination while reducing extraction. The mountain classification indicates failure to recognize design contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_threshold,
    'What level of algorithmic transparency would eliminate the extraction mechanism without eliminating the coordination function?',
    'Comparative analysis of platforms with high transparency (publishing ranking factors, deactivation criteria, earnings algorithms) vs opaque platforms; measurement of worker organizing capacity and labor standard compliance under different transparency regimes',
    'If transparency sufficient: reclassify from Snare to Rope or Scaffold (extraction drops below coordination benefit). If transparency insufficient: extraction persists despite disclosure (information asymmetry structural rather than epistemic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_threshold, empirical, 'Whether algorithmic transparency eliminates extraction or merely legitimizes it').

omega_variable(
    worker_collective_power_threshold,
    'Can gig workers organize at sufficient scale to convert from trapped to constrained exit options, changing the classification from Snare to Tangled Rope?',
    'Longitudinal tracking of organizing attempts; comparison of labor outcomes in jurisdictions with collective bargaining rights vs independent contractor jurisdictions; measurement of worker coalition capacity and coordination infrastructure',
    'If collective organizing reaches critical mass: worker perspective shifts from trapped to constrained, χ drops, classification becomes Tangled Rope. If organizational barriers prove insurmountable: Snare classification becomes stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_collective_power_threshold, empirical, 'Whether worker collective power can convert trapped exit to constrained exit').

omega_variable(
    platform_regulatory_competition,
    'Do platforms arbitrage between regulatory regimes (moving operations to avoid labor standards), or are network effects and consumer expectations creating convergent pressure toward higher standards?',
    'Geographic analysis of platform service areas and withdrawal; comparison of labor standards across jurisdictions with different regulatory frameworks; tracking of platform policy harmonization across regions',
    'If arbitrage dominates: Scaffold sunset is illusory, constraint remains Snare long-term. If regulatory harmonization occurs: Scaffold perspective confirmed, policy coalition has real structural path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_regulatory_competition, empirical, 'Whether platforms arbitrage between regulatory regimes or face convergent standards pressure').

omega_variable(
    algorithmic_coordination_necessity,
    'Is the current level of algorithmic opacity necessary for coordination efficiency, or is it technically optional and maintained for control purposes?',
    'Engineering analysis of algorithmic matching efficiency under different transparency constraints; comparison of worker productivity and platform efficiency in high-transparency vs opaque platforms; technical feasibility assessment of transparent decision criteria that preserve coordination function',
    'If opacity technically necessary: mountain perspective gains credibility (trade-off between coordination and transparency is inherent). If opacity optional: confirms extraction is a design choice, not a natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_coordination_necessity, empirical, 'Whether algorithmic opacity is technically necessary for coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_algorithmic_management, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pam_tr_t0, platform_algorithmic_management, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pam_tr_t3, platform_algorithmic_management, theater_ratio, 3, 0.52).
narrative_ontology:measurement(pam_tr_t6, platform_algorithmic_management, theater_ratio, 6, 0.62).
narrative_ontology:measurement(pam_tr_t10, platform_algorithmic_management, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pam_be_t0, platform_algorithmic_management, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pam_be_t3, platform_algorithmic_management, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(pam_be_t6, platform_algorithmic_management, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(pam_be_t10, platform_algorithmic_management, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_algorithmic_management, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_algorithmic_management, 0.18).
narrative_ontology:affects_constraint(platform_algorithmic_management, gig_economy_classification).
narrative_ontology:affects_constraint(platform_algorithmic_management, algorithmic_transparency_mandate).
narrative_ontology:affects_constraint(platform_algorithmic_management, labor_standard_evasion).

% DUAL FORMULATION NOTE:
% Platform algorithmic management decomposes into multiple structurally distinct constraints: (1) task allocation coordination (ε≈0.15, Rope) — the matching function genuine; (2) wage suppression mechanism (ε≈0.62, Snare) — extraction through opaque routing; (3) organizational opacity (ε≈0.45, Tangled Rope) — information asymmetry used for control. This story integrates all three as a unified constraint family with heterogeneous extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_algorithmic_management, institutional, 0.06).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
