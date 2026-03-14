% ============================================================================
% CONSTRAINT STORY: innovation_diffusion_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovation_diffusion_barriers, []).

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
 *   constraint_id: innovation_diffusion_barriers
 *   human_readable: Innovation Diffusion Barriers
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Innovation diffusion barriers represent the structural tension between
 *   rewarding innovation investment (through intellectual property protection
 *   and exclusivity periods) and enabling broad adoption of new technologies.
 *   This constraint manifests differently depending on the observer's
 *   structural position: for patent holders, it is a legitimate coordination
 *   mechanism enabling R&D investment; for potential adopters, it is pure
 *   extraction through licensing fees and regulatory gatekeeping; for
 *   open-source communities, it is a temporary problem being displaced by
 *   alternative coordination mechanisms. The constraint's extractiveness has
 *   increased over the measurement interval (0.35 to 0.52) as patent thickets
 *   have accumulated and regulatory complexity has grown, while theater ratio
 *   has risen (0.48 to 0.65) as patent prosecution increasingly functions as
 *   performative compliance rather than effective rights definition.
 *
 * KEY AGENTS:
 *   - Patent Holders: Primary beneficiary (institutional/arbitrage) — capture licensing revenue and market control during exclusivity windows
 *   - Potential Adopters: Primary victim (powerless/trapped) — face licensing costs, information barriers, and regulatory compliance requirements with no exit
 *   - Downstream Innovators: Secondary victim (moderate/constrained) — need access to foundational patents for follow-on innovation but face freedom-to-operate restrictions and patent thickets
 *   - Consumer Welfare: Abstract victim (powerless/trapped) — delayed innovation adoption, higher prices during monopoly periods
 *   - Open Innovation Communities: Organized alternatives (organized/mobile) — building decentralized innovation coordination pathways (open-source, Creative Commons, open-access) with exit options
 *   - Patent Prosecution System: Institutional actor (institutional/arbitrage) — maintains formal patent prosecution machinery despite low functionality; benefits from institutional inertia
 *   - Regulatory Gatekeepers: Secondary beneficiary (institutional/arbitrage) — extract through compliance requirements and licensing approval processes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovation_diffusion_barriers, 0.52).
domain_priors:suppression_score(innovation_diffusion_barriers, 0.58).
domain_priors:theater_ratio(innovation_diffusion_barriers, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovation_diffusion_barriers, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovation_diffusion_barriers, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(innovation_diffusion_barriers, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovation_diffusion_barriers, tangled_rope).
narrative_ontology:human_readable(innovation_diffusion_barriers, "Innovation Diffusion Barriers").
narrative_ontology:topic_domain(innovation_diffusion_barriers, "economic/technological/social").

domain_priors:requires_active_enforcement(innovation_diffusion_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovation_diffusion_barriers, incumbent_firms).
narrative_ontology:constraint_beneficiary(innovation_diffusion_barriers, patent_holders).
narrative_ontology:constraint_beneficiary(innovation_diffusion_barriers, regulatory_gatekeepers).
narrative_ontology:constraint_victim(innovation_diffusion_barriers, potential_adopters).
narrative_ontology:constraint_victim(innovation_diffusion_barriers, downstream_innovators).
narrative_ontology:constraint_victim(innovation_diffusion_barriers, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL ADOPTER (SNARE) — Individuals and small organizations without capital or network access to innovation face maximal extraction through licensing fees, patent royalties, and regulatory compliance costs. Trapped by resource barriers and information asymmetry. Cannot exit the constraint without abandoning adoption entirely.
constraint_indexing:constraint_classification(innovation_diffusion_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM INNOVATOR (TANGLED ROPE) — Medium-sized firms building on prior innovations face genuine coordination need (accessing foundational innovations accelerates development) alongside asymmetric extraction (licensing costs, patent thickets, freedom-to-operate restrictions). Can exit via alternative technical approaches but at high cost. Constrained by both the coordination benefits and extraction barriers.
constraint_indexing:constraint_classification(innovation_diffusion_barriers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATENT HOLDER (ROPE) — Institutional actors holding foundational patents experience the diffusion constraint as pure coordination: licensing enables follow-on innovation while securing return on R&D investment. Low experienced extraction — the arrangement solves the legitimate problem of rewarding innovation investment. Net beneficiary with significant exit options (can license broadly or narrowly).
constraint_indexing:constraint_classification(innovation_diffusion_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN INNOVATION MOVEMENT (SCAFFOLD) — Organized coalitions (open-source communities, Creative Commons, open-access mandates) have built alternative diffusion pathways with built-in sunset logic. As these ecosystems mature, their lower-extraction mechanisms become viable alternatives to traditional patent-based diffusion. Sees the constraint as temporary — not because patents will disappear, but because decentralized innovation coordination reduces their practical necessity. High agency and clear exit path.
constraint_indexing:constraint_classification(innovation_diffusion_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT PROSECUTION SYSTEM (PITON) — The formal patent system increasingly operates as theater: examining thousands of patents with overlapping claims, maintaining prosecution machinery that produces patent thickets rather than clear rights, and consuming enormous institutional resources while failing at its stated function of enabling rather than blocking diffusion. Maintained through institutional inertia despite low functional effectiveness. Theater ratio reflects that patent prosecution is largely performative compliance.
constraint_indexing:constraint_classification(innovation_diffusion_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, some innovation diffusion lag is inherent to technological change: complex innovations take time to understand, adapt, and integrate. The gap between invention and widespread adoption reflects structural limits on human learning capacity and capital constraints, not a contingent institutional arrangement. This perspective risks naturalizing what is actually policy-contingent through patent policy and regulatory design choices.
constraint_indexing:constraint_classification(innovation_diffusion_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovation_diffusion_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovation_diffusion_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovation_diffusion_barriers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovation_diffusion_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(innovation_diffusion_barriers, TR),
    TR >= 0.70.

:- end_tests(innovation_diffusion_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint genuinely solves the problem of rewarding R&D investment, so not all measured extraction represents pure rent-seeking. However, much of the current extractiveness reflects accumulation beyond what is necessary for innovation incentives — patent thickets, evergreening strategies, cross-licensing blocking, and regulatory gatekeeping add extraction layers without corresponding innovation benefits. The rising trajectory (0.35→0.52 over 20 years) reflects increasing sophistication in barrier maintenance. Suppression (0.58): Moderate-high. Significant barriers include licensing fees, patent prosecution costs, regulatory compliance requirements, information asymmetries favoring incumbents, and capability gaps in peripheral regions. However, suppression is not absolute — some actors escape through independent invention, open-source alternatives, and geographic arbitrage. Theater ratio (0.65): High and rising. Patent prosecution has become increasingly performative: the system examines thousands of patents with overlapping claims, produces thickets that block rather than enable, and consumes enormous institutional resources while failing at effective rights definition. The rise in theater ratio reflects the gap between the system's stated function (enabling innovation) and its actual operation (maintaining barriers for incumbents).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival disparity. The patent holder sees rope (legitimate coordination enabling investment recovery). The trapped potential adopter sees snare (pure extraction with no exit). The constrained downstream innovator sees tangled rope (genuine coordination function alongside asymmetric extraction). The open innovation coalition sees scaffold (temporary problem being displaced). The patent system sees piton (degraded ritual maintained through inertia). The analytical observer risks seeing mountain (diffusion lag as inherent to innovation) but the structural data reveals this as a false summit: the diffusion timeline is policy-contingent. Crucially, the same licensing mechanism that coordinates investment (rope from beneficiary perspective) simultaneously extracts from victims (snare from trapped perspective). The perspectival gap is not measurement error — it reflects the constraint's actual dual function as both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by structural position. Patent holders with arbitrage options experience low d (they benefit from the constraint) → low f(d) → negative effective extraction. Potential adopters who are trapped experience high d (they bear costs with no exit) → high f(d) → high effective extraction. Downstream innovators who are constrained experience moderate d (they bear costs but can exit at expense) → moderate f(d) → moderate effective extraction. Organized actors with mobile exit options experience low-moderate d (they can exit to alternative pathways) → lower f(d) → lower effective extraction despite similar base extractiveness. The constraint's effective extractiveness (χ) is dramatically different across agents: near zero for patent holders, very high for trapped adopters, moderate for constrained innovators. This directionality differentiation is the primary source of perspectival type diversity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that innovation diffusion barriers serve genuine coordination (rewarding investment) while simultaneously enabling extraction (monopoly rents, gatekeeping). The classification varies by perspective not because the base structural properties are ambiguous, but because the constraint genuinely operates as both rope and snare depending on position. The patent holder's rope experience is real — patents do coordinate investment. The adopter's snare experience is also real — licensing costs do extract. The mandatrophy is resolved by noting that a single constraint can be functionally dual: the same mechanism (exclusivity) that coordinates investment also extracts from adopters. This is not a measurement ambiguity but a structural property of how intellectual property constraints work. The piton classification of the patent prosecution system is separate: the formal prosecution machinery maintains the barriers through increasingly performative mechanisms, independent of whether the underlying IP coordination function is legitimate. The scaffold perspective is crucial for mandatrophy resolution: it shows that the constraint is not immutable (the mountain false summit) but subject to displacement by alternative coordination mechanisms with lower extraction. The existence of viable open-source innovation pathways demonstrates that diffusion acceleration is possible without patent-based IP control, making the patent barrier contingent rather than natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_diffusion_lag_vs_extraction,
    'What threshold distinguishes legitimate adoption lag (time required for learning, adaptation, capital formation) from extractive barriers maintained by incumbent actors?',
    'Historical comparison: diffusion timelines in high-patent-enforcement regimes vs low-enforcement regimes for equivalent innovations; correlation between patent strength and diffusion speed',
    'If thresholds are short (1-3 years): patent-based extraction is dominant. If long (10+ years): structural learning capacity dominates and extraction is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_diffusion_lag_vs_extraction, empirical, 'Threshold distinguishing diffusion lag from extraction').

omega_variable(
    open_innovation_substitution_timing,
    'At what maturity level of open-source and decentralized innovation ecosystems does patent-based diffusion control effectively transfer to distributed alternatives?',
    'Domain-by-domain analysis of adoption rates in patent-heavy vs open-source-heavy sectors; measurement of innovation velocity in each pathway; identification of inflection points where open alternatives become dominant',
    'If substitution occurs on 5-10 year horizon: scaffold sunset is realistic and constraint will naturally degrade. If substitution stalls: barrier persists and remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_innovation_substitution_timing, empirical, 'Timing of open-innovation substitution for patent-based diffusion').

omega_variable(
    patent_thicket_intentionality,
    'To what degree are patent thickets (overlapping, semi-blocking claims) an unintended consequence of increasing technical complexity vs an intentional strategy by incumbents to maintain control?',
    'Analysis of patent prosecution strategies: citation patterns, claim breadth evolution, defensive patenting vs offensive licensing; comparison of thicket density in concentrated vs competitive industries',
    'If primarily unintended: diffusion barriers are a side effect, not extraction mechanism, and piton classification is accurate. If intentional: barriers are maintained through deliberate strategy and represent active snare behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_thicket_intentionality, empirical, 'Whether patent thickets are intentional barrier strategy').

omega_variable(
    capability_gaps_vs_access_barriers,
    'For potential adopters in low-income or peripheral regions, what fraction of diffusion lag is due to technical/knowledge capability gaps vs access barriers (financing, licensing cost, regulatory requirement)?',
    'Comparative analysis: adoption rates when access barriers are reduced (subsidized licensing, technology transfer programs) vs when capability is invested; proxy measurement via regions with strong capability but weak access vs weak capability but strong access',
    'If capability gaps dominate: removing access barriers has limited effect and extraction is secondary. If access barriers dominate: licensing and regulatory reform would unlock rapid diffusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_gaps_vs_access_barriers, empirical, 'Relative impact of capability gaps vs access barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovation_diffusion_barriers, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idb_tr_t0, innovation_diffusion_barriers, theater_ratio, 0, 0.48).
narrative_ontology:measurement(idb_tr_t10, innovation_diffusion_barriers, theater_ratio, 10, 0.58).
narrative_ontology:measurement(idb_tr_t20, innovation_diffusion_barriers, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(idb_be_t0, innovation_diffusion_barriers, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(idb_be_t10, innovation_diffusion_barriers, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(idb_be_t20, innovation_diffusion_barriers, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovation_diffusion_barriers, resource_allocation).
narrative_ontology:affects_constraint(innovation_diffusion_barriers, patent_thicket_accumulation).
narrative_ontology:affects_constraint(innovation_diffusion_barriers, regulatory_compliance_costs).
narrative_ontology:affects_constraint(innovation_diffusion_barriers, knowledge_transfer_barriers).

% DUAL FORMULATION NOTE:
% Innovation diffusion barriers decompose into three structurally distinct constraints: (1) patent-based IP control (ε≈0.40, primary coordination + secondary extraction), (2) regulatory compliance gatekeeping (ε≈0.45, pure extraction with coordination facade), (3) knowledge transfer barriers (ε≈0.38, coordination failure producing diffusion lag). This story models the aggregate constraint; individual decomposed stories track how each mechanism contributes to overall diffusion barriers and show distinct pathways for barrier reduction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(innovation_diffusion_barriers, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
