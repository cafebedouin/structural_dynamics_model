% ============================================================================
% CONSTRAINT STORY: cognitive_diversity_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_diversity_arbitrage, []).

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
 *   constraint_id: cognitive_diversity_arbitrage
 *   human_readable: Cognitive Diversity Arbitrage in the Workplace
 *   domain: economic/social
 *
 * SUMMARY:
 *   Cognitive diversity arbitrage describes the modern corporate rebranding
 *   of neurodiversity (autism, ADHD, dyslexia, etc.) from a medical/social
 *   'deficit' framework to a strategic competitive asset. Technology firms,
 *   in particular, have pioneered hiring initiatives celebrating
 *   neurodivergent workers' pattern recognition, hyperfocus, and systematic
 *   thinking capabilities. However, this reframing conceals a structural
 *   extraction mechanism: the rhetoric of 'valuing diversity' enables
 *   employers to demand maximum performance output while systematically
 *   denying or deferring workplace accommodations. Neurodivergent workers are
 *   hired for their strengths, evaluated exclusively on those strengths, and
 *   withheld accommodations that would address their actual support needs
 *   (sensory management, executive function tools, social communication
 *   scaffolding) on the grounds that such accommodations would 'reduce their
 *   competitive edge.' The constraint's theater has increased dramatically
 *   over the past decade: corporate 'neurodiversity hiring programs' have
 *   proliferated in marketing and brand messaging while accommodation
 *   infrastructure investment has stagnated or declined. This is a diagnostic
 *   case of tangled rope — genuine coordination (matching workers to tasks
 *   that leverage their cognitive strengths) layered with asymmetric
 *   extraction (using valorization rhetoric to justify underinvestment in the
 *   infrastructure required for sustainable inclusion).
 *
 * KEY AGENTS:
 *   - Neurodivergent Workers: Primary victim (powerless/trapped) — hired for cognitive strengths but denied accommodations needed for sustainable employment; dependent on employer for both income and workspace accessibility
 *   - Technology Employers: Primary beneficiary (institutional/arbitrage) — access to specialized cognitive skills at lower cost due to deferred accommodation spending; can exit strategy if talent pool changes
 *   - HR/Inclusion Departments: Mixed actor (moderate/constrained) — tasked with both worker wellbeing and cost minimization; caught between authentic accommodation and performance metrics
 *   - Workplace Accommodation Infrastructure: Victim (powerless/trapped) — abstract structural capacity bearing the cost of underinvestment; zero-sum framing (accommodate = reduce productivity) justifies continued deprioritization
 *   - Neurodiversity Advocacy Networks: Organized agents (organized/constrained) — fighting for authentic inclusion standards while risk of co-optation into corporate DEI theater
 *   - Corporate DEI/Neurodiversity Brand: Institutional actor maintaining performative hiring (piton perspective) — divorced from accommodation investment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the constraint as inevitable feature of diversity rather than extractive practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_diversity_arbitrage, 0.58).
domain_priors:suppression_score(cognitive_diversity_arbitrage, 0.68).
domain_priors:theater_ratio(cognitive_diversity_arbitrage, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_diversity_arbitrage, tangled_rope).
narrative_ontology:human_readable(cognitive_diversity_arbitrage, "Cognitive Diversity Arbitrage in the Workplace").
narrative_ontology:topic_domain(cognitive_diversity_arbitrage, "economic/social").

domain_priors:requires_active_enforcement(cognitive_diversity_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, technology_companies).
narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, high_performance_teams).
narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, neurodivergent_workers).
narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, workplace_accommodation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEURODIVERGENT WORKER (SNARE) — Faces extraction through 'neurodiversity valorization' rhetoric that rebrands disability accommodation refusal as competitive asset-hunting. Career advancement promised in exchange for maximized output; accommodations denied or withheld to maintain 'peak performance' myth. Trapped by economic dependency and limited alternative employment. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TECHNOLOGY EMPLOYER (ROPE) — Captures genuine coordination benefit: neurodivergent workers often have pattern recognition, systematic thinking, and hyperfocus advantages for technical work. Experiences hiring neurodiversity as solving legitimate talent-matching problem. Can exit by switching talent strategies. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HR/INCLUSION DEPARTMENT (TANGLED ROPE) — Tasked with both worker wellbeing (coordination) and cost minimization (extraction). Benefits from neurodiversity rhetoric (DEI metrics, brand value) but constrained by budget caps on accommodations and pressure to deliver ROI on hiring. d≈0.58, f(d)≈0.77, σ=1.0 → χ≈0.45. Mixed: genuine coordination attempts blocked by resource scarcity and performance metrics.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKPLACE ACCOMMODATION INFRASTRUCTURE (SNARE) — Abstract structural capacity for supporting neurodivergent workers. Bears cost of extraction: underinvestment in accessible tech, quiet spaces, flexible scheduling, and sensory management. Trapped by the zero-sum framing (accommodate = reduce productivity). Extraction persists because accommodation is treated as cost rather than coordination investment. d≈0.94, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: NEURODIVERSITY ADVOCACY NETWORKS (TANGLED ROPE) — Organized agents (disability organizations, neurodiversity collectives) see both opportunity and extraction. Coordination function: advocacy for authentic inclusion policies. Extraction risk: co-optation into corporate DEI theater where rhetoric replaces investment. d≈0.54, f(d)≈0.72, σ=1.0 → χ≈0.42. Constrained by corporate messaging power but fighting to establish real accommodation standards.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CORPORATE NEURODIVERSITY BRAND (PITON) — Once-functional diversity hiring has atrophied into performative DEI marketing. Theater ratio=0.64 reflects that 'celebrating neurodiversity' has become decoupled from actual accommodation investment. Neurodiversity hiring is maintained through institutional inertia and brand value despite low functional benefit to workers. Employer can arbitrage out if brand value declines.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist view, neurodiversity appears natural/immutable: cognitive variation is inherent to human populations, making diversity in thinking styles inevitable. But structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts mountain classification — the false summit reveals that the 'naturalness' of diversity is being weaponized to justify extraction through reframed obligation.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_diversity_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_diversity_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_diversity_arbitrage, TR),
    TR >= 0.70.

:- end_tests(cognitive_diversity_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Employers capture significant value through higher-performance output during the hiring 'honeymoon' window (typically 1-3 years before accommodation deficits accumulate). The extraction is not maximal (0.70+) because genuine coordination benefits are real — neurodivergent workers do often possess legitimate skill advantages for certain tasks. However, the extraction rises as accommodation debt accumulates: workers experience burnout and forced exit as the initial strength-matching advantage is overwhelmed by unmet accommodation needs. Theater ratio (0.64): Moderate-high. DEI marketing around neurodiversity hiring has become substantially performative. Actual accommodation infrastructure — quiet workspaces, flexible scheduling, sensory management tools, social communication support — has not scaled proportionally with hiring rhetoric. The gap between 'celebrating neurodiversity' and 'funding neurodiversity support' has widened. Suppression (0.68): High. Multiple barriers constrain neurodivergent workers' alternatives: (1) labor market stigma outside tech (neurodiversity 'asset framing' is rare outside high-skill tech), (2) economic dependency (workers cannot afford to leave without alternative employment), (3) accommodation debt (workers exhausted from unmet needs have reduced capacity to job-search), (4) selective hiring (tech firms filter for specific cognitive profiles, reducing portability of hiring advantage across sectors).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The technology employer experiences genuine coordination — they are solving a legitimate talent-matching problem and benefit from workers whose cognitive strengths align with technical tasks. Neurodivergent workers experience extraction — hired for strengths that are then exploited without corresponding accommodation support. The HR department experiences a tangled hybrid — they understand both the coordination opportunity and the accommodation needs but are constrained by budgets and performance pressure. Neurodiversity advocacy sees both opportunity (legitimizing neurodivergent contribution) and risk (co-optation into DEI theater without material benefit). The corporate neurodiversity brand has become piton-like — a vestigial ritual maintained through institutional inertia and marketing value despite declining functional benefit to workers. The analytical observer risks naturalizing the constraint as inevitable diversity rather than recognizing it as an extraction mechanism leveraging diversity rhetoric as justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent workers: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Economic dependency, limited alternatives, and accommodation barriers create near-complete trap. Technology employers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can adjust hiring strategy if labor market changes; experience genuine coordination gain. HR/Inclusion departments: Mixed + constrained → d≈0.58, f(d)≈0.77. Constrained by budget caps and performance metrics; genuinely want to coordinate but cannot access resources. Accommodation infrastructure: Victim + trapped → d≈0.94, f(d)≈1.40. Maximal extraction — structural capacity has no independent agency; extraction justified by zero-sum framing. Neurodiversity advocacy: Organized + constrained → d≈0.54, f(d)≈0.72. Organized agents can apply pressure but constrained by corporate messaging dominance and co-optation risk. Corporate DEI brand: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater gate, not from beneficiary status. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit — naturalizing contingent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE: This constraint illustrates how diversity rhetoric can mask extraction. The mandatrophy initially appears as 'Is neurodiversity hiring coordination or extraction?' The tangled rope classification resolves it by showing that both are simultaneously true: (1) the coordination is genuine (workers do have strength advantages, and firms do benefit from matching), but (2) the extraction is also structural (accommodation is systematically deferred to maximize output). The false summit danger is that analysis might naturalize the constraint as an inevitable consequence of 'celebrating diversity' — treating underinvestment in accommodation as a necessary feature of competitive hiring rather than as a policy choice. The mandatrophy is resolved by rejecting the false summit: cognitive diversity itself is natural and valuable; cognitive diversity arbitrage (valorizing diversity while extracting from diverse workers) is a contingent institutional practice that can be reformed. The test: if a firm genuinely funded accommodation infrastructure proportionally to neurodiversity hiring, would the extraction mechanism persist? Current evidence suggests yes — because the extraction mechanism is not primarily about accommodation costs but about leveraging diversity rhetoric to justify demanding maximum output without corresponding support investment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_accommodation_vs_extraction,
    'Can corporate neurodiversity hiring achieve genuine accommodation investment, or does the profit-maximizing logic inevitably collapse accommodation into performance extraction?',
    'Comparative analysis of accommodation spend vs neurodivergent hiring rates across firms; worker satisfaction surveys; turnover rates for neurodivergent vs neurotypical employees; correlation between DEI marketing spend and actual accommodation accessibility',
    'If genuine accommodation possible: constraint may shift to Rope/Scaffold (coordination-primary). If inevitable extraction: constraint is structural Snare using diversity rhetoric as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_accommodation_vs_extraction, empirical, 'Whether corporate accommodation can be decoupled from extraction logic').

omega_variable(
    performance_measurement_circularity,
    'Does ''neurodiversity as competitive advantage'' rhetoric function as a circular justification — i.e., neurodivergent workers hired because of perceived strengths, then evaluated by metrics that privilege those strengths, creating appearance of validation without assessing worker welfare?',
    'Audit of performance evaluation criteria for neurodivergent hires; analysis of promotion rates vs neurotypical peers; assessment of whether evaluation metrics measure worker output vs worker wellbeing',
    'If circular: the apparent success of neurodiversity hiring is theater masking extraction. If falsifiable: metrics can be redesigned to include worker satisfaction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_measurement_circularity, conceptual, 'Whether neurodiversity performance metrics are circularly self-validating').

omega_variable(
    accommodation_cost_allocation,
    'Who bears the material cost of accommodation — the employer, the neurodivergent worker, or society (via benefits/disability insurance)?',
    'Accounting analysis of accommodation spending by employer; measurement of worker out-of-pocket costs for workplace adaptation (noise-canceling headphones, therapy, medication, sensory tools); comparison to statutory disability benefits',
    'If cost externalized to worker/society: extraction is severe and hidden. If borne by employer: constraint may shift toward Rope/Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_cost_allocation, empirical, 'Cost allocation for workplace accommodation').

omega_variable(
    exit_capacity_asymmetry,
    'Do neurodivergent workers in tech genuinely have better exit options than neurodivergent workers in other sectors, or is ''tech neurodiversity culture'' an illusion created by selective hiring?',
    'Cross-sector employment data for neurodivergent workers; barriers-to-exit analysis (skill transferability, alternative employers, benefits portability); comparison of accommodation availability across tech vs non-tech sectors',
    'If tech advantage is real: exit_options may shift to ''constrained'' or ''mobile'' (raising χ less severely). If illusion: workers are globally trapped, and d remains near 0.92.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_asymmetry, empirical, 'Whether tech sector genuinely offers better exit options for neurodivergent workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_diversity_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogdiv_tr_t0, cognitive_diversity_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cogdiv_tr_t5, cognitive_diversity_arbitrage, theater_ratio, 5, 0.52).
narrative_ontology:measurement(cogdiv_tr_t10, cognitive_diversity_arbitrage, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(cogdiv_be_t0, cognitive_diversity_arbitrage, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cogdiv_be_t5, cognitive_diversity_arbitrage, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cogdiv_be_t10, cognitive_diversity_arbitrage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_diversity_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(cognitive_diversity_arbitrage, workplace_accommodation_debt).
narrative_ontology:affects_constraint(cognitive_diversity_arbitrage, neurodivergent_labor_exit_capacity).

% DUAL FORMULATION NOTE:
% Cognitive diversity arbitrage is upstream of specific accommodation debt constraints. Workplace accommodation debt (ε≈0.72, Snare) describes the accumulated unmet needs within individual workers. Neurodivergent labor exit capacity (ε≈0.65, Tangled Rope) describes the sector-level labor market barriers. The cognitive diversity arbitrage constraint unifies these through the rhetoric mechanism: the valorization of neurodivergent strengths simultaneously justifies extraction and masks the accumulating accommodation debt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_diversity_arbitrage, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
