% ============================================================================
% CONSTRAINT STORY: workforce_burnout_and_exodus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_workforce_burnout_and_exodus, []).

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
 *   constraint_id: workforce_burnout_and_exodus
 *   human_readable: Workforce Burnout and Exodus
 *   domain: labor/organizational/social
 *
 * SUMMARY:
 *   Workforce burnout and exodus represents a structural snare in which
 *   workers bear escalating extraction through unsustainable workload, wage
 *   stagnation, and eroding social safety nets, while exit is suppressed
 *   through economic dependency and psychological identity capture. The
 *   constraint has intensified over the past decade as organizational
 *   complexity metrics (SKU count, customer segments, cross-functional
 *   dependencies) have increased without proportional headcount expansion.
 *   Management captures surplus through compressed labor costs while workers
 *   absorb the gap through unpaid overtime, stress-related health costs, and
 *   cognitive load. The theater ratio (0.62) reflects the elaborate apparatus
 *   of wellness initiatives, engagement surveys, flexible-work policies, and
 *   cultural messaging — all performative — that creates the appearance of
 *   solving burnout without addressing the structural drivers. The constraint
 *   manifests differently across organizational hierarchies: frontline
 *   workers are trapped by economic necessity; middle management experience
 *   conflicting pressure from above and below; senior management benefit from
 *   the extraction while maintaining plausible deniability; HR maintains the
 *   theater; labor organizing provides the only visible exit pathway.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victims (powerless/trapped) — bear full extraction through unsustainable workload, wage stagnation, benefit tying; no exit due to economic dependency
 *   - Middle Management: Secondary victims (moderate/constrained) — pressured to enforce productivity demands on subordinates while bearing compression from above; sunk career investment constrains exit
 *   - Capital Owners and Senior Management: Primary beneficiaries (institutional/arbitrage) — extract surplus from worker exhaustion while maintaining exit optionality through automation, relocation, outsourcing
 *   - Human Resources Department: Theater maintainers (institutional/arbitrage) — preserve appearance of solving burnout through programs that address symptoms but not structural drivers
 *   - Labor Organizers and Regulatory Advocates: Organized agents (organized/constrained) — building alternative pathways (sectoral bargaining, minimum standards, worker co-ops) with generational sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices (understaffing, metric obsession) as inevitable facts of modern work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(workforce_burnout_and_exodus, 0.58).
domain_priors:suppression_score(workforce_burnout_and_exodus, 0.65).
domain_priors:theater_ratio(workforce_burnout_and_exodus, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(workforce_burnout_and_exodus, extractiveness, 0.58).
narrative_ontology:constraint_metric(workforce_burnout_and_exodus, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(workforce_burnout_and_exodus, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(workforce_burnout_and_exodus, snare).
narrative_ontology:human_readable(workforce_burnout_and_exodus, "Workforce Burnout and Exodus").
narrative_ontology:topic_domain(workforce_burnout_and_exodus, "labor/organizational/social").

domain_priors:requires_active_enforcement(workforce_burnout_and_exodus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(workforce_burnout_and_exodus, capital_owners_and_senior_management).
narrative_ontology:constraint_victim(workforce_burnout_and_exodus, frontline_workers).
narrative_ontology:constraint_victim(workforce_burnout_and_exodus, middle_management).
narrative_ontology:constraint_victim(workforce_burnout_and_exodus, organizational_knowledge_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped by economic dependency and lack of alternatives. Bears full extraction: unsustainable workload, wage stagnation, healthcare tied to employment. No meaningful exit; economic survival requires enduring escalating demands. Experiences maximum suppression through financial vulnerability and depleted negotiating capacity.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE MANAGEMENT (TANGLED ROPE) — Constrained by sunk career investment and mortgage/family obligations, but possesses some marketable skills and education. Experiences mixed extraction: pressured to drive worker productivity (beneficiary of subordinate extraction) while bearing extraction from above (compressed margins, performance metrics, impossible KPIs). High suppression through conflicting loyalties and no clear exit path without significant cost.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNERS AND SENIOR MANAGEMENT (ROPE) — Net beneficiaries experiencing the constraint as coordination of resource extraction. Extract surplus from worker exhaustion while maintaining operational continuity. Low effective extraction because exit options are abundant — can relocate operations, automate, reduce headcount, or shift to contract labor. The constraint coordinates their interests: keep wages low, push productivity high, replace workers as they burn out.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RESOURCES DEPARTMENT (PITON) — Maintains extensive theater: wellness programs, culture initiatives, retention bonuses, and engagement surveys, all performative. The functional core (preventing burnout, building sustainable capacity) has atrophied — resources go to appearance rather than structural change. The HR apparatus persists through institutional inertia because organizations expect it to exist, not because it solves burnout.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ORGANIZING AND REGULATORY PRESSURE (SCAFFOLD) — Organized actors (unions, labor advocates, progressive legislators, worker co-ops) are building alternative institutional structures and sunset mechanisms. Minimum-wage increases, mandatory rest periods, worker-owned models, remote-work flexibility, and sectoral bargaining reduce the burnout constraint's extractive force. This is temporary coordination support with a genuine sunset — as worker organizing and labor protections mature, the extraction mechanism loses leverage.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, worker burnout can be framed as an immutable consequence of competitive pressures, technological acceleration, or human biological limits: 'people have always burned out under stress,' 'modern work is inherently demanding,' 'some churn is inevitable.' This naturalization masks contingent institutional choices (understaffing, metric obsession, zero-slack scheduling) as natural laws. The engine will identify this as a false summit — the constraint's base extractiveness (0.58) and measured suppression (0.65) indicate institutional arrangements, not natural limits.
constraint_indexing:constraint_classification(workforce_burnout_and_exodus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(workforce_burnout_and_exodus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(workforce_burnout_and_exodus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(workforce_burnout_and_exodus, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(workforce_burnout_and_exodus, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(workforce_burnout_and_exodus, TR),
    TR >= 0.70.

:- end_tests(workforce_burnout_and_exodus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant but not maximal extraction. Management captures surplus through compressed labor costs, but the extraction is bounded by labor market dynamics (workers can eventually exit), legal constraints (some jurisdictions enforce minimum protections), and organizational self-damage (excessive churn erodes capability). The trajectory (0.35→0.58 over ten years) shows escalating extraction correlated with organizational complexity growth and staffing underinvestment. Suppression (0.65): High. Workers face material barriers (economic dependency, healthcare tying, geographic constraints) and psychological barriers (identity fusion with professional role, internalized failure narratives, status anxiety about downward mobility). Exit is costly in both material and psychological dimensions. Theater ratio (0.62): Moderate-high. Organizational responses to burnout are substantially performative: wellness programs, mental health days, and culture initiatives address symptoms while metrics, headcount ratios, and performance expectations (the true drivers) remain unchanged. Theater has increased over the interval as organizations have added more visible support mechanisms without reducing workload intensity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates why a single 'management' perspective fails. Senior management see coordination (Rope) — they are solving the legitimate problem of maintaining competitiveness. They experience low extraction because they have abundant exit options (outsource, relocate, hire contractors, automate). Frontline workers see extraction (Snare) — unsustainable demands with no exit. Middle management experience the hybrid (Tangled Rope) — pressured to extract from subordinates while bearing extraction from above, with only constrained exit. HR maintains the theater (Piton) — the apparatus persists through inertia, not function. Labor organizers see a temporary problem with a sunset (Scaffold) — sectoral bargaining, minimum standards, and worker co-ops represent a generational transition that could transform the extraction mechanism. The analytical observer risks naturalizing the arrangement as inevitable (Mountain) — 'competitive pressure forces this,' 'people have always burned out,' 'some churn is necessary' — but the measurement trajectory and structural data reveal these as choices, not laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is determined by their structural relationship to the extraction flow. Capital owners and senior management are beneficiaries with arbitrage options (abundant exit pathways) — they derive d ≈ 0.10, producing negative or minimal effective extraction f(d) ≈ -0.01 to 0.02. Frontline workers are victims with no exit (trapped by economic dependency) — they derive d ≈ 0.95, producing maximum experienced extraction f(d) ≈ 1.42. Middle management are mixed: victims of compression from above, beneficiaries of subordinate extraction — d ≈ 0.55, producing moderate extraction f(d) ≈ 0.75. The identity_locked exit option appears relevant for some middle management (professional identity fused with organizational role, status anxiety about lateral moves) and potentially for workers with decades of organizational tenure (identity constituted through the role). The scope modifier σ(S) scales extractiveness upward: at global scope (σ=1.2), the constraint affects worldwide labor markets; at national scope (σ=1.0), effects are more localized. The engine derives chi = ε × f(d) × σ(S) for each perspective, producing the perspectival gap: senior management experience χ ≈ 0.58 × (-0.01) × 1.2 ≈ near zero; frontline workers experience χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint satisfies the Snare gate (extractiveness 0.58 ≥ 0.46, suppression 0.65 ≥ 0.60, χ from powerless perspective ≥ 0.66) at the powerless/trapped perspective. However, the mandatrophy analysis reveals that from other perspectives, the constraint classifies as Tangled Rope (middle management), Rope (senior management), Piton (HR), and Scaffold (organizing movements). This perspectival pluralism does not resolve the mandatrophy — it instantiates it. The 'correct' classification depends on the observer's structural position. From the powerless agent's view, this is pure extraction (Snare). From the beneficiary's view, this is coordination (Rope). The analytical observer who claims the constraint is a 'natural law of competitive markets' (Mountain) is naturalizing what is actually institutional choice (staffing ratios, metric design, benefit tying). The scaffold perspective (organizing movements) is not aspirational — it is a real structural feature: labor protections and sectoral bargaining are actively building alternative pathways that would reduce the extraction mechanism's force. The mandatrophy resolution is to recognize that all six types are simultaneously true from different structural positions, and that the presheaf of perspectives reveals the constraint's architecture better than any single type. The goal is not to choose which type is 'correct' but to understand how the same structural arrangement appears different to agents with different power, time horizons, exit options, and scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burnout_vs_normal_stress_threshold,
    'What threshold distinguishes sustainable work intensity from extractive burnout requiring suppression to maintain?',
    'Longitudinal health outcomes data: burnout workers show persistent physiological stress markers, cognitive decline, depression, and health incidents at rates significantly above control groups. Correlation with organizational metrics (headcount ratios, overtime hours, metric density).',
    'If threshold is low (current work is already extractive): Snare classification confirmed across more perspectives. If threshold is high (current work is normal): constraint is coordination (Rope) with some overhead. Current research indicates threshold is well below current industry norms, confirming extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burnout_vs_normal_stress_threshold, empirical, 'Burnout threshold relative to organizational work intensity').

omega_variable(
    exit_barrier_primacy,
    'Are workers primarily trapped by material economic dependency, or by identity fusion with work, or by internalized powerlessness despite structural mobility?',
    'Post-exit survey methodology: workers who leave organizations report (1) financial anxiety and uncertainty about next role (material barrier), (2) identity loss and status anxiety (identity lock), or (3) realization that outside options were available all along (cognitive lock). Proportions indicate whether ''trapped'' or ''identity_locked'' is the dominant mechanism.',
    'If material barriers dominate: workers are genuinely trapped — exit costs are high and real. If identity lock dominates: workers are identity-locked (especially professional identity, status anxiety) — the real barrier is cognitive, meaning interventions like identity reframing or community support could unlock exit. Mixed: both mechanisms bind.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_barrier_primacy, empirical, 'Whether exit barriers are material or cognitive').

omega_variable(
    knowledge_loss_externality_magnitude,
    'How much organizational knowledge and institutional memory is permanently lost when experienced workers burn out and depart?',
    'Knowledge audit post-exodus: document tacit knowledge in departed workers (client relationships, workaround expertise, tribal knowledge), measure recovery time for successors, quantify retraining cost, estimate productivity gap during knowledge transfer. Compare organizations with high churn vs low churn on this dimension.',
    'If loss is severe: burnout constraint has large negative externality on organizational function, which contradicts the beneficiary group''s rational self-interest — management appears to be extracting in ways that damage their own operations. If loss is minor: institutional knowledge is easily replaceable, and the extraction mechanism is more sustainable (less self-destructive). High loss suggests burnout is a coordination failure with poor information, not pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_loss_externality_magnitude, empirical, 'Magnitude of knowledge loss from worker exodus').

omega_variable(
    suppression_mechanism_structural_vs_psychological,
    'Is worker suppression primarily structural (legal constraints on organizing, wage theft, benefit tying) or psychological (internalized failure narratives, identity fusion with employer)?',
    'Comparative analysis: organizations with strong legal/structural suppression (union busting, non-competes, benefits hostage-taking) show different exit patterns than organizations with weak structural suppression but strong psychological/identity capture. Measure: worker perception of exit cost vs actual market cost for equivalent role.',
    'If structural dominates: removing legal barriers through labor regulation could unlock exit. If psychological dominates: workers need cognitive reframing and community support to recognize their own agency. Mixed: both mechanisms reinforce each other, making exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_psychological, empirical, 'Structural vs psychological mechanisms of suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(workforce_burnout_and_exodus, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burnout_tr_t0, workforce_burnout_and_exodus, theater_ratio, 0, 0.4).
narrative_ontology:measurement(burnout_tr_t3, workforce_burnout_and_exodus, theater_ratio, 3, 0.5).
narrative_ontology:measurement(burnout_tr_t6, workforce_burnout_and_exodus, theater_ratio, 6, 0.59).
narrative_ontology:measurement(burnout_tr_t10, workforce_burnout_and_exodus, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(burnout_be_t0, workforce_burnout_and_exodus, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(burnout_be_t3, workforce_burnout_and_exodus, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(burnout_be_t6, workforce_burnout_and_exodus, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(burnout_be_t10, workforce_burnout_and_exodus, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(workforce_burnout_and_exodus, resource_allocation).
narrative_ontology:affects_constraint(workforce_burnout_and_exodus, healthcare_system_employment_tying).
narrative_ontology:affects_constraint(workforce_burnout_and_exodus, wage_stagnation_productivity_divergence).
narrative_ontology:affects_constraint(workforce_burnout_and_exodus, organizational_metric_obsession).

% DUAL FORMULATION NOTE:
% Workforce burnout and exodus is a constraint family that encompasses multiple structurally distinct mechanisms: (1) unsustainable workload (scheduling/staffing constraint), (2) wage stagnation (economic extraction), (3) identity fusion with role (psychological lock-in), (4) benefits tying (systemic dependency). Each mechanism has its own extractiveness value and classification. The integrated story treats burnout as a composite snare where multiple mechanisms reinforce each other. Decomposition into separate stories (workload_intensity_constraint, wage_compression_constraint, professional_identity_lock_in, healthcare_employment_tying) would reveal different ε values and time horizons. The network links show how burnout is downstream of organizational complexity growth and upstream of labor organizing responses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(workforce_burnout_and_exodus, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
