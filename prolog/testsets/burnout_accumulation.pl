% ============================================================================
% CONSTRAINT STORY: burnout_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burnout_accumulation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burnout_accumulation
 *   human_readable: Burnout Accumulation in High-Effort Occupations
 *   domain: labor/organizational_psychology/occupational_health
 *
 * SUMMARY:
 *   Burnout accumulation is a constraint that arises from the structural
 *   coupling of (1) occupational roles with genuine coordination functions
 *   (healthcare, education, social support), (2) productivity cultures that
 *   extract maximum effort capacity without adequate recovery, (3) economic
 *   dependency that prevents exit, and (4) identity fusion that internalizes
 *   the role as the self. The constraint exhibits a distinct perspectival
 *   structure: workers trapped by economics and identity see pure extraction
 *   (snare); organizations and management see pure coordination (rope); labor
 *   movements and regulatory bodies see a temporary problem with available
 *   sunset mechanisms (scaffold); wellness initiatives appear as degraded
 *   ritual (piton); privileged professionals experience mixed coordination
 *   and extraction (tangled_rope); and analytical observers risk naturalizing
 *   the constraint as inherent to effort rather than contingent on
 *   institutional design. The theater ratio (0.62) reflects that substantial
 *   organizational response to burnout is performative — wellness programs
 *   address symptoms rather than root causes, creating the appearance of
 *   action while unsustainable workloads persist. Extractiveness (0.58)
 *   indicates moderate-to-high, measurable through indicators such as:
 *   persistent exhaustion despite time off, emotional depersonalization,
 *   reduced efficacy perception, health deterioration, and capacity ceiling
 *   effects where productivity gains plateau as workers approach systemic
 *   limits.
 *
 * KEY AGENTS:
 *   - Burned-Out Workers: Primary victim (powerless/trapped) — economically dependent on occupational role, health insurance linked to employment, identity fused with professional status. Exit is costly and identity-shattering.
 *   - Identity-Locked Professionals: Secondary victim (moderate/identity_locked) — structurally mobile but identity-fused with high-effort role. Internalized belief that they should 'be able to handle this' sustains commitment despite extraction.
 *   - Organizations and Management: Primary beneficiary (institutional/arbitrage) — extract productivity surplus without corresponding recovery investment. Exit option is replacement — if one worker burns out, hire another.
 *   - Labor Movements and Regulatory Bodies: Organized reformers (organized/constrained) — building alternative frameworks (work-hour caps, mandatory rest, disability protections, mental health standards) with sunset logic. Face political barriers but have strategic exit pathways.
 *   - Wellness Apparatus (Corporate HR, Apps, Training): Institutional theater (institutional/arbitrage) — maintains the appearance of addressing burnout while leaving extraction mechanisms intact. Persists through inertia.
 *   - Privileged Professionals: Secondary beneficiaries (powerful/mobile) — benefit from productivity culture and status, but have exit velocity to escape extraction costs. Experience tangled_rope rather than snare.
 *   - Worker Health Commons: Diffuse victim (powerless/trapped) — abstract collective health good that cannot organize or exit; contaminated by accumulation of burned-out, health-compromised workers leaving the field.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burnout_accumulation, 0.58).
domain_priors:suppression_score(burnout_accumulation, 0.65).
domain_priors:theater_ratio(burnout_accumulation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burnout_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(burnout_accumulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(burnout_accumulation, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burnout_accumulation, tangled_rope).
narrative_ontology:human_readable(burnout_accumulation, "Burnout Accumulation in High-Effort Occupations").
narrative_ontology:topic_domain(burnout_accumulation, "labor/organizational_psychology/occupational_health").

domain_priors:requires_active_enforcement(burnout_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burnout_accumulation, organizational_efficiency_extractors).
narrative_ontology:constraint_beneficiary(burnout_accumulation, productivity_maximizers).
narrative_ontology:constraint_victim(burnout_accumulation, burned_out_workers).
narrative_ontology:constraint_victim(burnout_accumulation, worker_health_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BURNED-OUT WORKER (SNARE) — Worker trapped by economic dependency, healthcare linkage to employment, and status investment in the role. Exhaustion accumulates irreversibly; recovery requires exiting the occupational category entirely. Maximum experienced extraction with minimal coordination benefit. The worker perceives the constraint as unchangeable within their biographical horizon.
constraint_indexing:constraint_classification(burnout_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED PROFESSIONAL (TANGLED ROPE) — Professional whose identity is constituted through the high-effort role: physician, nurse, teacher, social worker. Structurally mobile (could theoretically exit) but functionally trapped because leaving would require abandoning professional identity. Experience significant extraction while maintaining belief that they should 'be able to handle this' — the role's coordination function (serving patients/students) is genuine, but extraction accumulates through that same channel. Perceives the constraint as changeable in principle but unthinkable in practice.
constraint_indexing:constraint_classification(burnout_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZATIONAL EFFICIENCY EXTRACTOR (ROPE) — Institution or management extracting surplus labor capacity views the constraint as coordination: labor is allocated efficiently, maximum utility is extracted from each worker-hour. The organization experiences burnout accumulation as a feature enabling productivity goals, not a bug. Exit option is arbitrage — if one worker burns out, another can be hired. Benefits from the constraint significantly.
constraint_indexing:constraint_classification(burnout_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT / REGULATORY REFORM (SCAFFOLD) — Organized agents (unions, labor advocacy, occupational health regulation) see burnout as a temporary institutional failure with sunset logic: work-hour caps, mandatory rest periods, disability protections, and workplace mental health standards are building alternative frameworks. Constrained by political economy (regulatory capture, union weakening) but strategically focused on sunset mechanisms. Low effective extraction because the coalition has agency and a visible exit path.
constraint_indexing:constraint_classification(burnout_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SELF-CARE / WELLNESS APPARATUS (PITON) — Corporate wellness programs, meditation apps, resilience training, and burnout prevention rhetoric are substantially performative. They create the appearance of addressing burnout while leaving the underlying extraction mechanism (unsustainable workload, understaffing, performance metrics) intact. Theater ratio high because the apparatus maintains the framing that individual worker resilience is the problem, not structural overload. The apparatus persists through institutional inertia — organizations implement wellness initiatives because they appear to address burnout without reducing productive capacity.
constraint_indexing:constraint_classification(burnout_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVILEGED PROFESSIONAL (TANGLED ROPE) — High-status professional (academic, consultant, physician with financial cushion) with genuine exit options and mobility. Still experiences the coordination function (research, patient care, expertise deployment) but can negotiate work intensity, sabbaticals, or transition to lower-extraction roles. Asymmetric extraction relative to trapped workers: benefits from the same productivity culture that burns out colleagues, but has escape velocity. Sees constraint as mixed — genuine value in the role, but extractive overlay that only affects those without exit options.
constraint_indexing:constraint_classification(burnout_accumulation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some worker exhaustion is inherent to human effort: any role has a sustainable capacity threshold, and demanding work always carries fatigue costs. This perspective risks naturalizing what is actually a contingent institutional arrangement — the burnout accumulation constraint is not inherent to effort but to the specific pairing of (high workload) + (inadequate recovery time) + (economic dependency) + (identity fusion). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(burnout_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burnout_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burnout_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burnout_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burnout_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(burnout_accumulation, TR),
    TR >= 0.70.

:- end_tests(burnout_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, rising over the measurement interval from 0.32 to 0.58. The base extractiveness reflects that occupational burnout accumulation represents genuine surplus extraction — workers provide more capacity than is sustainable, organizations capture the surplus without compensating for recovery costs. The measurement trajectory shows worsening extractiveness over a 6-unit interval, consistent with studies documenting accelerating burnout in healthcare, education, and social work sectors. Suppression (0.65): Moderate-high, reflecting significant barriers to exit including economic dependency, healthcare linkage, status investment, and internalized identity fusion. The suppression is structural (external barriers) for trapped workers and internalized (cognitive capture) for identity-locked professionals — a dual mechanism tracked in the structural_vs_individual_suppression omega. Theater ratio (0.62): Moderate-high, indicating that organizational responses to burnout (wellness programs, resilience training, mental health apps) perform action without addressing root causes. The rise from 0.40 to 0.62 over the interval reflects increasing divergence between the appearance of burnout response and actual reduction in workload/extraction — as burnout awareness increased, performative responses increased without structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The trapped worker and the organization perceive opposite directions of value: the worker experiences pure extraction (snare), while the organization experiences coordination that enables productivity (rope). The identity-locked professional experiences mixed extraction and genuine value through the role itself (tangled_rope), but perceives the mixing as their personal inadequacy rather than structural asymmetry. The labor movement sees a sunset pathway through regulatory reform (scaffold), while the wellness apparatus explicitly avoids regulatory change and instead frames burnout as an individual resilience problem (piton). The privileged professional with exit options sees the same structural constraint but experiences it as surmountable (tangled_rope with negotiation potential), while the trapped worker sees it as absolute (snare with no negotiation). The civilizational observer risks saying 'this is inherent to effort' (mountain), while the structural data shows the constraint is contingent on occupational design choices, productivity culture, economic dependency mechanisms, and identity fusion processes — all of which are changeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from beneficiary/victim status and exit options. Trapped workers (economic dependency + identity fusion) show d = 0.95, maximum extraction target. Identity-locked professionals show d = 0.89 (structurally mobile but cognitively captured). Organizations show d = 0.05 (full beneficiary with arbitrage exit). Labor reformers show d = 0.50-0.55 (organized but constrained). Privileged professionals show d = 0.50-0.55 (mobile exit but complicit in productivity culture). Analytical observer shows d = 0.72 (observer position). The chi formula χ = ε × f(d) × σ(S) scales extractiveness by power (f(d)) and scope (σ(S)): for trapped workers at national scope, χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high extraction). For organizations at national scope with beneficiary status, χ ≈ 0.58 × (-0.12) × 1.0 ≈ -0.07 (negative extraction, experienced as coordination benefit). The gap is diagnostic.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION RECOMMENDED: The natural-language concept 'burnout accumulation' contains multiple structurally distinct constraints with different ε values that should be separated per the ε-invariance principle. Consider decomposing into: (1) burnout_in_identity_locked_professions (ε ≈ 0.50) — coordination function is genuine (patient care, student education), extraction arises through identity fusion and internalized obligation. Different from: (2) burnout_in_economically_trapped_occupations (ε ≈ 0.65) — pure extraction with minimal coordination benefit, constraint mechanism is economic dependency. The current story (ε=0.58) averages these, obscuring their distinct sunset mechanisms and omega variables. If decomposed, the identity-locked story would foreground the cognitive capture mechanism and the sunset through identity-reframing interventions; the economically trapped story would foreground the structural exit barriers and sunset through regulatory/economic reform. The constraint as currently written is tangled_rope, correctly, but at average ε masks important structural differences. Recommendation: keep as-is for single-story submission, but flag for family decomposition in future analysis. The mandatrophy here is not about mislabeling type, but about obscuring structural distinctions through averaging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_individual_suppression,
    'Is measured suppression (0.65) primarily structural (economic barriers, healthcare linkage, legal contract terms) or internalized (identity fusion, belief that suffering is virtuous, fear of judgment)?',
    'Post-exit trajectory analysis: if suppression persists after the worker exits the role (continuing to overwork in lower-stress position, internalized guilt about self-care, identity crisis), reclassify suppression as partially internalized. Longitudinal interviews with burned-out workers who exited vs those still trapped.',
    'If primarily structural: constraint is snare for trapped workers, rope for beneficiaries. If partially internalized: constraint''s effective suppression is higher than structural measure suggests — worker carries the suppression even after material barriers are removed. Affects long-term recovery timelines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_individual_suppression, empirical, 'Whether suppression is structural or internalized (identity-based)').

omega_variable(
    cyclical_vs_monotonic_extraction,
    'Does burnout accumulate monotonically over time, or does it cycle (peak exhaustion → crisis → recovery → renewed overcommitment → peak again)?',
    'Longitudinal measurement of worker extractiveness and theater_ratio across multiple work cycles. Tracking of burnout symptom trajectories: does recovery occur during low-demand periods, or does baseline elevation persist?',
    'If monotonic: constraint is snare with escalating extraction (no self-correction). If cyclical: constraint exhibits intermittent reinforcement mechanism — renewal of hope during recovery phase sustains commitment to the extractive role. Affects sunset timeline and exit likelihood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cyclical_vs_monotonic_extraction, empirical, 'Whether burnout accumulation is monotonic or cyclical').

omega_variable(
    occupational_variance_in_extraction,
    'Does burnout extraction vary significantly across occupational categories (medicine vs teaching vs social work vs tech), or is it a general labor constraint that scales with workload?',
    'Comparative analysis of burnout metrics across occupations with controlled for workload hours and compensation. Decompose into separate stories if ε differs by >0.15 across occupational groups.',
    'If variance is high: write separate constraint stories per occupational domain (burnout_in_medicine, burnout_in_teaching, etc.), each with its own ε, beneficiaries, and sunset mechanisms. If variance is low: constraint is general across occupations, suggesting focus on common structural factors (economic dependency, productivity culture, identity fusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_variance_in_extraction, empirical, 'Occupational variance in burnout extraction mechanisms').

omega_variable(
    recovery_sufficiency_of_interventions,
    'Do wellness programs, sabbaticals, and rest-period regulations actually reduce burnout accumulation, or do they create the appearance of recovery while underlying extraction mechanisms persist?',
    'Comparative outcomes: burnout trajectory in organizations with vs without implemented interventions, controlling for baseline workload. Tracking whether workers return to sustainable capacity after intervention or resume overcommitment within 6 months.',
    'If interventions effective: scaffold and labor movement perspectives are correct — sunset mechanisms are working. If ineffective: wellness apparatus is piton (degraded theater); constraint remains snare for trapped workers despite intervention programs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_sufficiency_of_interventions, empirical, 'Effectiveness of burnout interventions and recovery programs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burnout_accumulation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burnout_tr_t0, burnout_accumulation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(burnout_tr_t3, burnout_accumulation, theater_ratio, 3, 0.52).
narrative_ontology:measurement(burnout_tr_t6, burnout_accumulation, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(burnout_be_t0, burnout_accumulation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(burnout_be_t3, burnout_accumulation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(burnout_be_t6, burnout_accumulation, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burnout_accumulation, attachment_coordination).
narrative_ontology:affects_constraint(burnout_accumulation, occupational_health_regulation).
narrative_ontology:affects_constraint(burnout_accumulation, labor_market_dependency).

% DUAL FORMULATION NOTE:
% Burnout accumulation is downstream of labor market structures that create economic dependency and occupational identity fusion. Future decomposition should separate burnout_in_identity_locked_professions (coordination function genuine) from burnout_in_trapped_labor_markets (coordination function minimal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
