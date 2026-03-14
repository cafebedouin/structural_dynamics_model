% ============================================================================
% CONSTRAINT STORY: expertise_specialization_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expertise_specialization_trap, []).

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
 *   constraint_id: expertise_specialization_trap
 *   human_readable: Expertise Specialization Trap
 *   domain: institutional/professional/cognitive
 *
 * SUMMARY:
 *   The expertise specialization trap is a structural constraint in
 *   professional and academic systems where deep specialization, while
 *   enabling significant coordination benefits (knowledge accumulation,
 *   quality standards, methodological rigor), simultaneously creates
 *   extraction dynamics through gatekeeping, credentialing lock-in, and
 *   cognitive/identity capture. Specialists invest years acquiring domain
 *   expertise, become identity-fused with their specialization, and then find
 *   themselves locked into narrow labor markets with high suppression
 *   preventing lateral mobility. The constraint exhibits six distinct
 *   classifications from different perspectives, revealing how the same
 *   institutional structure appears as enabling (Rope) to beneficiaries,
 *   mixed (Tangled Rope) to practitioners, performative (Piton) to
 *   regulators, escapable (Scaffold) to organized challengers, immutable
 *   (Mountain) from the analytical view, and extractive (Snare) to trapped
 *   agents. The trajectory over the interval shows extractiveness increasing
 *   (0.35 → 0.58) as specialization becomes more granular and credentials
 *   more narrowly defined, while theater_ratio remains moderate (0.32 →
 *   0.48), indicating genuine coordination function alongside genuine
 *   extraction rather than pure performance.
 *
 * KEY AGENTS:
 *   - Specialized Practitioners: Primary victims (powerless/identity_locked) — invest decades in specialization, identity fused with expertise domain, face obsolescence risk and narrow labor market
 *   - Institutional Gatekeepers (Professional Licensing Bodies, Academic Departments): Primary beneficiaries (institutional/arbitrage) — maintain credentialing authority, control career pathways, benefit from specialization norm without experiencing mobility constraints
 *   - Field Communities: Secondary victims and beneficiaries (moderate/constrained) — genuinely benefit from specialization's coordination function (deep knowledge, methodological rigor) but suffer from knowledge siloing and methodological insularity
 *   - Credentialing Bodies: Institutional beneficiaries (institutional/arbitrage) — extract gatekeeping rents, set domain boundaries, determine recognition of alternative credentials
 *   - Cross-Disciplinary Movement: Organized challengers (organized/mobile) — building alternative pathways (T-shaped skills, problem-based research, generalist credentials) with sunset logic for traditional specialization regime
 *   - Knowledge Commons: Victim (powerless/trapped) — slow paradigm shifts, delayed synthesis across domains, lost opportunity for innovation at boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expertise_specialization_trap, 0.58).
domain_priors:suppression_score(expertise_specialization_trap, 0.65).
domain_priors:theater_ratio(expertise_specialization_trap, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expertise_specialization_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(expertise_specialization_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(expertise_specialization_trap, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expertise_specialization_trap, tangled_rope).
narrative_ontology:human_readable(expertise_specialization_trap, "Expertise Specialization Trap").
narrative_ontology:topic_domain(expertise_specialization_trap, "institutional/professional/cognitive").

domain_priors:requires_active_enforcement(expertise_specialization_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expertise_specialization_trap, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(expertise_specialization_trap, credentialing_bodies).
narrative_ontology:constraint_victim(expertise_specialization_trap, specialized_practitioners).
narrative_ontology:constraint_victim(expertise_specialization_trap, knowledge_generalization).
narrative_ontology:constraint_victim(expertise_specialization_trap, professional_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPECIALIST PRACTITIONER (SNARE) — Agent has invested decades in deep specialization. Identity is constituted through expertise domain (professional identity fused with specialization). Cannot exit without becoming 'a different person.' Structural mobility exists (could retrain) but identity lock prevents exercise. Bears full extraction cost: narrow labor market, credentialing lock, obsolescence risk.
constraint_indexing:constraint_classification(expertise_specialization_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD COMMUNITY (TANGLED ROPE) — Genuine coordination function: specialization enables depth, accumulation of tacit knowledge, cross-validation within domain. But also extraction: knowledge siloing prevents synthesis, methodological insularity, slow responses to paradigm shifts. High suppression (credentialing requirements, peer review gatekeeping) constrains mobility between fields.
constraint_indexing:constraint_classification(expertise_specialization_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CREDENTIALING BODY (ROPE) — Benefits from specialization norm: maintains gatekeeping authority, controls career pathways, sets standards. Experiences specialization as coordination mechanism: defines domain boundaries, ensures quality thresholds. Low suppression from their perspective — can arbitrage between different specialization regimes.
constraint_indexing:constraint_classification(expertise_specialization_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL LICENSING SYSTEM (PITON) — Licensing regime persists through institutional inertia. Originally functional (ensured minimum competence), now largely performative. Theater_ratio 0.48 reflects that licensure tests measure specialization depth rather than integration capacity. System maintains itself through credentialing infrastructure rather than demonstrated effectiveness at preventing harm or enabling innovation.
constraint_indexing:constraint_classification(expertise_specialization_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CROSS-DISCIPLINARY MOVEMENT (SCAFFOLD) — Organized agents (interdisciplinary research initiatives, problem-based hiring, T-shaped skills frameworks) are building alternative pathways with sunset logic. Emergence of 'generalist' credentials, team-based research that doesn't require individual hyper-specialization, and problem-focused funding that cuts across domain boundaries. Sunset clause implicit: as integrative methods mature, pure specialization becomes optionally rather than mandatory.
constraint_indexing:constraint_classification(expertise_specialization_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal civilizational view, deep specialization appears as a natural law: cognitive capacity constraints, time investment requirements, and knowledge accumulation dynamics create an apparent immutable tradeoff between depth and breadth. However, the structural data reveals this as naturalization of contingent institutional design. The mountain classification is a false summit.
constraint_indexing:constraint_classification(expertise_specialization_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expertise_specialization_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expertise_specialization_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expertise_specialization_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expertise_specialization_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expertise_specialization_trap, TR),
    TR >= 0.70.

:- end_tests(expertise_specialization_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Initial specialization provides genuine coordination benefits (knowledge accumulation, quality standards) with moderate extraction cost (0.35). Over time, as specialization becomes more granular and credentials more narrowly defined, extraction increases (0.58) because practitioners face steeper switching costs, credential systems become more rigid, and gatekeepers leverage credentialing authority. The trajectory reflects credential accumulation and path dependence. Suppression (0.65): High. Multiple suppression mechanisms operate: credentialing requirements create legal/institutional barriers to lateral mobility; tacit knowledge requirements create high retraining costs; career risk for mid-career domain switching (starting over, credential recognition risk); social stigma around 'breadth' framed as lack of rigor; professional identity fusion making domain switching psychologically costly. Suppression is not total — practitioners can and do retrain — but barriers are significant. Theater ratio (0.48): Moderate. Specialization has genuine coordination function (ensuring depth, enabling knowledge accumulation) alongside performative gatekeeping (credential requirements that test specialization rather than integration or problem-solving capacity). The ratio reflects both functions present without either dominating.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement about whether specialization is enabling or extractive. The credentialing body sees coordination (Rope) — specialization defines standards, ensures quality, enables peer review and knowledge validation. The specialist sees extraction (Snare) — depth comes at the cost of flexibility, identity fusion, and labor market vulnerability. The field community sees both (Tangled Rope) — genuine knowledge benefits from specialization alongside genuine costs from siloing. The cross-disciplinary movement sees a solvable problem (Scaffold) — alternative credentials and team-based research can reduce specialization requirements without sacrificing depth. The licensing system sees a degraded ritual (Piton) — licenses persist through inertia, measuring specialization depth rather than actual competence or problem-solving ability. The analytical observer risks seeing an immutable law (Mountain) — cognitive limits and knowledge accumulation dynamics create inherent depth-breadth tradeoff — but the structural data reveals contingency: different institutional regimes produce dramatically different specialization requirements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position. Credentialing bodies (beneficiaries with arbitrage exit) experience d ≈ 0.08 (low extraction, can shift between specialization regimes). Specialists at biographical time with identity_locked exit derive d ≈ 0.89 (high experienced extraction despite structural mobility at constrained level — the lock is cognitive). Field communities with constrained exit but mixed beneficiary/victim status derive d ≈ 0.55 (symmetric position — costs and benefits roughly balanced). The cross-disciplinary movement with organized power and mobile exit derives d ≈ 0.40 (moderate experienced extraction because organized agents can advocate for alternatives). The analytical observer derives d ≈ 0.73 (analytical position sees the full structure). Identity-locked exit status is crucial here: the specialist is structurally mobile (could retrain, could move disciplines) but identity-locked (their self-concept is constituted through specialization), making the biographical-time classification Rope (perceives as changeable in principle) rather than Mountain (perceives as immutable) — yet they experience Snare-level extraction because the identity frame prevents them from exercising their structural mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognizing that specialization is genuinely a mixed coordination-extraction hybrid (Tangled Rope). The mistake is attributing it to either pure coordination (Rope) or pure extraction (Snare). Rope analysis undercounts the extraction (specialist mobility costs, credentialing gatekeeping, knowledge siloing). Snare analysis undercounts the genuine coordination benefits (knowledge accumulation, quality standards, methodological rigor) and ignores the beneficiary perspective. The tangled_rope classification accurately reflects that both functions exist simultaneously: specialization genuinely enables deep knowledge coordination AND genuinely extracts from practitioners through gatekeeping, identity lock, and labor market narrowing. The resolution mechanism is not 'choose which is correct' but 'recognize that the constraint serves both functions and calibrate policy accordingly.' The scaffold perspective reveals that the extraction component is not immutable — alternative credentialing and team-based research reduce extraction without eliminating coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    depth_breadth_tradeoff_necessity,
    'Is the depth-breadth tradeoff inherent to human cognition and knowledge accumulation, or is it a contingent feature of credentialing systems and institutional structures?',
    'Empirical analysis of specialists who successfully maintain breadth (polymath careers, meta-researchers); historical analysis of periods with different specialization norms; cognitive science studies on attention allocation in deep learning systems.',
    'If inherent: specialization trap is a mountain — no exit without sacrificing depth. If contingent: trap is institutional design choice — could be restructured through alternative credentialing, team-based research, and sabbatical/retraining systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_breadth_tradeoff_necessity, empirical, 'Whether depth-breadth tradeoff is inherent or institutional').

omega_variable(
    identity_lock_mechanism_in_expertise,
    'When a specialist claims they ''cannot exit'' their domain, is the binding mechanism primarily identity fusion (cognitive frame) or structural/material (career path dependence, financial constraints)?',
    'Longitudinal study of specialists who changed domains; analysis of stated reasons for immobility vs. actual barriers; comparison of exit rates when material barriers are removed (sabbaticals, retraining funding, age-neutral hiring) vs. identity-lock predictions.',
    'If identity lock predominates: frame-breaking intervention (identity coaching, peer examples, retraining programs) could enable exit. If material barriers predominate: exit requires systemic change (pension portability, age-neutral hiring, credential recognition across fields).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_expertise, empirical, 'Whether expertise immobility is driven by identity lock or material barriers').

omega_variable(
    generalist_effectiveness_in_complex_systems,
    'Are generalist approaches (T-shaped skills, problem-based teams, interdisciplinary synthesis) actually more effective at solving novel problems than specialized experts working in collaboration?',
    'Comparative analysis of innovation outcomes from generalist vs specialist approaches; innovation speed, robustness, and unexpected application discovery; controlled studies of team composition effects on problem-solving.',
    'If generalists more effective: scaffold perspective is correct — specialization trap can be unwound. If specialists essential: tangled rope is the stable equilibrium — extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generalist_effectiveness_in_complex_systems, empirical, 'Whether generalist approaches outperform specialist approaches on novel problems').

omega_variable(
    credentialing_extraction_vs_quality_assurance,
    'How much of credentialing system suppression serves quality assurance vs. credentialing body gatekeeping and rent extraction?',
    'Comparison of licensure test content vs. demonstrated competence requirements; analysis of licensing system resistance to alternative credentials or pathway recognition; correlation between licensing strictness and public protection outcomes.',
    'If primarily quality assurance: suppression is justified coordination cost (lower extractiveness). If primarily gatekeeping: suppression is extractive overhead (higher extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_extraction_vs_quality_assurance, empirical, 'Proportion of credentialing suppression attributable to quality vs. gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expertise_specialization_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exspec_tr_t0, expertise_specialization_trap, theater_ratio, 0, 0.32).
narrative_ontology:measurement(exspec_tr_t15, expertise_specialization_trap, theater_ratio, 15, 0.4).
narrative_ontology:measurement(exspec_tr_t30, expertise_specialization_trap, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(exspec_be_t0, expertise_specialization_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exspec_be_t15, expertise_specialization_trap, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(exspec_be_t30, expertise_specialization_trap, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expertise_specialization_trap, identity_coordination).
narrative_ontology:affects_constraint(expertise_specialization_trap, knowledge_siloing_paradigm_lag).
narrative_ontology:affects_constraint(expertise_specialization_trap, mid_career_retraining_barriers).
narrative_ontology:affects_constraint(expertise_specialization_trap, credentialing_gatekeeping_rent).

% DUAL FORMULATION NOTE:
% Expertise specialization trap is upstream of three distinct structural constraints: knowledge siloing (ε ≈ 0.42, epistemological), mid-career retraining barriers (ε ≈ 0.68, economic/institutional), and credentialing gatekeeping (ε ≈ 0.55, institutional). Each story has its own ε because each isolates a different coordination-failure mechanism. The present story (ε = 0.58) is the parent constraint encompassing all three at the institutional/professional level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
