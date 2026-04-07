% ============================================================================
% CONSTRAINT STORY: cognitive_diversity_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Cognitive diversity arbitrage in the workplace represents a contemporary
 *   shift in how organizations frame neurodivergence — from medical pathology
 *   requiring accommodation to strategic asset requiring extraction. This
 *   constraint exhibits a six-perspective structure revealing how the same
 *   organizational practice (hiring and integrating neurodivergent workers)
 *   creates fundamentally different constraints for different agents.
 *   Corporate leadership coordinates talent acquisition and diversity
 *   signaling. Neurodivergent employees experience suppressed extraction: the
 *   reframing removes stigma but replaces medical accommodation with
 *   productivity demands, all while the appearance of inclusion prevents
 *   organizing or exit. Disability rights organizations experience
 *   institutional capture: the neurodiversity-as-asset narrative redirects
 *   resources and framing away from access/justice toward
 *   productivity/innovation. The medical diagnostic system (DSM-5) degrades
 *   into theater as it persists as gatekeeping authority despite losing core
 *   function. The neurodiversity advocacy movement sees a temporary
 *   extractive phase that will fade as neurodivergence becomes mainstream
 *   identity rather than corporate asset. The analytical observer risks
 *   naturalizing corporate extraction as an inevitable feature of labor
 *   markets and cognitive variation. The constraint's extractiveness has
 *   risen from 0.32 to 0.52 over the interval as corporate programs matured
 *   and performative elements solidified, while theater ratio rose from 0.48
 *   to 0.68 as actual accommodation infrastructure lagged behind public
 *   commitments.
 *
 * KEY AGENTS:
 *   - Corporate Leadership: Primary beneficiary (institutional/arbitrage) — captures talent, ESG metrics, and diversity signaling; can exit or shift strategy if market changes
 *   - Neurodivergent Employees: Primary victim (powerless/trapped) — economic necessity prevents exit; reframing removes legal/medical claims; suppressed organizing capacity
 *   - Disability Rights Organizations: Secondary victim/organized actor (organized/constrained) — funded dependencies create capture risk; neurodiversity narrative redirects from access/justice frame
 *   - Accessibility Advocates/Unions: Secondary organized actor (organized/constrained) — gain voice in accommodation policies but have limited enforcement power
 *   - Neurotypical Workforce: Secondary moderate actor (moderate/mobile) — benefit from cognitive complementarity; experience mild status extraction; retain exit options
 *   - Medical/Psychiatric System: Institutional actor (institutional/arbitrage) — diagnosis system persists as gatekeeping authority despite reduced core function (piton)
 *   - Neurodiversity Advocacy Movement: Organized reformer (organized/constrained) — pushing toward structural workplace redesign with sunset logic; constrained by resources
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing contingent extraction as inevitable labor market feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_diversity_arbitrage, 0.52).
domain_priors:suppression_score(cognitive_diversity_arbitrage, 0.65).
domain_priors:theater_ratio(cognitive_diversity_arbitrage, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_diversity_arbitrage, tangled_rope).
narrative_ontology:human_readable(cognitive_diversity_arbitrage, "Cognitive Diversity Arbitrage in the Workplace").
narrative_ontology:topic_domain(cognitive_diversity_arbitrage, "economic/social").

domain_priors:requires_active_enforcement(cognitive_diversity_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, corporate_leadership).
narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, neurotypical_workforce).
narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, neurodivergent_employees).
narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, workplace_accessibility_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEURODIVERGENT EMPLOYEE (SNARE) — Trapped by economic necessity. The reframing of neurodiversity from pathology to asset creates an appearance of acceptance while intensifying demands for productivity. Disclosure of neurodivergence offers no concrete accommodation; instead, the employee becomes an accessible intellectual resource. Suppression is severe: the appearance of inclusion prevents organizing, legal claims, or exit. No alternatives without economic catastrophe.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACCESSIBILITY ADVOCATE/UNION (TANGLED ROPE) — Constrained by corporate gatekeeping of neurodiversity programs. Experiences both coordination benefit (gaining formal voice in accommodation policies) and asymmetric extraction (concessions are performative; enforcement is minimal). Cannot fully exit corporate structures but has limited power to negotiate terms.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORPORATE LEADERSHIP (ROPE) — Experiences the constraint as pure coordination. Neurodiversity hiring programs solve talent acquisition problems while generating positive ESG metrics and diversity signaling. Leadership has arbitrage options: can shift investment to different workforce strategies if neurodiversity becomes unfashionable. Net beneficiary.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISABILITY RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized but constrained by funding dependencies on the same corporate sector driving the arbitrage. Experience coordination benefit (corporate resources for awareness) and extraction (institutional capture: neurodiversity framing disempowers the disability rights framework and redirects resources toward productivity enhancement rather than structural access).
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL/PSYCHIATRIC DIAGNOSTIC SYSTEM (PITON) — The DSM-5 classification system for ADHD, autism spectrum disorder, and related conditions persists as formal authority despite its neurodiversity critique. Theater ratio is high: diagnosis primarily functions as gatekeeping for employment and legal claims rather than enabling actual treatment or accommodation. The system has lost core function (supporting individual wellbeing) but maintains institutional authority (controlling who counts as 'officially' neurodivergent).
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NEUROTYPICAL WORKFORCE (TANGLED ROPE) — Mobile exit options but also benefits from neurodiversity programs (cognitive complementarity, innovation signals). Experience modest extraction: neurodiversity framing creates subtle status hierarchies ('we're inclusive of brilliant autistic programmers' vs. 'we hire neurotypical middle management'). Modest suppression: can exit if workplace becomes too neurodiversity-focused, though most do not.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: NEURODIVERSITY ADVOCACY MOVEMENT (SCAFFOLD) — Organized movement framing neurodiversity as identity and strength rather than disorder. See the corporate arbitrage as a temporary phase — stage of institutional transition toward genuine inclusion and accommodation. Sunset logic: as neurodiversity becomes mainstream identity (not corporate asset), the extractive framing loses power. Constrained by resource limitations but pushing toward structural change in workplace design itself.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risks naturalizing the constraint as inevitable: neurodiversity and neurotypicality are natural cognitive variations; markets will always arbitrage valuable variations; some degree of exploitation is inherent to labor markets. However, the structural data contradicts this — the extractiveness and suppression reflect specific corporate practices (non-disclosure agreements, lack of true accommodation infrastructure, performative DEI) that are contingent, not natural.
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
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderately high. The corporate sector extracts significant value from neurodivergent cognitive styles (pattern recognition, hyperfocus, unconventional problem-solving) without proportional accommodation or compensation. However, the value is real — neurodivergent employees do contribute genuinely valuable cognitive work — so extractiveness is not maximal (0.72+). The rise from 0.32 to 0.52 over the interval reflects escalating extraction as initial hiring efforts (genuine onboarding phase) gave way to normalized assumptions that neurodivergent workers will self-accommodate. Suppression (0.65): High. The reframing from pathology to asset creates a subtle but severe form of suppression: neurodivergent workers cannot claim accommodation as a right (medical/legal frame) without losing the 'asset' framing that justified hiring them; cannot exit without economic catastrophe; cannot organize effectively because the inclusive narrative delegitimizes collective action ('why organize if you're valued as an asset?'). Theater ratio (0.68): High-moderate. Corporate neurodiversity programs feature substantial performative content: public commitment to inclusion outpaces actual accommodation infrastructure; DEI programs emphasize hiring optics over workplace redesign; diagnostic gatekeeping persists despite neurodiversity critique.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Corporate leadership sees Rope (pure coordination: 'we're solving talent acquisition and innovation problems'). Neurodivergent employees see Snare (pure extraction: 'we're productivity resources without legal protection or genuine accommodation'). Disability rights organizations see Tangled Rope with institutional capture (mixed coordination and extraction: 'we gain some voice in policy but lose the justice/access frame'). The neurodiversity movement sees Scaffold (temporary extractive phase fading as identity becomes mainstream). The medical system sees Piton (performative gatekeeping). The neurotypical workforce sees Tangled Rope (mild benefits and mild extraction). The analytical observer risks Mountain (naturalizing variation and labor-market arbitrage). This seven-perspective gap (eight counting the false summit) reflects that the constraint is structurally authentic: the reframing from pathology to asset creates genuinely different constraints for agents with different structural positions. The gap is not a measurement error — it is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation shows how different agents experience the same constraint differently. Corporate leadership (institutional/arbitrage): beneficiary status + exit options → d ≈ 0.10 → low/negative f(d) → negative chi. They experience the constraint as beneficial coordination. Neurodivergent employees (powerless/trapped): victim status + no exit + suppressed organizing → d ≈ 0.95 → f(d) ≈ 1.42 → high chi. They experience full extraction. Disability rights organizations (organized/constrained): victim status + constrained exit + funding dependencies → d ≈ 0.65 → f(d) ≈ 1.00 → high chi, but mitigated by organized power. Neurotypical employees (moderate/mobile): both beneficiary (cognitive complementarity) and victim (subtle status extraction) + mobile exit → d ≈ 0.50 → f(d) ≈ 0.65 → moderate chi. The medical system (institutional/arbitrage) has low d despite performing gatekeeping (beneficiary of continued diagnostic authority) but loses core function, classifying as Piton rather than Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY VIA PERSPECTIVAL DECOMPOSITION: The central ambiguity is whether corporate neurodiversity programs are coordination (genuine talent + innovation benefits) or extraction (repackaged exploitation). The Tangled Rope classification resolves this by incorporating both: the programs ARE genuine coordination (neurodivergent cognitive styles do improve team outcomes) AND asymmetric extraction (without accommodation infrastructure, the benefit flows disproportionately to the employer). The Snare perspective (neurodivergent employee) reveals the extraction is severe from the powerless agent's view. The Rope perspective (corporate leadership) reveals genuine coordination from the beneficiary's view. The Piton perspective (medical gatekeeping) reveals that the diagnostic system has lost core function. The Scaffold perspective (neurodiversity movement) reveals that this is a temporary phase with a sunset — as neurodivergence becomes mainstream identity, the 'asset' framing will lose extractive power. The mandatrophy is fully resolved: this is not 'is it coordination or extraction?' but rather 'for whom is it which, and does the extractive phase have a bounded timeline?' The analytics show a rising theater ratio (performative inclusion rising faster than real accommodation) and rising extractiveness (escalation toward Snare), indicating the constraint is unstable — either suppression will trigger organizing (pushing toward genuine Rope) or theater will complete the transition to pure Snare (if accommodation infrastructure never materializes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_vs_performance,
    'Does corporate neurodiversity hiring genuinely accept neurodivergent cognitive styles, or does it simply extract the productivity gains while suppressing the identity aspects that would require structural workplace change?',
    'Longitudinal study of neurodivergent employee retention, advancement, and accommodation implementation rates; comparison of neurodiversity-hiring companies against inclusive-workplace design companies on metrics of sustained employment and equity outcomes',
    'If authentically integrated: constraint classifies as Rope or Scaffold from neurodivergent perspective. If performative extraction: constraint is Snare. Theater ratio difference of ~0.40 reflects this uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_vs_performance, empirical, 'Whether corporate neurodiversity programs represent genuine integration or extractive performance').

omega_variable(
    accommodation_infrastructure_sufficiency,
    'Do corporate neurodiversity programs include genuine accommodation infrastructure (flexible work, sensory-safe environments, clear communication norms) or merely selective hiring of ''high-functioning'' neurodivergent individuals who can mask neurotypical expectations?',
    'Audit of workplace accommodation policies; survey of neurodivergent employees on implementation and impact; analysis of which neurodivergent populations are actually hired vs. which are excluded',
    'If infrastructure genuine: suppression value drops to ~0.35 (moderate). If selective hiring only: suppression remains ~0.65 (severe). This is the key differentiator between Tangled Rope and Snare for the trapped neurodivergent employee.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_infrastructure_sufficiency, empirical, 'Whether true accommodation infrastructure exists or only selective hiring of ''high-functioning'' individuals').

omega_variable(
    disability_rights_capture,
    'Does the corporate neurodiversity narrative (reframing from pathology to asset) actively undermine disability rights advocacy by redirecting neurodivergence away from the access/justice framework toward the productivity/asset framework?',
    'Historical analysis of funding flows, advocacy messaging, and policy priorities before vs. after corporate neurodiversity adoption; interviews with disability rights organizations on institutional pressure and framing shifts',
    'If genuine capture: corporate leadership benefits from neurodiversity reframing while disability rights advocacy is weakened. If compatibility: both frameworks can coexist. This determines whether disability rights organizations experience Rope (coordination) or Tangled Rope (captured extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_rights_capture, conceptual, 'Whether corporate neurodiversity narrative captures and redirects disability rights advocacy').

omega_variable(
    escalation_vs_stabilization,
    'Will corporate neurodiversity arbitrage stabilize at a moderate extraction level (Tangled Rope equilibrium) or escalate toward pure extraction (Snare) as the asset becomes normalized and workplace accommodation investments decline?',
    '5-10 year longitudinal comparison of accommodation budget allocations, career advancement rates for neurodivergent employees, and employee wellbeing metrics across neurodiversity-hiring vs. traditional companies',
    'If stabilizes: current Tangled Rope classification is stable. If escalates: trajectory moves toward Snare, with theater ratio rising as performative inclusion replaces functional accommodation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_vs_stabilization, preference, 'Whether corporate neurodiversity programs stabilize or escalate toward pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_diversity_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cda_tr_t0, cognitive_diversity_arbitrage, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cda_tr_t5, cognitive_diversity_arbitrage, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cda_tr_t10, cognitive_diversity_arbitrage, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cda_be_t0, cognitive_diversity_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cda_be_t5, cognitive_diversity_arbitrage, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cda_be_t10, cognitive_diversity_arbitrage, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_diversity_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(cognitive_diversity_arbitrage, disability_access_infrastructure).
narrative_ontology:affects_constraint(cognitive_diversity_arbitrage, corporate_dei_performativity).
narrative_ontology:affects_constraint(cognitive_diversity_arbitrage, labor_market_skill_arbitrage).

% DUAL FORMULATION NOTE:
% Cognitive diversity arbitrage decomposes into three related constraints: (1) the medical/diagnostic gatekeeping system that defines neurodivergence (Piton, ε≈0.15); (2) the corporate reframing of neurodivergence as asset rather than access requirement (Tangled Rope, ε≈0.52); (3) the broader labor market arbitrage of any underutilized cognitive skill (Rope or Snare depending on worker power). This story focuses on constraint 2 (corporate arbitrage). Constraint 1 (diagnostic gatekeeping) is upstream and provides the categorization system that enables arbitrage. Constraint 3 (labor market skill arbitrage) is a broader context affecting all three. Stories are linked via network.affects_constraints to model this dependency structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_diversity_arbitrage, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
