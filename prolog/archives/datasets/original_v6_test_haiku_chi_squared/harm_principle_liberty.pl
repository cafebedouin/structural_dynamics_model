% ============================================================================
% CONSTRAINT STORY: harm_principle_liberty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harm_principle_liberty, []).

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
 *   constraint_id: harm_principle_liberty
 *   human_readable: Mill's Harm Principle as a Social Constraint
 *   domain: political/social/legal
 *
 * SUMMARY:
 *   Mill's Harm Principle, as articulated in 'On Liberty' (1859), proposes a
 *   meta-constraint on legitimate coercion: society may only use coercive
 *   power to prevent harm to others; self-regarding conduct cannot be
 *   legitimately restricted. This constraint generates a structural tension
 *   that reveals itself across six distinct indexical perspectives. For some
 *   agents (autonomous individuals in resourced contexts), the principle
 *   functions as a pure coordination mechanism establishing their right to
 *   liberty. For others (vulnerable populations whose harm the principle
 *   excludes from justifying intervention), it becomes a snare that
 *   systematizes their exclusion from protection. For institutional actors
 *   (liberal states, legal systems), the principle provides both a
 *   coordination function (legitimacy boundary) and an extraction mechanism
 *   (systematic underprotection of those deemed incapable of full consent or
 *   whose harm falls outside the principle's definition). The principle's
 *   theater_ratio has increased over 170 years as legal systems cite it while
 *   continuously redefining 'harm' and 'consent' to accommodate practical
 *   necessities — a classic piton pattern of performative citation masking
 *   degraded function. The constraint exemplifies mandatrophy: it was
 *   designed to resolve a real coordination problem (preventing tyranny of
 *   the majority against individual liberty) but has become entangled with
 *   extraction (protection of liberty-capable actors at the cost of others)
 *   without fully resolving either function.
 *
 * KEY AGENTS:
 *   - Vulnerable Populations: Primary victims (powerless/trapped) — children, mentally incapacitated individuals, domestic abuse victims whose harm the principle excludes from justifying intervention
 *   - Harm Prevention Advocates: Mixed (moderate/constrained) — public health officials, safety regulators, domestic violence advocates who operate under both the principle's coordination (clear jurisdiction) and its extraction (unable to prevent foreseeable harms)
 *   - Liberal State: Primary beneficiary (institutional/arbitrage) — uses the principle as a coordination framework for establishing legitimate coercion boundaries and liberal democracy legitimacy
 *   - Legal Establishment: Institutional actor (institutional/constrained) — courts cite the principle for legitimacy while redefining 'harm' and 'consent' to accommodate practical necessities, creating theater
 *   - Autonomous Individuals: Secondary beneficiary (powerful/mobile) — those with resources and capacity to exercise autonomy benefit from the principle as pure coordination protection
 *   - Organized Harm Prevention Coalition: Organized victims (organized/constrained) — groups advocating for intervention in harm cases have visibility but no power to override the principle's constraints
 *   - Analytical Observer: Civilizational (analytical/analytical) — can see both the coordination function (preventing tyranny) and the extraction mechanism (systematizing vulnerability) in a single constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harm_principle_liberty, 0.52).
domain_priors:suppression_score(harm_principle_liberty, 0.62).
domain_priors:theater_ratio(harm_principle_liberty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harm_principle_liberty, extractiveness, 0.52).
narrative_ontology:constraint_metric(harm_principle_liberty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(harm_principle_liberty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harm_principle_liberty, tangled_rope).
narrative_ontology:human_readable(harm_principle_liberty, "Mill's Harm Principle as a Social Constraint").
narrative_ontology:topic_domain(harm_principle_liberty, "political/social/legal").

domain_priors:requires_active_enforcement(harm_principle_liberty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harm_principle_liberty, liberty_advocates).
narrative_ontology:constraint_beneficiary(harm_principle_liberty, individual_autonomy_defenders).
narrative_ontology:constraint_victim(harm_principle_liberty, harm_prevention_mandate).
narrative_ontology:constraint_victim(harm_principle_liberty, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE VICTIM (SNARE) — Those whose harm cannot be prevented due to the principle's constraints (domestic abuse victims, children, minorities facing discrimination). Trapped by the principle's application in their context; cannot exit or organize. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(harm_principle_liberty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HARM PREVENTION ADVOCATE (TANGLED ROPE) — Public health officials, safety regulators, and social workers operate under both the coordination benefit (a clear principle that establishes jurisdiction) and extraction cost (unable to prevent foreseeable harms due to the principle's constraints). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LIBERAL STATE (ROPE) — Institutions committed to liberal democracy benefit from the principle as a coordinating framework that establishes clear legitimacy boundaries for coercion. The principle enables the state to justify intervention in some domains while prohibiting it in others — a pure coordination mechanism. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01. Net beneficiary through legitimacy coordination.
constraint_indexing:constraint_classification(harm_principle_liberty, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGAL ESTABLISHMENT (PITON) — Courts and legal systems cite Mill's principle as justification for specific rulings, but the principle's application is highly performative. The theater_ratio=0.68 reflects that citing 'the harm principle' in court does not resolve harm disputes — it merely frames them. Legal systems maintain rhetorical commitment to the principle while continuously redefining 'harm' and 'consent' to accommodate practical necessities. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(harm_principle_liberty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED HARM PREVENTION (SNARE) — Organized groups (domestic violence advocates, public health coalitions, child protection agencies) face systematic extraction: their demands for preventive intervention are repeatedly constrained by the principle, but they cannot fully exit or override it without losing institutional legitimacy. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.57. This perspective reveals the snare dynamic: organization gives them visibility but not power to break the constraint.
constraint_indexing:constraint_classification(harm_principle_liberty, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDIVIDUAL EXERCISING LIBERTY (ROPE) — Those with resources and agency to exercise autonomy benefit from the principle as pure coordination: it establishes their right to pursue interests without coercive intervention. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.08. Relatively beneficiary; the principle is a pure coordination good for this agent.
constraint_indexing:constraint_classification(harm_principle_liberty, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the principle exhibits genuine coordination function (establishing legitimate coercion limits) AND systematic extraction (protection of autonomy-capable actors at the cost of unprotected vulnerable populations). The tension is not an implementation failure but a structural feature. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Symmetric harm and benefit reveal the hybrid nature.
constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_principle_liberty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harm_principle_liberty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harm_principle_liberty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harm_principle_liberty, TR),
    TR >= 0.70.

:- end_tests(harm_principle_liberty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The principle creates systematic asymmetry: those capable of autonomous decision-making are protected; those deemed incapable (due to age, cognition, information asymmetry, or diminished capacity) are excluded from that protection. This is extractive — protection is contingent on capacity. The extraction is not maximal (0.66+) because the principle does allow intervention when consent cannot be given, and many jurisdictions have expanded the definition of 'harm' over time. The trajectory from 0.38 (1859) to 0.52 (present) reflects increasing application to edge cases (surveillance harms, collective action problems, psychological harms) that enlarge the principle's scope but also its ambiguity. Suppression (0.62): Moderate-high. Vulnerable populations face multiple barriers to exit or organize: legal subordination, cognitive/developmental barriers, and the principle's own framing that legitimates their exclusion. The principle suppresses alternative framings (perfectionist ethics, communitarianism, capabilities approaches) by establishing 'harm to others' as the sole legitimate justification. Theater ratio (0.68): High. Courts cite the principle to justify outcomes, but the principle's core disputes (what counts as harm? when is consent valid?) are not resolved by invoking the principle — they are deferred. The theatrical element has grown as more diverse harms (psychological, informational, collective) have been claimed under the banner, requiring continuous redefining. The principle provides a citation framework without a mechanism for resolving the definitional disputes.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable victim sees a snare (d≈0.92, χ≈0.72): trapped, unprotected, excluded by definition. The harm prevention advocate sees tangled rope (d≈0.68, χ≈0.55): benefits from the principle's clarity but systematically constrained from acting. The liberal state sees rope (d≈0.15, χ≈0.01): the principle is pure coordination legitimacy. The legal establishment sees piton (d≈0.35, χ≈0.24): the principle is cited but not enforced; its application is performative. The autonomous individual sees rope (d≈0.25, χ≈0.08): protection as coordination good. The analytical observer sees tangled rope (d≈0.50, χ≈0.34): both coordination and extraction in a single structure. The perspectival gap between the vulnerable victim's snare and the liberal state's rope reveals that the principle functions differently for those capable of autonomous decision-making versus those deemed incapable — this is not an implementation failure but the principle's structural logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations: Victims (excluded from protection by the principle) + trapped (cannot exit legal subordination) → d≈0.92, f(d)≈1.38. Maximum extraction. Harm prevention advocates: Victims (constrained from intervention) + constrained (must work within the principle's framework) → d≈0.68, f(d)≈1.05. High extraction. Liberal state: Beneficiary (gains legitimacy coordination) + arbitrage (can opt into or out of enforcement via reinterpretation) → d≈0.15, f(d)≈0.02. Net beneficiary. Legal establishment: Institutional actor constrained by precedent and citation obligation + benefits from performing legitimacy → d≈0.35, f(d)≈0.35. Moderate; piton classification comes from theater gate (0.68) not from high chi. Autonomous individuals: Beneficiary (protected from coercion) + mobile (have exit options) → d≈0.25, f(d)≈0.15. Beneficiary. Analytical observer: Neutral position seeing both coordination (legitimacy of coercion limits) and extraction (protection disparities) → d≈0.50, f(d)≈0.65. Symmetric; neither full beneficiary nor full victim.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits mandatrophy because it was designed to solve a real coordination problem (preventing tyranny and establishing liberty rights in liberal society) but has accumulated extraction layers (systematically differential protection based on ascribed capacity, with the principle itself justifying the exclusions). The mandatrophy is resolved NOT by claiming the principle is 'really' a snare or 'really' a rope, but by recognizing that it is a tangled rope with different ε values and classifications depending on the structural position of the observer. For the liberal state and autonomous individuals, the principle achieves coordination (rope) with acceptable extraction costs. For vulnerable populations and harm prevention advocates, the principle fails to solve their coordination problem — instead, it systematizes their exclusion while maintaining legitimacy rhetoric. The theater_ratio trajectory (0.52→0.68) shows that as the principle's scope has expanded to include more diverse harm definitions, the theatrical element (citing the principle without resolving the definitional disputes) has increased. This is a piton signal: the principle persists due to institutional inertia and legitimacy theater, not because it resolves the underlying disputes about consent, capacity, and collective harm. The mandatrophy is structural: no single modification to 'harm' definition or 'consent' threshold will resolve the tension — the principle systematically advantages liberty-capable agents while excluding others, and this is not a bug in the principle but its constitutive feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_boundary,
    'What constitutes ''harm to others''? Is psychological harm, financial loss, reputation damage, or disrupted preference satisfaction included, or only physical injury?',
    'Comparative legal analysis across jurisdictions; empirical measurement of how courts and regulators define harm in case law',
    'Broad harm definition: principle becomes a snare (nearly everything can be justified as harm prevention). Narrow harm definition: principle becomes a rope (genuinely constrains coercion, but allows foreseeable injuries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_boundary, conceptual, 'Definitional scope of ''harm to others''').

omega_variable(
    consent_capability_threshold,
    'At what developmental, cognitive, or informational stage can an individual give valid consent to actions that might harm them? Does the principle protect those deemed incapable of consent?',
    'Longitudinal analysis of consent doctrine in law (children, mental incapacity, unconscionability); comparison with Mill''s original text on paternalism exceptions',
    'High threshold (strict consent requirement): creates snare for those deemed incapable (children, mentally ill, cognitively impaired). Low threshold: principle becomes a rope that genuinely protects autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_capability_threshold, conceptual, 'Threshold for valid consent in harm principle').

omega_variable(
    collective_vs_individual_harm,
    'Does the harm principle apply to harms that affect groups collectively but not individuals in isolation (public goods depletion, cultural loss, norm degradation)?',
    'Analysis of how courts handle commons dilemmas, collective action problems, and aggregate harms; comparison with Mill''s text on justice and utilities of society',
    'If collective harms count: principle becomes extractive (can justify coercion to prevent diffuse harm). If only individual harms count: principle protects liberty but enables tragedy of commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_harm, conceptual, 'Whether collective harms trigger the principle').

omega_variable(
    harm_prevention_alternative_availability,
    'When alternative harm prevention methods exist (education, treatment, social support), does the principle require exhausting them before resorting to coercion?',
    'Empirical study of regulatory hierarchies; analysis of case law requiring graduated responses; comparison across countries with explicit ''least restrictive means'' doctrines',
    'If alternatives required: principle becomes a rope (genuine constraint on coercion). If alternatives optional: principle becomes piton (cited but not enforced), enabling theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_prevention_alternative_availability, empirical, 'Whether exhaustion of alternatives is required').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harm_principle_liberty, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_tr_t0, harm_principle_liberty, theater_ratio, 0, 0.52).
narrative_ontology:measurement(harm_tr_t85, harm_principle_liberty, theater_ratio, 85, 0.62).
narrative_ontology:measurement(harm_tr_t170, harm_principle_liberty, theater_ratio, 170, 0.68).

% Extraction over time
narrative_ontology:measurement(harm_be_t0, harm_principle_liberty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(harm_be_t85, harm_principle_liberty, base_extractiveness, 85, 0.48).
narrative_ontology:measurement(harm_be_t170, harm_principle_liberty, base_extractiveness, 170, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harm_principle_liberty, enforcement_mechanism).
narrative_ontology:affects_constraint(harm_principle_liberty, liberalism_state_legitimacy).
narrative_ontology:affects_constraint(harm_principle_liberty, autonomy_paternalism_tension).
narrative_ontology:affects_constraint(harm_principle_liberty, consent_doctrine_drift).

% DUAL FORMULATION NOTE:
% Mill's Harm Principle can be decomposed into three structurally distinct constraints: (1) the legitimacy boundary for coercion (pure coordination), (2) the capacity threshold for consent (definional), and (3) the scope of 'harm' (scope creep). These are linked by network: the legitimacy boundary depends on the capacity threshold, which depends on the harm definition. A separate constraint story on 'capacity_consent_doctrine' would have higher ε reflecting the ongoing redefiniton disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harm_principle_liberty, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
