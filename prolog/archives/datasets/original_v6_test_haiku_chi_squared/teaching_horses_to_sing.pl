% ============================================================================
% CONSTRAINT STORY: teaching_horses_to_sing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_teaching_horses_to_sing, []).

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
 *   constraint_id: teaching_horses_to_sing
 *   human_readable: The Sing-or-Die Gambit
 *   domain: political/power_dynamics
 *
 * SUMMARY:
 *   The Sing-or-Die Gambit is a classical narrative structure in which a
 *   condemned prisoner negotiates a stay of execution in exchange for an
 *   impossible promise. The thief offers to teach the monarch's horse to sing
 *   within one year; if successful, the thief goes free; if unsuccessful, the
 *   thief is executed. The constraint operates as a structural snare: it
 *   creates the appearance of escape while ensuring extraction of hope, time,
 *   and public spectacle. The thief gains a 12-month reprieve at the cost of
 *   a false promise that will inevitably result in execution and public
 *   humiliation. The monarch gains theater (demonstrating mercy and
 *   confidence in authority) while extracting deference, submission to an
 *   impossible test, and the performative confirmation of absolute
 *   sovereignty. The court gains institutional legitimacy through the
 *   spectacle of rational deliberation. The public gains narrative
 *   entertainment and a cautionary tale about the limits of cleverness
 *   against authority. The constraint's suppression (0.72) comes from the
 *   death sentence as enforcement; its extractiveness (0.58) comes from the
 *   asymmetry of power and the impossibility of the condition; its theater
 *   (0.68) comes from the performative framing of mercy and wisdom. Over the
 *   12-month interval, extractiveness increases (0.35→0.72) as the thief's
 *   situation becomes more desperate and the impossibility of the promise
 *   becomes undeniable. Theater increases (0.45→0.68) as the court performs
 *   its role and the narrative takes on ceremonial weight.
 *
 * KEY AGENTS:
 *   - Condemned Thief: Primary victim (powerless/trapped) — under death sentence; trades false hope for 12-month reprieve; ultimately executed or humiliated
 *   - Monarch: Primary beneficiary and actor (powerful/mobile) — extracts deference, theater, and confirmation of absolute sovereignty; simultaneously trapped by the need to maintain authority
 *   - Court and Nobility: Secondary institutional actors (organized/constrained) — trapped by precedent and the need to maintain the performance of rationality
 *   - Public (Spectators): Secondary victims/witnesses (moderate/constrained) — consume the spectacle while suspending disbelief about the impossibility of the condition
 *   - Institution of Sovereignty: Structural beneficiary (institutional/arbitrage) — gains legitimacy and confirmation through the ritual; benefits persist long after individual enforcement atrophies
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a structural mechanism for extracting deference through impossible demands
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(teaching_horses_to_sing, 0.58).
domain_priors:suppression_score(teaching_horses_to_sing, 0.72).
domain_priors:theater_ratio(teaching_horses_to_sing, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(teaching_horses_to_sing, extractiveness, 0.58).
narrative_ontology:constraint_metric(teaching_horses_to_sing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(teaching_horses_to_sing, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(teaching_horses_to_sing, snare).
narrative_ontology:human_readable(teaching_horses_to_sing, "The Sing-or-Die Gambit").
narrative_ontology:topic_domain(teaching_horses_to_sing, "political/power_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_victim(teaching_horses_to_sing, condemned_thief).
narrative_ontology:constraint_victim(teaching_horses_to_sing, collective_rationality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED THIEF (SNARE) — Faces execution. The gambit appears as salvation but is structurally a snare: extraction of hope under false pretense. Cannot refuse without immediate death; cannot succeed; cannot negotiate exit. d≈0.98, f(d)≈1.41, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MONARCH (SNARE) — Possesses formal power but becomes trapped by vanity and the need to maintain authority. If the horse sings, impossible credit accrues to the thief (threatens legitimacy); if it doesn't, executing the thief admits the bargain was theater (threatens authority). Simultaneously benefits from the constraint (extracting deference and public spectacle) and trapped by it (cannot exit without loss of face). d≈0.52, f(d)≈0.65, σ=0.8 → χ≈0.30.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE COURT AND NOBILITY (SNARE) — Organized actors (judges, nobles, advisors) are trapped by precedent and the need to appear rational. The bargain was publicly made; its theater is now institutional fact. They benefit from the spectacle and from demonstrating the monarch's mercy and wisdom, but are trapped by the impossible condition. Cannot advise the monarch to execute the thief without admitting the bargain was extractive theater. d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE PUBLIC (TANGLED ROPE) — Witnesses to the bargain. Experiences both coordination and extraction. Benefits from the spectacle, the display of royal mercy, and the entertainment value. Constrained by institutional norms requiring them to suspend disbelief (coordination function: accepts the bargain as structurally meaningful). Also bears cost: invests emotional and narrative capital in an outcome known to be impossible, experiencing collective delusion. d≈0.58, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(teaching_horses_to_sing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE INSTITUTION OF SOVEREIGNTY (PITON) — The bargain itself has become a performative ritual in folklore and pedagogy. The constraint persists as inert institutional memory (teaching wisdom about mortality, royal authority, the limits of cleverness) long after its functional extraction has dissipated. Theater ratio≈0.68 reflects that the actual power dynamics have degraded to symbolic reenactment. The institution benefits from the narrative (demonstrates absolute sovereignty) but the real enforcement mechanism is atrophied — modern retellings are cautionary tales, not active coercive tools.
constraint_indexing:constraint_classification(teaching_horses_to_sing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, the gambit is a structural snare with no natural law justification. It reveals how authority creates extraction mechanisms through the asymmetry of power and the performative closure of impossible demands. The constraint is maintained by suppression (legal authority + threat of death) and theater (public spectacle + narrative plausibility). d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(teaching_horses_to_sing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(teaching_horses_to_sing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(teaching_horses_to_sing, TR),
    TR >= 0.70.

:- end_tests(teaching_horses_to_sing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The thief gives up 12 months of life and ultimately faces execution or humiliation; the monarch extracts deference, public spectacle, and confirmation of authority; the court extracts institutional legitimacy; the public extracts narrative entertainment. However, extractiveness is not maximal (0.72) because the thief retains agency in the gambit itself (the promise is the thief's invention, not imposed by the monarch) and because the extraction is performative rather than material. The thief trades hope for time, not wealth or freedom. Suppression (0.72): High. The death sentence is a credible enforcement threat; the thief cannot refuse without immediate execution; the thief cannot break the promise without execution at the appointed time. The thief is trapped by legal authority and the asymmetry of power. Theater (0.68): Moderate-high. The spectacle of the bargain, the performance of royal mercy, the ceremonial progression toward the inevitable conclusion — these constitute a substantial performative component. The court knows the condition is impossible; the public knows the condition is impossible; the thief may or may not know. The constraint persists through theater as much as through coercion.
 *
 * PERSPECTIVAL GAP:
 *   The thief sees execution postponed (snare: temporary reprieve at the cost of a false promise). The monarch sees sovereignty confirmed (snare: extraction of deference and theater; simultaneously trapped by the need to maintain authority). The court sees institutional legitimacy (snare: forced to participate in a ritual that exposes the impossibility of rational deliberation within absolute monarchy). The public sees a narrative closure (tangled rope: consumption of spectacle while bearing the cost of collective delusion about the possible). The institution of sovereignty sees perpetuation of itself (piton: theater persists as cautionary tale long after the specific enforcement mechanism disappears). The analytical observer sees a structural mechanism for extracting submission through impossible demands (snare: universal pattern visible in all power asymmetries).
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned thief: Victim + trapped → d≈0.98, f(d)≈1.41. Maximal directionality as target. No exit options; under death sentence. Monarch: Powerful + mobile + beneficiary (extracts deference and theater) AND victim (trapped by need to maintain authority) → d≈0.52, f(d)≈0.65. Moderate directionality reflecting dual role. Court and nobility: Organized + constrained + victim (forced to rationalize impossible condition) → d≈0.65, f(d)≈1.00. High directionality reflecting constraint despite organizational capacity. Public: Moderate + constrained + mixed (benefit from spectacle; bear cost of delusion) → d≈0.58, f(d)≈0.75. Moderate-high directionality reflecting mixed role. Institution of sovereignty: Institutional + arbitrage + beneficiary → d≈0.00, f(d)≈-0.12. Net beneficiary through persistent narrative legitimacy. Analytical observer: Analytical → d≈0.70, f(d)≈1.12. High directionality reflecting observer's structural position outside the constraint but able to measure its mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves potential mandatrophy by displaying it fully. The thief experiences it as a snare (extraction of hope). The monarch experiences it as both snare (extracts deference) and tangled rope (trapped by authority). The court experiences it as pure snare (forced participation in ritual that exposes its own limits). The public experiences it as tangled rope (benefits from spectacle; bears cost of delusion). The institution experiences it as piton (theater persists long after functional extraction atrophies). No single type is correct — the constraint is a presheaf of readings. The mandatrophy is not resolved by choosing a type but by recognizing that the constraint's power lies precisely in enabling multiple, non-communicating readings of the same structural fact. The thief believes (or pretends to believe) the horse might sing; the monarch knows it cannot; the court performs certainty; the public suspends disbelief; the institution preserves the narrative. Each agent's reading validates the others' participation in the ritual. This is not a defect in the classification system but a deep feature of how authority-based constraints work: they require asymmetric knowledge and asymmetric readings to maintain their coercive force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thief_rational_expectation,
    'Does the thief rationally believe the horse might sing, or does the thief rationally accept death while extracting a one-year reprieve?',
    'Historical or textual analysis of the thief''s stated motivations; comparison with versions where the thief''s intent is explicitly revealed',
    'If belief is genuine: constraint is a tragedy (impossible hope). If rational acceptance: constraint is pure extraction of time (thief extracts 12 months of life for the cost of a false promise). If mixed: thief extracts time while the monarch extracts theater, making it symmetric snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thief_rational_expectation, conceptual, 'Whether the thief believes in the promise or rationally accepts death').

omega_variable(
    monarch_knowledge_of_impossibility,
    'Does the monarch believe the horse might sing, or does the monarch know the condition is impossible and structures the bargain as pure extraction?',
    'Textual analysis of the monarch''s speeches and asides; comparison across folklore versions; analysis of the monarch''s behavior after one year',
    'If monarch genuinely uncertain: constraint is tangled rope (both parties are trapped by shared delusion). If monarch knows: constraint is snare (monarch is pure extractor, using authority to extract deference and theater from the thief).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monarch_knowledge_of_impossibility, conceptual, 'Whether the monarch believes the promise is possible').

omega_variable(
    suppression_enforcement_mechanism,
    'What makes the thief unable to break the promise and flee? Is it legal authority (execution threat enforced), or social authority (inability to break a sworn bargain with the monarch), or resignation (rational acceptance of death after one year)?',
    'Analysis of historical enforcement: do thieves who break such bargains face capture/execution, or do they successfully flee? Analysis of cultural narratives about oath-breaking with sovereigns.',
    'If legal enforcement: suppression is institutional (0.72 is justified). If social enforcement: suppression is cultural (may be weaker; theater ratio higher). If resignation: suppression is psychological (thief is self-enforcing; constraint is piton not snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_enforcement_mechanism, empirical, 'The enforcement mechanism that binds the thief to the impossible promise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(teaching_horses_to_sing, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ths_tr_t0, teaching_horses_to_sing, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ths_tr_t6, teaching_horses_to_sing, theater_ratio, 6, 0.6).
narrative_ontology:measurement(ths_tr_t12, teaching_horses_to_sing, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(ths_be_t0, teaching_horses_to_sing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ths_be_t6, teaching_horses_to_sing, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ths_be_t12, teaching_horses_to_sing, base_extractiveness, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(teaching_horses_to_sing, enforcement_mechanism).
narrative_ontology:affects_constraint(teaching_horses_to_sing, absolute_monarchy_legitimacy).
narrative_ontology:affects_constraint(teaching_horses_to_sing, impossible_standards_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(teaching_horses_to_sing, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
