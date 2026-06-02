% ============================================================================
% CONSTRAINT STORY: unrequited_love_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unrequited_love_protocol, []).

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
 *   constraint_id: unrequited_love_protocol
 *   human_readable: The Limerence Negotiation Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Unrequited love operates as a negotiation protocol that has broken down
 *   at the acceptance/rejection stage. One party (the Suitor) continuously
 *   transmits connection requests — emotional labor, time, validation,
 *   vulnerability — that the second party (the Object) neither fully accepts
 *   nor explicitly rejects. This ambiguity suspension creates an extraction
 *   mechanism: the Object receives benefits (attention, validation, emotional
 *   labor, options preservation) without reciprocal commitment, while the
 *   Suitor bears costs (emotional labor, opportunity cost,
 *   hope-disappointment cycles, identity distortion). The constraint is not
 *   biological inevitability — it is a contingent institutional arrangement
 *   sustained by cultural narratives that valorize persistence, social norms
 *   that discourage explicit rejection as cruelty, and intermittent
 *   reinforcement that maintains the Suitor's effort. The protocol exhibits
 *   tangled characteristics: genuine coordination elements (the interaction
 *   provides both parties some value, some pleasure, some meaning-making) are
 *   hybrid with extraction (asymmetric cost-benefit structure, suppression of
 *   alternatives, active enforcement through cultural reinforcement). The
 *   Suitor is not simply powerless — they have agency, they choose to
 *   continue, they benefit from the fantasy structure. But their exit options
 *   are severely constrained by the specificity of limerent attachment,
 *   opportunity costs, and cultural shame.
 *
 * KEY AGENTS:
 *   - Suitor: Primary victim (powerless/trapped or moderate/constrained depending on perspective) — bears emotional labor cost, maintains hope despite dismissal signals, experiences extraction as trapped agent or constrained agent with limited alternatives
 *   - Object of Limerence: Primary beneficiary (institutional/arbitrage) — receives emotional labor and validation without reciprocal obligation, maintains strategic ambiguity to preserve benefits
 *   - Authentic Reciprocal Relationships: Victim (abstract) — time and emotional capacity consumed by limerence protocol is unavailable for relationships with mutual interest; field of possibility is constrained
 *   - Cultural Romantic Narrative System: Enforcer (moderate/constrained) — active maintenance of protocols that valorize persistence, celebrate unrequited love, discourage explicit rejection; reduces object's cost of maintaining ambiguity
 *   - Social Peer Group: Enforcer (moderate/constrained) — applies shame costs to explicit rejection, normalizes the suitor's persistence as romantic virtue
 *   - Analytical Observer: Sees structure (analytical/analytical) — can identify both natural law interpretation (attachment asymmetry is inevitable) and institutional interpretation (ambiguity maintenance is contingent choice)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrequited_love_protocol, 0.58).
domain_priors:suppression_score(unrequited_love_protocol, 0.68).
domain_priors:theater_ratio(unrequited_love_protocol, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrequited_love_protocol, extractiveness, 0.58).
narrative_ontology:constraint_metric(unrequited_love_protocol, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unrequited_love_protocol, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrequited_love_protocol, tangled_rope).
narrative_ontology:human_readable(unrequited_love_protocol, "The Limerence Negotiation Protocol").
narrative_ontology:topic_domain(unrequited_love_protocol, "social/psychological").

domain_priors:requires_active_enforcement(unrequited_love_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unrequited_love_protocol, object_of_limerence).
narrative_ontology:constraint_victim(unrequited_love_protocol, suitor).
narrative_ontology:constraint_victim(unrequited_love_protocol, authentic_connection_possibilities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUITOR (SNARE) — Trapped in a cycle of emotional labor and validation-seeking with no clear exit pathway. The suitor bears the cost of sustained hope against accumulated rejection signals, experiencing maximum extraction. No arbitrage option exists; the specificity of the limerent object forecloses mobility. The protocol enforces this through intermittent reinforcement — occasional reciprocal gestures (a reply, a smile, a moment of attention) sustain the attachment despite overall neglect.
constraint_indexing:constraint_classification(unrequited_love_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OBJECT OF LIMERENCE (ROPE) — Experiences the constraint as coordination infrastructure. The object receives emotional labor (attention, validation, availability) without reciprocal obligation. The protocol's ambiguity (neither full rejection nor acceptance) is beneficial to the object — it preserves the suitor's labor investment while avoiding the institutional cost of explicit rejection or commitment. The object has substantial exit options (can withdraw at will, can redirect the suitor's efforts, can shift to indifference) and exercises them strategically.
constraint_indexing:constraint_classification(unrequited_love_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SUITOR (REALISTIC VIEW, TANGLED ROPE) — A more nuanced reading than pure snare. The suitor does benefit from the interaction — episodic reciprocation, the fantasy structure itself, the identity as 'lover' provides psychological anchoring. But these benefits are asymmetric and unstable. The suitor faces career risk (time investment reduces productivity), emotional labor cost, and opportunity cost (time unavailable for reciprocated relationships). The protocol is hybrid: genuine coordination (both parties value the interaction in some frame) and extraction (one party bears disproportionate cost and has constrained exit).
constraint_indexing:constraint_classification(unrequited_love_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: SOCIAL/CULTURAL MAINTENANCE (TANGLED ROPE) — The limerence protocol is actively enforced through romantic cultural narratives (literature, film, music celebrate unrequited love as noble sacrifice; social norms discourage explicit rejection as 'cruel'). This enforcement creates coordination benefits (provides meaning-making narratives for painful experiences) and extraction costs (normalizes the suitor's sustained effort as virtuous, delaying resolution). The protocol requires active cultural reinforcement — without the romantic ideal of 'love conquers all' and 'never give up,' the extraction mechanism loses force.
constraint_indexing:constraint_classification(unrequited_love_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EVOLUTIONARY MATING STRATEGY (PITON) — The limerence protocol traces historically to honest-signaling mechanisms (suitor demonstrates commitment and resource investment; object assesses reliability through sustained behavior). Modern context: this function has largely atrophied. Contemporary mate selection operates through explicit negotiation, rapid feedback loops, and mutual evaluation. The limerence protocol persists as theatrical display — performative persistence divorced from actual mating success. Theater ratio (0.64) reflects that most of the suitor's effort is visible signaling rather than functional courtship. The constraint is maintained by romantic cultural inertia, not by actual selective advantage.
constraint_indexing:constraint_classification(unrequited_love_protocol, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, asymmetric attachment is an inevitable feature of human bonding: preference distributions are continuous, desires are specific, and mutual attraction is statistically rare. Unrequited love is thus a natural law of probability — no protocol can eliminate it. However, this perspective naturalizes what are actually contingent institutional arrangements (the ambiguity tolerance protocol, the cultural valorization of persistence, the absence of explicit negotiation norms). The false summit classification indicates this is naturalization rather than genuine immutability.
constraint_indexing:constraint_classification(unrequited_love_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrequited_love_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrequited_love_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrequited_love_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unrequited_love_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unrequited_love_protocol, TR),
    TR >= 0.70.

:- end_tests(unrequited_love_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The suitor bears substantial costs relative to benefits. Emotional labor, opportunity cost (time unavailable for reciprocated relationships), identity distortion (the suitor's self-concept becomes increasingly defined by the unrequited attachment), and hope-disappointment cycles. The object receives attention, validation, and preserved options. However, extractiveness is not maximal (0.90+) because both parties do derive some genuine value from the interaction — the suitor experiences pleasure (intermittent reciprocation, fantasy, meaning-making), and the object experiences genuine care and attention. The asymmetry is real but not total. The value trajectory shows increasing extractiveness over the interval (0.32 → 0.58): early in the protocol, hope is high and costs are abstract; as time passes, opportunity costs accumulate, hope declines, and the suitor's recognition of extraction increases, yet they remain trapped. Suppression (0.68): High. Multiple suppression mechanisms: (1) Intermittent reinforcement — occasional reciprocal gestures sustain attachment despite overall neglect; (2) Ambiguity maintenance — the Object neither accepts nor rejects, leaving the protocol state undefined and exit decision unresolved; (3) Cultural narrative enforcement — romantic ideology valorizes persistence and treats explicit rejection as cruel, reducing the Object's cost of maintaining ambiguity; (4) Neurochemical limerence — the Suitor's attachment is not purely volitional, constraining conscious exit choice. Theater ratio (0.64): Moderate-high. Much of the Suitor's effort is performative — demonstrating commitment, proving worth, signaling reliability through visible persistence. The historical mating-strategy function (honest signaling) has atrophied in modern contexts where mate selection operates through explicit negotiation. The protocol persists as theatrical display divorced from actual mating success. The trajectory (0.42 → 0.64) reflects increasing performativity: early efforts are functional courtship attempts; later efforts become displays of persistence itself, no longer aimed at changing the Object's disposition but at maintaining the performance.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal how a single constraint can be simultaneously Snare (from the Suitor's trapped position), Rope (from the Object's beneficiary position), Tangled Rope (from the realistic/moderate view and cultural view), Piton (from the evolutionary-atrophy view), and falsely Mountain (from the natural-law view). None of these is 'correct' — they are all correct from their respective structural positions. The perspectival gap is not resolved by finding the 'true' classification but by recognizing that the constraint structure supports multiple simultaneous classifications. This multiplicity is diagnostic: if all perspectives produced identical types, the constraint would be simple (pure coordination or pure extraction). The fact that it produces all six types indicates a complex hybrid structure with institutional reinforcement and cultural embedding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position and exit capacity. The Suitor with trapped exit has d ≈ 0.95 (full target: f(d) ≈ 1.42, maximum experienced extraction). The Object with arbitrage exit has d ≈ 0.05 (full beneficiary: f(d) ≈ -0.12, negative extraction, benefits rather than costs). The cultural system with constrained exit (cannot easily shift romantic narratives) has d ≈ 0.55 (moderate/mixed: f(d) ≈ 0.75), reflecting that the system both benefits from and bears some cost of the protocol's maintenance. These directional asymmetries explain why each agent has different incentives: the Suitor is incentivized to change the protocol but cannot; the Object is incentivized to maintain it; the cultural system is incentivized to maintain it because it provides meaning-making narrative structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification prevents mislabeling this as either pure coordination (Rope) or pure extraction (Snare). Pure coordination theories (e.g., game-theoretic analyses treating it as a symmetrical problem of preference revelation) would miss the extraction component: the Object benefits from avoiding explicit commitment while receiving emotional labor; the Suitor bears the cost of sustained hope and opportunity foregone. Pure extraction theories (e.g., coercion-based models) would miss the coordination component: both parties do find value in the interaction; the Object is not a tyrant forcing the Suitor to persist, but rather an agent strategically maintaining ambiguity; the protocol provides meaning-making narratives that both parties partially benefit from. The tangled rope classification requires BOTH a genuine coordination function (interaction provides value) AND asymmetric extraction (value flows unidirectionally), AND active enforcement (cultural narratives, social norms, neurochemical limerence maintain the ambiguity). The measurement trajectory (theater and extractiveness both increasing) confirms the mandatrophy: the protocol is not becoming more coordinated (theater should decrease if function improved) nor purely extractive (beneficiaries would lose incentive to maintain). Instead, it is becoming increasingly theatrical while extraction deepens — a classic sign of institutional degradation. The old function (honest mate-signaling) has atrophied; the new institutional function (meaning-making narrative, romantic identity, cultural resource) is maintained through theatrical performance divorced from actual romantic success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intermittent_reinforcement_threshold,
    'What frequency of reciprocal gestures (replies, attention, affection) is sufficient to maintain the suitor''s attachment despite overall dismissal?',
    'Behavioral analysis of suitor persistence relative to object reciprocation frequency; controlled variation in feedback intervals to identify extinction thresholds',
    'If threshold is very low (< 10%): the protocol extracts maximum effort for minimal reinforcement, confirming snare classification. If threshold is high (> 40%): intermittent reinforcement is less efficient, and ambiguity is not the binding mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intermittent_reinforcement_threshold, empirical, 'Threshold frequency of reciprocal gestures maintaining suitor attachment').

omega_variable(
    explicit_rejection_availability,
    'Why does the object avoid explicit rejection despite clear lack of reciprocal interest? Is it conflict avoidance, benefit extraction, or genuine ambivalence?',
    'Interview/narrative analysis of the object''s reasoning; comparison between scenarios where explicit rejection is socially costless vs costly',
    'If conflict avoidance: the suppression is partly external (social norms) and the protocol is entrenched by institutional pressure. If benefit extraction: the object deliberately maintains ambiguity to preserve labor access — confirms tangled rope. If ambivalence: protocol reflects genuine uncertainty, shifting classification toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_rejection_availability, empirical, 'Motivations for object''s avoidance of explicit rejection').

omega_variable(
    suitor_authentic_agency,
    'To what extent is the suitor''s persistence autonomous choice vs compelled by neurochemical limerence state? Does this distinction affect the extraction classification?',
    'Neuroscience of attachment bonding; comparison of suitor''s stated intentions vs behavioral persistence; analysis of suitor''s ability to exit if given information about object''s true disposition',
    'If limerence is neurochemically compelled: the suitor is less of an agent and more of a patient, shifting the classification toward victimization and away from snare (which assumes agency). If autonomous: snare classification is stronger. If mixed: tangled rope is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suitor_authentic_agency, conceptual, 'Whether suitor persistence is autonomous choice or neurochemically compelled').

omega_variable(
    cultural_exit_costs,
    'How much do social costs (shame, loss of identity as ''romantic,'' peer judgment) contribute to the suitor''s inability to exit relative to object-imposed costs?',
    'Comparative analysis of exit costs in cultures with different romantic narratives; measurement of suitor''s stated exit barriers vs object-mediated barriers',
    'If social costs dominate: suppression is cultural/institutional rather than dyadic, and the protocol is a scaffold with cultural sunset rather than a snare. If object-mediated costs dominate: snare classification strengthens. If mixed: tangled rope with institutional reinforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_exit_costs, empirical, 'Contribution of cultural vs object-imposed costs to suitor''s exit barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrequited_love_protocol, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unreq_tr_t0, unrequited_love_protocol, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unreq_tr_t6, unrequited_love_protocol, theater_ratio, 6, 0.55).
narrative_ontology:measurement(unreq_tr_t12, unrequited_love_protocol, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(unreq_be_t0, unrequited_love_protocol, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(unreq_be_t6, unrequited_love_protocol, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(unreq_be_t12, unrequited_love_protocol, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrequited_love_protocol, information_standard).
narrative_ontology:affects_constraint(unrequited_love_protocol, attachment_system_dysregulation).
narrative_ontology:affects_constraint(unrequited_love_protocol, romantic_narrative_entrenchment).

% DUAL FORMULATION NOTE:
% The limerence negotiation protocol decomposes into two structurally distinct constraints: (1) the dyadic interaction structure (asymmetric emotional labor exchange with suppressed negotiation) and (2) the cultural/institutional maintenance system (romantic narratives, social norms, identity structures that reinforce protocol persistence). The dyadic constraint has ε ≈ 0.58 and exhibits tangled rope characteristics. The cultural maintenance system has higher theater_ratio (0.75+) and exhibits piton characteristics (performative cultural inertia). These are linked via network causality: the cultural system sustains the dyadic protocol by reducing the Object's cost of maintaining ambiguity and increasing the Suitor's cost of exit through shame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unrequited_love_protocol, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
