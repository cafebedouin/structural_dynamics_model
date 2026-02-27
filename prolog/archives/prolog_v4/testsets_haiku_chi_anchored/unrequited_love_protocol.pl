% ============================================================================
% CONSTRAINT STORY: unrequited_love_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The limerence negotiation protocol models unrequited love as a failed
 *   communication system where one party (the Suitor) continuously transmits
 *   emotional labor, vulnerability, and connection requests that are neither
 *   fully accepted nor explicitly rejected by the other party (the Object).
 *   The constraint exhibits classic tangled-rope structure: it contains a
 *   genuine coordination function (the Suitor and Object do interact,
 *   communicate, spend time together, exchange emotional and practical
 *   support) but is asymmetric in extraction (the Object benefits from
 *   attention and validation without reciprocal cost, the Suitor bears
 *   emotional labor and psychological risk). The suppression is high (0.68)
 *   because the Suitor faces multiple barriers to clarity: fear of explicit
 *   rejection, social shame, loss of daily proximity to the Object, identity
 *   investment in the 'devoted lover' role, and the Object's strategic or
 *   unconscious ambiguity that preserves the status quo. The theater ratio
 *   (0.65) reflects that roughly two-thirds of the Suitor's effort is
 *   performative enactment of cultural romantic tropes (grand gestures,
 *   poetic expression, persistent availability) rather than genuine
 *   negotiation. The protocol's extractiveness has increased over time (from
 *   0.32 to 0.58) as sunk costs accumulate and the Object's implicit benefit
 *   extraction continues without renegotiation. Six distinct perspectives
 *   reveal the constraint's structural complexity: the Suitor experiences it
 *   as a Snare (pure extraction with no exit), the Object experiences it as a
 *   Rope (pure coordination with net benefit), the social sphere experiences
 *   it as a Tangled Rope (mixed coordination and entertainment value),
 *   therapeutic intervention sees it as a Scaffold (temporary structure with
 *   explicit sunset), cultural romanticism sees it as a Piton (degraded
 *   ritual), and the civilizational analytical view risks naturalizing it as
 *   a Mountain (immutable property of attachment psychology) — though this
 *   mountain is a false summit.
 *
 * KEY AGENTS:
 *   - The Suitor: Primary victim (powerless/trapped) — bears emotional labor, psychological risk, and sunk identity investment; cannot exit without significant cost
 *   - The Object of Affection: Primary beneficiary (institutional/arbitrage) — receives attention, validation, and practical assistance without reciprocal obligation; maintains plausible deniability
 *   - The Mutual Social Sphere: Secondary actors (moderate/constrained) — derive entertainment value and relationship drama; exercise social pressure and normative judgment
 *   - Therapeutic Intervention: Powerful agent (powerful/mobile) — sees constraint as temporary and explicitly aims at protocol termination via boundary-setting
 *   - Romantic Cultural Institution: Institutional force (institutional/arbitrage) — maintains performative rituals (grand gestures, poetic expression) that no longer function as negotiation tools
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent social arrangement as inherent psychological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrequited_love_protocol, 0.58).
domain_priors:suppression_score(unrequited_love_protocol, 0.68).
domain_priors:theater_ratio(unrequited_love_protocol, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrequited_love_protocol, extractiveness, 0.58).
narrative_ontology:constraint_metric(unrequited_love_protocol, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unrequited_love_protocol, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrequited_love_protocol, tangled_rope).
narrative_ontology:human_readable(unrequited_love_protocol, "The Limerence Negotiation Protocol").
narrative_ontology:topic_domain(unrequited_love_protocol, "social/psychological").

domain_priors:requires_active_enforcement(unrequited_love_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unrequited_love_protocol, object_of_affection).
narrative_ontology:constraint_victim(unrequited_love_protocol, suitor).
narrative_ontology:constraint_victim(unrequited_love_protocol, mutual_social_sphere).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUITOR (SNARE) — Trapped in continuous emotional labor. Cannot exit without psychological cost (sunk identity investment, hope depletion, social humiliation). Sends unreciprocated connection requests that are neither accepted nor fully rejected. High suppression: fear of explicit rejection, social shame, loss of daily proximity. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(unrequited_love_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OBJECT OF AFFECTION (ROPE) — Benefits from suitor's emotional labor (validation, attention, practical assistance) without reciprocal obligation. Maintains plausible deniability ('I never promised anything'). Enjoys coordination benefits of ongoing attention while avoiding commitment costs. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.05. Net beneficiary experiencing constraint as pure coordination.
constraint_indexing:constraint_classification(unrequited_love_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: MUTUAL SOCIAL SPHERE (TANGLED ROPE) — Constrained by drama, alliance pressure, and social friction. Coordination function: manages group dynamics and emotional regulation. Extraction function: observes entertainment value and relationship gossip. Active enforcement through social pressure ('You should just tell them'), normative judgment, and alliance expectations. d≈0.65, f(d)≈0.95, σ=0.8 → χ≈0.36.
constraint_indexing:constraint_classification(unrequited_love_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ROMANTIC REALISM INTERVENTION (SCAFFOLD) — Powerful agents (therapist, trusted friend, life coach) see the constraint as a temporary coordination failure with clear sunset logic. Therapy/boundary-setting conversations represent the scaffold: temporary supportive structure that explicitly aims at its own obsolescence. Goal is suitor self-awareness and protocol termination within defined timeframe (6-12 months). d≈0.35, f(d)≈0.32, σ=0.8 → χ≈0.16. Low extraction because intervention is transparent about its sunset.
constraint_indexing:constraint_classification(unrequited_love_protocol, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: THE ROMANTIC IDEAL INSTITUTION (PITON) — Cultural narrative that 'true love conquers all' and 'grand romantic gestures work' is substantially performative. The suitor's emotional labor is ritualized: flowers, poetry, persistent availability all follow cultural theater templates. These rituals no longer function as effective negotiation tools in the constraint's actual mechanics, but persist through institutional inertia (movies, literature, dating advice columns continue to endorse them). theater_ratio=0.65 reflects that 60%+ of suitor's effort is performative mimicry of romantic tropes rather than genuine communication. The institution maintains this theater despite low functional payoff.
constraint_indexing:constraint_classification(unrequited_love_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ATTACHMENT THEORY (MOUNTAIN) — Civilizational/universal view treats limerence negotiation failure as an emergent property of human attachment psychology: the mismatch between suitor's secure/preoccupied attachment and object's avoidant/dismissive attachment is inherent to psychological structure. Cannot be negotiated away — only understood. However, this mountain classification is a FALSE SUMMIT: the structural data (ε=0.58, suppression=0.68, theater=0.65) reveals this as contingent institutional arrangement, not natural law. Attachment patterns are real, but the 'unreciprocated love trap' is a function of failed communication protocol + social shame + romantic ideology, not attachment psychology alone.
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
 *   Extractiveness (0.58): High-moderate. The Object extracts value (emotional labor, validation, practical assistance, sustained attention) while the Suitor invests without reciprocal agreement or boundary clarity. The value is real and measurable: the Object enjoys the Suitor's company, benefits from their support, receives ego validation through romantic attention. However, extractiveness is not maximal (0.70+) because the constraint still has coordination elements — the Suitor and Object do interact genuinely, share experiences, and have moments of authentic connection; the extraction is parasitic on coordination rather than pure coercion. The increase over time (0.32→0.58) reflects sunk cost escalation: as the Suitor's identity becomes invested in the relationship, their willingness to exit decreases, and the Object's implicit benefit (sustained attention at lower commitment cost) increases. Suppression (0.68): High. Multiple barriers prevent protocol renegotiation: (1) Fear of explicit rejection and its psychological consequences. (2) Social shame — the Suitor's persistent devotion is culturally visible and subject to judgment ('they should just move on'). (3) Proximity loss — explicit rejection would terminate daily contact, creating immediate deprivation. (4) Object's ambiguity strategy — by neither accepting nor rejecting, the Object eliminates the Suitor's external pressure point for clarity. (5) Identity investment — the Suitor has constructed their self-narrative around this dynamic ('I am the devoted one'). Theater ratio (0.65): High-moderate. The Suitor's effort is substantially performative mimicry of cultural romantic templates: flowers/gifts (0.2), poetic expression/declarations (0.15), persistent availability/waiting (0.15), grand romantic gestures (0.15). These rituals follow scripted patterns rather than genuine negotiation. They no longer function as protocol negotiation tools — they are cultural theater. The Object interprets them as reinforcement of the current ambiguous status quo rather than as proposals for change. Theater has increased over time (0.35→0.65) as the Suitor becomes more performatively committed and the Object becomes more skilled at interpreting theater as a sign of continued acceptance of the status quo.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The Suitor and Object inhabit incompatible classification systems: Suitor experiences Snare (extraction with no exit), Object experiences Rope (coordination with net benefit). These are not just different valuations of the same constraint — they are incommensurable protocols. The Suitor is attempting to negotiate an upgrade to reciprocal commitment; the Object is experiencing the current arrangement as a stable, low-cost coordination mechanism. The social sphere's Tangled Rope perspective captures this gap as entertainment: they observe both coordination function (the dyad does spend time together and interact) and asymmetric extraction (the Object benefits more). Therapy's Scaffold perspective is a meta-intervention: it attempts to impose a third protocol (boundary-setting toward termination) that neither Suitor nor Object has spontaneously adopted. The Piton perspective notes that the Suitor's romantic rituals are degraded communication tools — they no longer function to change the Object's behavior, only to reinforce the status quo. The analytical mountain perspective risks erasing the gap entirely by naturalizing it as 'attachment psychology' — but this hides the real constraint: failed protocol negotiation + social shame + romantic ideology + sunk costs, not psychological determinism.
 *
 * DIRECTIONALITY LOGIC:
 *   Suitor: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction directionality. The Suitor cannot exit without extreme psychological cost. They have already invested identity, time, emotional labor, and social capital. Explicit exit requires processing grief, humiliation, identity reconstruction, and loss of daily contact. The only exit less costly than continued investment is psychological dissociation or delusional hope ('maybe they will change their mind'). Object: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Strong negative directionality — net beneficiary. The Object receives attention, emotional labor, validation, and practical support without reciprocal cost or obligation. They maintain arbitrage exit: if the Suitor becomes inconvenient, they can escalate boundary-setting; if the Suitor is valuable, they can maintain ambiguity. This is the lowest cost strategy available to the Object — they externalize all emotional cost while internalizing all benefit. Social sphere: Both + constrained → d≈0.65, f(d)≈0.95. Moderate extraction. The social sphere derives value from the dyad's ongoing drama and from the entertainment of watching emotional labor play out. But they also experience costs: alliance pressure ('whose side are you on?'), awkwardness at group gatherings, obligation to offer advice, emotional labor of supporting the Suitor's distress. Suppression for the social sphere is low (they can exit via non-involvement), so they choose constrained engagement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is subtle and critical: is this constraint primarily a communication protocol failure (Tangled Rope: coordination + extraction) or primarily an attachment psychology trap (Mountain: immutable)? The false summit risk is high. Attachment theory naturalization would argue: 'Preoccupied attachment seeking distant avoidant attachment is inherent to human psychology. The suitor cannot exit because their neurobiology compels pursuit. This is a natural law.' But this hides the contingent institutional factors: (1) The constraint exists in a culture that valorizes romantic love as supreme value. In transactional or hierarchical-affection cultures, the same attachment asymmetry produces different outcomes. (2) The Suitor's exit costs are socially constructed: shame, identity loss, community reaction are cultural, not biological. (3) The Object's ambiguity strategy is available only in low-commitment cultures; in cultures with explicit courtship protocols, ambiguity itself is socially costly. (4) The theater ratio (0.65) suggests most of the Suitor's effort is cultural mimicry, not authentic attachment expression. Mandatrophy is UNRESOLVED (mandatrophy_resolved: false) because the constraint contains both genuine attachment dynamics AND social/institutional contingency. The engine cannot classify this as a pure mountain (immutable law) without missing the institutional decomposability. It cannot classify as pure rope (low extraction) because the Object's benefit extraction is real and asymmetric. Tangled Rope is the correct classification: it has coordination (Suitor and Object do interact, share experiences) and asymmetric extraction (Object benefits without reciprocal cost), with active enforcement via social shame and romantic ideology theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rejection_clarity_threshold,
    'What level of explicit rejection would terminate the protocol? Is ''I''m not interested romantically'' sufficient, or does the object''s ambiguity serve a function the protocol depends on?',
    'Counterfactual analysis: would explicit rejection actually free the suitor, or would it deepen the constraint through pain/rumination cycles? Longitudinal tracking of suitors post-explicit-rejection to measure psychological exit vs. continued monitoring.',
    'If explicit rejection would free the suitor: constraint is pure suppression via ambiguity avoidance (higher snare classification). If explicit rejection deepens attachment: constraint includes psychological feedback loops that transcend communication (attachment trap, higher mandatrophy concern).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rejection_clarity_threshold, empirical, 'Whether explicit rejection would actually terminate the protocol').

omega_variable(
    object_awareness_and_agency,
    'To what degree is the object consciously maintaining ambiguity as an extraction strategy vs. unconsciously benefiting from it while experiencing genuine uncertainty about their own feelings?',
    'Direct narrative analysis: post-hoc interviews with object exploring their subjective experience during the constraint period. Observation of object''s behavior patterns (how they signal boundaries, escalate/de-escalate suitor attention) to infer strategic vs. genuine uncertainty.',
    'If strategic: constraint is intentional extraction (higher χ, snare classification more justified). If unconscious benefit: constraint is parasitic coordination (suitor provides value the object enjoys without explicit agreement), still tangled rope but with lower moral agency attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(object_awareness_and_agency, conceptual, 'Degree of object''s conscious vs. unconscious maintenance of protocol ambiguity').

omega_variable(
    sunk_cost_entrapment_mechanism,
    'Does the suitor''s exit cost increase monotonically over time (sunk identity investment, social awkwardness, lost community), or does it plateau, creating a genuine escape window?',
    'Temporal analysis of suitor''s decision points: tracking moments where suitor considers exit vs. continues investing. Measurement of suitor''s narrative complexity (''I''ve invested years'' vs. ''I could leave tomorrow''). Comparison with suitors who successfully exited at different timelines.',
    'If costs increase monotonically: constraint deepens via sunk cost escalation (higher suppression, longer biographical timescale needed for exit). If costs plateau: there exists a negotiation window where exit is psychologically cheaper, enabling protocol termination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunk_cost_entrapment_mechanism, empirical, 'Temporal trajectory of suitor''s exit costs').

omega_variable(
    romantic_ideology_necessity,
    'Would the protocol collapse if suitor and object inhabited a culture without the romantic love narrative (e.g., arranged marriage norms, transactional courtship, explicit hierarchical affection systems)?',
    'Cross-cultural comparison: documented patterns of unreciprocated attachment in low-romantic-ideology cultures. Analysis of how different social structures create different outcomes for identical attachment asymmetries.',
    'If romantic ideology is causal: constraint is partially decomposable via cultural shift (theater_ratio would drop, suppression via shame would decrease). If attachment asymmetry alone is sufficient: constraint persists cross-culturally in different forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(romantic_ideology_necessity, empirical, 'Whether romantic ideology is necessary for the protocol to function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrequited_love_protocol, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulp_tr_t0, unrequited_love_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ulp_tr_t6, unrequited_love_protocol, theater_ratio, 6, 0.55).
narrative_ontology:measurement(ulp_tr_t12, unrequited_love_protocol, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(ulp_be_t0, unrequited_love_protocol, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ulp_be_t6, unrequited_love_protocol, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ulp_be_t12, unrequited_love_protocol, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrequited_love_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(unrequited_love_protocol, social_shame_signaling).
narrative_ontology:affects_constraint(unrequited_love_protocol, romantic_ideology_naturalness).

% DUAL FORMULATION NOTE:
% The limerence negotiation protocol is downstream of two structural constraints: (1) social_shame_signaling (which creates suppression for protocol renegotiation) and (2) romantic_ideology_naturalness (which creates theater for Suitor's effort). Each downstream constraint has distinct ε values. This story addresses the emergent constraint at the dyadic level where these two constraints couple.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unrequited_love_protocol, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
