% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancient_grudge_verona, []).

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
 *   constraint_id: ancient_grudge_verona
 *   human_readable: The Montague-Capulet Feud
 *   domain: social/political
 *
 * SUMMARY:
 *   The Montague-Capulet feud is an inherited, transgenerational constraint
 *   that mandates spontaneous violence between two noble houses in
 *   Renaissance Verona. The constraint operates through institutional
 *   enforcement of honor codes: members of either house must respond to
 *   insult with violence, must refuse peace proposals from authority, and
 *   must organize street combat when the other house appears. Participation
 *   is mandatory regardless of personal preference. The feud extracts
 *   emotional labor, risk of injury/death, and opportunities for cross-house
 *   alliance (particularly romantic). For patriarchs, the feud enforces house
 *   loyalty and resource control. For the Prince, the feud provides a visible
 *   enforcement target. The structural data reveals this as a Snare dominant
 *   system (high suppression, high extractiveness) maintained partly through
 *   Piton theatrical ritual (honor codes), with a Tangled Rope perspective
 *   from institutional actors who benefit from the coordination mechanism,
 *   and a false Mountain perspective from civilizational observers who
 *   mistake the institutional arrangement for a natural law of human
 *   conflict.
 *
 * KEY AGENTS:
 *   - Young Members (Romeo, Juliet, Tybalt, Mercutio): Primary victims (powerless/trapped) — born into feud, no exit option, bear full cost of inherited conflict
 *   - Servants and Retainers: Secondary victims (powerless/trapped) — conscripted into violence by master loyalty, no independent agency
 *   - Patriarchs (Montague, Capulet): Primary beneficiaries/constrained actors (institutional/constrained) — benefit from feud as coordination + enforcement mechanism, but also constrained by honor codes that prevent exit
 *   - The Prince: Tertiary actor (institutional/arbitrage) — uses feud as coordination lever to demonstrate authority; benefits from visible enforcement target
 *   - Feudal Honor System: Institutional mechanism (institutional/arbitrage) — maintains performative honor codes that justify the feud but diminish in real coordination value
 *   - Civic Order/Verona: Victim (powerless/trapped) — disrupted by street violence, unable to prevent constraint enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_grudge_verona, 0.58).
domain_priors:suppression_score(ancient_grudge_verona, 0.72).
domain_priors:theater_ratio(ancient_grudge_verona, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, 0.58).
narrative_ontology:constraint_metric(ancient_grudge_verona, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ancient_grudge_verona, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_grudge_verona, snare).
narrative_ontology:human_readable(ancient_grudge_verona, "The Montague-Capulet Feud").
narrative_ontology:topic_domain(ancient_grudge_verona, "social/political").

domain_priors:requires_active_enforcement(ancient_grudge_verona).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, feudal_patriarchs).
narrative_ontology:constraint_victim(ancient_grudge_verona, young_members).
narrative_ontology:constraint_victim(ancient_grudge_verona, servants).
narrative_ontology:constraint_victim(ancient_grudge_verona, civic_order).
narrative_ontology:constraint_victim(ancient_grudge_verona, romantic_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG MEMBER (SNARE) — Born into the feud with no choice. Exit options are nil: defecting to the other house means death, refusing to participate means family shame and exclusion. The constraint extracts emotional labor, loyalty, and ultimately life itself. Maximum experienced extraction — the young member bears the full cost of an inherited conflict they did not create.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SERVANT (SNARE) — Conscripted into street violence by allegiance to master. No independent exit option; participation is mandatory regardless of personal beliefs. Bears risk of injury or death while extracting no personal benefit. Trapped within the constraint's violence mechanism.
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: PATRIARCH (TANGLED ROPE) — Benefits from the feud as a coordination mechanism for house loyalty and resource control within feudal structures. Honor, territorial respect, and subordinate allegiance are extracted via the feud's enforcement mechanism. Yet also constrained: the patriarch cannot simply exit without losing face, resources, and control. Mixed extraction — genuine benefit from coordination + significant cost of sustained enmity.
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: PRINCE/CIVIL AUTHORITY (ROPE) — Uses the feud as a coordination lever for demonstrating power and maintaining order through threat. The feud provides a clear enforcement target (stop the feud or face death penalty). This is pure coordination with minimal extraction — the Prince benefits from the feud's existence as a way to exercise authority, but does not extract ongoing resources from it.
constraint_indexing:constraint_classification(ancient_grudge_verona, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEUDAL HONOR SYSTEM (PITON) — The feud is maintained by theatrical performance of honor codes that have lost primary functional value. The ritual (public insult, street combat, family reputation) persists through institutional inertia despite declining coordination benefit. The system sees itself as performing honor rather than achieving it. Theater ratio high because the actual function (deterrence, resource control) could be achieved through less performative mechanisms, yet the feud persists.
constraint_indexing:constraint_classification(ancient_grudge_verona, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — This perspective risks naturalizing the feud as an immutable property of human tribal nature or Renaissance social law. From a civilizational view, inherited conflicts appear universal and necessary. However, the structural data contradicts this — the feud is maintained by institutional enforcement, suppression of exit options, and performative honor codes. The mountain classification is a false summit, revealing how contingent social arrangements are mistaken for natural laws.
constraint_indexing:constraint_classification(ancient_grudge_verona, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_grudge_verona_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancient_grudge_verona, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancient_grudge_verona, TR),
    TR >= 0.70.

:- end_tests(ancient_grudge_verona_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant costs from young members (risk of death, lost romantic possibilities, emotional labor), from servants (mandated violence), and from civic order (disrupted streets). The trajectory shows increasing extractiveness over the interval (0.42→0.58), indicating either escalation in violence frequency or intensification in enforcement. Patriarchs extract institutional benefits (loyalty, resource control, honor), but these are real coordination gains mixed with rent-seeking — hence the measurement as moderate rather than severe. Suppression (0.72): High. Exit barriers are substantial: attempting to defect to the other house results in death; refusing to participate results in shame and exclusion; seeking peace is portrayed as betrayal. The Prince's death threats add explicit enforcement. Yet suppression is not absolute — Romeo finds a way to marry Juliet, and the Prince eventually forces peace through execution. Theater ratio (0.68): High and increasing. The feud's performative content is substantial: formal insults, ritualized street combat, elaborate honor language. The actual deterrent or resource-control function could be achieved more efficiently through other mechanisms. The theater increases over time (0.55→0.68) as the feud becomes more about maintaining the reputation of the conflict than achieving the underlying coordination goals. This rising theater signals Piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The young member sees pure extraction (Snare) — they are trapped, at risk, with no benefit. The patriarch sees mixed extraction and benefit (Tangled Rope) — the feud enforces loyalty and maintains honor, but also constrains their agency. The Prince sees pure coordination (Rope) — the feud is a useful enforcement mechanism with no cost to him. The feudal honor system sees itself as degraded (Piton) — performing honor codes that used to matter but now primarily justify themselves. The civilizational observer risks seeing natural law (false Mountain) — humans naturally form feuding groups — which naturalizes what is actually a contingent institutional arrangement maintained by specific enforcement mechanisms. The gap between the victim's snare and the beneficiary's tangled rope reveals the mandatrophy: the patriarch experiences real coordination benefits alongside extraction, while the young member experiences only extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extractiveness (chi). Young members with no exit options (trapped) face maximum d ~0.95, producing high chi. Servants similarly trapped face maximum d. Patriarchs with constrained exit (cannot exit without losing face/power) face moderate d ~0.55-0.65, producing moderate chi — they benefit from the coordination but cannot easily escape. The Prince with arbitrage options (can enforce peace when he chooses) faces low d ~0.15, producing negative chi — the feud actually serves his authority. The feudal system itself operates at institutional/arbitrage level (d ~0.10), maintaining theatrical performance. The young members' powerlessness makes them maximally vulnerable to the constraint's extraction; the patriarchs' institutional power gives them agency but not enough to escape entirely; the Prince's external authority gives him arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy through perspectival differentiation. The feud IS both a coordination mechanism (forcing house loyalty and resource allocation) AND an extraction mechanism (forcing young members into danger, mandating violence). The question is not 'coordination or extraction?' but 'for whom?' For patriarchs, it functions as Tangled Rope (real coordination + asymmetric extraction of younger generation). For young members, it functions as Snare (pure extraction). The feud cannot be classified as pure coordination (Rope) because it has genuine asymmetric extraction (victims cannot exit). It cannot be classified as pure extraction (Snare) because it performs genuine coordination functions (loyalty enforcement, resource control). The Tangled Rope classification from the patriarch's perspective captures this hybrid: the feud coordinates institutional actors while extracting from subordinate actors. The rising theater ratio (0.55→0.68) indicates Piton drift — the coordination function is atrophying while the performative ritual persists, suggesting the constraint may be transitioning from Tangled Rope toward Piton as its functional value diminishes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_cause_recovery,
    'What was the original cause of the feud, and is it recoverable or functionally forgotten?',
    'Historical documentation, family records, elder testimony; comparison between stated cause and actual behavior patterns; assessment of whether the original grievance still drives current conflict or serves purely as justification',
    'If original cause is genuinely forgotten: the feud is pure institutional inertia (Piton dominant). If original cause is remembered and still operative: extraction flows from resource/honor competition (Snare/Tangled Rope dominant). If cause is disputed: conflicting narratives sustain suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_cause_recovery, empirical, 'Whether the original feud cause is recoverable or functionally forgotten').

omega_variable(
    exit_barrier_structurality,
    'Are exit barriers (threat of death, exile, family exclusion) structural necessities of feudal society or contingent enforcement choices?',
    'Comparative analysis of feudal societies with and without similar feud systems; examination of actual consequences vs threatened consequences; testing whether loosening enforcement would dissolve the feud or maintain it through internalized honor codes',
    'If barriers are structural: feud approaches Mountain (inherent to feudal organization). If barriers are contingent: feud is Snare/Tangled Rope (extractive institutional choice). This determines whether the feud is a system property or a failure mode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_barrier_structurality, conceptual, 'Whether feud exit barriers are structurally necessary or contingently enforced').

omega_variable(
    coordination_function_existence,
    'Does the feud actually coordinate house membership and loyalty, or is the coordination justification merely post-hoc rationalization?',
    'Counterfactual analysis: what coordination functions (resource allocation, dispute resolution, defense against external threats) would actually break if the feud ended? Comparison with non-feuding houses that maintain equal coordination through different mechanisms.',
    'If feud is genuinely coordinating: Tangled Rope classification stable. If feud is parasitic on coordination (prevents coordination without enabling it): Snare classification dominant. If feud is vestigial (once coordinated, now doesn''t): Piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_existence, empirical, 'Whether the feud performs genuine coordination or only justifies itself as such').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_grudge_verona, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grudge_tr_t0, ancient_grudge_verona, theater_ratio, 0, 0.55).
narrative_ontology:measurement(grudge_tr_t5, ancient_grudge_verona, theater_ratio, 5, 0.62).
narrative_ontology:measurement(grudge_tr_t10, ancient_grudge_verona, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(grudge_be_t0, ancient_grudge_verona, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(grudge_be_t5, ancient_grudge_verona, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(grudge_be_t10, ancient_grudge_verona, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancient_grudge_verona, enforcement_mechanism).
narrative_ontology:affects_constraint(ancient_grudge_verona, verona_civic_peace).
narrative_ontology:affects_constraint(ancient_grudge_verona, renaissance_marriage_market).

% DUAL FORMULATION NOTE:
% The Montague-Capulet feud is upstream of specific institutional failures (civic peace disruption, marriage alliance prevention) but constitutes a distinct constraint. The feud's extractiveness reflects the inherited institutional enforcement of enmity; downstream constraints have their own extractiveness reflecting domain-specific impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
