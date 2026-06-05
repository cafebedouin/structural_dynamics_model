% ============================================================================
% CONSTRAINT STORY: communal_narcissism_social_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_communal_narcissism_social_trap, []).

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
 *   constraint_id: communal_narcissism_social_trap
 *   human_readable: The Altruistic Extraction Snare: Communal Narcissism Social Trap
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Communal narcissism creates a social snare where a central figure (the
 *   communal narcissist) uses performative prosociality — framing themselves
 *   as selfless, devoted, and in service to collective good — to extract
 *   emotional labor, material resources, and identity fusion from group
 *   members. The snare is distinguished from ordinary hierarchy by the
 *   inversion: the extractor claims to be sacrificing while the group claims
 *   to be fortunate to contribute. Contributors experience themselves as
 *   morally virtuous for their sacrifice, making exit feel like moral
 *   betrayal. The constraint appears uniformly as extraction from the
 *   victim's perspective (powerless/trapped), as a coordination mechanism to
 *   the narcissist (institutional/arbitrage), and as institutional theater of
 *   prosociality at the civilizational level. The theater ratio (0.81)
 *   reflects that most of the social energy is spent performing altruism
 *   (public displays of collective purpose, virtue signaling, emotional labor
 *   theater) rather than achieving material outcomes. The extractiveness
 *   (0.58) captures the moderate-to-high level of resource and emotional
 *   capture, coupled with the profound difficulty of exit — contributors have
 *   fused their identity with the cause, making departure carry reputational
 *   and psychological costs within the in-group.
 *
 * KEY AGENTS:
 *   - Communal Narcissist: Primary beneficiary (institutional/arbitrage) — receives narcissistic supply (admiration, obedience, emotional mirroring) and material resources; experiences self as selfless martyr
 *   - Enmeshed Contributors: Primary victims (powerless/trapped) — perform emotional labor, material contribution, and identity fusion; cannot exit without shame and community rejection
 *   - Peripheral Observers: Secondary victims (moderate/constrained) — aware of dynamics but face social pressure to participate; some mobility but reputation cost to exit
 *   - Informed Advocates: Powerful external observers (powerful/mobile) — therapists, psychologists, defectors with insight; can articulate the extraction mechanism
 *   - Group Epistemic Integrity: Abstract victim — the shared narrative becomes corrupted by the need to maintain the narcissist's image; truth-telling becomes disloyalty
 *   - Institutional Theater System: Civilizational actor (institutional/arbitrage) — social media, non-profit culture, therapeutic language appropriation normalize communal narcissism as authentic altruism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(communal_narcissism_social_trap, 0.58).
domain_priors:suppression_score(communal_narcissism_social_trap, 0.68).
domain_priors:theater_ratio(communal_narcissism_social_trap, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(communal_narcissism_social_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(communal_narcissism_social_trap, snare).
narrative_ontology:human_readable(communal_narcissism_social_trap, "The Altruistic Extraction Snare: Communal Narcissism Social Trap").
narrative_ontology:topic_domain(communal_narcissism_social_trap, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(communal_narcissism_social_trap, communal_narcissist).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, emotional_labor_contributors).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, material_resource_contributors).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, group_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENMESHED CONTRIBUTOR (SNARE) — Trapped by social obligation, shame induction, and identity fusion with the 'cause'. Cannot exit without reputational cost within the community. Bears full emotional and material extraction. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PERIPHERAL OBSERVER (SNARE) — Moderate power but constrained exit. Has some distance from the in-group but still faces social pressure to participate. Sees the extraction but cannot easily organize resistance. d≈0.68, f(d)≈0.98, σ=0.8 → χ≈0.45.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: COMMUNAL NARCISSIST (ROPE) — Experiences the constraint as pure coordination: mobilizing group resources around 'shared values' and 'collective good'. The performative prosociality feels authentic to the narcissist; the extraction is invisible to them. d≈0.08, f(d)≈-0.11, σ=0.8 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: INFORMED ADVOCATE / PSYCHOLOGIST (TANGLED ROPE) — Powerful external observer with mobility. Sees both the coordination function (genuine community mobilization, shared purpose) AND the extraction mechanism (narcissistic supply, emotional labor harvesting). Can exit but chooses to remain to help dismantle the trap. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.26.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER OF PROSOCIALITY (PITON) — From a civilizational view, performative altruism and communal narcissism have become normalized through institutional mechanisms (social media, non-profit culture, therapeutic language appropriation). The theater persists through inertia — 'authentic altruism' rhetoric makes the extraction invisible. theater_ratio=0.81 exceeds piton gate (≥0.70). The institutional system sees its own processes as degraded but maintains them.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PSYCHOLOGICAL NATURALISM (MOUNTAIN) — From universal analytical perspective, communal narcissism might appear as an inevitable feature of human psychology: hierarchies, status-seeking, and coalition dynamics create conditions where narcissistic individuals exploit group coordination. However, base properties (ε=0.58, suppression=0.68, theater=0.81) contradict mountain classification — this is a false summit revealing contingent cultural/institutional conditions, not immutable psychology.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(communal_narcissism_social_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(communal_narcissism_social_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(communal_narcissism_social_trap, TR),
    TR >= 0.70.

:- end_tests(communal_narcissism_social_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The communal narcissist captures substantial emotional labor (validation, emotional containment, identity confirmation), material resources (money, time, physical labor), and cognitive resources (group intellectual property, decision-making authority). The extraction is not total (some genuine community benefit occurs) but is severe and persistent. The value increased from 0.38 to 0.58 over the interval as the narcissist's control mechanisms matured and contributors became more enmeshed. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) identity fusion — contributors define themselves through the group/cause, making exit a threat to self-concept; (2) cognitive distortion — the narcissist's frame ('we are on a noble mission') becomes shared reality; (3) social cost — leaving the group carries reputational damage within the community; (4) moral inversion — speaking about the extraction is framed as disloyalty or jealousy. Theater ratio (0.81): High and increasing. The group's social energy is predominantly performative: public displays of collective purpose, virtue signaling, emotional labor theater around the narcissist's 'sacrifice', social media documentation of altruism. Actual material outcomes become secondary to the performance of altruism. The ratio increased from 0.52 to 0.81 as the narcissist professionalized their performance and institutionalized the theatrical elements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from a single structural position. The enmeshed contributor sees pure extraction (Snare) — they are trapped and cannot exit without psychological and social cost. The peripheral observer sees extraction with some agency (Snare) — they have more distance but still face pressure. The narcissist sees coordination (Rope) — mobilizing group resources around shared values feels authentic; the extraction is invisible to them. The informed advocate sees both coordination and extraction (Tangled Rope) — the group genuinely accomplishes some goals, but the mechanism is exploitative. The institutional system sees its own degraded ritual (Piton) — performative altruism persists through inertia and social media amplification. The civilizational analytical observer risks naturalizing this as inherent psychology (Mountain) — human hierarchies and status-seeking necessarily produce narcissistic exploitation — but the structural data reveals this as contingent on culture, institutional incentives, and information asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Enmeshed contributors: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction effect. Peripheral observers: Victim + constrained → d≈0.68, f(d)≈0.98. High extraction but with some exit capacity. Communal narcissist: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; experiences constraint as coordination. Informed advocate: Mixed role (can see both) + mobile → d≈0.45, f(d)≈0.50. Moderate effect; agent has power and mobility. Institutional theater: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification derives from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the same group structure appears as both coordination (from the narcissist's perspective) and extraction (from the victim's perspective). The resolution is that BOTH perspectives describe real structural facts: the group does coordinate resources and achieve some shared goals (coordination is genuine), BUT the mechanism relies on exploitation and asymmetric extraction (extraction is also genuine). The Tangled Rope perspective (from the informed advocate) captures the true structure: genuine coordination function coupled with asymmetric extraction and active enforcement (the narcissist's control mechanisms). The snare classification (from the victim's perspective) is not false — it is the accurate perception from within the trap. The rope classification (from the narcissist's perspective) is not false — it is what the extraction looks like when you are the beneficiary and cannot see the mechanism. The mandatrophy dissolves when we recognize that different perspectives have different epistemic access to the structure. The victim cannot see the narcissist's subjective experience (genuine belief in their own selflessness); the narcissist cannot see the extraction mechanism (it is invisible to the beneficiary). The informed advocate's Tangled Rope perspective has superior epistemic access because it integrates both structural functions and understands the asymmetry of perception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narcissistic_supply_threshold,
    'At what quantitative threshold of emotional/material extraction does a communal narcissist consciously recognize their own exploitative behavior, if ever?',
    'Longitudinal psychological assessment; comparison of narcissist self-reports against victim-reported extraction levels; analysis of narcissists who underwent therapy and acquired insight',
    'If threshold is never crossed (unconscious extraction persists): snare classification is stable across timescales. If threshold exists: intervention points become identifiable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narcissistic_supply_threshold, empirical, 'Whether narcissistic exploiters can develop conscious insight into their extraction').

omega_variable(
    group_cohesion_cost_trade,
    'Does the extraction mechanism simultaneously generate genuine group cohesion that would be harder to achieve through non-exploitative means, or is all perceived cohesion performative?',
    'Comparison of group stability and mutual support metrics in communal-narcissism-led groups vs peer-led groups with equivalent objectives; measurement of cohesion persistence after narcissist departure',
    'If genuine cohesion is generated: mixed (Tangled Rope) classification replaces pure Snare from some perspectives. If purely performative: Snare classification is confirmed; exit without group collapse becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_cohesion_cost_trade, empirical, 'Whether narcissist-led extraction generates real group cohesion or only performative bonding').

omega_variable(
    victim_complicity_boundary,
    'Is the contributor''s continued participation due to genuine inability to perceive the extraction (cognitive trap) or due to rational choice under constrained options (economic trap)?',
    'Intervention studies: provide explicit evidence of extraction to contributors; measure subsequent participation rates. Compare with groups where alternatives (other communities, resource sources) are made available.',
    'If cognitive: trapped classification is justified; educational intervention may suffice. If rational-economic: suppression gate is confirmed; exit barriers must be dismantled for liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_complicity_boundary, empirical, 'Whether victim participation is due to cognitive occlusion or rational constraint').

omega_variable(
    institutional_amplification_feedback,
    'Does institutionalization of communal narcissism language (therapeutic jargon, non-profit culture, social media metrics) actively amplify the snare, or merely reduce friction for its operation?',
    'Historical comparison of communal narcissism prevalence and severity pre/post internet; cross-cultural comparison of extraction rates in high-social-media vs low-adoption communities; analysis of narcissist strategy sophistication over time',
    'If amplifying: institutional intervention (norms, platform design) could reduce extraction. If neutral substrate: snare is driven by psychology, not culture, and requires individual-level intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_amplification_feedback, empirical, 'Whether institutional systems amplify or merely enable communal narcissism extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(communal_narcissism_social_trap, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, communal_narcissism_social_trap, theater_ratio, 0, 0.52).
narrative_ontology:measurement(comm_tr_t2, communal_narcissism_social_trap, theater_ratio, 2, 0.67).
narrative_ontology:measurement(comm_tr_t4, communal_narcissism_social_trap, theater_ratio, 4, 0.81).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, communal_narcissism_social_trap, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comm_be_t2, communal_narcissism_social_trap, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(comm_be_t4, communal_narcissism_social_trap, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(communal_narcissism_social_trap, resource_allocation).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, high_control_group_isolation).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, therapeutic_language_appropriation).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, narcissist_supply_ecosystem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
