% ============================================================================
% CONSTRAINT STORY: harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harm_threshold_reading, []).

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
 *   constraint_id: harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Absence of Demonstrable Harm (Harm-Threshold Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The harm-threshold reading of speech protection establishes that speakers
 *   may be restricted when their speech causes demonstrable harm to
 *   identifiable victims. This reading represents a middle position between
 *   absolutism (no threshold; speech always protected) and broad
 *   harm-inclusive approaches (psychological, reputational, cultural harms
 *   count). The constraint operates by converting the abstract principle
 *   'speech is free' into a concrete rule: 'speech is free unless it causes
 *   harm above an adjudicated threshold.' This rule coordinates legitimate
 *   state interests in preventing concrete harms while creating systematic
 *   extraction opportunities for speakers with institutional power to define
 *   what counts as harm. The extractiveness value (0.58) reflects that the
 *   harm-threshold mechanism produces moderate but significant asymmetric
 *   outcomes: well-resourced speakers can navigate threshold criteria and
 *   often find their harms recognized, while powerless speakers face
 *   suppression both when their harm claims fall below the threshold AND when
 *   speaking itself is classified as harmful. The theater ratio (0.48)
 *   indicates moderate performative content: adjudication of harm involves
 *   genuine legal/scientific work (theater is not dominant) but also includes
 *   performative risk assessment and reputation signaling.
 *
 * KEY AGENTS:
 *   - Speakers at Harm Threshold: Primary victims (powerless/trapped) — face suppression once harm claims cross adjudicated threshold; no effective exit
 *   - Marginalized Communities with Contested Harm Claims: Primary victims (powerless/trapped) — whose harms are not yet recognized at threshold; trapped in unequal protection regimes
 *   - Established Speakers with Threshold-Protected Claims: Primary beneficiaries (powerful/mobile) — enjoy both protection coordination (legitimate defense against weaponized speech) and extraction advantage (ability to deploy harm-suppression authority against competitors)
 *   - State Enforcement Apparatus: Beneficiary (institutional/arbitrage) — gains clear criteria for speech regulation and institutional legitimacy for enforcement decisions
 *   - Adjudicating Authority (Courts/Regulators): Mixed position (moderate/constrained) — coordinates harm determination while extracting institutional power through arbitration of disputes
 *   - Absolutist Free Speech Tradition: Institutional constraint (institutional/arbitrage) — persists formally while functionally hollowed by harm-threshold rule (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating constructed institutional boundary as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harm_threshold_reading, 0.58).
domain_priors:suppression_score(harm_threshold_reading, 0.65).
domain_priors:theater_ratio(harm_threshold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harm_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(harm_threshold_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(harm_threshold_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(harm_threshold_reading, "Speech Protection Conditional on Absence of Demonstrable Harm (Harm-Threshold Reading)").
narrative_ontology:topic_domain(harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(harm_threshold_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harm_threshold_reading, harm_victims_and_vulnerable_groups).
narrative_ontology:constraint_beneficiary(harm_threshold_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(harm_threshold_reading, speakers_at_harm_threshold).
narrative_ontology:constraint_victim(harm_threshold_reading, marginalized_communities_with_contested_harm_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPEAKER AT HARM THRESHOLD (SNARE) — Once a harm claim reaches threshold credibility, speaker faces near-total suppression of contested speech. No effective exit: continuing speech incurs legal/social sanctions; silence is extraction of autonomy. The threshold is opaque (harm is context-dependent) so speaker cannot predict safety. Maximal extraction without organization or appeal option.
constraint_indexing:constraint_classification(harm_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES WITH CONTESTED HARM CLAIMS (SNARE) — Groups whose harm claims are not yet at threshold (or are rejected by adjudicating authority) face extraction: their harms are silenced while others' harms trigger protection. No exit from the harm itself; trapped in unequal recognition of damage. The harm-threshold rule becomes a mechanism that redistributes speech protection by power asymmetry.
constraint_indexing:constraint_classification(harm_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED SPEAKERS WITH THRESHOLD-PROTECTED CLAIMS (TANGLED ROPE) — Well-resourced speakers whose speech is recognized as harmful at threshold enjoy coordination benefit (legitimate protection from weaponized speech) alongside extraction benefit (expanded suppression authority that can be deployed against competitors). Mobile exit: can migrate speech platforms, jurisdictions, or frames. The constraint coordinates legitimate harm prevention while extracting competitive advantage.
constraint_indexing:constraint_classification(harm_threshold_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENFORCEMENT APPARATUS (ROPE) — Sees the harm-threshold rule as a coordination mechanism: establishes criteria for speech regulation, reduces arbitrary censorship, and provides institutional legitimacy for enforcement. State benefits from clear(ish) standards and can arbitrage across jurisdictions. The constraint coordinates legitimate state interests in preventing concrete harms.
constraint_indexing:constraint_classification(harm_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ADJUDICATING AUTHORITY (COURTS/REGULATORS) (TANGLED ROPE) — Bears both coordination function (determines harm threshold, applies criteria) and extraction function (authority grows as harm-claim disputes increase, creating institutional power through arbitration). Constrained by precedent and political pressure. The authority experiences the constraint as empowering (relative to no standard) while becoming a bottleneck (all threshold disputes route through this actor).
constraint_indexing:constraint_classification(harm_threshold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ABSOLUTIST FREE SPEECH TRADITION (PITON) — The prior institutional commitment (no prior restraint, speaker autonomy as paramount) persists formally while the harm-threshold rule hollows it from inside. The absolutist doctrine is maintained performatively (courts still cite speaker protection) while its functional immunity erodes. Theater ratio reflects the gap between the principle invoked and the outcomes produced.
constraint_indexing:constraint_classification(harm_threshold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the harm-threshold principle appears as a natural limit on speech freedom: all social coordination requires boundaries, and preventing concrete demonstrable harm is a natural starting point for speech regulation. This perspective sees the constraint as inevitable and unchangeable — a law of social order. However, the structural data contradicts the mountain classification: identified beneficiaries (state enforcement, threshold-protected speakers) and asymmetric extraction suggest this is a constructed institutional arrangement, not a natural boundary.
constraint_indexing:constraint_classification(harm_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harm_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_threshold_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harm_threshold_reading, TR),
    TR >= 0.70.

:- end_tests(harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The harm-threshold reading produces moderate-high extraction because threshold determination itself becomes a mechanism for allocating speech protection asymmetrically. Speakers with institutional resources, professional credibility, or demographic position have higher probability of their harm claims being adjudicated at threshold-crossing levels. Marginalized speakers face dual extraction: (1) their own harm claims are discounted (threshold not reached), and (2) speech they produce is more likely to be classified as harmful to others (lower threshold bar applied to their speech than to established speakers' speech). The extractiveness rises over the interval (0.35 → 0.58) as harm-claim litigation accumulates and threshold jurisprudence becomes more developed, entrenching power asymmetries. Suppression (0.65): High. The harm-threshold rule generates suppression through both direct enforcement (speech is prohibited) and anticipatory suppression (speakers self-censor to avoid harm claims). The threshold creates epistemic uncertainty: speakers cannot ex-ante know whether their speech will cross it, so suppression risk is continuous, not just post-decision. Theater ratio (0.48): Moderate. Harm adjudication involves genuine legal work (evidence of harm, causal reasoning, precedent application) so the ratio is not dominated by performative content. However, some theater exists: risk assessment signaling by adjudicators, performative victim-consideration by institutions, and reputation management by speakers all contribute.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a four-way perspectival split. The speaker at threshold sees a snare: once a harm claim reaches adjudicated credibility, exit is closed off and suppression is near-total. Marginalized communities with unrecognized harm claims see a snare: their harms are extracted (silence) while more powerful actors' harms trigger protection. Established speakers with threshold-protected claims see a tangled rope: they experience both coordination benefit (protection from weaponized speech) and extraction benefit (expanded authority to suppress competitors). The state enforcement apparatus sees a rope: the harm-threshold rule provides coordination criteria and institutional legitimacy. The adjudicating authority sees a tangled rope: it coordinates threshold determination while accumulating institutional power. The absolutist tradition sees itself as mountain (permanent principle) but is actually piton (formally preserved, functionally hollowed). The analytical observer risks seeing mountain (harm prevention is a natural limit on speech) but the structural data reveals false summit (the rule has identifiable beneficiaries and power-asymmetric application).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the constraint. Speakers at threshold and marginalized communities with unrecognized harm claims are trapped victims: d ≈ 0.92-0.98, producing maximum experienced extractiveness. Established speakers with recognized harms are mobile beneficiaries: d ≈ 0.10-0.25, producing low or negative extraction experiences — they experience the constraint as coordinating protection. The state and adjudicatory authority are institutional beneficiaries with arbitrage: d ≈ 0.05-0.15, producing institutional benefits (clear standards, legitimacy). The analytical observer is analytical: d ≈ 0.72 (canonical fallback), producing moderate experienced extraction of the observer's analytical framing by the constraint's natural-law claim. The beneficiary declarations (harm victims and vulnerable groups as nominal beneficiaries; speakers at threshold and marginalized communities as nominal victims) route through the derivation chain to produce high d for the trapped/powerless perspectives and low d for the arbitrage perspectives, which produces the perspectival gap observed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the harm-threshold rule is a genuine hybrid: it coordinates legitimate state interests in preventing concrete demonstrable harms (coordination function) while asymmetrically distributing speech suppression according to speaker power (extraction function). Neither function dominates; both are structural. The tangled_rope classification captures this: the constraint is not pure coordination (rope) because it produces systematic asymmetric harm, and it is not pure extraction (snare) because it does coordinate legitimate harm prevention. The classification is contingent on the threshold definition — if thresholds are power-neutral (equally applied to all speaker groups), extractiveness would be lower and the constraint would move toward rope. If thresholds are power-saturated (applied asymmetrically), extractiveness would rise toward snare. The measurement trajectory (extractiveness rising from 0.35 to 0.58 over 20 years) suggests the rule is functioning with increasing asymmetry: as threshold case law accumulates, patterns of unequal application become entrenched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_contestation,
    'Who has authority to define ''demonstrable harm'' and at what threshold does a claim qualify?',
    'Comparative institutional analysis of harm definitions across jurisdictions, time periods, and identity groups; correlation between speaker identity/power and likelihood of harm claim credibility',
    'If definition is narrow (physical injury only): constraint is closer to rope (coordination function dominant). If definition is broad (psychological, reputational, community damage): constraint is snare (extraction dominant because threshold becomes arbitrary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_contestation, empirical, 'Authority over harm definition and threshold credibility').

omega_variable(
    power_asymmetry_in_threshold_adjudication,
    'Do speakers from dominant groups have systematically higher probability of their harm claims crossing the threshold than speakers from marginalized groups?',
    'Statistical analysis of harm threshold acceptances by speaker group identity, institutional affiliation, resource access; case law patterns showing differential threshold application',
    'If yes: constraint is snare with systematic extraction of voice from less-resourced speakers. If no: constraint is closer to tangled rope with coordination function more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_in_threshold_adjudication, empirical, 'Whether threshold adjudication is power-asymmetric').

omega_variable(
    boundary_between_readings,
    'This constraint instantiates the HARM-THRESHOLD reading. What structural elements would change under sibling readings?',
    'Comparison with absolutist_reading (no threshold — speech protected unconditionally), marketplace_reading (truth crowd-sorts harm claims), dignity_reading (harm includes identity attack), democratic_participation_reading (harm assessed by impact on collective self-governance)',
    'Under absolutist_reading: extractiveness drops to ~0.15, snare classifications become ropes. Under dignity_reading: extractiveness rises to ~0.72, piton classifications become tangled ropes. Under democratic_participation_reading: beneficiary set shifts to ''public'', extractiveness may rise or fall depending on how democratic participation is measured. The omegas in THIS reading document that the constraint-as-written instantiates only the harm-threshold frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_between_readings, conceptual, 'Structural difference between harm-threshold reading and sibling readings of the speech protection kernel').

omega_variable(
    false_summit_natural_law_risk,
    'Is the harm-threshold principle a natural boundary on speech freedom (mountain) or a contingent institutional choice?',
    'Historical institutional analysis: does harm-threshold rule appear in all speech systems or only some? Do legal systems without this rule experience qualitatively different speech dynamics? Are there observable alternatives that produce similar coordination outcomes without extractive asymmetry?',
    'If natural: mountain classification is correct; the constraint is unavoidable. If contingent: mountain classification is false summit; the rule is constructed and has identifiable beneficiaries who sustain it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether harm-threshold principle is natural law or constructed institutional choice').

omega_variable(
    chilling_effect_measurement,
    'How much speech is suppressed by the anticipation of harm claims (chilling effect) relative to speech actually suppressed by enforcement?',
    'Survey data on speaker self-censorship motivated by harm-threshold fear; comparison of pre/post harm-threshold adoption in same jurisdiction; linguistic analysis of speech corpus for markers of anticipatory suppression',
    'If chilling effect is large (≥60% of suppression): extractiveness rises to 0.68+, snare classifications become more dominant. If small (≤20%): extractiveness falls to 0.40, tangled rope becomes more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_measurement, empirical, 'Magnitude of chilling effect from harm-threshold rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harm_threshold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_theater_t0, harm_threshold_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(harm_theater_t10, harm_threshold_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(harm_theater_t20, harm_threshold_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(harm_extractiveness_t0, harm_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(harm_extractiveness_t10, harm_threshold_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(harm_extractiveness_t20, harm_threshold_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(harm_threshold_reading, absolutist_reading).
narrative_ontology:affects_constraint(harm_threshold_reading, marketplace_reading).
narrative_ontology:affects_constraint(harm_threshold_reading, dignity_reading).
narrative_ontology:affects_constraint(harm_threshold_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: speech_protection_kernel. The harm-threshold reading instantiates a specific institutional boundary rule (speech protection conditional on absence of demonstrable harm). The sibling readings (absolutist, marketplace, dignity, democratic_participation) instantiate alternative boundaries on the same kernel. These are not the same constraint viewed from different angles — they are distinct constraints with different ε values, different beneficiary/victim structures, and different institutional mechanisms. The network links document the kernel-based family relationship. The harm-threshold reading has extractiveness 0.58; the absolutist reading has extractiveness ≈0.15 (minimal extraction by definition); the dignity reading has extractiveness ≈0.72 (broader harm category produces more threshold disputes and more extraction opportunity); the democratic participation reading has extractiveness ≈0.45 (variable depending on how democratic harm is measured).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harm_threshold_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
