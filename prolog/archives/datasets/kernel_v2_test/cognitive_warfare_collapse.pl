% ============================================================================
% CONSTRAINT STORY: cognitive_warfare_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_warfare_collapse, []).

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
 *   constraint_id: cognitive_warfare_collapse
 *   human_readable: Cognitive Warfare Collapse: Self-Defeating Information Operations
 *   domain: military_operations/information_warfare/institutional_dysfunction
 *
 * SUMMARY:
 *   Russian information operations claiming simultaneous territorial advances
 *   and imminent Ukrainian collapse face a structural contradiction:
 *   observable Ukrainian counterattacks in areas Russia claims to control,
 *   Ukrainian territorial gains contradicting the collapse narrative, and
 *   international media coverage documenting the gap between Russian claims
 *   and ISW assessments. This constraint represents a self-defeating
 *   information warfare strategy where maximalist claims intended to
 *   demoralize adversaries and secure domestic support instead erode
 *   credibility capital through repeated contradiction. The theater_ratio
 *   (0.85) reflects that the information operations apparatus has become
 *   primarily performative — claims are produced to satisfy institutional
 *   mandate rather than to achieve psychological effects, as evidenced by the
 *   predictable pattern of overclaim followed by observable contradiction.
 *   The constraint exhibits identity-lock dynamics: the Russian IO apparatus
 *   is structurally capable of moderating claims but cannot do so without
 *   dissolving its institutional identity, which is constituted through the
 *   production of maximalist narratives. The suppression requirement (0.72)
 *   has increased over the interval as maintaining the narrative requires
 *   escalating control over domestic information access to prevent exposure
 *   to contradictory evidence.
 *
 * KEY AGENTS:
 *   - Russian Information Operations Apparatus: Primary victim (institutional/identity_locked) — identity-fused with maximalist narrative production; each contradicted claim erodes credibility but institutional frame prevents acknowledgment
 *   - Russian Domestic Audience: Secondary victim (powerless/trapped) — trapped in state media ecosystem; bears cognitive dissonance cost of contradictory claims with no exit option
 *   - Russian Military Field Commanders: Tertiary victim (moderate/constrained) — constrained by institutional pressure to report optimistically; bears operational cost when false reports contaminate planning
 *   - Ukrainian Information Operations: Primary beneficiary (institutional/arbitrage) — exploits Russian overclaims for reputational gain; can document observable reality and let contradiction do the work
 *   - International Media Verification Systems: Secondary beneficiary (institutional/arbitrage) — gains relevance and authority through verification of contradicted claims
 *   - Russian State Media Apparatus: Institutional actor (institutional/constrained) — maintains performative reporting ritual despite observable dysfunction; sees own process as degraded
 *   - Analytical Observer: Cross-position view (analytical/analytical) — sees mixed coordination and extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_warfare_collapse, 0.68).
domain_priors:suppression_score(cognitive_warfare_collapse, 0.72).
domain_priors:theater_ratio(cognitive_warfare_collapse, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_warfare_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(cognitive_warfare_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cognitive_warfare_collapse, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_warfare_collapse, snare).
narrative_ontology:human_readable(cognitive_warfare_collapse, "Cognitive Warfare Collapse: Self-Defeating Information Operations").
narrative_ontology:topic_domain(cognitive_warfare_collapse, "military_operations/information_warfare/institutional_dysfunction").

domain_priors:requires_active_enforcement(cognitive_warfare_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_warfare_collapse, ukrainian_information_operations).
narrative_ontology:constraint_beneficiary(cognitive_warfare_collapse, international_media_verification_systems).
narrative_ontology:constraint_beneficiary(cognitive_warfare_collapse, western_intelligence_assessment_infrastructure).
narrative_ontology:constraint_victim(cognitive_warfare_collapse, russian_information_operations).
narrative_ontology:constraint_victim(cognitive_warfare_collapse, russian_domestic_audience).
narrative_ontology:constraint_victim(cognitive_warfare_collapse, russian_military_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN DOMESTIC AUDIENCE (SNARE) — Trapped in information environment where contradictory claims (simultaneous advances and Ukrainian collapse vs observable Ukrainian counterattacks) create cognitive dissonance. No exit from state media ecosystem. Bears full cost of epistemic contamination — cannot distinguish signal from noise, cannot verify claims independently, cannot exit the narrative frame without severe social and legal penalties.
constraint_indexing:constraint_classification(cognitive_warfare_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RUSSIAN INFORMATION OPERATIONS (SNARE) — Identity-locked institutional actor whose professional identity and organizational mandate are constituted through the production of maximalist claims. Cannot exit the escalatory narrative logic without dissolving institutional purpose. Experiences the constraint as pure extraction: each contradicted claim erodes credibility capital, but the institutional frame prevents acknowledgment of the contradiction. The apparatus is structurally mobile (could revise claims) but identity-fused with the maximalist narrative — exit would require becoming a different institution.
constraint_indexing:constraint_classification(cognitive_warfare_collapse, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: RUSSIAN MILITARY FIELD COMMANDERS (TANGLED ROPE) — Constrained by institutional pressure to report optimistic assessments while facing observable battlefield reality. Benefits from the information operations apparatus when it successfully demoralizes adversaries or secures domestic support, but bears extraction when contradicted claims undermine operational credibility and force allocation of resources to defend narratively 'controlled' areas that are actually contested. Mixed coordination (reporting structure enables command decisions) and extraction (false reports contaminate operational planning).
constraint_indexing:constraint_classification(cognitive_warfare_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: UKRAINIAN INFORMATION OPERATIONS (ROPE) — Primary beneficiary. Russian maximalist claims create verification opportunities: each contradicted claim is a reputational asset. Ukrainian IO can simply document observable reality (counterattacks in 'controlled' areas, territorial gains vs collapse narrative) and let the contradiction do the work. Experiences the constraint as coordination: the Russian narrative structure creates predictable targets for counter-messaging. Net beneficiary with arbitrage-level exit — can choose when and how to exploit Russian overclaims.
constraint_indexing:constraint_classification(cognitive_warfare_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MEDIA VERIFICATION (ROPE) — Benefits from the constraint through increased relevance and authority. Russian maximalist claims create demand for verification infrastructure (ISW assessments, OSINT analysis, satellite imagery). The contradiction between claims and observable reality is a coordination problem the verification systems solve, enhancing their institutional position. Low extraction — the verification work is their mandate and they have exit options (can choose which claims to verify).
constraint_indexing:constraint_classification(cognitive_warfare_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RUSSIAN STATE MEDIA (PITON) — The media apparatus maintains the ritual of reporting maximalist claims despite observable contradictions. The function (shaping domestic perception) has atrophied into performance: audiences increasingly recognize the claims as theatrical, but the apparatus persists through institutional inertia. The media sees its own process as degraded — maintained because the institutional structure demands it, not because it achieves the intended psychological effect. High theater ratio, low functional coordination.
constraint_indexing:constraint_classification(cognitive_warfare_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both coordination function (information operations as legitimate military capability) and extraction mechanism (self-defeating overclaims that invert intended effect). The constraint coordinates domestic morale and international perception management (genuine military function) while extracting from Russian credibility capital through contradicted claims. The analytical perspective recognizes the mixed structure: not pure extraction (there is a real coordination problem being addressed) and not pure coordination (the execution creates asymmetric costs).
constraint_indexing:constraint_classification(cognitive_warfare_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_warfare_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_warfare_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_warfare_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_warfare_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_warfare_collapse, TR),
    TR >= 0.70.

:- end_tests(cognitive_warfare_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Russian information operations apparatus extracts from its own credibility capital through repeated contradicted claims. Each maximalist claim that is observably contradicted (Ukrainian counterattacks in 'controlled' areas, territorial gains vs collapse narrative) depletes the credibility reserve that future claims draw on. The extraction is substantial because the contradiction is systematic rather than occasional — the pattern is predictable and the international verification infrastructure (ISW, OSINT, satellite imagery) documents it in real time. The value reflects that the apparatus is not merely making errors but is structurally committed to overclaiming in ways that invert the intended psychological effect. Suppression (0.72): High. Maintaining the maximalist narrative requires substantial suppression of contradictory information: domestic media control, VPN restrictions, legal penalties for 'discrediting' the military, and social pressure against questioning official claims. The suppression requirement has increased over the interval as the gap between claims and observable reality has widened — more coercion is needed to prevent domestic audiences from accessing verification sources. Theater ratio (0.85): Very high. The information operations have become primarily performative. The apparatus produces maximalist claims to satisfy institutional mandate and domestic political expectations, but the claims no longer achieve their intended psychological effects — adversaries are not demoralized (Ukrainian forces continue counterattacking), international audiences are not persuaded (verification systems document contradictions), and domestic audiences increasingly recognize the theatrical nature of the claims (though they cannot publicly acknowledge this recognition). The theater has increased over the interval as the contradiction pattern has become more predictable and the verification infrastructure has matured.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a critical inversion: the Russian information operations apparatus (institutional actor) experiences the constraint as a snare due to identity-lock, while Ukrainian information operations (also institutional) experience it as rope due to beneficiary status and arbitrage exit. This is not a power differential — both are institutional actors with global scope — but a structural position differential determined by relationship to the extraction flow. The Russian domestic audience sees snare (trapped, maximum extraction), field commanders see tangled rope (mixed coordination and extraction), state media sees piton (degraded performative function), and the analytical observer sees tangled rope (genuine coordination function undermined by extractive execution). The perspectival gap reveals that institutional power does not immunize against extraction when the institution is identity-locked into a self-defeating strategy. The gap between the Russian IO apparatus's snare experience and Ukrainian IO's rope experience measures the credibility capital transfer: what Russia loses through contradicted claims, Ukraine gains through documented reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Russian information operations apparatus is the primary victim despite being an institutional actor because it is identity-locked: the institution's professional identity and organizational mandate are constituted through the production of maximalist claims, making exit from the escalatory narrative logic equivalent to institutional dissolution. The apparatus is structurally mobile (could revise claims toward defensible assessments) but functionally trapped by identity fusion. This produces high directionality (d approaching 1.0) and high effective extraction despite institutional power level. The Russian domestic audience is a secondary victim with maximum directionality (d = 1.0): powerless, trapped, and bearing full cognitive dissonance cost with no exit option. Russian military field commanders are tertiary victims with moderate directionality: constrained by institutional pressure but retaining some agency, and experiencing mixed coordination (reporting structure) and extraction (false reports contaminating planning). Ukrainian information operations and international verification systems are primary beneficiaries with low directionality (d approaching 0.0): they benefit from Russian overclaims through reputational gain and increased institutional relevance, with arbitrage-level exit options (can choose when and how to exploit contradictions). The Russian state media apparatus has moderate directionality: constrained by institutional mandate to report maximalist claims but aware of the performative nature of the process, experiencing the constraint as degraded function rather than pure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: the Russian information operations apparatus's original mandate (shape perception to support military objectives) has inverted into dysfunction (erode credibility through contradicted claims). However, mandatrophy is not yet resolved because the institutional structure cannot acknowledge the inversion without dissolving its identity. The apparatus continues producing maximalist claims not because they work but because the institutional frame defines success as claim production rather than psychological effect. The measurements show mandatrophy progression: theater_ratio rising from 0.55 to 0.85 indicates the function-to-performance ratio inverting over time, while base_extractiveness rising from 0.42 to 0.68 indicates accumulating credibility capital depletion. The constraint will resolve into terminal mandatrophy when credibility capital is exhausted such that even true Russian claims are dismissed — at that point the apparatus's mandate becomes literally impossible to fulfill. The omega variables identify the empirical questions that determine whether mandatrophy is reversible (can the apparatus learn and adjust?) or terminal (is it structurally locked into escalatory maximalism?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    audience_segmentation_effectiveness,
    'Do Russian information operations successfully segment audiences such that domestic audiences never encounter contradictory evidence, making the ''collapse'' narrative effective domestically even while failing internationally?',
    'Survey data on Russian domestic awareness of Ukrainian counterattacks; correlation between state media consumption and belief in maximalist claims; VPN usage and access to alternative information sources',
    'If segmentation effective: extraction is lower than measured (domestic audience is coordinated, not trapped). If segmentation fails: extraction is as measured or higher (domestic audience experiences cognitive dissonance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_segmentation_effectiveness, empirical, 'Whether audience segmentation prevents domestic exposure to contradictions').

omega_variable(
    institutional_learning_capacity,
    'Can the Russian information operations apparatus learn from contradicted claims and adjust toward more defensible narratives, or is it structurally locked into escalatory maximalism?',
    'Longitudinal analysis of claim patterns: do contradicted claims lead to subsequent moderation, or do they trigger further escalation? Comparison of early-war vs late-war claim magnitudes and verification rates.',
    'If learning capacity exists: the constraint is scaffold (temporary dysfunction being corrected). If structurally locked: the constraint is snare (identity-locked extraction with no exit path).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_learning_capacity, empirical, 'Whether the IO apparatus can learn from contradicted claims').

omega_variable(
    credibility_capital_depletion_threshold,
    'At what point does accumulated contradiction exhaust Russian information operations'' credibility capital such that even true claims are dismissed?',
    'Measurement of international media treatment of Russian claims over time; correlation between contradiction rate and dismissal rate of subsequent claims; threshold analysis of credibility recovery after verified claims',
    'If threshold already crossed: the constraint has resolved into terminal dysfunction (mandatrophy). If threshold not yet reached: extraction is ongoing but reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_capital_depletion_threshold, empirical, 'Threshold at which credibility capital is exhausted').

omega_variable(
    domestic_vs_international_mandate_priority,
    'Is the Russian information operations apparatus optimizing for domestic morale (where contradictions may not penetrate) or international perception (where contradictions are immediately visible)?',
    'Analysis of claim distribution patterns: are maximalist claims concentrated in domestic media or also pushed internationally? Resource allocation between domestic and international IO channels; institutional incentive structures within the apparatus.',
    'If domestic-optimized: international credibility loss is acceptable cost, and extraction is lower than measured from the apparatus''s perspective. If international-optimized: the constraint is pure dysfunction (snare from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_vs_international_mandate_priority, conceptual, 'Whether the apparatus prioritizes domestic or international audiences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_warfare_collapse, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogwar_theater_early, cognitive_warfare_collapse, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cogwar_theater_mid, cognitive_warfare_collapse, theater_ratio, 3, 0.68).
narrative_ontology:measurement(cogwar_theater_late, cognitive_warfare_collapse, theater_ratio, 6, 0.78).
narrative_ontology:measurement(cogwar_theater_current, cognitive_warfare_collapse, theater_ratio, 9, 0.85).

% Extraction over time
narrative_ontology:measurement(cogwar_extract_early, cognitive_warfare_collapse, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cogwar_extract_mid, cognitive_warfare_collapse, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(cogwar_extract_late, cognitive_warfare_collapse, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(cogwar_extract_current, cognitive_warfare_collapse, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cogwar_suppress_early, cognitive_warfare_collapse, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cogwar_suppress_mid, cognitive_warfare_collapse, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(cogwar_suppress_late, cognitive_warfare_collapse, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(cogwar_suppress_current, cognitive_warfare_collapse, suppression_requirement, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_warfare_collapse, information_standard).
narrative_ontology:affects_constraint(cognitive_warfare_collapse, verification_authority_fragmentation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of verification_authority_fragmentation (the broader epistemic crisis in information verification) but represents a distinct structural phenomenon: the self-defeating dynamics of maximalist information operations in an environment with mature verification infrastructure. The upstream constraint describes the fragmentation of verification authority; this constraint describes what happens when an institutional actor committed to maximalist claims operates in that fragmented environment. The two constraints have different victim sets (verification_authority_fragmentation victimizes epistemic commons broadly; cognitive_warfare_collapse victimizes the Russian IO apparatus specifically) and different extraction mechanisms (fragmentation creates coordination failure; cognitive warfare creates credibility capital depletion through contradiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_warfare_collapse, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
