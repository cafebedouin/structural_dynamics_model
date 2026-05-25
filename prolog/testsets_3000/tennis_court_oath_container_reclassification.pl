% ============================================================================
% CONSTRAINT STORY: tennis_court_oath_container_reclassification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tennis_court_oath_container_reclassification, []).

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
 *   constraint_id: tennis_court_oath_container_reclassification
 *   human_readable: Tennis Court Oath: Inner-Container Claim to Outer Authority
 *   domain: french_history/revolutionary_moment
 *
 * SUMMARY:
 *   The Tennis Court Oath of June 20, 1789 represents a structural moment
 *   when an inner container (the Third Estate's deputies) claims authority
 *   that the outer container (the Crown and Estates-General machinery) has
 *   become unable to grant through legitimate procedure. The Estates-General
 *   was convened to address fiscal crisis, but the Crown's control of agenda
 *   and voting rules (estate-by-estate rather than individual deputy) blocked
 *   Third Estate proposals for meaningful constitutional change. When the
 *   Crown dissolved the Estates-General to prevent assembly votes, deputies
 *   responded by reconvening in a nearby tennis court and swearing not to
 *   disperse until a constitution was drafted. This oath was simultaneously a
 *   coordination mechanism (enabling unified constitutional action) and an
 *   authority extraction mechanism (claiming constitutional power without
 *   Crown consent or procedural legitimacy). The constraint demonstrates the
 *   framework's prediction about blocked outer-container bandwidth: when the
 *   legitimate channel becomes inert, inner containers will accumulate
 *   authority claims that the outer container has no procedure to process.
 *   The oath's reclassification from revolutionary illegitimacy (snare from
 *   Crown perspective) to constitutional foundation (rope from National
 *   Assembly perspective) tracks the migration of de facto authority and the
 *   system's eventual acknowledgment that procedural legitimacy must align
 *   with actual power distribution.
 *
 * KEY AGENTS:
 *   - Third Estate Deputies: Primary claimant (moderate/constrained at oath moment, then organized/mobile in National Assembly) — seek to transform constitutional authority; coordinate collective action while extracting authority from Crown
 *   - Revolutionary Leadership (Mirabeau, Sieyès, radical faction): Tactical organizers (powerful/mobile) — drive the oath's specific form and timing; benefit from the constraint's authority transfer
 *   - Absolute Monarchy and Crown Authority: Primary target (institutional/trapped) — experiences the oath as illegitimate seizure; cannot grant legitimacy through existing procedure; must either suppress militarily or accept reclassification
 *   - Traditional Estates-General Procedure: Secondary target (institutional/trapped) — rendered inert by the oath; authority flows around procedural channels
 *   - Aristocratic Moderates: Secondary claimant (powerful/constrained) — join National Assembly, benefit from coordination but bear cost of authority displacement from estate-based hierarchy
 *   - Blocked Legitimate Channels: Structural victim (procedural machinery) — petitions, motions, voting produce no action; procedure itself becomes inert
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent constitutional choice as inevitable social law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tennis_court_oath_container_reclassification, 0.52).
domain_priors:suppression_score(tennis_court_oath_container_reclassification, 0.68).
domain_priors:theater_ratio(tennis_court_oath_container_reclassification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tennis_court_oath_container_reclassification, extractiveness, 0.52).
narrative_ontology:constraint_metric(tennis_court_oath_container_reclassification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tennis_court_oath_container_reclassification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tennis_court_oath_container_reclassification, tangled_rope).
narrative_ontology:human_readable(tennis_court_oath_container_reclassification, "Tennis Court Oath: Inner-Container Claim to Outer Authority").
narrative_ontology:topic_domain(tennis_court_oath_container_reclassification, "french_history/revolutionary_moment").

domain_priors:requires_active_enforcement(tennis_court_oath_container_reclassification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tennis_court_oath_container_reclassification, third_estate_deputies).
narrative_ontology:constraint_beneficiary(tennis_court_oath_container_reclassification, revolutionary_leadership).
narrative_ontology:constraint_victim(tennis_court_oath_container_reclassification, absolute_monarchy_authority).
narrative_ontology:constraint_victim(tennis_court_oath_container_reclassification, traditional_estates_hierarchy).
narrative_ontology:constraint_victim(tennis_court_oath_container_reclassification, blocked_legitimate_channels).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABSOLUTE MONARCHY / TRADITIONAL STRUCTURE (SNARE) — The Crown and the estate-based hierarchy experience the Tennis Court Oath as illegitimate seizure of authority with no procedural legitimacy. The outer container (Crown + Estates-General machinery) has no exit from this claim — cannot grant legitimacy through existing procedure (would require constitutional convention that the Crown has no power to call), cannot suppress the claim without military force (contingent on armies' willingness). The constraint extracts authority from the Crown without compensation or institutional recognition. Suppression is high but depends on coercive capacity, not structural legitimacy. Maximum experienced extraction from this perspective.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD ESTATE DEPUTIES (TANGLED ROPE) — The deputies coordinate genuine constitutional action (coordination function: unified drafting platform, delegation, collective will expression) while simultaneously extracting authority from the legitimate procedural order that locked them out. The constraint has dual function: enables unprecedented collective action (coordination benefit) and bypasses the Crown's veto (extraction mechanism). Exit cost is high — breaking the oath means returning to powerlessness within blocked Estates-General. Extraction runs toward the Third Estate but at cost of institutional illegitimacy and military suppression risk.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONAL ASSEMBLY COALITION (ROPE) — Once the oath transforms into operational National Assembly, the constraint becomes pure coordination. Multiple parties (radical Third Estate, moderate aristocrats, clergy sympathizers) benefit from a unified constitutional drafting platform with collective decision-making. The coordination function becomes dominant: writing a constitution is a genuine multi-party problem requiring unified mechanism. Low suppression once the Assembly has de facto authority. Effective extraction is minimal because all parties benefit from the coordination structure itself.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: BLOCKED LEGITIMATE CHANNELS / PROCEDURAL MACHINERY (SNARE) — The Estates-General structure and its procedural rules are rendered inert — locked out deputies cannot work within the system; attempts to use legitimate channels (petitions, formal motions, voting) produce no action. The procedural machinery experiences extraction: authority flows around it without consent, procedure itself becomes irrelevant. No exit from this degradation without Crown intervention (restore the Channel, grant constitutional convention power). The constraint suppresses the legitimacy of traditional procedure itself.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ANCIEN RÉGIME INSTITUTIONAL MACHINERY (PITON) — From a generational view, the estate-based hierarchy and Crown prerogative persist as performative structures even after the Tennis Court Oath. The formal Estates-General continues to meet; royal authority formally persists; traditional deference language continues. But the actual authority has migrated to the National Assembly. The old machinery is maintained through theater (formal sessions, ceremonial acknowledgment, legal fiction that the Crown retains veto) despite loss of functional power. Theater ratio is high because the institutional forms persist while real authority has evacuated to the Assembly's constitutional work.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ARISTOCRATIC MODERATES (TANGLED ROPE) — Moderate aristocrats who join the National Assembly experience mixed coordination and extraction. The Assembly coordinates genuine constitutional drafting (benefit); however, the oath's framing bypasses aristocratic legitimacy claims and subordinates estate representation to individual deputy authority (extraction mechanism). The moderates benefit from the Assembly's existence but bear cost of authority displacement. Their exit is constrained — breaking with the Assembly means aligning with a Crown that has lost de facto power, or isolation from constitution-writing entirely.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CIVILIZATIONAL (MOUNTAIN) — From a long-duration analytical view, the Tennis Court Oath appears as an inevitable expression of blocked container dynamics: when outer-container bandwidth is saturated and inner containers accumulate unprocessed claims, some inner container must eventually claim authority outside procedure. This perspective risks naturalizing the oath as a law of social dynamics — 'when procedure fails, direct action becomes inevitable.' However, the structural data reveals this as a false summit: the oath's specific form (constitutional claim, collective oath, venue, timing) reflects contingent political choices, not structural necessity. The analytical perspective may wrongly naturalize what was actually contingent mobilization.
constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tennis_court_oath_container_reclassification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tennis_court_oath_container_reclassification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tennis_court_oath_container_reclassification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tennis_court_oath_container_reclassification, TR),
    TR >= 0.70.

:- end_tests(tennis_court_oath_container_reclassification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the dual nature of the constraint. The oath extracts constitutional authority from the Crown (+0.35 base for procedural bypass) but creates genuine coordination value for the Third Estate and emerging Assembly (~-0.15 coordination benefit). The net is significant extraction because the authority transfer is uncompensated and non-consensual from the Crown's perspective. The trajectory shows rising extractiveness immediately post-oath (0.52) as Assembly asserts power, plateauing (0.58) as Assembly consolidates, then declining (0.48) as constitutional framework provides new legitimacy baseline and procedural regularity. Suppression (0.68): High. The oath's enforceability depends on Third Estate deputies' collective will to reconvene, Assembly members' refusal to disperse individually, and passive resistance to Crown commands. The Crown could suppress through military force (logistics difficult), but actual suppression never occurs — instead, de facto authority migration and eventual constitutional legitimacy eventually reduce suppression requirements. Theater ratio (0.55): Moderate. The oath itself is highly symbolic and performative (swearing not to disperse until constitution is drafted), but the Assembly's subsequent constitutional work is substantive. As the Assembly transitions from oath-bound collective action to committee-based drafting (t=5-10), theater declines because the actual constitutional mechanism becomes the substantive political work.
 *
 * PERSPECTIVAL GAP:
 *   The Tennis Court Oath produces perspectival disagreement across all six type classifications from a single structural base. The Crown/traditional structure classify snare (illegitimate extraction, high suppression, no procedural legitimacy). The Third Estate classify tangled_rope (mixed coordination benefit and extracted authority, constrained exit due to oath and suppression risk). The National Assembly coalition classify rope (genuine coordination mechanism with low extraction once Assembly is operational). Aristocratic moderates classify tangled_rope (benefit from coordination, cost of status displacement). The procedural machinery classifies snare (authority flows past procedure, rendering it inert). The ancien régime institutions classify piton (persist formally but lose functional authority). The analytical perspective risks classifying mountain (inevitable structural response to blocked procedure), but structural data reveals this as false summit naturalization. The gap between snare (Crown), tangled_rope (Third Estate), and rope (Assembly) reflects genuine differences in each agent's structural relationship to the authority claim and its enforcement mechanisms. No single type is 'correct' — the perspectival distribution is the correct answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from agent power, exit options, and structural position relative to authority flow. Third Estate deputies (moderate/constrained) experience d ≈ 0.55 (target of Crown suppression, beneficiary of authority claim) → f(d) ≈ 0.75 → experienced extraction amplified. Crown (institutional/trapped) experiences d ≈ 0.90 (full target of extraction, cannot exit regime framework) → f(d) ≈ 1.28 → maximum experienced extraction. National Assembly coalition (organized/mobile) experiences d ≈ 0.50 (symmetric position between coordination and extraction benefits) → f(d) ≈ 0.65 → moderate experienced extraction. Aristocratic moderates (powerful/constrained) experience d ≈ 0.45 (beneficiary of Assembly existence, target of status displacement) → f(d) ≈ 0.55 → mixed extraction. Analytical observer (analytical/analytical) experiences d ≈ 0.72 (external to the constraint, observes full structure) → f(d) ≈ 1.15. The directionality mapping reveals why the Crown experiences maximum extraction (trapped institutional position with no procedural exit) while the National Assembly coalition experiences minimal extraction (mobile agents coordinating genuine constitutional work with mutual benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tennis Court Oath resolves mandatrophy by showing that classification type is genuinely dependent on the observer's structural position. The Crown must classify the oath as snare (extraction without legitimacy) because the Crown's institutional position offers no procedural path to grant or acknowledge the Third Estate's authority claim. The Third Estate classify tangled_rope because the oath simultaneously coordinates their collective action (benefit) and extracts authority they are claiming (extraction), with constrained exit due to the oath's binding nature. The National Assembly coalition classify rope once operational because the primary function becomes genuine constitutional coordination with minimal asymmetric extraction. No type is 'more correct' — each reflects a real structural difference in how the constraint operates from different positions. The constraint does not resolve to a single classification because the perspectives genuinely occupy different structural positions with different extraction flows, different exit options, and different coordination benefits. The mandatrophy is resolved by accepting the perspectival distribution as the accurate description of the constraint's structure: it is snare from the Crown, tangled_rope from the Third Estate, rope from the Assembly, piton from the ancien régime institutions, and a potential false summit (mountain) from the analytical observer who must resist naturalizing the oath as inevitable. The classification plurality reflects structural reality, not analytical failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bandwidth_saturation_threshold,
    'At what level of blocked procedural channels does an inner container''s authority claim become structurally rational rather than merely revolutionary?',
    'Comparative analysis of other blocked Estates-General sessions, petition response rates, voting outcomes under Crown veto; identification of the specific channel-blockage that triggered the oath',
    'If threshold is low (few blocked channels): many institutional conflicts risk misclassification as revolutionary extraction. If threshold is high (exhaustive blockage): real procedural failures may be mischaracterized as legitimate Crown prerogative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bandwidth_saturation_threshold, empirical, 'Threshold of procedural blockage justifying inner-container authority claims').

omega_variable(
    third_estate_coordination_capacity,
    'Did the oath genuinely enable Third Estate deputies to coordinate constitutional action, or did it primarily serve as symbolic legitimation for decisions already made by radical leadership?',
    'Analysis of Assembly voting patterns, deliberation records, and constitutional proposals pre- vs. post-oath; identification of coordination function vs. rubber-stamp dynamic',
    'If genuine coordination: tangled_rope classification confirmed (mixed coordination benefit + extracted authority). If symbolic legitimation: snare classification more accurate (extraction with minimal real coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_estate_coordination_capacity, empirical, 'Whether the oath enabled genuine collective coordination or symbolic legitimation').

omega_variable(
    outer_container_suppression_capacity,
    'Could the Crown have suppressed the Tennis Court Oath and National Assembly through military force without risking regime collapse? Or was suppression capacity already degraded?',
    'Military force analysis: army loyalty assessment, troop deployment logistics, cost estimates for sustained suppression; comparison with subsequent military interventions (October march, royal flight, counterrevolution)',
    'If suppression was feasible: the oath was extractive seizure of authority against a still-functional regime. If suppression capacity was already gone: the oath was realignment to match actual power distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outer_container_suppression_capacity, empirical, 'Crown''s actual military suppression capacity in June 1789').

omega_variable(
    legitimacy_vs_procedural_authority,
    'Is a claim to authority that violates existing procedure but addresses a genuine procedural failure itself illegitimate extraction, or a correction to a blocked legitimate process?',
    'Philosophical and historical analysis of procedural legitimacy; examination of whether the Estates-General had any procedure for constitutional convention when blocked; conceptual determination of whether legitimacy inheres in procedure alone or in response to procedural failure',
    'If procedure is supreme: Tennis Court Oath is snare (pure extraction from legitimate authority structure). If legitimacy can override procedure: oath is tangled_rope or rope (genuine coordination with authority reclassification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_procedural_authority, conceptual, 'Whether procedural legitimacy can be overridden by addressing procedural failure').

omega_variable(
    revolutionary_vs_constitutional_extraction,
    'Does extracting authority from an illegitimate regime (absolute monarchy) constitute extraction in the DR sense, or does structural illegitimacy of the regime render the concept of ''extraction'' inapplicable?',
    'Conceptual analysis of whether DR''s beneficiary/victim framework applies to revolutions against delegitimized regimes; examination of whether extraction requires an initially legitimate baseline',
    'If extraction applies: the Third Estate is beneficiary, Crown is victim, and the constraint is extractive by definition. If extraction requires legitimate baseline: the constraint is reclassification rather than extraction, and tangled_rope or rope may be misclassified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolutionary_vs_constitutional_extraction, conceptual, 'Applicability of extraction concept to authority claims against delegitimized regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tennis_court_oath_container_reclassification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tco_theater_t0_before_oath, tennis_court_oath_container_reclassification, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tco_theater_t1_oath_moment, tennis_court_oath_container_reclassification, theater_ratio, 1, 0.55).
narrative_ontology:measurement(tco_theater_t5_assembly_work, tennis_court_oath_container_reclassification, theater_ratio, 5, 0.48).
narrative_ontology:measurement(tco_theater_t10_constitutional_text, tennis_court_oath_container_reclassification, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(tco_extractiveness_t0_before_oath, tennis_court_oath_container_reclassification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tco_extractiveness_t1_oath_sworn, tennis_court_oath_container_reclassification, base_extractiveness, 1, 0.52).
narrative_ontology:measurement(tco_extractiveness_t5_assembly_consolidated, tennis_court_oath_container_reclassification, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(tco_extractiveness_t10_constitutional_framework, tennis_court_oath_container_reclassification, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tennis_court_oath_container_reclassification, enforcement_mechanism).
narrative_ontology:affects_constraint(tennis_court_oath_container_reclassification, estates_general_bandwidth_saturation).
narrative_ontology:affects_constraint(tennis_court_oath_container_reclassification, old_regime_legitimacy_crisis).
narrative_ontology:affects_constraint(tennis_court_oath_container_reclassification, revolutionary_authority_claim_structure).

% DUAL FORMULATION NOTE:
% The Tennis Court Oath is downstream of the Estates-General bandwidth saturation (procedural blockage) and upstream of the National Assembly constitutional authority (procedural legitimacy migration). These three constraints form a decomposition: bandwidth saturation (ε≈0.25, institutional coordination failure) → oath and container reclassification (ε≈0.52, authority extraction via procedural bypass) → constitutional legitimacy establishment (ε≈0.15, new procedure's consolidation). Each has distinct ε reflecting different structural properties: saturation is coordination failure, oath is extraction-with-coordination-benefit, constitutional establishment is institutional success.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tennis_court_oath_container_reclassification, institutional, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
