% ============================================================================
% CONSTRAINT STORY: classification_as_accountability_shield
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classification_as_accountability_shield, []).

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
 *   constraint_id: classification_as_accountability_shield
 *   human_readable: Classification As Accountability Shield
 *   domain: governance/institutional_dynamics
 *
 * SUMMARY:
 *   Classification systems are ubiquitous in governance: administrative
 *   categories, risk assessments, threat designations, benefit eligibility
 *   determinations, regulatory compliance frameworks. Classification serves
 *   genuine coordination functions — shared taxonomies enable consistent
 *   decision-making across organizations and scales. However, classification
 *   systems also function as accountability shields. When an institutional
 *   actor makes a decision justified by 'the data fell into category X,' the
 *   classification framework absorbs the accountability claim. The decision
 *   is no longer subject to moral or political challenge; it is merely 'what
 *   the system does.' This constraint examines the structural mechanism by
 *   which classification systems, even when technically accurate and
 *   genuinely useful for coordination, simultaneously enable the evasion of
 *   accountability. The constraint's extractiveness (0.58) reflects moderate
 *   to high asymmetric extraction: beneficiaries (institutional
 *   decision-makers) use classifications to justify decisions while appearing
 *   to defer to technical authority; victims (accountability claimants and
 *   affected populations) find their claims dissolved into taxonomic
 *   abstraction. The theater ratio (0.68) reflects that compliance with
 *   classification procedures increasingly serves as performance of good
 *   governance rather than as actual accountability mechanism. The
 *   suppression (0.65) reflects substantial barriers to escaping the
 *   classification framework: challenging a decision requires first
 *   challenging the classification that justified it, which requires
 *   disputing technical/bureaucratic authority.
 *
 * KEY AGENTS:
 *   - Institutional Decision Maker: Primary beneficiary (institutional/arbitrage) — uses classification to justify decisions while maintaining appearance of neutrality; has arbitrage options (can select alternative classifications or appeal frameworks)
 *   - Accountability Claimant: Primary victim (powerless/trapped) — cannot exit without abandoning accountability claim; bears cost of claim dissolution into technical framework
 *   - Affected Population: Secondary victim (powerless/trapped) — receives end of decisions; has no mechanisms to contest classification schemes that justify harm
 *   - Governance Reform Coalition: Organized actor (organized/constrained) — sees both coordination value and extraction risk; constrained by high political cost of framework reform
 *   - Technical Classification Apparatus: Institutional system (institutional/arbitrage) — persists through inertia; its accountability-shielding function has become primary even as coordination function has been accomplished
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing classification as necessary feature of complex governance, obscuring it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classification_as_accountability_shield, 0.58).
domain_priors:suppression_score(classification_as_accountability_shield, 0.65).
domain_priors:theater_ratio(classification_as_accountability_shield, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classification_as_accountability_shield, extractiveness, 0.58).
narrative_ontology:constraint_metric(classification_as_accountability_shield, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(classification_as_accountability_shield, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classification_as_accountability_shield, snare).
narrative_ontology:human_readable(classification_as_accountability_shield, "Classification As Accountability Shield").
narrative_ontology:topic_domain(classification_as_accountability_shield, "governance/institutional_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classification_as_accountability_shield, institutional_decision_maker).
narrative_ontology:constraint_victim(classification_as_accountability_shield, accountability_claimants).
narrative_ontology:constraint_victim(classification_as_accountability_shield, affected_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCOUNTABILITY CLAIMANT (SNARE) — Trapped by procedural requirement to accept classification outcome. Cannot exit without abandoning claim for accountability. Bears full cost of institutional classification shield — decisions fade into taxonomic abstraction, removing grounds for holding actors responsible. Maximum experienced extraction.
constraint_indexing:constraint_classification(classification_as_accountability_shield, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED POPULATION (SNARE) — Structurally trapped in receiving end of decisions justified through technical classification. No mechanisms to contest the classification scheme itself. Extraction is distributed and diffuse but impossible to escape — decisions justified as 'following protocol' prevent accountability mechanisms from functioning.
constraint_indexing:constraint_classification(classification_as_accountability_shield, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL DECISION MAKER (ROPE) — Benefits from classification as coordination mechanism: shared taxonomies enable consistent decision-making, bureaucratic efficiency, and inter-organizational alignment. Net beneficiary position — classification enables the coordination function they depend on. Has arbitrage options (can reconfigure classification schemes or appeal to alternative governance frameworks).
constraint_indexing:constraint_classification(classification_as_accountability_shield, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNANCE REFORM COALITION (TANGLED ROPE) — Organized actors (transparency advocates, civil society organizations, regulatory bodies) see classification as both coordination tool and extraction shield. They benefit from technical precision in classification but are constrained by the fact that reform requires organizing collective action against institutional resistance. Some exit options through legislative or administrative channels but at high political cost.
constraint_indexing:constraint_classification(classification_as_accountability_shield, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TECHNICAL CLASSIFICATION APPARATUS (PITON) — The machinery of institutional classification persists largely through inertia. Once established, classification systems become self-perpetuating even as their accountability function degrades. Theater is high: compliance with classification procedures becomes evidence of good governance, independent of whether accountability actually occurs. Functional capacity has atrophied as the system's accountability shielding role has become its primary function.
constraint_indexing:constraint_classification(classification_as_accountability_shield, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From civilizational scale, the constraint risks appearing as a natural feature of complex governance: 'any large system must use classification to manage complexity.' This naturalizes what is actually a contingent institutional choice to use classification as a shield rather than as a transparency mechanism. The engine's false summit detector will identify this perspective as illegitimate naturalization.
constraint_indexing:constraint_classification(classification_as_accountability_shield, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classification_as_accountability_shield_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(classification_as_accountability_shield, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(classification_as_accountability_shield, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(classification_as_accountability_shield, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(classification_as_accountability_shield, TR),
    TR >= 0.70.

:- end_tests(classification_as_accountability_shield_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The extraction is not maximal because classification systems do provide genuine coordination benefits. However, the use of classification to shield accountability is extractive. The beneficiary (institutional decision-maker) gains the coordination benefit while victims (claimants, affected populations) bear the cost of obscured accountability. The measurement shows extractiveness increasing over time (0.35 → 0.58), reflecting institutional deepening of classification systems and their use as accountability shields. Suppression (0.65): Moderate-high. Substantial barriers prevent escape: (a) legal requirement to accept classification outcomes, (b) technical/expert authority that claims are not competent to dispute, (c) resource barriers to mounting alternative classification schemes, (d) organizational inertia. Theater ratio (0.68): High and increasing. Compliance with classification procedures increasingly functions as performance of good governance independent of actual accountability outcomes. Classification schemes that produce no measurable improvement in outcomes or accountability continue to expand, driven by institutional momentum rather than by functional value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from the perspective of different actors relative to the same institutional mechanism. The gap between the Rope perspective (institutional decision-maker) and the Snare perspective (accountability claimant) reveals the asymmetry: the same classification system is coordinative from the beneficiary's view and extractive from the victim's view. This is the defining feature of Snare classification at scale — the beneficiary genuinely benefits from coordination; the victim genuinely bears the cost of obscured accountability. The Piton perspective reveals that the system has become largely performative even from the beneficiary's view — compliance with classification procedures is evidence of legitimacy independent of actual accountability. The mountain false summit reveals the analytical observer's risk of naturalizing institutional choice as inherent feature of complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships to the constraint. Institutional decision-makers who benefit from classification while maintaining appearance of neutrality have low d (near 0.0) — they are beneficiaries with arbitrage options. Accountability claimants who cannot exit without abandoning their claim have high d (near 1.0) — they are victims with no structural mobility. Affected populations have maximal d (approaching 1.0) — they are trapped victims with no exit mechanisms. The governance reform coalition has intermediate d (0.4-0.6) — they are partially victims (constrained by political costs) but have organized agency and some exit options. The analytical observer has d ≈ 0.72 (canonical for analytical context) — they observe the full structure but are not embedded in the extraction. The pipeline applies the sigmoid f(d) to these values to compute effective extractiveness chi experienced by each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH INSTITUTIONAL CAPTURE: This constraint resolves the mandatrophy by showing that classification systems genuinely provide coordination value (legitimate Rope function) but are structurally designed to prevent accountability (pure Snare extraction). The snare classification is appropriate because the extraction mechanism — making accountability claims unthinkable by dissolving them into technical categories — is the primary function, not a side effect. The system does not accidentally shield accountability; it is designed to do so. The beneficiary institutions have captured the classification apparatus to serve accountability-shielding while maintaining the appearance of technical neutrality. The mandatrophy is resolved by recognizing that coordination and extraction are not contradictory in this context — the system coordinates while extracting; the coordination is the vehicle for the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_precision_vs_accountability_tradeoff,
    'Is the classification apparatus genuinely necessary for coordination, or does its complexity primarily serve to obscure accountability lines?',
    'Comparative analysis: measure accountability closure rates under detailed technical classification vs under simplified categorical schemes; track whether increased classification detail correlates with improved outcomes or merely with reduced accountability',
    'If coordination is genuine: the constraint is Tangled Rope (mixed function). If obscuration dominates: the constraint is Snare (extraction mechanism masquerading as coordination). Classification detail that doesn''t improve outcomes is extractive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_precision_vs_accountability_tradeoff, empirical, 'Whether classification serves coordination or accountability obscuration').

omega_variable(
    institutional_capture_of_classification_scheme,
    'To what degree has the classification apparatus itself been captured by the decision-makers it is supposed to constrain?',
    'Historical analysis of classification scheme evolution: who controls revisions, whose interests are served by particular categories, whether affected populations have voice in scheme definition',
    'If captured: beneficiaries can reshape classifications to evade accountability (higher extraction). If independent: classifications constrain even institutional actors (lower extraction, genuine coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_classification_scheme, empirical, 'Degree to which decision-makers control the classification apparatus').

omega_variable(
    alternative_accountability_mechanisms,
    'Do viable alternatives to classification-based governance exist that would enable coordination without enabling accountability shields?',
    'Case studies of direct accountability (participatory governance, mandatory explanation, exposure-based accountability, relational transparency); measurement of accountability closure rates under alternative frameworks',
    'If alternatives exist and are effective: the constraint is an institutional choice, not a necessity (Snare classification confirmed). If alternatives fail: some version of classification is unavoidable (Tangled Rope). The shield function is not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accountability_mechanisms, empirical, 'Viability of alternatives to classification-based accountability').

omega_variable(
    suppression_internalization_in_classification_culture,
    'Is the suppression of accountability claims structural (legal barriers, resource constraints) or internalized (affected populations have internalized that classification defeats claims)?',
    'Analysis of claim attempt patterns: do affected populations attempt accountability claims less frequently over time (learned helplessness), or do external barriers prevent claims from succeeding?',
    'If internalized: suppression persists after formal barriers are removed; the constraint is identity-locked for some populations. If structural: removal of formal barriers would enable claims to proceed (higher exit capacity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_classification_culture, empirical, 'Structural vs. internalized suppression of accountability claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classification_as_accountability_shield, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(claas_tr_t0, classification_as_accountability_shield, theater_ratio, 0, 0.42).
narrative_ontology:measurement(claas_tr_t5, classification_as_accountability_shield, theater_ratio, 5, 0.55).
narrative_ontology:measurement(claas_tr_t10, classification_as_accountability_shield, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(claas_be_t0, classification_as_accountability_shield, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(claas_be_t5, classification_as_accountability_shield, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(claas_be_t10, classification_as_accountability_shield, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classification_as_accountability_shield, information_standard).
narrative_ontology:affects_constraint(classification_as_accountability_shield, administrative_discretion_opacity).
narrative_ontology:affects_constraint(classification_as_accountability_shield, technical_authority_capture).
narrative_ontology:affects_constraint(classification_as_accountability_shield, procedure_as_legitimacy).

% DUAL FORMULATION NOTE:
% Classification as accountability shield is downstream of institutional adoption of technical authority claims. The upstream constraint is the use of expertise to justify decisions; this constraint is the specific mechanism by which classification enables that shield. Separate stories track (1) technical authority capture (epistemic framing), (2) classification as accountability shield (procedural mechanism), (3) administrative discretion opacity (outcome pattern).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classification_as_accountability_shield, institutional, 0.12).
constraint_indexing:directionality_override(classification_as_accountability_shield, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
