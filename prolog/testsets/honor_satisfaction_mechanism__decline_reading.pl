% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Through Dueling (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   Dueling as a formal constraint on honor satisfaction persisted at
 *   declining frequency across the 18th and 19th centuries, gradually
 *   transitioning from a structurally embedded practice to a fringe and
 *   eventually illegal activity. This constraint story instantiates the
 *   'decline reading' of the honor-satisfaction mechanism kernel: the
 *   constraint weakens through increasing legal enforcement, social stigma,
 *   and the maturation of alternative dispute-resolution mechanisms, yet
 *   remains conceptually available and structurally embedded in upper-class
 *   honor cultures until near-complete suppression. The decline reading
 *   models a constraint that does not disappear through categorical
 *   repudiation of its legitimacy (that is the 'contraction reading' — a
 *   sibling story) but rather through enforcement, cost escalation, and
 *   institutional replacement. In the decline phase, dueling persists in
 *   clandestine or discretionary forms among elites while becoming
 *   increasingly unavailable to lower classes and non-martial professions.
 *   The constraint exhibits tangled rope classification: genuine coordination
 *   function (honor disputes resolved through culturally legible, binding
 *   mechanisms) coexists with asymmetric extraction (lower classes bear legal
 *   penalties while upper classes evade enforcement; female relatives bear
 *   psychological and legal costs; society bears the cost of legal exceptions
 *   and judicial deference to honor claims). The theater ratio rises across
 *   the interval as the dueling code's elaborate ritual structure persists as
 *   performative theater long after genuine coordination function has
 *   atrophied — enforcement increases, adherence declines, yet the code
 *   remains formally available within elite circles.
 *
 * KEY AGENTS:
 *   - Upper-Class Honor Claimants: Primary beneficiaries (powerful/mobile) — experience dueling as coordination mechanism; capture status capital and reputation benefits
 *   - Non-Dueling Population: Primary victims (powerless/trapped) — bear legal penalties and social contamination from dual-class legal system that grants exceptions to honor claims
 *   - Anti-Dueling Legal Reformers: Secondary beneficiaries (organized/constrained) — extract political legitimacy from suppression narrative while pursuing coordinated legal enforcement
 *   - Martial Professional Class: Beneficiaries with constrained exit (powerful/constrained) — military and dueling-code specialists capture professional status from honor mechanism
 *   - Female Relatives of Duelists: Secondary victims (moderate/constrained) — bear psychological and legal costs; face vulnerability during honor disputes
 *   - Society/Legal Order: Victim (institutional/trapped) — bears extraction through enforcement costs, legal exceptions, and institutional deference to honor claims
 *   - Dueling Code Institution: Institutional actor (institutional/arbitrage) — maintains formal availability of the constraint through preservation of ritual structure despite declining function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.62).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Through Dueling (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb').
narrative_ontology:cs_kernel_codification('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', distributed).
narrative_ontology:cs_authority_grounding('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', lineage).
narrative_ontology:cs_interpretation_layer_present('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb').
narrative_ontology:cs_reading_relation('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', foundational, honor_claims_structurally_persist_despite_enforcement).
narrative_ontology:cs_axiom_status(honor_claims_structurally_persist_despite_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', honor_claims_structurally_persist_despite_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', foundational, enforcement_cost_escalation_reduces_but_does_not_eliminate_constraint).
narrative_ontology:cs_axiom_status(enforcement_cost_escalation_reduces_but_does_not_eliminate_constraint, holdable).
narrative_ontology:cs_axiom_grounding('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', enforcement_cost_escalation_reduces_but_does_not_eliminate_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', honor_through_formal_violent_resolution).
narrative_ontology:cs_drift_state('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', late_enforcement_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0d22b3eb-adb1-4dcc-bb2f-31909aeabfbb', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, upper_class_honor_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, martial_professional_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, lower_class_non_combatants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, society_legal_order).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, female_relatives_of_duelists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-DUELING POPULATION (SNARE) — Trapped within legal systems that treat dueling as a special category requiring appeasement. Cannot exit the jurisdiction without abandoning livelihood; bears extraction through biased legal treatment, insurance penalties, and social contamination. As dueling frequency declines, the burden becomes more nakedly extractive (fewer see it as 'natural law') yet suppression mechanism remains intact through residual legal deference to honor claims.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ANTI-DUELING LEGAL REFORMERS (TANGLED ROPE) — Organized agents (prosecutors, reformers, emerging bourgeoisie) pursue coordinated legal suppression of dueling while also extracting legitimacy and political power from the role of 'defender of rational law order.' They benefit from the constraint's existence (their movement is defined against it) while claiming to dissolve it. The constraint simultaneously coordinates their institutional identity and extracts political capital from the suppression narrative.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UPPER-CLASS HONOR CLAIMANTS (ROPE) — Mobile agents with resources to exit through emigration, private accommodation, or discretionary enforcement. Experience the constraint as coordination mechanism: dueling establishes reputation capital, resolves honor disputes through culturally legible means, and maintains status hierarchy. As legal suppression increases, the coordination function persists within private or discretionary spheres (the 'duel of honor' becomes semi-clandestine but structurally available to those with resources to evade enforcement).
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE DUELING CODE ITSELF (PITON) — The institutional encoding of dueling (codes of honor, seconds, formal challenge protocols) persists through inertia despite declining adherence and increasing legal enforcement. The code's theater ratio is high in the decline phase: the elaborate ritual structure (seconds, witnesses, formal cartel negotiations) persists as performative theater long after genuine coordination function has atrophied. The code remains available for edge cases and fringe practitioners but is largely maintained through institutional memory rather than functional necessity.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: EMERGING ALTERNATIVE DISPUTE RESOLUTION (SCAFFOLD) — Organized legal and social institutions (courts, tort law, professional reputation systems, insurance mechanisms) are building alternative pathways for honor satisfaction that bypass violence. These mechanisms have sunset logic: as they mature and gain legitimacy, dueling's extraction and suppression mechanisms lose function. The scaffold perspective sees dueling as a temporary coordination failure being replaced by bureaucratic and institutional alternatives — the constraint's extractiveness declines as alternatives strengthen.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURALIZATION RISK (MOUNTAIN) — At civilizational scale, this perspective risks viewing dueling as an immutable expression of honor culture or masculine identity, treating the constraint as a natural-law consequence of how honor-based status systems organize. This framing naturalizes what is actually a contingent institutional arrangement: dueling persists because specific legal structures grant it partial immunity and specific class interests benefit from it, not because honor-satisfaction requires violence. The engine's false summit detector reveals this as naturalization of institutional choice.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__decline_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, TR),
    TR >= 0.70.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low in decline phase. The constraint's extraction mechanism is being actively dismantled through legal enforcement and social stigma, reducing the base extraction value from what it was in the ascendant phase (estimated 0.52 at t=0). However, extraction persists because legal enforcement is uneven (upper classes evade through discretionary prosecution) and cultural legitimacy persists in elite circles. The decline reading models gradual weakening rather than sudden collapse. Suppression (0.62): Moderate-high and rising. Legal enforcement machinery escalates across the interval as statutes tighten, prosecution rates increase, and social stigma accumulates. The suppression requirement rises as the constraint's cultural legitimacy erodes — the system must increasingly rely on coercive enforcement rather than voluntary coordination. Theater ratio (0.68): High and rising. The elaborate ritual structure of the dueling code (seconds, formal cartel negotiations, coded challenges, witnesses) persists as performative theater long after genuine coordination function has declined. In the early phase (t=0), the code still coordinates genuine honor settlements; by t=50, the theater is largely institutional inertia — the code persists because alternatives have not fully replaced it, not because it functions well.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the upper-class perspective (Rope) and the non-dueling population perspective (Snare) is maximal. The same structural phenomenon — legal deference to honor claims, enforcement gaps, status hierarchy — appears as coordination mechanism to those who benefit and as pure extraction to those who bear costs. The reformer perspective (Tangled Rope) is genuinely mixed: pursuing legal suppression while extracting political legitimacy from the suppression role. The piton perspective captures the constraint's institutional degradation: the ritual persists despite declining function. The scaffold perspective sees dueling as a temporary problem being solved by institutional alternatives. The analytical observer risks naturalizing a contingent institutional arrangement as inherent to honor culture.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper-class honor claimants with mobile exit options experience low or negative extractiveness (d ≈ 0.20–0.30): they are beneficiaries who can arbitrage out of enforcement through discretionary prosecution and social networks. Anti-dueling reformers with constrained exit and split beneficiary/victim roles experience moderate extractiveness (d ≈ 0.45–0.55): they benefit from the suppression narrative but are constrained by the slow rate of cultural change. The non-dueling population with trapped exit experiences high extractiveness (d ≈ 0.85–0.95): they are victims with no exit capacity, bearing the cost of dual-class legal systems. Female relatives with identity-locked exit and victim status experience high extractiveness (d ≈ 0.80–0.85): structurally mobile (could relocate, marry into different circles) but identity-fused with family honor claims that bind them to the constraint. The dueling code institution with arbitrage exit (can relocate to jurisdictions where dueling persists) experiences very low extractiveness (d ≈ 0.10): it is beneficiary and mobile, experiencing the constraint as coordinating its own preservation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_enforcement_efficacy_threshold,
    'At what level of legal enforcement (penalty severity, prosecution rate, social stigma) does dueling cease being a structurally available constraint rather than merely a fringe practice?',
    'Historical analysis: correlation between legislative severity, prosecution rates, and frequency of recorded duels; identification of the threshold prosecution rate that ends all dueling or pushes it into clandestine sphere where it ceases affecting social structure',
    'If threshold is low (< 10% prosecution): legal enforcement is performative and dueling remains structurally active. If threshold is high (> 50% prosecution): dueling transitions from constraint to deviance and no longer coordinates honor claims. Classification may shift from tangled_rope to piton or toward dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_enforcement_efficacy_threshold, empirical, 'Enforcement threshold for transition from constraint to deviance').

omega_variable(
    alternative_dispute_adequacy,
    'Do emerging legal and social mechanisms (courts, professional reputation, insurance, social clubs) actually provide equivalent honor satisfaction to dueling, or do they satisfy different psychological/status needs?',
    'Historical analysis of adoption rates among upper classes; comparison of honor-satisfaction persistence after dueling suppression in jurisdictions with vs. without developed alternative dispute mechanisms; analysis of persistence of private dueling among cohorts with access to alternatives',
    'If alternatives are adequate: dueling''s extraction mechanism is coordination problem solvable through institutional replacement (scaffold confirms). If inadequate: dueling persists because it satisfies needs alternatives cannot meet, suggesting the constraint''s core is not honor satisfaction but something else (hierarchical violence, exclusion, theater) — reclassifies to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispute_adequacy, empirical, 'Whether alternative mechanisms provide equivalent honor satisfaction').

omega_variable(
    reading_ambiguity__decline_vs_contraction,
    'Does this ''decline reading'' (constraint weakens through enforcement and social cost while remaining conceptually available) correctly model the historical process, or does the constraint actually undergo categorical contraction (the cultural framework itself is repudiated, not merely suppressed)?',
    'Textual and archival analysis: do declining-era honor codes still articulate the legitimacy of dueling-as-honor-satisfaction (constraint available but illegitimate), or do they explicitly reject the premise that violence can satisfy honor (framework collapsed)? Compare elites'' legal defense strategies: do they defend dueling as honor mechanism (decline reading) or concede it was never legitimate (contraction reading)?',
    'If decline (this reading): epsilon drops via enforcement and social cost, but the constraint remains formally available. If contraction (sibling reading): epsilon may drop more sharply and classification may shift from tangled_rope to defunct piton or dissolved entirely. This omega resolves which sibling reading better captures the actual historical dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ambiguity__decline_vs_contraction, conceptual, 'Ambiguity between decline model (constraint weakens) vs. contraction model (framework repudiated)').

omega_variable(
    female_relatives_extraction_specificity,
    'In the decline phase, how much of the suppression borne by female relatives (sisters, wives, mothers of duelists) is a direct effect of the dueling constraint versus a side effect of broader gender hierarchies that use dueling as one extraction mechanism among many?',
    'Comparative analysis: female relatives'' status and legal vulnerability in jurisdictions with vs. without active dueling traditions; analysis of whether suppression of female relatives decreases when dueling declines; examination of whether alternative patriarchal extraction mechanisms (arranged marriage, property control) persist after dueling suppression',
    'If extraction is dueling-specific: dueling''s decline reduces suppression of female relatives. If extraction is patriarchal-system-level: female relatives remain suppressed even as dueling declines (dueling was one mechanism, not the root). Classification of victims may narrow or recalibrate; omega indicates whether dueling is an autonomous constraint or subsystem of larger patriarchal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_relatives_extraction_specificity, empirical, 'Specificity of female relatives'' extraction to dueling versus broader patriarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_decline_theater_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(honor_decline_theater_t25, honor_satisfaction_mechanism__decline_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(honor_decline_theater_t50, honor_satisfaction_mechanism__decline_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(honor_decline_extract_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(honor_decline_extract_t25, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(honor_decline_extract_t50, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(honor_decline_suppress_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(honor_decline_suppress_t25, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(honor_decline_suppress_t50, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, attachment_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, legal_exceptionalism__upper_class).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, patriarchal_extraction__female_relatives).

% DUAL FORMULATION NOTE:
% The honor-satisfaction mechanism kernel decomposed into three reading-specific constraints: decline_reading (this story) models the constraint weakening through enforcement and social cost while remaining conceptually available; contraction_reading models categorical repudiation of the legitimacy claim; composite_reading models dueling as one manifestation of broader status hierarchies rather than the primary honor mechanism. Each reading has its own epsilon value reflecting different modeling assumptions about the transition trajectory. The decline_reading assumes epsilon drops from ~0.52 to ~0.38 over the interval due to enforcement and cost escalation. Sibling readings may model more rapid collapse (contraction) or alternative extraction mechanisms (composite).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
