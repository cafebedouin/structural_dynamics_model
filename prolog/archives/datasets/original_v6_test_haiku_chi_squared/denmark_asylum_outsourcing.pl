% ============================================================================
% CONSTRAINT STORY: denmark_asylum_outsourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_denmark_asylum_outsourcing, []).

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
 *   constraint_id: denmark_asylum_outsourcing
 *   human_readable: Denmark's Asylum Processing Outsourcing to Third Countries
 *   domain: political/migration
 *
 * SUMMARY:
 *   Denmark's policy of outsourcing asylum processing to third countries
 *   (initially Rwanda, later Libya-adjacent arrangements) represents a
 *   structural transfer of cost and risk from the Danish state and receiving
 *   EU member states to vulnerable asylum seekers and economically dependent
 *   non-EU countries. The constraint exhibits multiple classification
 *   perspectives reflecting genuine structural ambiguities: Danish
 *   policymakers experience it as efficient coordination (Rope); asylum
 *   seekers experience it as inescapable extraction (Snare); third-country
 *   recipients experience it as coercive cooperation (Tangled Rope); the
 *   international refugee protection regime experiences it as a temporary
 *   institutional deviation with formal override mechanisms (Scaffold); EU
 *   legal frameworks experience it as degraded theater maintaining procedural
 *   compliance while violating substantive obligations (Piton); and the
 *   global governance observer sees pure extraction with sophisticated
 *   laundering through neutralized language. The theater_ratio (0.58)
 *   reflects the gap between Denmark's public positioning ('burden-sharing,'
 *   'partnerships,' 'efficient processing') and the actual mechanisms
 *   (transfer of responsibility without equivalent transfer of resources,
 *   placement of asylum seekers in countries with weaker legal protections
 *   and human rights enforcement). Extractiveness has risen from 0.55 to 0.68
 *   over the observation interval as the policy has hardened and
 *   third-country participation has contracted, forcing more intensive
 *   extraction from each transaction.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary victims (powerless/trapped) — subject to processing in jurisdictions with weaker protections; no choice about location or timeline
 *   - Receiving Third Countries: Secondary beneficiaries/victims (moderate/constrained) — receive financial compensation (coordination) but bear disproportionate burden and risk; constrained by economic dependence
 *   - Danish Government: Primary beneficiary (institutional/arbitrage) — reduces domestic political pressure, externalizes costs, captures arbitrage between Danish and third-country operational/political economies
 *   - EU Legal/Governance Framework: Institutional observer (institutional/constrained) — formally maintains commitment to refugee protection but tolerates outsourcing through judicial discretion and procedural theater
 *   - International Refugee Protection Regime (UNHCR, 1951 Convention): Organized observer (organized/mobile) — retains formal enforcement mechanisms but lacks practical override power against member states
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as pure extraction with sophisticated legitimation narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(denmark_asylum_outsourcing, 0.68).
domain_priors:suppression_score(denmark_asylum_outsourcing, 0.72).
domain_priors:theater_ratio(denmark_asylum_outsourcing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, extractiveness, 0.68).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(denmark_asylum_outsourcing, snare).
narrative_ontology:human_readable(denmark_asylum_outsourcing, "Denmark's Asylum Processing Outsourcing to Third Countries").
narrative_ontology:topic_domain(denmark_asylum_outsourcing, "political/migration").

domain_priors:requires_active_enforcement(denmark_asylum_outsourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, danish_government).
narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, danish_taxpayers_anti_immigration).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, asylum_seekers).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, receiving_third_countries).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, international_refugee_protection_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER (SNARE) — Entrapped by geography and desperation. No exit option from the constraint; subject to processing in third countries with weaker legal protections, longer timelines, worse conditions. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.92. Pure extraction from the most vulnerable.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RECEIVING THIRD COUNTRY (TANGLED ROPE) — Receives both financial compensation (coordination function) and burden of processing asylum claims in underdeveloped contexts (extraction). Constrained by economic dependence on Danish funding; coordination function exists but is asymmetric. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DANISH GOVERNMENT (ROPE) — Experiences outsourcing as coordination mechanism: externalizing processing reduces domestic political pressure and operational costs. Benefits from arbitrage between domestic political constraints and lower-cost third-country operations. d≈0.10, f(d)≈0.02, σ=1.0 → χ≈0.01. Net beneficiary; sees problem-solving.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL REFUGEE PROTECTION REGIME (SCAFFOLD) — Recognizes outsourcing as a temporary deviation from the 1951 Convention framework. The regime retains mechanisms to enforce return to Convention compliance: court challenges, treaty obligations, and normative pressure. Sunset mechanism exists in principle (return to EU-based processing or treaty renegotiation). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.25. Moderate extraction hidden by procedural theater.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EU LEGAL FRAMEWORK (PITON) — The outsourcing policy persists despite formal inconsistency with EU asylum law (CEAS) and international refugee convention obligations. The constraint is maintained through procedural theater: legal opacity, selective application, and institutional inertia. theater_ratio=0.58 reflects the gap between stated EU values (burden-sharing, protection) and actual enforcement (permitted through judicial discretion). d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.03.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GLOBAL GOVERNANCE VIEW (SNARE) — From the perspective of the international refugee protection system, outsourcing represents pure extraction dressed in neutral language ('processing,' 'burden-sharing,' 'partnerships'). The system structurally extracts compliance from vulnerable actors (asylum seekers and weak states) without reciprocal protection guarantees. d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.86. High effective extraction revealed by analyzing who bears costs and who captures benefits.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(denmark_asylum_outsourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(denmark_asylum_outsourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(denmark_asylum_outsourcing, TR),
    TR >= 0.70.

:- end_tests(denmark_asylum_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Danish state captures substantial benefit (reduced domestic political pressure, cost externalization, maintained immigration restrictions) while asylum seekers bear concentrated costs (longer processing, weaker legal protections, geographic displacement). The extractiveness is not maximal (0.95) because the policy includes formal procedural components (compensation to third countries, stated asylum processing standards) that create a coordination appearance. However, the structural asymmetry is severe: Denmark determines terms unilaterally, receives benefit, and exits without bearing verification costs. Suppression (0.72): High. Asylum seekers have zero exit options (trapped by international geography and legal status). Third countries are constrained by economic dependence. Alternative mechanisms (EU burden-sharing, domestic processing) are formally available but politically closed to asylum seekers and weak states due to Danish political veto. Theater_ratio (0.58): Moderate-high. The policy maintains procedural compliance with formal refugee protection norms (asylum determination occurs, appeals theoretically available) while systematically disadvantaging the most vulnerable actors. The theater has increased as the policy has faced legal challenges — Denmark has added procedural language around 'partnerships' and 'standards' without substantively changing the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The Danish government sees a coordination solution to a collective action problem (EU burden-sharing failure, domestic political pressure): outsourcing allows Denmark to maintain restrictive immigration policy while formally complying with refugee protection obligations. Asylum seekers see pure extraction: they have no choice, cannot exit, and bear concentrated costs. Receiving third countries experience a coercive hybrid (Tangled Rope): they receive compensation (coordination function) but are forced to bear burdens (extraction function) through economic dependence — they could theoretically refuse, but the economic incentive structure eliminates genuine exit. The EU legal framework experiences institutional degradation (Piton): the formal legal obligation to protect refugees exists, but practical enforcement is theater — member states retain discretion through court processes and treaty ambiguity. The international refugee protection regime sees a temporary deviation with formal override mechanisms (Scaffold): the 1951 Convention and UNHCR standards formally apply, and legal challenges can force compliance — but practical enforcement is weak due to state sovereignty norms. The analytical observer sees the entire arrangement as pure extraction (Snare) from the perspective of the global governance system: the constraint distributes costs to the powerless and displaces responsibility to the weak, while concentrating benefits to the institutionally powerful.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum Seekers: Victims + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. No exit option from the constraint; subject to processing in third-country jurisdiction. Danish Government: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Net beneficiary with maximum discretion. Can exit or modify policy unilaterally; captures arbitrage between domestic political economy and third-country operational economics. Receiving Third Countries: Mixed beneficiary-victim + constrained → d≈0.70, f(d)≈1.05. Receive compensation (low-d beneficiary function) but constrained by economic dependence (high-d victim function); no genuine exit despite formal choice. EU Legal Framework: Institutional observer + arbitrage → d≈0.05, f(d)≈-0.12. Maintains formal authority to enforce refugee protection but exercises that authority through discretionary judicial processes that permit outsourcing; net beneficiary due to political cover (appears to protect refugees while permitting restrictions). International Refugee Protection Regime: Organized observer + mobile → d≈0.55, f(d)≈0.75. Organized enough to mount challenges but mobile enough to shift responsibility to member states; sees outsourcing as a problem but lacks enforcement power. Analytical Observer: Analytical → d≈0.88, f(d)≈1.30. Observes the extraction from the structural baseline; recognizes the policy as pure asymmetric cost transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by clarifying that the high extractiveness (0.68) does NOT constitute a false natural law claim. The analytical observer's Snare classification is the correct structural reading: the policy objectively extracts from powerless agents (asylum seekers) and transfers costs to constrained agents (third countries) while concentrating benefits to the institutionally powerful (Danish state). The Danish government's Rope experience is a perspective-dependent misframing: they genuinely experience the policy as coordination-solving, but the structural data (who bears costs, who captures benefits, who exits) reveals this as a beneficiary perspective that naturalizes asymmetry. The EU and international regime perspectives (Scaffold, Piton) are institutionally coherent but analytically secondary — they describe how powerful institutions manage the extraction (through theater and formal override mechanisms) rather than revealing the underlying structural asymmetry. Mandatrophy is resolved by recognizing that perspectives are valid descriptions of experience but do not determine the constraint's structural type once the extraction metrics are measured. The Snare classification is robust because it directly reflects the directional flow of costs (to asylum seekers) and benefits (to Danish state), independent of framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_country_agency_level,
    'Are receiving third countries genuinely cooperative partners (Rope) or coerced participants (Snare) in the outsourcing arrangement?',
    'Analysis of: (a) negotiating power during agreement formation; (b) ability to unilaterally withdraw; (c) whether compensation reflects actual processing costs; (d) whether countries would participate without Danish financial incentive',
    'If genuinely cooperative: classification shifts toward Tangled Rope with stronger coordination function. If coerced: classification hardens to Snare from their perspective; Danish extraction is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_country_agency_level, empirical, 'Whether receiving countries are cooperative partners or coerced').

omega_variable(
    processing_quality_differential,
    'Does outsourcing demonstrably reduce the quality of asylum determination compared to EU-based processing, or are differences merely procedural?',
    'Comparative analysis: approval rates, appeal success rates, time-to-decision, access to legal representation, adherence to UNHCR standards between Danish third-country processing and EU member state processing',
    'If significant quality reduction: extraction mechanism is confirmed (suppression ≥0.72 justified). If comparable quality: extraction narrative weakens; constraint may degrade toward Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_quality_differential, empirical, 'Whether outsourcing reduces asylum determination quality').

omega_variable(
    escape_velocity_of_international_pressure,
    'Can Danish outsourcing be overridden by EU supranational enforcement (Court of Justice rulings, treaty amendments) or does Denmark retain effective veto power through exit threats or political leverage?',
    'Historical analysis of EU court decisions on outsourcing; tracking of treaty renegotiation outcomes; analysis of Denmark''s leverage (EU veto, border/Schengen negotiation capacity); comparison with other EU member noncompliance cases',
    'If EU can force compliance: sunset mechanism is real (Scaffold classification legitimate). If Denmark retains veto/escape path: international pressure is theater (Piton classification reinforced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_velocity_of_international_pressure, conceptual, 'Whether international pressure can force Danish compliance').

omega_variable(
    compensation_adequacy_threshold,
    'What level of financial compensation to third countries would transform the constraint from Snare to Rope — i.e., genuine mutual benefit?',
    'Cost-benefit analysis: comparison of Danish cost-saving vs third-country operational costs + compensation; survey of third-country government satisfaction and voluntary participation indicators',
    'If current compensation < 50% of actual costs: third-country victimhood is real (Snare from their perspective). If compensation > 150% of costs: approaching genuine Rope or benefit-sharing model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_adequacy_threshold, empirical, 'Whether compensation to third countries is adequate for cooperation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(denmark_asylum_outsourcing, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(denm_tr_t0, denmark_asylum_outsourcing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(denm_tr_t3, denmark_asylum_outsourcing, theater_ratio, 3, 0.5).
narrative_ontology:measurement(denm_tr_t6, denmark_asylum_outsourcing, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(denm_be_t0, denmark_asylum_outsourcing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(denm_be_t3, denmark_asylum_outsourcing, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(denm_be_t6, denmark_asylum_outsourcing, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(denmark_asylum_outsourcing, enforcement_mechanism).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, eu_asylum_burden_sharing).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, third_country_state_capacity).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, refugee_protection_norm_erosion).

% DUAL FORMULATION NOTE:
% Denmark's outsourcing policy is downstream of EU asylum burden-sharing failures (institutional coordination breakdown) and upstream of third-country state capacity constraints and global refugee protection norm erosion. The outsourcing mechanism itself (ε=0.68) is structurally distinct from the upstream coordination failure (higher ε due to institutional disagreement) and the downstream capacity problem (separate ε due to structural developmental constraints).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(denmark_asylum_outsourcing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
