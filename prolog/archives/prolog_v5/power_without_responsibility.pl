% ============================================================================
% CONSTRAINT STORY: power_without_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_without_responsibility, []).

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
 *   constraint_id: power_without_responsibility
 *   human_readable: The Asymmetric Mandate: Power Without Responsibility
 *   domain: political/organizational/legal
 *
 * SUMMARY:
 *   The asymmetric mandate is a structural arrangement in which formal
 *   authority is granted to an agent or institution for managing high-stakes
 *   systems (emergency governance, automated enforcement, sovereign immunity,
 *   pandemic response, military command, financial regulation, or
 *   intelligence operations) without corresponding accountability mechanisms
 *   or responsibility for outcomes. The constraint appears across political
 *   systems, organizational hierarchies, and legal frameworks. It exhibits
 *   classic tangled rope structure: the mandate solves a genuine coordination
 *   problem (enabling rapid, decisive action when normal deliberative
 *   processes are too slow) while simultaneously creating an asymmetric
 *   extraction mechanism (the authority captures benefits—decisional
 *   autonomy, resource concentration, blame-shifting—while the subject
 *   population and oversight bodies bear costs). The constraint's
 *   extractiveness has grown over time (0.35 → 0.58 in the interval) as
 *   mandated authorities have accumulated ancillary powers and oversight
 *   bodies have remained structurally subordinate. Theater ratio has risen
 *   correspondingly (0.45 → 0.68), indicating that formal accountability
 *   mechanisms have become increasingly performative: review boards convene,
 *   audits are conducted, and appeals processes exist, but authority
 *   decisions are rarely reversed, sanctions are rare, and the mandate
 *   expands despite oversight findings.
 *
 * KEY AGENTS:
 *   - Mandated Authority: Primary beneficiary (institutional/arbitrage) — captures decisional autonomy, resource control, blame-shifting, and mandate expansion; can reinterpret or exceed formal authorization
 *   - Delegating Institution: Secondary beneficiary (institutional/arbitrage) — maintains formal oversight while delegating implementation; can disavow authority if outcomes fail
 *   - Subject Population: Primary victim (powerless/trapped) — subject to authority decisions without exit mechanism; cannot appeal, opt out, or influence mandate scope
 *   - Oversight Body: Secondary victim (moderate/constrained) — tasked with monitoring but lacks enforcement teeth, operates on delayed information, depends on authority for resources
 *   - Accountability Theater: Institutional artifact (institutional/constrained) — formal mechanisms (review boards, appeals, audits) that persist through inertia despite low functional constraint
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the coordination-extraction hybrid and the conditions under which mandate can be reformed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_without_responsibility, 0.58).
domain_priors:suppression_score(power_without_responsibility, 0.72).
domain_priors:theater_ratio(power_without_responsibility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_without_responsibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(power_without_responsibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(power_without_responsibility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_without_responsibility, tangled_rope).
narrative_ontology:human_readable(power_without_responsibility, "The Asymmetric Mandate: Power Without Responsibility").
narrative_ontology:topic_domain(power_without_responsibility, "political/organizational/legal").

domain_priors:requires_active_enforcement(power_without_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(power_without_responsibility, mandated_authority).
narrative_ontology:constraint_beneficiary(power_without_responsibility, delegating_institution).
narrative_ontology:constraint_victim(power_without_responsibility, subject_population).
narrative_ontology:constraint_victim(power_without_responsibility, oversight_body).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — The population subject to mandated authority has no exit mechanism. They cannot opt out of the system, cannot appeal to alternative governance, and face escalating enforcement costs. The authority's decisions are imposed without corresponding accountability pathways. Maximum extraction: the population bears full cost of arbitrary or negligent exercise of power.
constraint_indexing:constraint_classification(power_without_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OVERSIGHT BODY (TANGLED ROPE) — Ostensibly charged with monitoring mandated authority but structurally constrained: lacks enforcement teeth, operates on delayed information, and depends on the authority for resource access and cooperation. Experiences both coordination benefit (stabilizes governance) and extraction cost (subordinated monitoring role). Exit options are constrained by institutional position and lack of alternative mechanisms.
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANDATED AUTHORITY (ROPE) — From the authority's perspective, the mandate is a coordination mechanism: it legitimizes rapid response, consolidates decision-making, and enables action in high-stakes environments. The authority benefits from the asymmetry (speed, decisional autonomy) and can reinterpret or expand its mandate as circumstances permit. Experiences minimal suppression of its own goals; effectively arbitrage between formal mandate and practical implementation.
constraint_indexing:constraint_classification(power_without_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DELEGATING INSTITUTION (ROPE) — The parent body that issued the mandate benefits from delegation: it can claim oversight while avoiding direct accountability for implementation failures. Operates with arbitrage: delegates power, maintains formal authority, and can disavow the authority if outcomes become toxic. The mandate coordination solves the delegation problem while preserving institutional buffer.
constraint_indexing:constraint_classification(power_without_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTABILITY THEATER (PITON) — Formal oversight mechanisms (review boards, appeals processes, external audits) persist as performative rituals. They create the appearance of accountability without functional constraint on the authority. Theater ratio is high: reports are filed, committees meet, but decision reversal and authority limitation remain rare. The theater decays when high-profile failures expose it, but the underlying mandate persists — inertia maintains the facade.
constraint_indexing:constraint_classification(power_without_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the asymmetric mandate is a genuine hybrid: it solves a real coordination problem (enabling rapid action in crises) while creating an asymmetric extraction mechanism (concentrating power without corresponding responsibility). The structure is not a natural law but a persistent institutional design choice. Mandatrophy is resolvable through structural reform: splitting authority, embedding real-time oversight, or rotating mandate holders.
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_without_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(power_without_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_without_responsibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(power_without_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(power_without_responsibility, TR),
    TR >= 0.70.

:- end_tests(power_without_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The mandated authority captures substantial benefits: decisional autonomy, speed advantages, resource concentration, and ability to expand mandate scope without full justification. The subject population and oversight body bear corresponding costs. However, extractiveness is not maximal (0.66+) because the mandate does solve a real coordination problem (enabling rapid response), and in some implementations oversight mechanisms do provide partial constraint. The growth trajectory (0.35 → 0.58) reflects mandate scope creep and oversight degradation over time. Suppression (0.72): High. The subject population faces multiple suppression mechanisms: (a) no exit option from the mandate's jurisdiction, (b) limited appeal mechanisms with low reversal rates, (c) information asymmetry (authority knows its own constraints; population does not), (d) collective action barriers to organizing resistance, (e) legal immunity or near-immunity for authority actions. Suppression is not total because some jurisdictions have stronger oversight frameworks, but the baseline is high. Theater ratio (0.68): High. Formal accountability mechanisms are substantially performative. Review boards meet, audits are conducted, and appeals processes exist, but the core power asymmetry persists. The theater increases over time as oversight institutions are created to manage political pressure while actual authority constraints remain minimal. ArXiv preprints and distributed scrutiny have no equivalent in the political sphere—there is no parallel 'open governance' channel bypassing the formal accountability theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The mandated authority and delegating institution experience rope-like coordination benefits (low chi). The oversight body experiences tangled rope (mixed benefit and cost). The subject population experiences snare-like extraction (high chi, no exit). The accountability theater is a piton (performative ritual maintained by inertia). The divergence reflects different structural positions: those who control the mandate see coordination; those who are subject to it see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the mandate's power asymmetry. The mandated authority derives d ≈ 0.05-0.15: they are the beneficiary (low d, negative chi). The delegating institution derives d ≈ 0.10-0.20: they are a secondary beneficiary with arbitrage options (low-moderate d). The oversight body derives d ≈ 0.50-0.65: they experience mixed benefits (coordination function they help maintain) and extraction costs (subordinated position, lack of enforcement power). The subject population derives d ≈ 0.85-0.95: they are the primary target (high d, high chi). The analytical observer derives d ≈ 0.72-0.75: they are neutral observers of the structure (canonical d for analytical power). These derivations reflect the actual structural relationships: beneficiaries have exit (can reinterpret mandate or reallocate resources), while targets are trapped (cannot exit the mandate's jurisdiction).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The asymmetric mandate's tangled rope classification is RESOLVABLE. The constraint currently exhibits both genuine coordination benefits (enables rapid action, solves delegation problem) and asymmetric extraction (concentrates power, lacks accountability). The mandatrophy asks: which dominates? The answer depends on three structural parameters. (1) OVERSIGHT ENFORCEMENT POWER: Does the oversight body have real authority to reverse decisions, impose sanctions, or revoke the mandate? If yes: genuine tangled rope. If no: snare with theatrical oversight. (2) MANDATE SCOPE BOUNDARIES: Are there enforceable limits on authority expansion, or does the authority expand de facto? If boundaries are enforced: tangled rope. If not: snare with creeping extraction. (3) ACCOUNTABILITY MECHANISM EFFECTIVENESS: What fraction of authority decisions are actually overruled or sanctioned? If >20%: tangled rope. If <5%: snare. Current measurements suggest mandate degradation from tangled rope toward snare: extractiveness grew from 0.35 to 0.58, and theater ratio grew from 0.45 to 0.68. This indicates that formal oversight is becoming increasingly theatrical while the authority's practical power increases. Mandatrophy is resolved by identifying specific institutional reforms: rotating mandate holders (resets asymmetry), embedding real-time oversight (transforms retrospective review into concurrent constraint), splitting decision authority (distributes power), or implementing mandatory sunset with costly renewal (creates accountability pressure). Without these reforms, the mandate is a snare with theatrical accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_scope_creep,
    'Does the mandated authority''s scope inevitably expand beyond the formal authorization, or can contractual and institutional design prevent scope creep?',
    'Comparative analysis of mandate expansion across jurisdictions and time periods; correlation between mandate renewal frequency and actual scope limitation; case studies of authorities that maintained strict mandate boundaries vs those that expanded',
    'If scope creep is inevitable: mandate is structurally a snare regardless of oversight. If preventable: snare classification depends on institutional design choices (and becomes tangled rope if well-designed oversight exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_scope_creep, empirical, 'Whether mandated authority scope inevitably expands beyond formal authorization').

omega_variable(
    oversight_sufficiency_threshold,
    'What threshold of oversight power (enforcement authority, real-time monitoring, decision reversal capability) transforms asymmetric mandate from snare into genuine tangled rope?',
    'Analysis of oversight bodies with varying enforcement powers; measurement of override frequency and decision reversal rates; comparative study of outcomes under weak vs strong oversight; institutional design experiments with real-time vs retrospective monitoring',
    'If low threshold: many mandates with minimal oversight are still snares. If high threshold: most historical mandates are snares, and genuine tangled rope is rare. Directly addresses mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oversight_sufficiency_threshold, empirical, 'Threshold of oversight power for converting snare to tangled rope').

omega_variable(
    emergency_legitimacy_boundary,
    'Is there a structural distinction between legitimate emergency authority (temporary, bounded, with sunset) and illegitimate asymmetric power (permanent, expanding, without exit)? Or are they part of a continuum?',
    'Historical analysis of emergency declarations and mandate renewals; comparison of outcomes for time-limited vs indefinite mandates; institutional mechanism audits (does sunset clause actually trigger constraints on authority? is emergency always renewed?)',
    'If continuum: both are extractive, differ only in speed of degradation. If distinct: legitimate emergency is rope/scaffold, illegitimate asymmetry is tangled rope/snare. Mandatrophy is resolved by identifying the boundary conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_legitimacy_boundary, conceptual, 'Distinction between legitimate emergency authority and illegitimate asymmetric power').

omega_variable(
    accountability_mechanism_effectiveness,
    'Do formal accountability mechanisms (review boards, appeals, external audits, legislative oversight) actually constrain mandated authority, or are they predominantly theatrical?',
    'Audit of decision reversal rates; comparison of authority behavior pre/post oversight institution creation; analysis of sanctions imposed on authorities for mandate violations; longitudinal tracking of authority decisions that survive vs are overruled by oversight',
    'If effective: mandate is tangled rope (real coordination + real oversight cost). If theatrical: mandate is snare (coordination benefit to authority, but no meaningful extraction cost for subject population).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_mechanism_effectiveness, empirical, 'Whether formal accountability mechanisms actually constrain mandated authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_without_responsibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwr_tr_t0, power_without_responsibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pwr_tr_t5, power_without_responsibility, theater_ratio, 5, 0.6).
narrative_ontology:measurement(pwr_tr_t10, power_without_responsibility, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pwr_be_t0, power_without_responsibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pwr_be_t5, power_without_responsibility, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pwr_be_t10, power_without_responsibility, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_without_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(power_without_responsibility, emergency_governance_abuse).
narrative_ontology:affects_constraint(power_without_responsibility, regulatory_capture_asymmetry).
narrative_ontology:affects_constraint(power_without_responsibility, sovereign_immunity_extraction).

% DUAL FORMULATION NOTE:
% The asymmetric mandate decomposes into three downstream constraints reflecting different institutional contexts: emergency governance (war, pandemic, disaster response), regulatory capture (captured agencies exercising broad enforcement), and sovereign immunity (state actors exempt from accountability). Each has its own ε value depending on empirical constraints and oversight mechanisms. This story addresses the generic structural pattern common to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(power_without_responsibility, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
