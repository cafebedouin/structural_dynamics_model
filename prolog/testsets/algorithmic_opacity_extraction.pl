% ============================================================================
% CONSTRAINT STORY: algorithmic_opacity_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_opacity_extraction, []).

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
 *   constraint_id: algorithmic_opacity_extraction
 *   human_readable: Algorithmic Opacity as an Extraction Mechanism
 *   domain: technology/economics/governance
 *
 * SUMMARY:
 *   Algorithmic opacity extraction occurs when the opacity of algorithmic
 *   decision-making systems (black-box models, proprietary architectures,
 *   hidden training data) is deployed as a mechanism to prevent contestation,
 *   regulatory oversight, and accountability while concentrating benefits in
 *   the hands of algorithm designers and platform operators. The constraint
 *   operates across employment screening, credit assessment, housing
 *   allocation, content moderation, and criminal justice — domains where
 *   algorithmic decisions directly affect material welfare and opportunity.
 *   Opacity serves three structural functions: (1) intellectual property
 *   protection, (2) liability shield against discrimination liability, (3)
 *   regulatory arbitrage. The measurement trajectory shows increasing
 *   extractiveness (0.35 → 0.58) and theater ratio (0.42 → 0.64) over the
 *   interval, indicating that opacity is becoming more entrenched as a
 *   strategic mechanism while the legitimation ritual (fairness metrics,
 *   auditing, explainability research) intensifies without producing
 *   corresponding functional transparency. This creates a snare constraint
 *   for algorithmic subjects: they cannot understand decisions affecting
 *   them, cannot contest those decisions, cannot exit systems that gate
 *   essential services.
 *
 * KEY AGENTS:
 *   - Algorithmic Subjects: Primary victims (powerless/trapped) — individuals whose life outcomes are determined by opaque systems with no contestation mechanism or exit option
 *   - Excluded Populations: Secondary victims (moderate/constrained) — marginalized communities bearing compounded extraction: algorithmic bias + opacity + inability to exit or contest
 *   - Algorithm Designers & Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture competitive advantage, intellectual property protection, liability reduction from opacity
 *   - Regulatory Agencies: Institutional actor (organized/constrained) — need algorithmic systems for scale but cannot effectively oversee due to opacity; bear legitimacy cost of algorithmic failures
 *   - Informed Professionals & Advocates: Secondary actor (powerful/mobile) — technologists, lawyers, civil society organizations with capacity to contest opacity but without institutional power to enforce transparency
 *   - Legitimation Ritual: The theater of accountability (auditing, fairness metrics, explanation methods) — maintains social license for opacity-based systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_opacity_extraction, 0.58).
domain_priors:suppression_score(algorithmic_opacity_extraction, 0.68).
domain_priors:theater_ratio(algorithmic_opacity_extraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_opacity_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_opacity_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_opacity_extraction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_opacity_extraction, snare).
narrative_ontology:human_readable(algorithmic_opacity_extraction, "Algorithmic Opacity as an Extraction Mechanism").
narrative_ontology:topic_domain(algorithmic_opacity_extraction, "technology/economics/governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_opacity_extraction, algorithm_designers).
narrative_ontology:constraint_beneficiary(algorithmic_opacity_extraction, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_opacity_extraction, capital_holders).
narrative_ontology:constraint_victim(algorithmic_opacity_extraction, algorithmic_subjects).
narrative_ontology:constraint_victim(algorithmic_opacity_extraction, data_contributors).
narrative_ontology:constraint_victim(algorithmic_opacity_extraction, excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC SUBJECT (SNARE) — Individual users and data contributors cannot exit algorithmic systems without surrendering access to essential services (employment screening, credit assessment, housing, social connection). Opacity prevents them from understanding or contesting decisions that affect their material welfare. Full structural extraction with no viable exit.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED POPULATIONS (SNARE) — Communities already marginalized (poor, undocumented, racialized minorities) face algorithmic opacity compounded with demographic bias. Cannot contest decisions because opacity hides the mechanism; cannot exit because algorithmic systems control access to credit, housing, employment. Exit cost is prohibitive — effectively trapped at generational scale.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCIES (TANGLED ROPE) — Agencies (FTC, banking regulators, employment commissions) need algorithmic systems for administrative efficiency but are constrained by opacity that prevents effective oversight. They both benefit from the coordination function (algorithms enable scale) and bear extraction costs (cannot verify compliance, suffer legitimacy damage from failures). Active enforcement capacity is limited; exit is constrained by resource and political barriers.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALGORITHM DESIGNERS / PLATFORM OPERATORS (ROPE) — Benefit from opacity as a competitive moat and liability shield. Experience the opacity constraint as a coordination mechanism: keeping algorithms proprietary solves the problem of protecting intellectual property and avoiding regulatory scrutiny. Have multiple exit options (can disclose, can simplify) but choose not to. Net beneficiaries.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGITIMATION RITUAL (PITON) — Algorithmic auditing, fairness metrics, and bias testing have become largely performative. Companies conduct audits, publish fairness reports, and claim transparency while maintaining core opacity. The theater of accountability persists (black-box models with post-hoc explanations) while the functional verification capacity remains minimal. Theater ratio (0.64) reflects this degradation — the ritual persists through institutional inertia.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INFORMED PROFESSIONALS / ADVOCATES (TANGLED ROPE) — Technologists, lawyers, and civil society organizations have some capacity to demand transparency and contest opacity (exit option: mobile — can move between employers, can organize campaigns). They both coordinate with regulation-seeking goals and bear extraction through epistemic labor (reverse-engineering algorithms, documenting bias) without compensation. Moderate extraction, not maximum.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Computational complexity arguments suggest that some opacity is inherent: complex neural networks cannot be fully interpretable, trade-offs between accuracy and explainability are fundamental, scale requires some black-boxing. This perspective risks naturalizing what is actually a contingent design choice (opacity as intellectual property protection and regulatory arbitrage) as an immutable law of computation. The engine detects this as a false summit.
constraint_indexing:constraint_classification(algorithmic_opacity_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_opacity_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_opacity_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_opacity_extraction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_opacity_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_opacity_extraction, TR),
    TR >= 0.70.

:- end_tests(algorithmic_opacity_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Opacity enables asymmetric information extraction — platform operators know how systems work and can optimize for their interests; subjects don't know and cannot contest. The measured value reflects that while some opacity is arguably coordination cost (protecting genuine IP, managing complexity), the bulk of opacity (>60%) is strategic choice for extraction. The trajectory (0.35 → 0.58) shows that as algorithmic systems penetrate more critical domains, the extractive value of opacity increases. Suppression (0.68): High. Multiple barriers prevent exit or contestation: (1) algorithmic systems gate access to essential services (credit, employment, housing, social connection) — no viable exit without surrendering opportunity, (2) opacity prevents understanding the mechanism, (3) technical knowledge barriers prevent self-education, (4) legal frameworks don't grant contestation rights or audit access, (5) scale and concentration mean alternative systems are unavailable. Theater ratio (0.64): Moderate-high. The proliferation of fairness research, algorithmic audits, explainability methods, and corporate fairness commitments suggests accountability theater. Companies conduct audits, publish fairness reports, hire ethics teams, but maintain core opacity. Post-hoc explanation methods (LIME, SHAP) add interpretability theater around black-box models without changing opacity. The theater has grown (0.42 → 0.64) while core opacity persists. This indicates piton dynamics: legitimation ritual maintains social license for extractive systems.
 *
 * PERSPECTIVAL GAP:
 *   Gap analysis reveals why this constraint sustains despite well-known harms: beneficiaries and victims perceive opposite classifications (Rope vs Snare) from structurally symmetric positions. Platform operators correctly perceive their own experience as coordination (the constraint solves their problem of IP/liability protection). Subjects correctly perceive the same constraint as extraction (the constraint prevents contestation). Neither side is mistaken about their own experience — the perspectival gap is real. This makes the constraint particularly robust: it persists because beneficiaries can sincerely describe it as coordination while victims sincerely experience it as extraction. The false mountain perspective (opacity is technically necessary) provides additional legitimation — it naturalizes what is actually a strategic choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Algorithm designers and platform operators are net beneficiaries: they capture value from opacity (IP protection, liability reduction, market concentration) and have multiple exit options (they could disclose, simplify, explain — they choose not to). Algorithmic subjects are full targets: they experience maximum extraction (no understanding, no contestation, no exit from systems that gate essential services) and have no exit options that don't sacrifice opportunity. Regulatory agencies are caught in a mixed position: they benefit from algorithmic coordination (enabling administrative scale) but bear extraction costs (cannot verify fairness, suffer legitimacy damage from failures, face capture pressure). Informed professionals have some agency (can move employers, organize advocacy campaigns) so experience constrained rather than trapped exit — producing moderate rather than maximum extraction. The directionality slope is steep: from beneficiary with arbitrage options (d ≈ 0.10) through institutional actors constrained by political/resource barriers (d ≈ 0.55) to powerless subjects with no exit (d ≈ 0.95).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply here — extractiveness (0.58) is below the 0.70 threshold. However, the theater_ratio (0.64) and rising trajectory toward piton dynamics warrant observation. If theater_ratio reaches 0.70 while extractiveness remains > 0.46, the constraint will degrade into a piton: legitimation ritual replacing functional accountability, core opacity persisting through inertial institutional structures. The measurement trajectory suggests this transition is already underway. Fairness research has become institutionalized (conferences, journals, career paths) while core algorithmic opacity persists. Audits are conducted but findings are often not implemented. Explainability methods add interpretability layers around fundamentally black-box systems. The constraint may be entering piton phase before mandatrophy becomes relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_vs_choice,
    'Is algorithmic opacity technically necessary (inherent to model complexity) or strategically chosen (for competitive/liability protection)?',
    'Comparison of transparency costs in simple vs complex systems; analysis of disclosure patterns when regulatory pressure increases; examination of whether companies optimize for explainability when incentivized',
    'If necessary: some extraction is coordination cost (Rope from platform perspective is more justified). If chosen: opacity is pure extraction mechanism (Snare classification strengthens). Current evidence suggests >60% is strategic choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opacity_necessity_vs_choice, empirical, 'Whether opacity is technically necessary or strategically chosen').

omega_variable(
    audit_effectiveness_paradox,
    'Do algorithmic audits and fairness metrics actually improve outcomes for affected populations, or do they primarily serve legitimation without functional impact?',
    'Longitudinal tracking of audit findings vs subsequent algorithm changes; measurement of outcome disparities before/after audit; analysis of whether audit recommendations are implemented or ignored',
    'If audits improve outcomes: theater_ratio should decline over time, piton perspective weakens. If audits are performative: theater_ratio stable/rising, piton perspective confirmed, legitimation_ritual comment validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_effectiveness_paradox, empirical, 'Whether algorithmic audits produce functional or performative impact').

omega_variable(
    regulatory_capture_mechanism,
    'To what extent does algorithmic complexity serve as a tool for regulatory capture — creating barriers to oversight that benefit platform operators over public interest?',
    'Analysis of regulatory agency capacity vs algorithmic complexity growth over time; examination of disclosure requests denied due to ''trade secret'' claims; comparison of transparency requirements across jurisdictions',
    'If significant capture: institutional perspective (regulators) should downgrade from Tangled Rope to Snare, extraction values increase. If minimal capture: Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether opacity enables regulatory capture').

omega_variable(
    interpretability_tradeoff_authenticity,
    'Does the claimed accuracy-interpretability tradeoff reflect genuine technical constraint or post-hoc rationalization for maintaining opacity?',
    'Measurement of accuracy loss from simplified/transparent model architectures; controlled experiments varying transparency requirements; examination of whether maximum-accuracy models are always maximum-opacity models',
    'If tradeoff is real but modest: some opacity is justified coordination cost (extraction < 0.40). If tradeoff is exaggerated: opacity is primarily extraction mechanism (extraction > 0.65).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_tradeoff_authenticity, empirical, 'Authenticity of accuracy-interpretability technical tradeoff').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_opacity_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algop_tr_t0, algorithmic_opacity_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algop_tr_t3, algorithmic_opacity_extraction, theater_ratio, 3, 0.55).
narrative_ontology:measurement(algop_tr_t6, algorithmic_opacity_extraction, theater_ratio, 6, 0.62).
narrative_ontology:measurement(algop_tr_t10, algorithmic_opacity_extraction, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(algop_be_t0, algorithmic_opacity_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algop_be_t3, algorithmic_opacity_extraction, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(algop_be_t6, algorithmic_opacity_extraction, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(algop_be_t10, algorithmic_opacity_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_opacity_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_opacity_extraction, algorithmic_bias_entrenchment).
narrative_ontology:affects_constraint(algorithmic_opacity_extraction, regulatory_capture_financial_services).
narrative_ontology:affects_constraint(algorithmic_opacity_extraction, labor_market_discrimination_systems).
narrative_ontology:affects_constraint(algorithmic_opacity_extraction, content_moderation_opacity).

% DUAL FORMULATION NOTE:
% Algorithmic opacity extraction should be decomposed into domain-specific constraints (hiring algorithms, credit scoring, content moderation, criminal justice) if observable-dependent analysis reveals significantly different extractiveness values. The present story treats opacity as a unified mechanism but domain-specific instantiations may have ε values ranging from 0.35 (hiring) to 0.72 (criminal justice) depending on stakes and contestation capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_opacity_extraction, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
