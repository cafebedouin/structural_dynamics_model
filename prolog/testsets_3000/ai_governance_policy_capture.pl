% ============================================================================
% CONSTRAINT STORY: ai_governance_policy_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_policy_capture, []).

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
 *   constraint_id: ai_governance_policy_capture
 *   human_readable: AI Governance Policy Capture
 *   domain: technology_policy/governance
 *
 * SUMMARY:
 *   AI governance policy capture represents a hybrid coordination-extraction
 *   constraint where the institutions designed to manage AI risk are
 *   systematically shaped by the technology companies they are meant to
 *   oversee. This constraint exhibits the classic tangled_rope structure:
 *   genuine coordination happens (regulators and companies do collaborate on
 *   technical standards, safety testing frameworks, and governance design),
 *   but coordination is asymmetrically extracted — the capture ensures that
 *   coordination serves company interests at the expense of labor, public
 *   safety, and developing-nation autonomy. The extractiveness metric (0.62)
 *   reflects that policy capture is neither pure coordination (rope would be
 *   ≤0.45) nor pure extraction (snare would require ≥0.66 with high
 *   suppression). The theater ratio (0.58) indicates that significant
 *   governance work is performative: international AI committees, corporate
 *   'responsible AI' initiatives, and multi-stakeholder forums create the
 *   appearance of democratic governance while substantive power moves through
 *   bilateral corporate-state arrangements. The suppression score (0.68)
 *   reflects coordinated efforts to limit transparency in AI development,
 *   concentration of technical expertise among corporations, and asymmetric
 *   access to data and model weights that regulators need to evaluate claims.
 *
 * KEY AGENTS:
 *   - AI Technology Companies: Primary beneficiary (institutional/arbitrage) — shape policy, establish market entry barriers, externalize risks
 *   - Labor-Displaced Workers: Primary victim (powerless/trapped) — face retraining costs and policy neglect; no organizing capacity or political power
 *   - Developing Nations: Secondary victim (powerless/trapped) — dependent on foreign AI systems, excluded from governance, trapped by capital constraints
 *   - Regulatory Agencies: Secondary victim (moderate/constrained, identity_locked) — embedded in industry networks; identity increasingly fused with being 'pro-innovation'
 *   - Civil Society Coalitions: Secondary victim (organized/constrained) — sufficient power to participate but insufficient power to redirect policy; extraction through diluted influence
 *   - International Governance Bodies: Secondary institution (institutional/arbitrage) — perform legitimacy while substantive power flows elsewhere; maintain through institutional inertia (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_policy_capture, 0.62).
domain_priors:suppression_score(ai_governance_policy_capture, 0.68).
domain_priors:theater_ratio(ai_governance_policy_capture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_policy_capture, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_governance_policy_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_governance_policy_capture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_policy_capture, tangled_rope).
narrative_ontology:human_readable(ai_governance_policy_capture, "AI Governance Policy Capture").
narrative_ontology:topic_domain(ai_governance_policy_capture, "technology_policy/governance").

domain_priors:requires_active_enforcement(ai_governance_policy_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_policy_capture, ai_technology_companies).
narrative_ontology:constraint_beneficiary(ai_governance_policy_capture, ai_capital_investors).
narrative_ontology:constraint_victim(ai_governance_policy_capture, public_safety_interests).
narrative_ontology:constraint_victim(ai_governance_policy_capture, labor_displaced_workers).
narrative_ontology:constraint_victim(ai_governance_policy_capture, developing_nations).
narrative_ontology:constraint_victim(ai_governance_policy_capture, regulatory_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR-DISPLACED WORKER (SNARE) — Faces retraining costs, geographic constraints, and no meaningful policy protection. The constraint extracts labor value through disruption while suppressing worker organizing and alternative skill recognition. No exit options; experiences maximum extraction.
constraint_indexing:constraint_classification(ai_governance_policy_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION (SNARE) — Trapped in asymmetric AI dependency without capacity to develop indigenous governance or technical capacity. Policy capture ensures external control over AI deployment in their markets. Maximum extraction with minimal agency.
constraint_indexing:constraint_classification(ai_governance_policy_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-LEVEL REGULATOR (TANGLED ROPE) — Constrained by budget, technical expertise gap, and revolving-door career incentives. Benefits from coordination (regulation improves industry legitimacy) while suffering extraction (capture prevents effective oversight). Identity increasingly locked into industry relationship.
constraint_indexing:constraint_classification(ai_governance_policy_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AI TECHNOLOGY COMPANY (ROPE) — Experiences constraint as coordination: participating in policy governance creates legitimacy, shapes rules in favorable direction, and establishes market entry barriers for competitors. Net beneficiary with full arbitrage options.
constraint_indexing:constraint_classification(ai_governance_policy_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIL SOCIETY COALITION (TANGLED ROPE) — Organized but resource-constrained. Benefits from participation in governance discourse; suffers extraction through diluted influence and agenda-setting capture. Can mobilize but faces asymmetric power.
constraint_indexing:constraint_classification(ai_governance_policy_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL GOVERNANCE BODIES (PITON) — UN AI committees, WHO guidance, EU AI Act consultations are largely performative. Industry dominates working groups, capture is embedded in the process design itself. These bodies produce theater while substantive power moves to bilateral corporate-state deals.
constraint_indexing:constraint_classification(ai_governance_policy_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_policy_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_governance_policy_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_governance_policy_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_policy_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_governance_policy_capture, TR),
    TR >= 0.70.

:- end_tests(ai_governance_policy_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts value from multiple groups (labor, public safety interest, regulatory capacity) while producing genuine but asymmetric coordination. The extraction has increased over the interval (0.35→0.62) as company influence has grown and regulatory capture has deepened. This is not the maximal extraction of a snare (which would require ≥0.66 with truly minimal coordination function), but rather the moderate-high extraction of a hybrid system. Suppression (0.68): High. Significant suppression mechanisms exist: technical expertise concentration, asymmetric access to model weights and training data, public relations dominance, regulatory capture through revolving doors and consultant networks, and suppression of alternative governance pathways (e.g., public AI development, cooperative models). Theater ratio (0.58): Moderate-high. International AI committees produce governance theater (appearance of democratic process) while real power moves through bilateral arrangements. Corporate 'responsible AI' initiatives are substantially performative compliance. However, some genuine coordination occurs (technical safety testing, vulnerability disclosure, international standards bodies have real function despite capture).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is extreme. The AI technology company sees rope — pure coordination that improves legitimacy and enables faster scaling. The powerless agent (labor-displaced worker, developing nation) sees snare — pure extraction with no exit. The regulator occupies the most complex position: they genuinely coordinate (regulation improves safety through disclosure requirements, testing protocols, incident reporting), yet they are captured (their coordination work is shaped to serve company interests). The civil society coalition sees tangled_rope — they both participate in governance (giving them some voice) and are systematically diluted (their voice is outnumbered and agenda-setting is captured). The international bodies see piton — they perform legitimacy while real governance happens elsewhere. This perspectival range is the diagnostic signature of a hybrid constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from power level, exit options, and structural position relative to extraction flow. AI technology companies (institutional/arbitrage) have d≈0.05 (pure beneficiary, full exit options); labor-displaced workers (powerless/trapped) have d≈0.95 (pure target, no exit); regulators (moderate/constrained) have d≈0.70 (mixed — they coordinate but are captured); civil society (organized/constrained) have d≈0.60 (they have power to constrain but insufficient power to redirect). The directionality pipeline produces f(d) values that, when multiplied by base extractiveness and scope modifier, yield the experienced extraction (χ) for each perspective. Beneficiaries experience negative or minimal χ; victims experience high χ; intermediaries experience moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between genuine coordination (which exists) and asymmetric extraction (which overlays it). The mandate for AI governance is real — regulators genuinely need to coordinate on safety standards, testing protocols, and international frameworks. Companies genuinely benefit from legitimacy-producing governance that reduces litigation and enables faster deployment. The mandatrophy would arise if the system were purely extractive (no coordination), which it is not. But the coordination is captured — designed to serve extraction rather than public safety. The resolution: (1) identify the genuine coordination functions (safety testing, vulnerability disclosure, standards setting); (2) identify how capture distorts each function (e.g., safety testing is self-reported, standards are shaped by industry preferences); (3) classify as tangled_rope (both present); (4) measure extraction within the coordination framework (how much value flows to beneficiaries vs victims despite the coordination mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_deliberate_vs_structural,
    'Is policy capture primarily deliberate corporate strategy or inevitable structural misalignment between AI development speed and governance capacity?',
    'Documentary evidence of coordinated capture campaigns vs analysis of institutional capacity gaps and revolving-door patterns',
    'If deliberate: snare perspective dominates; if structural: tangled_rope is more accurate; if mixed: need multiple constraint stories per domain (e.g., separate ai_governance_capacity_gap story)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_deliberate_vs_structural, empirical, 'Whether capture is deliberate strategy or structural capability gap').

omega_variable(
    exit_options_for_regulator_legitimacy,
    'Can captured regulators exit by whistleblowing or institutional reform without destroying the legitimacy they ostensibly provide to governance?',
    'Historical analysis of regulator exit attempts; measurement of how public trust changes when capture is revealed',
    'If exit is possible: regulators are constrained not trapped, move toward identity_locked classification. If exit destroys legitimacy: regulators are trapped by their own function (piton with extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_options_for_regulator_legitimacy, empirical, 'Whether regulators can exit capture without destroying their institutional legitimacy').

omega_variable(
    developing_nation_alternatives,
    'Can developing nations create alternative AI governance frameworks outside the captured international regime, or is technological dependency absolute?',
    'Analysis of Chinese, Indian, and African AI governance initiatives; measurement of actual policy autonomy vs rhetorical autonomy',
    'If real alternatives exist: developing nation classification shifts from trapped to constrained. If alternatives are blocked: confirms snare classification and deep structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_alternatives, empirical, 'Whether developing nations have realistic exit options from captured global governance').

omega_variable(
    civil_society_coalition_efficacy_threshold,
    'At what level of organized coalition power does policy capture shift from snare (for unorganized) to contested tangled_rope (for organized)?',
    'Comparative analysis of policy outcomes correlated with coalition size, budget, and technical expertise; measurement of policy wins vs corporate objectives',
    'If threshold is high: civil society remains trapped in tangled_rope indefinitely. If threshold is low: coalition power approaches critical mass (dynamic coalition extension), potentially shifting classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_society_coalition_efficacy_threshold, empirical, 'Coalition power threshold for shifting from snare to contested tangled rope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_policy_capture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_policy_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ai_g_tr_t3, ai_governance_policy_capture, theater_ratio, 3, 0.5).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_policy_capture, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_policy_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_g_be_t3, ai_governance_policy_capture, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_policy_capture, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_policy_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_policy_capture, ai_labor_displacement).
narrative_ontology:affects_constraint(ai_governance_policy_capture, algorithmic_transparency_suppression).
narrative_ontology:affects_constraint(ai_governance_policy_capture, model_weight_asymmetry).
narrative_ontology:affects_constraint(ai_governance_policy_capture, developing_nation_ai_dependency).

% DUAL FORMULATION NOTE:
% AI governance policy capture is the parent constraint affecting multiple domain-specific extractions. The upstream constraint is the fundamental misalignment between corporate AI development speed and democratic governance capacity (ai_governance_capacity_gap). Decompose into separate stories for labor impacts (ai_labor_displacement), transparency suppression (algorithmic_transparency_suppression), model access asymmetry (model_weight_asymmetry), and structural dependency of developing nations (developing_nation_ai_dependency). Each has different epsilon, victims, and measurement signatures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_policy_capture, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
