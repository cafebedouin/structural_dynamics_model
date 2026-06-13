% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the feudal oath as interpreted and mediated by
 *   ecclesiastical authorities, emphasizing Christian charity and sacramental
 *   obligations to limit secular lords' extractive power. It is one reading
 *   of the broader 'feudal_oath_reciprocity' kernel, which also includes
 *   readings focused on maximal lord extraction or fixed vassal obligations.
 *   This reading positions the Church as a crucial, self-interested arbiter,
 *   creating a Tangled Rope where coordination (stability) comes with a cost
 *   (church authority).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.6).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '76f67b19-aefa-457d-b468-7c197fabc67f').
narrative_ontology:cs_kernel_codification('76f67b19-aefa-457d-b468-7c197fabc67f', formalized).
narrative_ontology:cs_authority_grounding('76f67b19-aefa-457d-b468-7c197fabc67f', lineage).
narrative_ontology:cs_interpretation_layer_present('76f67b19-aefa-457d-b468-7c197fabc67f').
narrative_ontology:cs_reading_relation('76f67b19-aefa-457d-b468-7c197fabc67f', feudal_oath_reciprocity__lord_extraction_reading, influences).
narrative_ontology:cs_reading_relation('76f67b19-aefa-457d-b468-7c197fabc67f', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('76f67b19-aefa-457d-b468-7c197fabc67f', foundational, christian_charity_limits_secular_power).
narrative_ontology:cs_axiom_status(christian_charity_limits_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('76f67b19-aefa-457d-b468-7c197fabc67f', christian_charity_limits_secular_power, deontological).
narrative_ontology:cs_axiom('76f67b19-aefa-457d-b468-7c197fabc67f', foundational, sacramental_oath_binds_divinely).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_divinely, holdable).
narrative_ontology:cs_axiom_grounding('76f67b19-aefa-457d-b468-7c197fabc67f', sacramental_oath_binds_divinely, theological).
narrative_ontology:cs_reference_frame('76f67b19-aefa-457d-b468-7c197fabc67f', divinely_ordered_feudal_society).
narrative_ontology:cs_drift_state('76f67b19-aefa-457d-b468-7c197fabc67f', late_medieval_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76f67b19-aefa-457d-b468-7c197fabc67f', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the moral and sacramental dimensions of feudal oaths, using Christian charity and canon law to limit secular lords' demands. Benefits from increased moral authority and influence over secular affairs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Bound by the ecclesiastical interpretation of their oaths, which limits their ability to extract maximal resources or service from their vassals. They face moral censure, excommunication threats, and potential rebellion if they overstep these religiously defined bounds.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Benefit from the ecclesiastical limits on their lords' demands, receiving a degree of protection from arbitrary extraction. Their obligations are still substantial, but the church provides a recourse against extreme abuses. Their identity is tied to the feudal system and Christian faith.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    moderate, biographical, identity_locked, local).

% While indirectly benefiting from any stability or reduced extraction on vassals, they have no direct voice or standing in the interpretation or enforcement of feudal oaths. Their lives are shaped by the outcomes of these elite-level constraints.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasantry, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a moral and legal framework for reciprocal obligations between lords and vassals, preventing outright anarchy or total exploitation by providing an external, divinely sanctioned arbiter.
% TRANSFER_FUNCTION: Transfers interpretive authority over feudal obligations from secular power to ecclesiastical institutions, and limits the transfer of resources/service from vassals to lords by imposing moral constraints.
% ABSENT_VOICES: The peasantry, who bear the ultimate burden of the feudal system, have no direct voice in the interpretation of these oaths. Their 'voice' would be for a more equitable distribution of land and labor, but they are structurally excluded from the discourse.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical interpretation and enforcement of feudal oaths vanished, secular lords would likely revert to maximal extraction, leading to increased vassal resistance, widespread conflict, and a breakdown of the existing social order. The balance of power would shift dramatically.
% FOUNDING_PROBLEM: The inherent instability of purely secular feudal contracts, prone to arbitrary power grabs and constant warfare, required a higher moral authority to bind parties and ensure a degree of reciprocal obligation.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles, papal bulls, and legal treatises from the period attest to the church's active role in mediating feudal disputes and asserting moral limits on secular power. Historians outside the church's direct influence corroborate this function, even while noting its self-interested aspects.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).
:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while the church limits secular extraction, it also extracts its own form of authority and influence. Suppression (0.6) is significant, as both secular and ecclesiastical power structures enforce compliance, with spiritual penalties (excommunication) backing physical force. Theater ratio (0.4) reflects the performative aspects of religious authority and ritual in maintaining the constraint, alongside genuine moral guidance. The metrics show a slight increase in extractiveness and suppression over time, indicating a gradual hardening of the church's interpretive power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities, this constraint is a necessary Rope, bringing divine order and justice to a chaotic secular world. From the perspective of secular lords, it is a Snare, an external imposition limiting their rightful authority and resources. Vassals experience it as a Tangled Rope, offering protection but still binding them to a system of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are primary beneficiaries (d=0.0-0.1) due to increased moral and political influence. Vassals are also beneficiaries (d=0.2-0.3) as their burdens are moderated, though they still bear significant costs. Secular lords are the primary targets (d=0.7-0.8) as their traditional power to extract is constrained. The peasantry is excluded from direct participation but indirectly affected.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to bring moral order and limit secular excess) is still live, but its function has shifted to also serve the institutional interests of the Church. The 'contested' status of the founding problem reflects this: while some coordination function remains, the extractive aspects have become more prominent, preventing a clear 'mandatrophy resolved' declaration. The classification as Tangled Rope captures this hybrid nature, avoiding mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''ecclesiastical mediation'' reading of the feudal oath, or is it primarily a ''lord extraction'' reading with a thin veneer of religious justification?',
    'Analysis of primary sources (e.g., papal decrees, episcopal letters, court records) to quantify the actual impact of ecclesiastical intervention on secular lords'' extractive practices versus mere rhetorical claims.',
    'If primarily a ''lord extraction'' reading, the extractiveness and suppression metrics would be higher, and the claimed_type would shift towards Snare. If genuine, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Distinguishing genuine ecclesiastical mediation from rhetorical cover for secular extraction.').

omega_variable(
    church_self_interest_vs_charity,
    'To what extent did the Church''s ''charity'' and ''sacramental obligations'' serve its own institutional power and wealth accumulation, rather than purely limiting secular extraction?',
    'Comparative historical analysis of church landholdings, tithe collection, and political influence in regions with strong vs. weak ecclesiastical mediation of feudal oaths.',
    'If self-interest was dominant, the ''ecclesiastical authorities'' would shift further towards a ''payer'' or ''agenda_setter'' role with higher directionality, and the constraint''s overall extractiveness would be higher, potentially pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_self_interest_vs_charity, empirical, 'Assessing the balance between genuine charity and institutional self-interest in ecclesiastical mediation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.32).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.35).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1150, 0.37).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1250, 0.39).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.4).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1000, 0.35).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.38).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.4).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1150, 0.42).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.43).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1250, 0.44).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.52).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.55).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1150, 0.57).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(feud_su_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1250, 0.59).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.1).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel. This 'ecclesiastical_mediation_reading' emphasizes the Church's role in limiting secular extraction through moral and sacramental obligations, contrasting with the 'lord_extraction_reading' (maximal secular power) and 'vassal_coordination_reading' (fixed reciprocal duties).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
