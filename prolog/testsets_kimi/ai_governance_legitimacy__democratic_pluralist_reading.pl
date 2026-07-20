% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: AI Governance Legitimacy - Democratic Pluralist Reading
 *   domain: theological ethics/technology governance/political theology
 *
 * SUMMARY:
 *   This constraint instantiates the democratic pluralist reading of the
 *   ai_governance_legitimacy kernel. It treats AI governance legitimacy as
 *   deriving from inclusive democratic deliberation and public reason,
 *   rejecting any single tradition's interpretive monopolyâincluding the
 *   Magisterium's unique authorityâwhile accepting the encyclical's dignity
 *   claims as one voice among many. The constraint is authored as a scaffold:
 *   transitional participatory infrastructure meant to build deliberative
 *   capacity with an intended sunset, though its trajectory shows
 *   institutional solidification.
 *
 * KEY AGENTS:
 *   - democratic_institutions: Primary agenda-setter (institutional/global) â administers deliberative forums and derives legitimacy from electoral mandate.
 *   - civil_society_organizations: Primary beneficiary (organized/global) â gains institutionalized access to norm-setting processes.
 *   - minority_rights_holders: Secondary beneficiary (powerless/global) â receives procedural protections against majoritarian override.
 *   - excluded_populations: Primary target (powerless/global) â structurally excluded from deliberation and bears governance outcomes without voice.
 *   - authoritarian_regime_populations: Secondary target (powerless/global) â subject to democratic coalition norms without electoral consent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy - Democratic Pluralist Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological ethics/technology governance/political theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'beba162e-401d-4795-85d0-609adee0a076').
narrative_ontology:cs_kernel_codification('beba162e-401d-4795-85d0-609adee0a076', distributed).
narrative_ontology:cs_authority_grounding('beba162e-401d-4795-85d0-609adee0a076', distributed).
narrative_ontology:cs_reading_relation('beba162e-401d-4795-85d0-609adee0a076', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('beba162e-401d-4795-85d0-609adee0a076', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('beba162e-401d-4795-85d0-609adee0a076', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('beba162e-401d-4795-85d0-609adee0a076', foundational, democratic_consent_as_legitimacy_source).
narrative_ontology:cs_axiom_status(democratic_consent_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('beba162e-401d-4795-85d0-609adee0a076', democratic_consent_as_legitimacy_source, conventional).
narrative_ontology:cs_axiom('beba162e-401d-4795-85d0-609adee0a076', foundational, pluralist_interpretive_equality).
narrative_ontology:cs_axiom_status(pluralist_interpretive_equality, holdable).
narrative_ontology:cs_axiom_grounding('beba162e-401d-4795-85d0-609adee0a076', pluralist_interpretive_equality, deontological).
narrative_ontology:cs_reference_frame('beba162e-401d-4795-85d0-609adee0a076', inclusive_public_reason).
narrative_ontology:cs_drift_state('beba162e-401d-4795-85d0-609adee0a076', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('beba162e-401d-4795-85d0-609adee0a076', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, democratic_deliberation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene and administer transnational deliberative bodies for AI governance, translating public reason into regulatory frameworks. They derive authority from electoral and constitutional mandates and are responsible for maintaining the participatory infrastructure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, beneficiary).

% Participate in global AI governance forums, public consultations, and multi-stakeholder deliberations. They gain institutionalized channels to influence norm-setting but remain dependent on the procedural openness of democratic institutions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, constrained, global).

% Access procedural safeguards and representational mechanisms within AI governance deliberations that are designed to prevent majoritarian override of their fundamental interests. Their inclusion depends on the enforcement of inclusive public reason requirements.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, identity_locked, global).

% Populations lacking digital access, linguistic recognition, or formal documentation who are structurally unable to participate in global AI deliberations. They are subject to governance outcomes shaped by forums they cannot enter.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations, payer,
    powerless, immediate, trapped, global).

% Citizens of states that reject democratic legitimacy frameworks, who are subject to AI governance standards and norms developed by democratic coalitions without their direct consent or electoral accountability.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations, payer,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transnational deliberative infrastructure where diverse ethical traditions, technical expertise, and affected communities negotiate AI governance principles without granting any single tradition interpretive monopoly.
% TRANSFER_FUNCTION: Moves authority over AI norm-setting from unilateral claimants to distributed participatory forums, and moves procedural voice and protections to marginalized groups who would otherwise be excluded from global governance.
% ABSENT_VOICES: Populations under authoritarian regimes who cannot participate in democratic deliberation; future generations and non-human entities whose interests are mediated rather than directly represented; communities that reject pluralist legitimacy in favor of comprehensive doctrines.
% DISAPPEARANCE_RATIONALE: If the democratic pluralist framework vanished, AI governance legitimacy would revert to technocratic optimization, market libertarian default, or magisterial unilateralism. Civil society would lose institutionalized access points; minority protections would rely on majoritarian charity; and the encyclical's dignity claims would be interpreted by a single tradition rather than balanced through public reason.
% FOUNDING_PROBLEM: The absence of legitimate global governance for transformative AI technologies, where unilateral actors claimed monopoly on ethical interpretation, producing systemic exclusion and contestation.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists of global governance and critical technology scholars outside the Catholic tradition attest that no extant institution holds democratic mandate for planetary AI governance, corroborating the need for deliberative scaffolding. The encyclical itself acknowledges the lack of comprehensive international authority.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) because the constraint genuinely coordinates multi-stakeholder participation but also concentrates authority in democratic institutions and excludes non-democratic populations. Suppression is moderate (0.35): enforcement relies on electoral accountability and judicial review rather than raw coercion, yet the framework structurally bars non-participants from exit. Theater ratio is moderate-low (0.25): much deliberative activity is functional, but performative legitimation increases as the scaffold ages. Accessibility collapse (0.40) reflects that alternatives to democratic deliberation remain conceptually available but are institutionally marginalized. Resistance (0.45) is substantial due to opposition from authoritarian regimes and technocratic actors.
 *
 * PERSPECTIVAL GAP:
 *   Democratic institutions and civil society organizations experience the constraint as legitimate coordination that solves the global governance deficit. Excluded populations and authoritarian regime populations experience it as extractive exclusionâa legitimacy framework that claims universality while structurally denying them voice. The engine computes this divergence from the structural data: beneficiaries with constrained exit sit near symmetric, while trapped populations sit near full target.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions and civil society organizations are declared beneficiaries, deriving directionality near the subsidy end. Minority rights holders are beneficiaries but with identity_locked exit, placing them closer to symmetric than unconstrained beneficiaries. Excluded populations and authoritarian regime populations are declared victims with trapped exit, placing them near the full-target end and amplifying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this constraint as pure coordination (rope) because it declares victims and requires active enforcement. It prevents mislabeling as pure extraction (snare) because it carries a sunset clause and a founding coordination problem that remains live. If the sunset clause atrophies and the infrastructure becomes permanent without transition, mandatrophy would shift the classification toward tangled_rope or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the democratic pluralist reading foreclose magisterial authority or merely deny its monopoly while coexisting within a polycentric order?',
    'Comparative analysis of jurisdictions integrating both democratic and religious authority, tracking whether magisterial claims are treated as illegitimate or as parallel contributors.',
    'If foreclosed, the constraint carries asymmetric suppression against religious authority; if coexistent, it functions as genuine pluralist coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural relationship between democratic pluralist and magisterial readings').

omega_variable(
    scaffold_sunset_veracity,
    'Is the deliberative infrastructure genuinely transitional with a determinate sunset, or has it become a permanent governance layer?',
    'Audit institutional charters for automatic dissolution clauses, transition triggers, and empirical trajectory of authority accumulation.',
    'If permanent without sunset activation, the scaffold claim is suspect and the constraint may function as a tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_veracity, empirical, 'Whether the scaffold has a genuine sunset or is permanent').

omega_variable(
    deliberative_exclusion_nature,
    'Is the exclusion of marginalized and authoritarian-regime populations a remediable structural gap in the deliberative framework, or a constitutive feature that legitimates democratic closure?',
    'Track whether procedural reforms expand inclusion over time or whether exclusion persists despite institutional maturation.',
    'If constitutive, effective suppression exceeds structural measures and the constraint tends toward snare; if remediable, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_exclusion_nature, conceptual, 'Nature of exclusion in deliberative legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, global_infrastructure).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_governance_legitimacy kernel. The natural-language concept of AI governance legitimacy decomposes into four structurally distinct readings (democratic pluralist, magisterial subsidiarity, technocratic optimization, market libertarian), each with distinct epsilon values, beneficiary/victim structures, and authority groundings. This reading instantiates the democratic pluralist position; siblings are alternative structural instantiations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
