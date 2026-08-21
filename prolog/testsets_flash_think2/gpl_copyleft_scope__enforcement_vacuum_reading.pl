% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope: Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint is the 'enforcement vacuum' reading of the
 *   `gpl_copyleft_scope` kernel. It describes the situation where the absence
 *   of definitive judicial precedent creates a constraint of uncertainty,
 *   allowing diverse interpretations to coexist. Actual enforcement depends
 *   on which interpretive community has enforcement capacity in specific
 *   contexts (e.g., FSF-aligned projects vs. industry-dominated ecosystems).
 *   This uncertainty itself becomes a structural feature, leading to elevated
 *   transaction costs for clarity-seeking adopters and opportunities for
 *   pragmatic adopters to exploit ambiguity. Sibling readings include
 *   `strong_copyleft_reading` and `narrow_scope_reading`.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope: Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0').
narrative_ontology:cs_kernel_codification('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', distributed).
narrative_ontology:cs_authority_grounding('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', distributed).
narrative_ontology:cs_reading_relation('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', foundational, legal_ambiguity_is_structural).
narrative_ontology:cs_axiom_status(legal_ambiguity_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', legal_ambiguity_is_structural, conventional).
narrative_ontology:cs_axiom('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', foundational, enforcement_capacity_determines_outcome).
narrative_ontology:cs_axiom_status(enforcement_capacity_determines_outcome, holdable).
narrative_ontology:cs_axiom_grounding('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', enforcement_capacity_determines_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', unresolved_legal_pluralism).
narrative_ontology:cs_drift_state('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('edbc6c6f-aa8c-4104-82e3-0783b7bb0ac0', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystems).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for a strong interpretation of GPL copyleft, but face elevated transaction costs and legal risk due to the lack of definitive judicial precedent. They bear the cost of uncertainty and the effort to assert their interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, payer,
    organized, biographical, constrained, global).

% Benefit from the ambiguity, as it allows them to adopt more flexible licensing strategies and selectively enforce narrow interpretations of copyleft where they have market power. They can exploit the vacuum for strategic advantage.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystems, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystems, agenda_setter).

% Developers and companies who require clear legal guidance to manage their licensing obligations and avoid potential infringement claims. They face elevated legal costs for risk assessment and compliance due to the interpretive pluralism.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Developers and companies willing to navigate the legal ambiguity to gain flexibility in their software development and distribution. They benefit from the lack of strict enforcement, allowing them to operate in a 'licensed plurality'.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% The ultimate arbiter of legal disputes, but its absence of definitive rulings on key aspects of GPL copyleft scope (e.g., dynamic linking, aggregation) is the structural feature that creates the 'enforcement vacuum' constraint.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, judicial_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze and comment on the legal ambiguities surrounding GPL copyleft, contributing to the discourse but not directly enforcing or resolving the vacuum. They document the interpretive pluralism.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystems).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse interpretations of GPL copyleft scope to coexist in practice, preventing immediate legal gridlock by deferring definitive resolution to contextual power dynamics and risk assessment.
% TRANSFER_FUNCTION: Transfers legal risk and transaction costs to clarity-seeking parties and those advocating for strong copyleft, while transferring flexibility and opportunities for ambiguity exploitation to pragmatic adopters and industry ecosystems.
% ABSENT_VOICES: A definitive, universally accepted judicial precedent or legislative clarification on the precise scope of GPL copyleft, particularly regarding derivative works in complex software architectures. Such a voice would resolve the core ambiguity.
% DISAPPEARANCE_RATIONALE: If a clear, universally binding judicial precedent on GPL copyleft scope were to emerge overnight, the entire software licensing landscape would immediately reorganize. Licensing strategies, compliance efforts, and enforcement actions would shift dramatically to align with the new clarity, fundamentally altering the dynamics of open-source adoption and development.
% FOUNDING_PROBLEM: The inherent ambiguity in copyright law regarding what constitutes a 'derivative work' in the context of software, combined with the GPL's strong copyleft clauses, without sufficient judicial clarification to establish clear boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, open-source foundations (e.g., Linux Foundation, not just FSF), and corporate legal departments all consistently acknowledge the ongoing ambiguity and its implications for software development and distribution, even if they prefer different resolutions. This is widely documented in legal analyses and industry whitepapers.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) stems from the transaction costs associated with legal uncertainty, risk premiums, and the asymmetric advantage gained by those who can exploit ambiguity. Suppression (0.45) reflects the way the lack of clear alternatives (definitive legal guidance) forces parties into a state of managed risk or strategic ambiguity. The theater ratio is low (0.10) because the constraint is a genuine legal vacuum, not a performative facade. The claimed type is 'tangled_rope' because it coordinates a 'licensed plurality' of interpretations, but with asymmetric costs and benefits depending on a party's position and enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of FSF-aligned projects, the enforcement vacuum is a failure of the legal system to uphold the intended strength of copyleft, leading to erosion of its protective function. From the perspective of industry ecosystems, it's a pragmatic reality that allows for necessary flexibility in complex software environments. The engine's classification captures this divergence by modeling the costs and benefits of this ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Industry ecosystems and pragmatic adopters are beneficiaries (low directionality) as they gain flexibility and can leverage their power to assert preferred interpretations. FSF-aligned projects and clarity-seeking adopters are targets (high directionality) as they bear the costs of uncertainty and the effort to maintain strong copyleft principles without clear legal backing. The judicial system, by its inaction, acts as an agenda-setter for the vacuum itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_as_feature_or_bug,
    'Is the ongoing legal ambiguity regarding GPL copyleft scope a structural ''feature'' that enables flexibility and innovation, or a ''bug'' that creates unnecessary risk and undermines copyleft''s intent?',
    'Analysis of long-term economic and innovation metrics in jurisdictions with and without clearer precedent, coupled with a conceptual analysis of the normative goals of open-source licensing.',
    'If a feature, the measured extractiveness might be re-evaluated as a necessary cost of flexibility. If a bug, it reinforces the classification as an extractive tangled rope that imposes undue costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, conceptual, 'Whether the legal vacuum is a beneficial flexibility or a detrimental uncertainty.').

omega_variable(
    strategic_exploitation_vs_genuine_uncertainty,
    'To what extent is the ''extraction'' (transaction costs, risk premiums) driven by genuine, unavoidable legal uncertainty, versus strategic exploitation of that uncertainty by powerful actors?',
    'Detailed case studies of licensing disputes and corporate compliance strategies, analyzing the intent and outcomes of specific actions taken within the ambiguous legal space.',
    'If primarily strategic exploitation, the extractiveness is more clearly a rent-seeking behavior. If primarily genuine uncertainty, it points to a fundamental flaw in the legal framework itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_exploitation_vs_genuine_uncertainty, empirical, 'Distinguishing between inherent costs of ambiguity and costs imposed by strategic behavior.').

omega_variable(
    enforcement_capacity_threshold,
    'What specific threshold of ''enforcement capacity'' (e.g., legal budget, market share, community support) allows an interpretive community to effectively assert its reading of GPL copyleft scope in a given context?',
    'Empirical study of successful and unsuccessful enforcement actions and licensing negotiations across different ecosystems and actor types.',
    'Understanding this threshold would clarify the true power dynamics within the ''licensed plurality'' and refine the directionality of stakeholders, especially for those with ''constrained'' exit options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Quantifying the power required to assert an interpretation in the legal vacuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gpl__tr_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gpl__tr_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(gpl__be_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(gpl__be_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(gpl__be_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(gpl__su_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(gpl__su_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(gpl__su_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
