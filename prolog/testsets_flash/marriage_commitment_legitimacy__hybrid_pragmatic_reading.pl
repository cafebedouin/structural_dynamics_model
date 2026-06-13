% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Reading of Marriage Commitment Legitimacy
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the 'hybrid pragmatic' reading of a
 *   religious institution's Manifesto concerning marriage doctrine. This
 *   reading interprets the Manifesto as a strategic institutional adaptation,
 *   using prophetic authority to manage an exogenous crisis (federal legal
 *   pressure) while preserving core theological commitments through scope
 *   ambiguity. It is one of three contested readings of the
 *   'marriage_commitment_legitimacy' kernel, alongside
 *   'exogenous_override_reading' and 'endogenous_reinterpretation_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.55).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Hybrid Pragmatic Reading of Marriage Commitment Legitimacy").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '5c34c6b7-43e1-41ce-821e-5a2238f21bd4').
narrative_ontology:cs_kernel_codification('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', formalized).
narrative_ontology:cs_authority_grounding('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', lineage).
narrative_ontology:cs_interpretation_layer_present('5c34c6b7-43e1-41ce-821e-5a2238f21bd4').
narrative_ontology:cs_reading_relation('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', foundational, prophetic_adaptation_preserves_doctrine).
narrative_ontology:cs_axiom_status(prophetic_adaptation_preserves_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', prophetic_adaptation_preserves_doctrine, conventional).
narrative_ontology:cs_axiom('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', foundational, institutional_survival_is_theological_imperative).
narrative_ontology:cs_axiom_status(institutional_survival_is_theological_imperative, holdable).
narrative_ontology:cs_axiom_grounding('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', institutional_survival_is_theological_imperative, instrumental).
narrative_ontology:cs_reference_frame('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', doctrinal_continuity_through_prophetic_guidance).
narrative_ontology:cs_drift_state('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c34c6b7-43e1-41ce-821e-5a2238f21bd4', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, traditionalist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigates the tension between federal legal requirements and core theological commitments. Benefits from preserving institutional legitimacy and continuity by adapting doctrine while maintaining a narrative of divine guidance. Bears the burden of interpretive management.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Experience interpretive uncertainty and legitimacy ambiguity regarding the shift in marriage doctrine. They are expected to align with the new interpretation, bearing the cognitive and social costs of reconciling past teachings with current practice. Exit means abandoning their community and identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    moderate, biographical, identity_locked, local).

% Bear the direct cost of doctrinal change, feeling their core commitments are undermined. They may resist or seek alternative interpretations, but their options are constrained by institutional authority and social pressure. Their resistance is managed through narrative and selective enforcement.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, traditionalist_factions, payer,
    organized, generational, constrained, national).

% Benefits from institutional compliance with federal law, reducing legal challenges and social friction. Exerts pressure through legal frameworks, but does not directly administer the religious institution's internal doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Analyze the theological and historical implications of the Manifesto, examining its consistency with past doctrine and its impact on the institution's prophetic claims. Their analysis influences broader academic and public discourse.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional response to an exogenous legal crisis, allowing the institution to maintain its legal standing while preserving a narrative of theological consistency and prophetic authority for its members.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and institutional survival to the leadership, in exchange for rank-and-file members bearing the cognitive load of reconciling shifting doctrine and the suppression of alternative interpretations.
% ABSENT_VOICES: Hardline traditionalists who view any adaptation as apostasy are marginalized or excommunicated; their voices are suppressed to maintain institutional unity and the narrative of divinely guided adaptation.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished, the institution would face an immediate crisis of legitimacy, either by directly confronting federal law (exogenous_override_reading) or by admitting a fundamental doctrinal shift (endogenous_reinterpretation_reading). The current hybrid approach allows it to navigate this tension, and its absence would force a starker, more disruptive choice.
% FOUNDING_PROBLEM: The institution faced a direct conflict between its established marriage doctrine and evolving federal legal requirements, threatening its legal status and tax-exempt privileges.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership consistently frames the problem as ongoing, requiring continuous adaptation. External legal analysts and sociological observers corroborate the initial legal pressure and the institution's strategic response, though they may dispute the 'prophetic authority' aspect.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the institution's legal compliance and theological narrative (beneficiary: institutional_leadership) while simultaneously extracting interpretive flexibility and imposing cognitive costs on its members (victims: rank_and_file_members, traditionalist_factions). Active enforcement is required to manage internal dissent and maintain the official narrative. Extractiveness is moderate (0.55) as it involves a significant, though not total, shift in member understanding. Suppression is higher (0.65) due to the active management of dissent and the pressure on members to conform. Theater ratio is moderate (0.40) as the 'prophetic authority' framing serves to legitimize a pragmatic institutional adaptation.
 *
 * PERSPECTIVAL GAP:
 *   Institutional leadership experiences this as a necessary, divinely guided adaptation that preserves the institution. Rank-and-file members and traditionalist factions experience it as a coercive reinterpretation that undermines their prior commitments. The engine will compute these divergent classifications based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership benefits from the constraint's ability to navigate legal and theological challenges, maintaining its authority and continuity. Rank-and-file members and traditionalist factions bear the costs of interpretive uncertainty and suppressed dissent, making them targets of extraction. The federal government is an indirect beneficiary of the institution's compliance. Theological scholars are analytical observers, not directly subject to the constraint's extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving institutional integrity amidst external pressure) is still live, but its method (strategic adaptation via scope ambiguity) introduces extraction. The classification as Tangled Rope prevents mislabeling this as pure coordination (Rope) by acknowledging the asymmetric costs, or as pure extraction (Snare) by recognizing the genuine coordination function for the institution's survival. The 'contested' status of the founding problem reflects the ongoing tension between the original theological commitment and the adapted practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_ambiguity_resolution,
    'How much of the Manifesto''s ''prophetic authority'' is genuinely theological guidance, and how much is strategic ambiguity designed to manage internal and external pressures?',
    'Longitudinal study of internal institutional communications and member adherence over time, compared with external legal and political pressures. Analysis of internal theological debates and dissent.',
    'If the ambiguity is primarily strategic, the constraint''s theater_ratio would be higher, and its extractiveness from members (who bear the cognitive dissonance) would be more clearly defined as institutional rent-seeking. If genuinely theological, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_resolution, conceptual, 'Distinguishing genuine theological guidance from strategic ambiguity in the Manifesto''s interpretation.').

omega_variable(
    internalized_suppression_degree,
    'To what extent is the suppression of dissent among rank-and-file members internalized (due to identity-lock and social pressure) versus structurally enforced by institutional mechanisms?',
    'Qualitative sociological research among former and current members, analyzing post-exit trajectories of belief and practice. Comparison of dissent rates in contexts with varying institutional enforcement capacity.',
    'If suppression is largely internalized, the effective suppression for identity-locked members is higher than the structural measure suggests, making their exit options even more constrained and amplifying their effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_degree, empirical, 'Structural vs. internalized suppression mechanism for members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. This 'hybrid pragmatic' reading coexists with the 'exogenous override' and 'endogenous reinterpretation' readings, each representing a distinct structural interpretation of the Manifesto's impact on marriage doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
