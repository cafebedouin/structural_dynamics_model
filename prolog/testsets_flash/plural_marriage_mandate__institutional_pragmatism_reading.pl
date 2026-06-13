% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: Plural Marriage Mandate (Institutional Pragmatism Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto, issued by the Church of Jesus Christ of Latter-day
 *   Saints, publicly announced the suspension of plural marriage. This
 *   reading interprets the Manifesto as a strategic institutional adaptation,
 *   where the church leadership, facing overwhelming federal coercion, used a
 *   narrative of divine revelation to legitimate a pragmatic capitulation.
 *   The constraint's function was to ensure the survival of the institution
 *   and restore its political rights, while extracting compliance from
 *   members and maintaining a degree of doctrinal continuity through
 *   performative adherence and, for a period, secret continuations. The
 *   period up to 1904 marks the gradual cessation of even secret plural
 *   marriages.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.7).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.8).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate (Institutional Pragmatism Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '5909bcb1-d442-4de7-b47c-53b278696afc').
narrative_ontology:cs_kernel_codification('5909bcb1-d442-4de7-b47c-53b278696afc', formalized).
narrative_ontology:cs_authority_grounding('5909bcb1-d442-4de7-b47c-53b278696afc', lineage).
narrative_ontology:cs_interpretation_layer_present('5909bcb1-d442-4de7-b47c-53b278696afc').
narrative_ontology:cs_reading_relation('5909bcb1-d442-4de7-b47c-53b278696afc', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('5909bcb1-d442-4de7-b47c-53b278696afc', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('5909bcb1-d442-4de7-b47c-53b278696afc', foundational, institutional_survival_is_paramount).
narrative_ontology:cs_axiom_status(institutional_survival_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('5909bcb1-d442-4de7-b47c-53b278696afc', institutional_survival_is_paramount, instrumental).
narrative_ontology:cs_axiom('5909bcb1-d442-4de7-b47c-53b278696afc', foundational, revelation_can_legitimate_pragmatism).
narrative_ontology:cs_axiom_status(revelation_can_legitimate_pragmatism, holdable).
narrative_ontology:cs_axiom_grounding('5909bcb1-d442-4de7-b47c-53b278696afc', revelation_can_legitimate_pragmatism, theological).
narrative_ontology:cs_reference_frame('5909bcb1-d442-4de7-b47c-53b278696afc', institutional_autonomy_and_doctrinal_purity).
narrative_ontology:cs_drift_state('5909bcb1-d442-4de7-b47c-53b278696afc', post_1890_manifesto, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5909bcb1-d442-4de7-b47c-53b278696afc', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto, publicly suspending plural marriage while privately allowing some continuations. Benefited from restored political rights and institutional survival, but faced internal dissent and external pressure. Their primary goal was to preserve the institution at all costs.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Were compelled by church authority to abandon existing plural marriages or face excommunication, often leading to family dissolution and social ostracization. Their identity was deeply tied to the practice, making exit unthinkable.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, identity_locked, local).

% Were encouraged to enter monogamous marriages after the Manifesto, often without knowledge of the continued secret practice of plural marriage by some leaders. Their trust was exploited, and their choices were made under false pretenses.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    powerless, biographical, identity_locked, local).

% Exerted immense legal and political pressure, including disincorporation of the church and confiscation of assets, to force the abandonment of plural marriage. Their objective was to enforce federal law and assimilate the territory into the Union.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% The abstract concept of the church's continued existence and ability to operate as a legal entity. This was the ultimate goal achieved by the Manifesto, allowing the institution to persist despite doctrinal compromise.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the church's public stance and actions to comply with federal anti-polygamy laws, allowing the institution to regain legal standing and political rights, and preventing its complete dissolution.
% TRANSFER_FUNCTION: Transferred the right to practice plural marriage from individual members to the discretion of church leadership, and transferred political autonomy from the church to the federal government in exchange for institutional survival.
% ABSENT_VOICES: Many members who continued to practice plural marriage in secret, or who felt betrayed by the public disavowal, were silenced by institutional pressure. Their dissent was managed internally to maintain a unified public front.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its subsequent enforcement vanished, the church's relationship with the federal government would be fundamentally altered, potentially leading to renewed conflict over religious freedom and social norms. The institutional structure and its historical narrative would be profoundly different.
% FOUNDING_PROBLEM: The church faced existential threat from the federal government due to its practice of plural marriage, including disincorporation, asset confiscation, and imprisonment of leaders, jeopardizing its ability to function as a religious and social institution.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, from outside the church's direct beneficiaries, corroborate that the federal government's coercive power was the primary driver for the Manifesto. The problem of federal persecution for plural marriage is largely resolved, though the legacy of the conflict continues to shape the church's identity.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the constraint forced members to abandon deeply held religious practices and family structures, while the church leadership gained institutional stability. Suppression is very high (0.8) due to both federal legal pressure and internal ecclesiastical authority, which enforced compliance through threats of excommunication. Theater ratio is high (0.6) because the public declaration of suspension was accompanied by a period of continued, albeit secret, plural marriages, indicating a performative aspect to the compliance. The M-set gap (doctrine vs. practice) is a key observable.
 *
 * PERSPECTIVAL GAP:
 *   Church leadership experienced this as a necessary, divinely guided adaptation for survival, a 'tangled rope' of coordination under duress. Coerced polygamists and deceived monogamists experienced it as a 'snare' of extraction and betrayal, where their deeply held beliefs and family structures were sacrificed for institutional pragmatism. The federal government saw it as a successful 'rope' of law enforcement and assimilation.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership (agenda_setter) is a primary beneficiary (d=0.1) due to institutional survival and restored political rights. Coerced polygamists and deceived monogamists (payers) are targets (d=0.9) due to the profound personal costs and identity-locked exit. The federal government (agenda_setter) is also a beneficiary (d=0.05) as its laws were enforced and its political objectives achieved. Institutional survival (non-agent beneficiary) is the abstract recipient of the gains.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a tangled_rope because it served a genuine coordination function (institutional survival) but did so through asymmetric extraction, leveraging doctrinal claims to legitimate a pragmatic shift. The 'mandatrophy resolved' flag is not set because the underlying tension between religious freedom and federal law, and the internal doctrinal implications, remain live issues, even if the specific practice of plural marriage has largely ceased.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_revelation_vs_pragmatism,
    'Was the 1890 Manifesto a genuine divine revelation, or a pragmatic institutional response framed as revelation?',
    'Analysis of internal church records, contemporary statements, and historical context to determine the primary drivers behind the decision, and comparison with other instances of ''revelation'' in response to external pressure.',
    'If primarily pragmatic, the constraint''s extractiveness and theater_ratio are higher, and its claimed_type shifts closer to a snare for members. If genuine revelation, the coordination function for members is stronger, and the extraction is framed as a divinely mandated sacrifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_revelation_vs_pragmatism, conceptual, 'Ambiguity of the Manifesto''s origin: divine command or institutional strategy.').

omega_variable(
    extent_of_secret_continuations,
    'What was the true extent and duration of secret plural marriages after the 1890 Manifesto, and how widely was this known within the church?',
    'Further historical research into private diaries, letters, and oral histories, cross-referenced with official records and disciplinary actions.',
    'Greater extent of secret continuations would increase the theater_ratio and the perceived deception of monogamists, pushing the constraint closer to a snare. Lesser extent would reduce the theatricality and strengthen the coordination aspect for public compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_secret_continuations, empirical, 'Uncertainty regarding the M-set gap (doctrine vs. practice) and its impact on internal perceptions.').

omega_variable(
    coercion_source_ambiguity,
    'Is the primary coercive force sustaining this constraint internal (church authority) or external (federal government)?',
    'Analysis of the relative impact of federal legal penalties versus ecclesiastical disciplinary actions on individual compliance over time.',
    'If primarily internal, the constraint''s suppression is more directly attributable to church leadership''s agency. If primarily external, the church leadership is also a constrained actor, and the constraint''s overall extractiveness is more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_source_ambiguity, empirical, 'Ambiguity regarding the source of coercive power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.55).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.65).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.68).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.75).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.78).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'plural_marriage_mandate' kernel. This 'institutional_pragmatism_reading' emphasizes the strategic adaptation of church leadership to external coercion, using doctrinal claims to legitimate survival-driven capitulation. It differs from the 'exogenous_override_reading' (which sees pure federal coercion) and the 'endogenous_reinterpretation_reading' (which sees legitimate prophetic reinterpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
