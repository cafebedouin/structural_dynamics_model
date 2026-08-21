% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint is the `judicial_harmonization_reading` of the
 *   `marriage_authority` kernel. It describes the process of Supreme Court
 *   review imposing a constitutional floor on personal law codes, leading to
 *   harmonization without formal Uniform Civil Code legislation. This
 *   mechanism acts as a 'scaffold' by providing transitional support for
 *   legal convergence. Sibling readings include `communal_autonomy_reading`,
 *   `secularist_reading`, `gender_rights_reading`, and
 *   `federalist_millet_reading`.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.7).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.65).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'f0d47c31-e759-4e36-8115-5f2b1b01cd45').
narrative_ontology:cs_kernel_codification('f0d47c31-e759-4e36-8115-5f2b1b01cd45', formalized).
narrative_ontology:cs_authority_grounding('f0d47c31-e759-4e36-8115-5f2b1b01cd45', lineage).
narrative_ontology:cs_interpretation_layer_present('f0d47c31-e759-4e36-8115-5f2b1b01cd45').
narrative_ontology:cs_reading_relation('f0d47c31-e759-4e36-8115-5f2b1b01cd45', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('f0d47c31-e759-4e36-8115-5f2b1b01cd45', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('f0d47c31-e759-4e36-8115-5f2b1b01cd45', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0d47c31-e759-4e36-8115-5f2b1b01cd45', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('f0d47c31-e759-4e36-8115-5f2b1b01cd45', foundational, constitutional_supremacy_over_personal_law).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('f0d47c31-e759-4e36-8115-5f2b1b01cd45', constitutional_supremacy_over_personal_law, conventional).
narrative_ontology:cs_axiom('f0d47c31-e759-4e36-8115-5f2b1b01cd45', foundational, judicial_role_as_rights_guarantor).
narrative_ontology:cs_axiom_status(judicial_role_as_rights_guarantor, holdable).
narrative_ontology:cs_axiom_grounding('f0d47c31-e759-4e36-8115-5f2b1b01cd45', judicial_role_as_rights_guarantor, deontological).
narrative_ontology:cs_reference_frame('f0d47c31-e759-4e36-8115-5f2b1b01cd45', constitutional_floor_as_minimum_standard).
narrative_ontology:cs_drift_state('f0d47c31-e759-4e36-8115-5f2b1b01cd45', contemporary_legal_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f0d47c31-e759-4e36-8115-5f2b1b01cd45', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_lawyers).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, secular_legal_framework).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, diverse_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, local_personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, federalist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor, expanding its authority by interpreting constitutional principles to establish a floor for marriage rights across diverse personal law codes. Benefits from increased judicial power and influence over social norms.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the cost of diminished autonomy in defining marriage according to their religious traditions. Their internal norms are increasingly subject to external constitutional review and override.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, diverse_religious_communities, payer,
    powerless, generational, constrained, local).

% Administrative bodies for various personal law codes, now operating under a constitutional floor imposed by judicial review. They face challenges in reconciling traditional practices with evolving constitutional mandates.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, local_personal_law_boards, payer,
    organized, biographical, constrained, regional).

% Benefit from the increased complexity and litigation surrounding constitutional interpretation of marriage law. Their expertise becomes more central to legal practice and reform efforts.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_lawyers, beneficiary,
    powerful, biographical, mobile, national).

% Advocate for decentralized authority and legal pluralism, viewing judicial harmonization as an overreach that erodes the autonomy of states and communities in family law matters.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, federalist_advocates, payer,
    organized, generational, constrained, national).

% Benefits from the progressive harmonization of marriage laws towards secular, rights-based standards, reinforcing the principle of constitutional supremacy over diverse personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, secular_legal_framework, beneficiary,
    institutional, civilizational, analytical, universal).

% Its role in defining marriage and enacting a Uniform Civil Code is largely bypassed by the judicial process. While theoretically capable of legislating, it often defers to or is constrained by judicial precedent.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, democratic_legislature, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform constitutional floor for marriage rights across diverse personal law codes, reducing legal fragmentation and ensuring fundamental rights are upheld nationwide.
% TRANSFER_FUNCTION: Transfers ultimate authority over marriage definition and reform from diverse local/communal legal systems to the centralized constitutional judiciary, which then imposes standards on lower courts and legislatures.
% ABSENT_VOICES: Democratic legislatures, whose role in defining marriage is largely bypassed by judicial action, and communities whose traditions are overridden without direct legislative input. They would argue for legislative supremacy or greater communal autonomy.
% DISAPPEARANCE_RATIONALE: If judicial harmonization vanished overnight, personal law codes would revert to their diverse, potentially unconstitutional, forms, leading to significant legal fragmentation, inequality, and social conflict regarding marriage rights. The legal landscape would reorganize around disparate local norms.
% FOUNDING_PROBLEM: Legal fragmentation and inequality in marriage rights across diverse personal law codes, leading to inconsistent application of fundamental rights and social injustices.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, human rights advocates, and affected individuals attest to the ongoing problem of legal inequality and the need for a harmonizing force. Legislative inaction on a Uniform Civil Code further corroborates the judiciary's perceived necessity in this role.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because it provides a transitional pathway for legal harmonization in the absence of legislative action, aiming for a more unified legal landscape. Its 'sunset clause' is implicit: the scaffold's function would ideally cease if a comprehensive Uniform Civil Code were enacted. Extractiveness is high (0.7) because the judiciary centralizes significant authority, overriding local legal traditions. Suppression is moderate-high (0.65) due to the active enforcement of judicial decrees against dissenting local practices. Theater ratio is low (0.2) as the judicial process is genuinely functional in achieving its stated goals of rights protection and legal consistency.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Supreme Court and constitutional lawyers, this mechanism is a necessary and legitimate exercise of judicial power to ensure fundamental rights. From the perspective of diverse religious communities and federalist advocates, it represents an overreach of judicial authority and an erosion of local autonomy and legal pluralism. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is a primary beneficiary, expanding its institutional power and interpretive role (low d). Constitutional lawyers also benefit from increased litigation and the centrality of constitutional interpretation. Diverse religious communities and local personal law boards are targets, experiencing a loss of autonomy and bearing the costs of adapting to new legal standards (high d). Federalist advocates also bear costs as their vision of decentralized authority is challenged. The democratic legislature is largely excluded, its potential role in defining marriage being bypassed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_permanence_ambiguity,
    'Is the judicial harmonization truly a temporary ''scaffold'' meant to prompt legislative action, or has it become a permanent mode of governance due to legislative inaction?',
    'Observation of legislative activity: if a Uniform Civil Code is enacted, the scaffold''s function would sunset. If legislative inaction persists over a prolonged period (e.g., 20+ years), the scaffold has become a de facto permanent structure.',
    'If permanent, the constraint would reclassify from Scaffold to Tangled Rope, as its transitional justification would have expired, leaving only the coordination-with-extraction function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_permanence_ambiguity, empirical, 'Whether the constraint''s implicit sunset clause will ever be triggered by legislative action.').

omega_variable(
    democratic_deficit_vs_rights_protection,
    'Does the judicial imposition of a constitutional floor, in the absence of legislative consensus, create an unacceptable democratic deficit, or is it a necessary safeguard for fundamental rights?',
    'Conceptual analysis of constitutional theory and comparative legal systems, alongside empirical studies of public opinion and legislative responsiveness.',
    'If a severe democratic deficit is established, the constraint''s legitimacy would be undermined, potentially increasing resistance and reclassifying it towards a Snare from the perspective of those who prioritize legislative sovereignty. If deemed a necessary safeguard, its Scaffold classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_deficit_vs_rights_protection, conceptual, 'The tension between judicial activism for rights and legislative democratic process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1980, marriage_authority__judicial_harmonization_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__judicial_harmonization_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority__judicial_harmonization_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__judicial_harmonization_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority__judicial_harmonization_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1980, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(marr_be_t2000, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(marr_be_t2020, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1980, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(marr_su_t2000, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(marr_su_t2020, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
