% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'expansive universalist' reading of
 *   constitutional equality clauses, asserting that equality is a
 *   self-evident, universal truth applicable to all humans, and that
 *   historical exclusions are hypocritical deviations to be corrected, not
 *   binding precedent. This reading advocates for a low legitimacy threshold
 *   for rights expansion via judicial interpretation. While claimed as a
 *   'mountain' (a self-evident truth), its application requires active
 *   enforcement against entrenched historical discrimination, leading to high
 *   measured extractiveness and suppression from those who benefit from such
 *   exclusions. The divergence between the 'mountain' claim and the active
 *   enforcement metrics is central to its classification.
 *
 * KEY AGENTS:
 *   - all_humans: Primary beneficiary (powerless/identity_locked)
 *   - civil_rights_advocates: Agenda setter/beneficiary (organized/constrained)
 *   - judiciary: Agenda setter (institutional/constrained)
 *   - proponents_of_historical_exclusions: Primary target (powerful/constrained)
 *   - discriminatory_institutions: Target (institutional/constrained)
 *   - restrictive_originalists: Excluded (analytical/analytical)
 *   - progressive_textualists: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.85).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.9).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.85).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).
domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'c146610d-e3eb-49ed-9d57-c44dabde8dbf').
narrative_ontology:cs_kernel_codification('c146610d-e3eb-49ed-9d57-c44dabde8dbf', fixed_text).
narrative_ontology:cs_authority_grounding('c146610d-e3eb-49ed-9d57-c44dabde8dbf', lineage).
narrative_ontology:cs_interpretation_layer_present('c146610d-e3eb-49ed-9d57-c44dabde8dbf').
narrative_ontology:cs_reading_relation('c146610d-e3eb-49ed-9d57-c44dabde8dbf', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('c146610d-e3eb-49ed-9d57-c44dabde8dbf', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('c146610d-e3eb-49ed-9d57-c44dabde8dbf', foundational, human_dignity_is_intrinsic).
narrative_ontology:cs_axiom_status(human_dignity_is_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('c146610d-e3eb-49ed-9d57-c44dabde8dbf', human_dignity_is_intrinsic, deontological).
narrative_ontology:cs_axiom('c146610d-e3eb-49ed-9d57-c44dabde8dbf', foundational, equality_is_universal_and_inalienable).
narrative_ontology:cs_axiom_status(equality_is_universal_and_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('c146610d-e3eb-49ed-9d57-c44dabde8dbf', equality_is_universal_and_inalienable, deontological).
narrative_ontology:cs_reference_frame('c146610d-e3eb-49ed-9d57-c44dabde8dbf', enlightenment_universalism).
narrative_ontology:cs_drift_state('c146610d-e3eb-49ed-9d57-c44dabde8dbf', contemporary_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c146610d-e3eb-49ed-9d57-c44dabde8dbf', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_humans).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, proponents_of_historical_exclusions).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, discriminatory_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiaries of a universalist application of equality, gaining inherent dignity and rights regardless of historical or social status. Their 'exit' from this identity is not possible.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_humans, beneficiary,
    powerless, civilizational, identity_locked, universal).

% Actively champion and litigate for the expansive application of equality, pushing against historical exclusions. They invest careers and movements in this interpretation, making exit from advocacy costly.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocates, agenda_setter,
    organized, generational, constrained, national).

% Interprets and applies constitutional equality clauses, often expanding their scope through precedent. While bound by legal tradition, individual judges can influence the pace and direction of this expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of this expansive reading, as it dismantles systems and privileges from which they historically benefited. Their 'exit' involves abandoning deeply held beliefs and social structures.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, proponents_of_historical_exclusions, payer,
    powerful, generational, constrained, national).

% Forced to dismantle discriminatory practices and policies, incurring costs of compliance, legal challenges, and cultural shifts. Their continued existence depends on adapting to the universalist interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, discriminatory_institutions, payer,
    institutional, biographical, constrained, local).

% Hold a competing interpretation of equality, limiting its scope to historical understandings. They are structurally excluded from the premise of universal, evolving application that this reading asserts, though they remain active in legal and political discourse.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalists, excluded,
    analytical, generational, analytical, national).

% Agree with the expansive outcome but prefer a different mechanism (e.g., legislative amendment rather than judicial reinterpretation). They observe and critique the methods of this reading while often supporting its goals.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, progressive_textualists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral and legal baseline for human treatment, coordinating social and legal norms towards non-discrimination and equal protection under the law for all individuals.
% TRANSFER_FUNCTION: Transfers moral and legal standing, as well as access to rights and resources, from historically privileged groups to historically excluded groups. It also transfers the burden of enforcement and adaptation to legal and social systems.
% ABSENT_VOICES: Those who would argue for inherent, immutable hierarchies based on race, gender, religion, or other arbitrary characteristics are structurally excluded from the foundational premise of universal equality. Their arguments are treated as antithetical to the 'self-evident' truth.
% DISAPPEARANCE_RATIONALE: If the principle of universal equality vanished overnight, legal systems would revert to historical hierarchies, social norms would fragment, and civil rights protections would collapse. This would lead to widespread social and political upheaval, re-entrenching discrimination and denying fundamental human dignity.
% FOUNDING_PROBLEM: Historical and ongoing discrimination, oppression, and denial of fundamental rights based on arbitrary characteristics such as race, gender, religion, and origin.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, civil rights movements, historical scholarship, and ongoing social justice advocacy corroborate the persistence of discrimination and the continuous need for this principle to challenge and correct it. Legislative hearing testimony and independent sociological studies also support the live status of the problem.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant dismantling of historical privileges and discriminatory structures required by this reading. Suppression (0.90) is high because the universalist application actively and coercively suppresses alternative, exclusionary interpretations and practices. The low theater ratio (0.10) indicates that the enforcement is genuinely aimed at achieving equality, not merely performing it. Accessibility collapse (0.92) is high for those who wish to maintain exclusionary practices, as the legal and social landscape increasingly forecloses such options. Resistance (0.75) is substantial, reflecting ongoing political and social pushback from those who oppose the expansion of equality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_humans' and 'civil_rights_advocates', this constraint is a fundamental, liberating principle. From the perspective of 'proponents_of_historical_exclusions' and 'discriminatory_institutions', it is a coercive force dismantling their established order. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a 'rope' or 'mountain' and targets experiencing it as a 'snare' or 'tangled_rope', despite the overarching 'mountain' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   'All_humans' are full beneficiaries (d near 0.0) as the constraint subsidizes their rights and dignity. 'Civil_rights_advocates' are also beneficiaries, actively working to implement this reading. 'Proponents_of_historical_exclusions' and 'discriminatory_institutions' are full targets (d near 1.0) as the constraint directly extracts privileges and resources from them and suppresses their practices. The 'judiciary' acts as an agenda-setter, balancing its institutional role with the pressure to expand equality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as a pure 'snare' by acknowledging its 'mountain' claim and genuine coordination function (establishing universal human dignity). However, the high extractiveness and suppression metrics, coupled with active enforcement, prevent it from being certified as a benign 'rope' or genuine 'mountain' without further scrutiny. The 'false summit mountain' signature will likely trigger due to the presence of beneficiaries on a claimed mountain, prompting deeper analysis of its constructed vs. natural aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''self-evident universal truth'' of equality a genuine natural law (a Mountain) or a powerful, actively enforced social construct (a Snare or Tangled Rope)?',
    'Philosophical analysis of foundational ethics and empirical observation of its historical contingency and enforcement mechanisms. If its persistence relies entirely on active human enforcement and suppression of alternatives, it leans towards a construct.',
    'If resolved as a pure construct, the ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'', reflecting its coercive enforcement and extraction from those who benefit from historical exclusions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between inherent truth and enforced social norm.').

omega_variable(
    scope_of_universalism_in_practice,
    'To what extent does the ''universal'' application of equality truly extend to all humans in practice, given ongoing disparities and new forms of exclusion?',
    'Empirical sociological and legal studies tracking the actual lived experiences of various marginalized groups, and the emergence of new forms of discrimination not yet addressed by existing interpretations.',
    'If the practical scope is significantly narrower than the claimed universalism, the effective extractiveness from those still excluded would be lower (as the constraint doesn''t reach them), but the theater_ratio might rise (as the universal claim becomes more performative than real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_universalism_in_practice, empirical, 'Gap between theoretical universalism and practical application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1950, equality_clause_scope__expansive_universalist, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__expansive_universalist, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(equa_tr_t1980, equality_clause_scope__expansive_universalist, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(equa_tr_t1995, equality_clause_scope__expansive_universalist, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equality_clause_scope__expansive_universalist, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__expansive_universalist, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1950, equality_clause_scope__expansive_universalist, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__expansive_universalist, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(equa_be_t1980, equality_clause_scope__expansive_universalist, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(equa_be_t1995, equality_clause_scope__expansive_universalist, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(equa_be_t2010, equality_clause_scope__expansive_universalist, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__expansive_universalist, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1950, equality_clause_scope__expansive_universalist, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__expansive_universalist, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(equa_su_t1980, equality_clause_scope__expansive_universalist, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(equa_su_t1995, equality_clause_scope__expansive_universalist, suppression_requirement, 1995, 0.88).
narrative_ontology:measurement(equa_su_t2010, equality_clause_scope__expansive_universalist, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__expansive_universalist, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
