% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Ontology as Rhetorical Scaffold
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rhetorical scaffold' reading of
 *   the Deferential Realism ontology. In this reading, the typology
 *   (Mountain, Rope, Snare, etc.) functions primarily as a normative
 *   vocabulary for policy critique and advocacy. Its value lies in its
 *   persuasive power to frame social mechanisms as 'snares' or 'tangled
 *   ropes,' thereby mobilizing action. Classification is understood as an act
 *   of normative judgment and declaration, rather than empirical discovery.
 *   The framework acts as a temporary support (scaffold) for building
 *   critical arguments, intended to be superseded by policy change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Ontology as Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'ddf998aa-d9ed-4884-b2a1-99bd2677b8ef').
narrative_ontology:cs_kernel_codification('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', distributed).
narrative_ontology:cs_authority_grounding('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', distributed).
narrative_ontology:cs_reading_relation('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', foundational, classification_as_normative_declaration).
narrative_ontology:cs_axiom_status(classification_as_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', classification_as_normative_declaration, deontological).
narrative_ontology:cs_axiom('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', foundational, framework_value_in_persuasive_power).
narrative_ontology:cs_axiom_status(framework_value_in_persuasive_power, holdable).
narrative_ontology:cs_axiom_grounding('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', framework_value_in_persuasive_power, instrumental).
narrative_ontology:cs_reference_frame('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', critical_theory_advocacy_framework).
narrative_ontology:cs_drift_state('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ddf998aa-d9ed-4884-b2a1-99bd2677b8ef', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_critique_efficacy).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, social_construction_of_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize the typology to frame policy mechanisms as 'snares' or 'tangled ropes,' thereby mobilizing public opinion and political action against them. The framework provides a powerful, accessible vocabulary for critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    moderate, biographical, mobile, global).

% Adopt the typology as a tool for strategic communication and coalition building, using its classifications to highlight perceived injustices and rally support for alternative policies. The framework's persuasive power is its primary value.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Analyze the framework's utility as a normative language, its impact on policy discourse, and its philosophical underpinnings. They are interested in its performative effects rather than its descriptive accuracy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, academic_theorists, observer,
    analytical, generational, analytical, global).

% Are the targets of critique framed by the typology. They must respond to the rhetorical force of 'snare' classifications, even if they dispute the underlying normative judgments or empirical claims. The framework creates pressure for policy change.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers, payer,
    institutional, immediate, constrained, national).

% Believe the typology is an objective diagnostic tool. From their perspective, this rhetorical reading undermines the framework's scientific credibility by reducing classification to advocacy. They are excluded from this reading's core premise.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading_proponents, excluded,
    analytical, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates critical discourse by providing a shared, evocative vocabulary for identifying and challenging perceived extractive or unjust social mechanisms, enabling diverse groups to coalesce around common targets for critique.
% TRANSFER_FUNCTION: Transfers legitimacy and persuasive power to policy critiques by framing complex social arrangements in morally charged terms ('snare', 'tangled rope'), from the framework's users (critics, advocates) to their arguments, and from the targets of critique (policy makers, beneficiaries of existing arrangements) to the need for reform.
% ABSENT_VOICES: Proponents of an 'immutable diagnostic' reading are structurally excluded from this rhetorical framing; they would argue that reducing the typology to a persuasive tool undermines its capacity for objective analysis and risks misrepresenting empirical reality.
% DISAPPEARANCE_RATIONALE: If this rhetorical reading of the typology vanished, policy critique would lose a powerful, widely understood vocabulary for framing social mechanisms. Advocacy efforts would need to find new ways to articulate and coordinate opposition to perceived injustices, leading to a significant rearrangement of critical discourse strategies.
% FOUNDING_PROBLEM: The problem of effectively communicating and coordinating critique against complex, often opaque, social and institutional arrangements that benefit some at the expense of others, without resorting to overly academic or inaccessible language.
% FOUNDING_PROBLEM_CORROBORATION: Policy critics and advocacy groups attest that the problem of effective critique remains live, and the typology provides a vital tool. Academic theorists, from an observer seat, corroborate the framework's utility in shaping public discourse and policy debates, even if they debate its ontological status.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.2) and suppression (0.1) are low because this reading itself is a tool for critique, not a mechanism of extraction or suppression. It extracts 'persuasive power' for its users, but not material resources. Suppression is minimal because the reading thrives on open discourse and the contestation of alternative framings. The 'scaffold' classification reflects its intended transitional nature: it supports critique aimed at changing the underlying social mechanisms, after which its specific rhetorical function might diminish. The low theater ratio (0.05) indicates that its function is direct and instrumental to its users' goals.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between those who use the typology as a rhetorical tool (beneficiaries in this reading) and those who view it as an objective diagnostic instrument (excluded in this reading). The former see its value in its capacity to drive change, while the latter see its value in its descriptive accuracy. This reading explicitly prioritizes the former, leading to a low extractiveness and suppression score because its 'extraction' is persuasive power, not material resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics and advocacy groups are the primary beneficiaries, gaining a powerful rhetorical tool. Policy makers are the 'payers' in the sense that they bear the cost of responding to critiques framed by this vocabulary. Academic theorists are observers, analyzing its impact. Proponents of other readings are 'excluded' from this framing's core premise, as it fundamentally redefines the typology's purpose.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontology_purpose_ambiguity,
    'Is the primary purpose of the Deferential Realism ontology to provide an objective diagnostic framework, or a normative vocabulary for critique?',
    'Analysis of the framework''s actual use in policy debates and academic discourse: if its primary impact is to shift normative judgments and mobilize action, this reading is corroborated. If its primary impact is to enable precise, empirically verifiable classification, the ''immutable diagnostic'' reading is strengthened.',
    'If resolved towards the ''immutable diagnostic'' reading, the extractiveness and suppression of this ''rhetorical scaffold'' reading would be re-evaluated as potentially higher, as it would be seen as ''extracting'' intellectual capital from a diagnostic tool for advocacy purposes, and ''suppressing'' objective analysis. If resolved towards this reading, its current low extractiveness/suppression is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontology_purpose_ambiguity, conceptual, 'Ambiguity regarding the fundamental purpose of the DR ontology.').

omega_variable(
    classification_source_ambiguity,
    'Are classifications like ''snare'' discovered through empirical measurement, or declared through normative judgment?',
    'Examination of the methodology employed by users of the typology: if classifications are consistently derived from empirical data and objective criteria, the ''immutable diagnostic'' reading is supported. If they are primarily driven by ethical considerations and policy goals, this ''rhetorical scaffold'' reading is supported.',
    'If classifications are found to be primarily discovered, this reading''s premise of ''declaration'' is weakened, potentially leading to a re-evaluation of its claimed type and metrics. If they are primarily declared, this reading''s internal consistency is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_source_ambiguity, empirical, 'Ambiguity regarding the source of typology classifications (discovery vs. declaration).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
