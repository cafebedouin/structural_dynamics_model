% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: State Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the principled_intervention_reading of the
 *   constitutional_secularism kernel. Under this reading, the state is
 *   constitutionally permitted to intervene in religious affairs to advance
 *   social reform and protect weaker sections within communities. The reading
 *   is contested against strict neutrality (non-interference) and reformist
 *   (mandatory elimination) siblings. The arrangement coordinates protection
 *   for marginalized individuals but simultaneously extracts autonomy from
 *   religious institutions, carrying a documented risk of majoritarian
 *   capture where intervention is applied selectively. The authored metrics
 *   and claimed type are independent: the claim is tangled_rope because the
 *   structure contains both a genuine coordination function and asymmetric
 *   extraction requiring active enforcement.
 *
 * KEY AGENTS:
 *   - state_secular_authority (institutional/analytical): Agenda-setter and secondary beneficiary â expands regulatory authority over religious domains.
 *   - marginalized_community_members (powerless/constrained): Primary beneficiary â receives state-backed rights enforcement within communities.
 *   - religious_community_institutions (organized/constrained): Primary payer â bears loss of autonomous governance over religious affairs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.7).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "State Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '37d14e90-a23f-4956-9397-758c71222f75').
narrative_ontology:cs_kernel_codification('37d14e90-a23f-4956-9397-758c71222f75', formalized).
narrative_ontology:cs_authority_grounding('37d14e90-a23f-4956-9397-758c71222f75', lineage).
narrative_ontology:cs_interpretation_layer_present('37d14e90-a23f-4956-9397-758c71222f75').
narrative_ontology:cs_reading_relation('37d14e90-a23f-4956-9397-758c71222f75', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('37d14e90-a23f-4956-9397-758c71222f75', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('37d14e90-a23f-4956-9397-758c71222f75', foundational, social_reform_legitimizes_differential_intervention).
narrative_ontology:cs_axiom_status(social_reform_legitimizes_differential_intervention, holdable).
narrative_ontology:cs_axiom_grounding('37d14e90-a23f-4956-9397-758c71222f75', social_reform_legitimizes_differential_intervention, conventional).
narrative_ontology:cs_axiom('37d14e90-a23f-4956-9397-758c71222f75', foundational, weaker_sections_entitled_to_communal_protection).
narrative_ontology:cs_axiom_status(weaker_sections_entitled_to_communal_protection, holdable).
narrative_ontology:cs_axiom_grounding('37d14e90-a23f-4956-9397-758c71222f75', weaker_sections_entitled_to_communal_protection, deontological).
narrative_ontology:cs_reference_frame('37d14e90-a23f-4956-9397-758c71222f75', pluralist_secularism_tempered_by_reform).
narrative_ontology:cs_drift_state('37d14e90-a23f-4956-9397-758c71222f75', contemporary_majoritarian_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37d14e90-a23f-4956-9397-758c71222f75', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_community_members).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, state_secular_authority).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_community_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional and statutory framework permitting intervention into religious practices for social reform. Expands regulatory oversight into family law, worship administration, and educational institutions run by religious communities. Derives increased authority and jurisdictional reach from the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_secular_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, state_secular_authority, beneficiary).

% Receive protective legislation and judicial remedies intended to secure rights within their religious communities. Depend on state institutions to enforce these protections because internal community mechanisms are inaccessible or hostile. Leaving the community is costly, and leaving the state offers no alternative protection.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_community_members, beneficiary,
    powerless, biographical, constrained, local).

% Administer religious affairs, worship, education, and personal law. Subject to state-mandated reforms that override internal norms and leadership authority. Can resist through litigation or limited noncompliance but remain bound by constitutional and statutory enforcement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_community_institutions, payer,
    organized, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the protection of marginalized individuals within religious communities by providing an external enforcement mechanism when internal governance structures fail to guarantee equal rights or remedies.
% TRANSFER_FUNCTION: Moves regulatory authority over religious affairs from community institutions to the state, and transfers protective benefits and legal standing to marginalized sections within those communities.
% ABSENT_VOICES: Religious conservatives who view any state intervention as community destruction; strict secularists who oppose state engagement with religion entirely; and members of marginalized groups who prefer community-internal reform over state imposition. They are excluded from reform commissions and constitutional benches where the scope of intervention is interpreted.
% DISAPPEARANCE_RATIONALE: If the intervention framework vanished, community autonomy would reassert in family law, worship practices, and educational institutions; marginalized individuals would lose external enforcement of intra-community rights; and the state's authority to regulate religious domains would contract, forcing a wholesale reorganization of constitutional secularism.
% FOUNDING_PROBLEM: Internal discrimination within religious communities against weaker sectionsâsuch as caste or gender-based exclusionâlacking effective internal remedy, coupled with post-colonial constitutional commitment to social reform.
% FOUNDING_PROBLEM_CORROBORATION: Social reform movements and marginalized-caste organizations attest the problem remains live from outside the state apparatus. Religious conservative organizations contest the problem's framing and severity. Independent human rights commissions provide mixed corroboration, documenting both ongoing discrimination and state-overreach harms.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the arrangement systematically transfers authority from religious institutions to the state, and differential treatment is built into the intervention logic. Suppression (0.70) reflects active legal and administrative enforcement needed to override community autonomy claims. Theater (0.42) indicates a moderate performative component: reform rhetoric sometimes masks majoritarian homogenization or state capacity building. Accessibility_collapse (0.60) captures the partial closure of strict-neutrality alternatives once intervention is constitutionalized. Resistance (0.65) measures sustained litigation and political pushback from affected communities. Temporal measurements track rising extraction and theater from the constitutional founding (T0) through contemporary majoritarian politics (T70) on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the constraint is experienced as legitimate coordination â solving a governance failure that communities cannot solve internally. From the religious institution seat, the same constraint is experienced as top-down extraction of authority, with reform objectives serving as the legitimizing frame. The engine computes this divergence from the structural role and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_secular_authority sits near the beneficiary end (low d): it sets the rules and captures expanded authority. Marginalized_community_members also sit on the beneficiary side, though their constrained exit and dependency on state protection prevent a fully subsidized position. Religious_community_institutions sit near the target end (high d): they bear the direct cost of lost autonomy and are the object of enforcement. The beneficiary-victim asymmetry is what drives the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination-function requirement, the framework would mislabel this as a snare (pure extraction) because the state clearly gains authority and religious communities lose it. However, the protection of marginalized individuals is a genuine coordination problem: internal community governance often fails to guarantee equal rights, and an external enforcement mechanism produces real benefits for a vulnerable population. The Tangled Rope classification captures that both stories are structurally true â the constraint coordinates and extracts through the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_risk,
    'Does the intervention framework protect weaker sections uniformly, or has it been structurally captured by majority religious interests to homogenize minority practices?',
    'Comparative case analysis of intervention outcomes across majority and minority religious communities; detection of asymmetrical application rates and differential enforcement intensity.',
    'If captured, the beneficiary/victim structure inverts for minority communities, raising extractiveness and shifting the computed seat type for those communities toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Whether differential treatment advances reform or majority dominance').

omega_variable(
    cs_framing_alternative,
    'Is the authority of this constraint better framed as lineage (constitutional text and interpretive tradition) or extraction (state expanding power through control of religious affairs)?',
    'Historical analysis of whether the interpretive layer absorbs drift toward state power expansion or genuinely returns to constitutional text and founding intent.',
    'If extraction framing is adopted, the state seat''s directionality shifts toward beneficiary-capture, increasing coupling and potentially triggering false-summit evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative commitment-system framing of authority grounding').

omega_variable(
    beneficiary_autonomy_paradox,
    'Do marginalized beneficiaries experience state intervention as empowerment or as substituted dependency, and does this alter their effective exit options?',
    'Ethnographic and legal-capacity study of beneficiary communities post-intervention, measuring actual autonomy versus dependency on state institutions.',
    'If dependency is created, the beneficiary seat''s directionality rises toward symmetric or target, reducing the net coordination benefit and tightening the rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_autonomy_paradox, empirical, 'Whether protection creates dependency that alters beneficiary exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cons_tr_t45, constitutional_secularism__principled_intervention_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__principled_intervention_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__principled_intervention_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cons_be_t45, constitutional_secularism__principled_intervention_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__principled_intervention_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__principled_intervention_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cons_su_t45, constitutional_secularism__principled_intervention_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__principled_intervention_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__principled_intervention_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_secularism kernel, decomposed from the colloquial label 'secularism' into three structurally distinct claims: strict neutrality (non-interference), principled intervention (permissive reform-oriented intervention), and reformist (mandatory elimination of oppression). Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
