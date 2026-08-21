% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strict neutrality' reading of
 *   constitutional secularism, where the state maintains an equal distance
 *   from all religions, refraining from preferential treatment or
 *   interference. It aims to coordinate a pluralistic society by establishing
 *   clear boundaries for state action, protecting individual religious
 *   freedom and minority rights. The constraint is claimed as a Rope,
 *   reflecting its coordination function, but its enforcement requires active
 *   suppression of state actors or majoritarian religious groups seeking to
 *   leverage state power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.25).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.4).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '905b89e1-15f8-4615-ba5a-b53f05deba3b').
narrative_ontology:cs_kernel_codification('905b89e1-15f8-4615-ba5a-b53f05deba3b', fixed_text).
narrative_ontology:cs_authority_grounding('905b89e1-15f8-4615-ba5a-b53f05deba3b', lineage).
narrative_ontology:cs_interpretation_layer_present('905b89e1-15f8-4615-ba5a-b53f05deba3b').
narrative_ontology:cs_reading_relation('905b89e1-15f8-4615-ba5a-b53f05deba3b', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('905b89e1-15f8-4615-ba5a-b53f05deba3b', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('905b89e1-15f8-4615-ba5a-b53f05deba3b', foundational, state_impartiality_principle).
narrative_ontology:cs_axiom_status(state_impartiality_principle, holdable).
narrative_ontology:cs_axiom_grounding('905b89e1-15f8-4615-ba5a-b53f05deba3b', state_impartiality_principle, deontological).
narrative_ontology:cs_axiom('905b89e1-15f8-4615-ba5a-b53f05deba3b', foundational, individual_religious_autonomy).
narrative_ontology:cs_axiom_status(individual_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('905b89e1-15f8-4615-ba5a-b53f05deba3b', individual_religious_autonomy, deontological).
narrative_ontology:cs_reference_frame('905b89e1-15f8-4615-ba5a-b53f05deba3b', state_non_endorsement_framework).
narrative_ontology:cs_drift_state('905b89e1-15f8-4615-ba5a-b53f05deba3b', contemporary_pluralistic_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('905b89e1-15f8-4615-ba5a-b53f05deba3b', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, individual_citizens).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, state_actors_seeking_religious_promotion).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_majorities_seeking_state_endorsement).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, separation_of_church_and_state).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, individual_religious_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for upholding the constitutional principle of strict neutrality, ensuring no preferential treatment or interference with any religion. They must actively resist pressures to promote or endorse specific religious views, which can be a political cost.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_actors, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from protection against state-sponsored discrimination or coercion by a religious majority. Their religious practices are safeguarded from state interference, allowing for autonomy and diversity within a pluralistic society.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    moderate, generational, constrained, national).

% Enjoy freedom of conscience and the right to practice (or not practice) religion without state endorsement or interference. They are not compelled to support or conform to any state-favored religion.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, individual_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of not being able to leverage state power or resources to promote their religious views or establish their religion as officially favored. Their attempts to influence state policy towards religious promotion are actively suppressed by the neutrality principle.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_majorities_seeking_state_endorsement, payer,
    organized, generational, constrained, national).

% Experience a constraint on their capacity to use state resources or platforms to promote specific religious beliefs or practices. This includes politicians, educators, or public officials who might wish to integrate religion into public life beyond neutral accommodation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_actors_seeking_religious_promotion, payer,
    powerful, biographical, constrained, national).

% Operate in a public sphere free from religious interference or preferential treatment, allowing for a focus on their civic or educational missions without sectarian influence. This includes public schools, government agencies, and non-religious civil society organizations.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_institutions, beneficiary,
    organized, generational, mobile, national).

% Advocate for state intervention in religious affairs to advance social reform or protect vulnerable groups within religious communities. From the strict neutrality perspective, their calls for intervention are seen as violating the principle of non-interference and are therefore excluded from legitimate state action.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, principled_intervention_advocates, excluded,
    organized, generational, constrained, national).

% Believe the state has an affirmative duty to eliminate oppressive religious practices, even if it means superseding religious autonomy. This reading of secularism is fundamentally at odds with strict neutrality's non-interference stance, leading to their exclusion from the policy discourse under this framework.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_advocates, excluded,
    organized, generational, constrained, national).

% Interpret and enforce the constitutional principle of secularism, adjudicating disputes related to state-religion relations. They act as a check on both state overreach and attempts to undermine neutrality, shaping the practical application of the constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a diverse, multi-religious society by establishing clear, impartial boundaries for state action regarding religion, preventing state favoritism or discrimination and fostering peaceful coexistence.
% TRANSFER_FUNCTION: Prevents the transfer of state legitimacy, resources, or coercive power to any specific religious group, ensuring that religious influence remains a matter of private conviction and voluntary association, not state endorsement.
% ABSENT_VOICES: Advocates for state-sponsored religious promotion or for state intervention in religious affairs (e.g., to reform religious practices) are structurally excluded from the framework of strict neutrality, as their positions inherently violate the principle of non-interference.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished, the state would likely become a battleground for religious groups seeking endorsement or control, leading to potential discrimination, conflict, and a loss of religious freedom for minorities. The entire structure of state-religion relations would reorganize, likely towards a more majoritarian or interventionist model.
% FOUNDING_PROBLEM: Historical religious conflicts, state-sponsored religious discrimination, and the suppression of individual religious freedom by established churches or state-favored religions.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, human rights organizations, and historical records attest to the ongoing need for state neutrality to prevent religious conflict and protect individual liberties. While the specific forms of religious conflict may evolve, the underlying problem of state-religion entanglement remains live, as evidenced by contemporary debates and legal challenges.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily limits state action rather than extracting from individuals, though it imposes a 'cost' on those who would seek state religious promotion. Suppression is moderate (0.40) as active enforcement is required to prevent state entanglement with religion and to resist pressures from religious majorities. Theater ratio is low (0.10) as the principle is fundamental and genuinely applied, not merely performative. Accessibility collapse is moderate-high (0.70) because alternatives like state-sponsored religion are largely foreclosed. Resistance is low-moderate (0.30) as the principle is generally accepted, though continually challenged by specific groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious minorities and individual citizens, this constraint is a protective Rope, ensuring their autonomy. From the perspective of state actors or religious majorities seeking to promote their faith through state channels, it is a restrictive force, limiting their capacity to act. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minorities and individual citizens are clear beneficiaries (low d) as the constraint protects their freedom and prevents state overreach. State actors seeking religious promotion and religious majorities seeking state endorsement are targets (high d) as the constraint actively suppresses their desired actions. Secular institutions also benefit from a neutral public sphere. Constitutional courts act as agenda-setters, interpreting and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_secular_bias,
    'Does strict neutrality, in practice, implicitly favor a secular worldview or non-religious forms of public expression, thereby disadvantaging religious forms?',
    'Empirical studies on the impact of neutrality policies on religious visibility and participation in the public sphere, compared to non-religious forms.',
    'If an implicit bias is demonstrated, the effective extractiveness from religious communities might be higher than measured, and the constraint''s classification might shift towards a Tangled Rope for religious actors, as it coordinates public order while extracting religious expression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_secular_bias, conceptual, 'Whether strict neutrality has an unacknowledged secularizing effect.').

omega_variable(
    neutrality_vs_internal_reform,
    'Is the state''s strict non-interference compatible with addressing internal inequalities or oppressive practices within religious communities, or does it inadvertently protect them?',
    'Comparative legal analysis of jurisdictions with different secularism models and their effectiveness in protecting vulnerable groups within religious communities.',
    'If strict neutrality is found to protect oppressive practices, its classification might shift towards a Snare for vulnerable individuals within those communities, as the coordination story (non-interference) would cover an extractive outcome (lack of protection). This is the core contest with the ''principled_intervention_reading'' and ''reformist_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_vs_internal_reform, conceptual, 'The tension between state neutrality and internal religious reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__strict_neutrality_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__strict_neutrality_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_secularism' kernel. Each reading represents a distinct structural claim about the state's relationship with religion, with differing ε values and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
