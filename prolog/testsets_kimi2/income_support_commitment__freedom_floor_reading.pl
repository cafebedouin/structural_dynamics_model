% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel. It treats unconditional income support
 *   not as charity, economic stimulus, or social control, but as a structural
 *   precondition for individual autonomy and dignified refusal of
 *   exploitative arrangements. The arrangement coordinates collective
 *   provisioning of a material floor through the tax system, with
 *   universality eliminating the administrative extraction and stigma typical
 *   of means-tested alternatives. The claim is rope; the metrics are authored
 *   independently to describe a low-extraction coordination mechanism.
 *
 * KEY AGENTS:
 *   - state_fiscal_authority (agenda_setter/institutional): sets the floor level and manages the tax base
 *   - caregivers (beneficiary/moderate): gain independent income decoupled from employment or partner status
 *   - precarious_workers (beneficiary/powerless): gain exit capacity from exploitative labor
 *   - abuse_survivors (beneficiary/powerless): gain economic independence enabling household exit
 *   - artists_entrepreneurs (beneficiary/moderate): gain risk-buffering floor for speculative activity
 *   - means_test_bureaucracy (excluded/moderate): would defend conditional administration; structurally excluded by universality
 *   - low_wage_employers (excluded/organized): would resist worker exit capacity and reservation wage effects
 *   - policy_researchers (observer/analytical): assess macroeconomic and behavioral outcomes against alternative architectures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.22).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '199bdab8-1cd8-4ab0-846b-8bb384b54a74').
narrative_ontology:cs_kernel_codification('199bdab8-1cd8-4ab0-846b-8bb384b54a74', formalized).
narrative_ontology:cs_authority_grounding('199bdab8-1cd8-4ab0-846b-8bb384b54a74', lineage).
narrative_ontology:cs_interpretation_layer_present('199bdab8-1cd8-4ab0-846b-8bb384b54a74').
narrative_ontology:cs_reading_relation('199bdab8-1cd8-4ab0-846b-8bb384b54a74', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('199bdab8-1cd8-4ab0-846b-8bb384b54a74', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('199bdab8-1cd8-4ab0-846b-8bb384b54a74', foundational, universality_precedes_desert).
narrative_ontology:cs_axiom_status(universality_precedes_desert, holdable).
narrative_ontology:cs_axiom_grounding('199bdab8-1cd8-4ab0-846b-8bb384b54a74', universality_precedes_desert, deontological).
narrative_ontology:cs_axiom('199bdab8-1cd8-4ab0-846b-8bb384b54a74', foundational, exit_capacity_as_liberty).
narrative_ontology:cs_axiom_status(exit_capacity_as_liberty, holdable).
narrative_ontology:cs_axiom_grounding('199bdab8-1cd8-4ab0-846b-8bb384b54a74', exit_capacity_as_liberty, deontological).
narrative_ontology:cs_reference_frame('199bdab8-1cd8-4ab0-846b-8bb384b54a74', universal_material_security).
narrative_ontology:cs_drift_state('199bdab8-1cd8-4ab0-846b-8bb384b54a74', contemporary_austerity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('199bdab8-1cd8-4ab0-846b-8bb384b54a74', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the unconditional transfer level and funds it through the tax system. Administers the universal payment infrastructure without means-testing. Its legitimacy depends on maintaining tax compliance and macroeconomic stability.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, state_fiscal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receive the floor income regardless of formal labor market participation, enabling them to sustain caregiving work without means-test conditionality or partner income scrutiny. Their economic security is decoupled from employment status.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, mobile, national).

% Gain a guaranteed income floor that reduces survival pressure to accept any available job, increasing bargaining power and allowing refusal of exploitative terms. Exit from bad jobs becomes structurally viable.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, mobile, national).

% Gain independent income access not contingent on partner disclosure, employment status, or bureaucratic approval, enabling economic exit from coercive households.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, mobile, national).

% Receive a stabilizing floor that supports intermittent or speculative income streams, reducing risk of catastrophic failure during creative or entrepreneurial development periods.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Would defend conditional, targeted administration as necessary for fiscal discipline and behavioral steering. Made redundant by universality; their exclusion is structural to the unconditional design.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_test_bureaucracy, excluded,
    moderate, biographical, constrained, national).

% Would object to enhanced worker exit capacity because it tightens labor supply and raises reservation wages, compressing their access to cheap flexible labor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, low_wage_employers, excluded,
    organized, biographical, mobile, regional).

% Evaluate the macroeconomic and behavioral effects of the floor against alternative welfare architectures. They sit outside the benefit and cost flow and assess whether the coordination function is realized.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, unconditional income floor that eliminates the coordination failure of means-testing â administrative complexity, stigma, non-take-up, and poverty traps â and secures a baseline of material existence enabling labor market and household exit.
% TRANSFER_FUNCTION: Moves resources from the general tax base to every resident unconditionally, without behavior-contingent reciprocity or need demonstration.
% ABSENT_VOICES: Means-test administrators and low-wage employers are structurally excluded: the former because universality abolishes their institutional function, the latter because worker exit capacity is an intended feature they would resist.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished, caregivers and survivors would lose independent economic security, precarious workers would face coerced acceptance of exploitative terms, entrepreneurial risk-taking would contract, and the welfare state would revert to administratively costly targeting with associated stigma and non-take-up.
% FOUNDING_PROBLEM: Industrial welfare states created poverty traps, administrative exclusion, and forced dependency through means-tested, behavior-contingent income support.
% FOUNDING_PROBLEM_CORROBORATION: Social policy historians and feminist economists attest the founding problem from outside the immediate beneficiary set; libertarian and conservative critics attest the problem was exaggerated or that the cure is worse. The corroboration is cross-ideological but disputed.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because universality removes the conditionality and surveillance that convert income support into administrative extraction. The transfer is not behavior-contingent and carries no stigma penalty. Suppression is low (0.22) because the arrangement does not require suppressing alternatives â private employment, charity, and supplementary insurance persist. Theater is low (0.10): the floor is a direct cash transfer with minimal performative intermediation. Accessibility collapse is moderate (0.35) because once a universal floor is established, political alternatives (pure means-testing) lose constituency and become hard to reinstate, though they do not vanish. Resistance is moderate (0.40) because tax funding generates political opposition, though this is democratic contestation rather than violent suppression of the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (caregivers, precarious workers, survivors, entrepreneurs) experience the constraint as expanded choice and reduced survival anxiety â a subsidy on autonomy. The state fiscal authority experiences it as a macroeconomic coordination problem (funding, inflation anchoring, labor supply effects). The excluded seats (means-test bureaucracy, low-wage employers) experience the same arrangement as a loss of control or institutional displacement. The engine computes these divergences from the structural data: no victims are declared, so directionality for all named seats clusters near the beneficiary or symmetric end, while the excluded seats carry no directionality because they sit outside the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are explicitly declared and have mobile or constrained exit, which drives directionality toward the low-d beneficiary end. No victims are declared, so no seat is structurally targeted. The absence of a payer or victim array is structurally meaningful: it encodes the rope claim that the arrangement is not extractive at the level of individual agent relationships. If net taxpayers were structurally victimized, they would appear in the victims array with role payer; their absence asserts that the tax burden is either symmetric (all receive the floor) or a coordination cost rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mandatrophy errors. First, it prevents mislabeling the tax funding as extraction: in a rope, the cost is the coordination price of the floor, not a transfer from victims to captors. Second, it prevents mislabeling the arrangement as a piton: the founding problem (means-test poverty traps and survival coercion) remains live and contested, the floor is functionally operative, and theater is low. Were the floor to persist after full automation made wage labor obsolete, the classification might drift toward piton or scaffold; currently it is active coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_incidence_ambiguity,
    'Does the unconditional floor''s tax funding extract asymmetrically from any specific group, converting the rope into a disguised tangled rope?',
    'Fiscal incidence analysis tracing net benefits and burdens across income deciles.',
    'If an identifiable group is a trapped net payer with no benefit, the constraint develops victim-side directionality and reclassifies toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_incidence_ambiguity, empirical, 'Whether tax funding creates asymmetric extraction').

omega_variable(
    kernel_reading_contest,
    'Which reading of the income_support_commitment kernel â freedom floor, dependency trap, or targeting efficiency â best describes the empirical operation of unconditional income support?',
    'Comparative analysis of UBI pilot data on employment, autonomy metrics, and administrative cost; normative adjudication of the kernel''s legitimacy claims.',
    'If the dependency_trap_reading is more accurate, this constraint''s epsilon is misauthored and should be substantially higher with declared victims; if targeting_efficiency dominates, the universal design is a misallocation and the constraint reclassifies as scaffold or rope with narrower beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity across sibling readings of the income support kernel').

omega_variable(
    labor_market_exit_validity,
    'Does the income floor genuinely enable sustainable labor market exit and bargaining power, or merely subsidize a reserve army of low-wage labor?',
    'Longitudinal studies on reservation wages, job quality, and sectoral wage effects in jurisdictions with generous unconditional floors.',
    'If exit is illusory, the constraint''s coordination function (autonomy) is overstated and extraction (subsidy to employers paying below-subsistence wages) may be present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_exit_validity, empirical, 'Whether the floor produces autonomy or employer subsidy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_ff_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(income_support_ff_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(income_support_ff_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(income_support_ff_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(income_support_ff_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(income_support_ff_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(income_support_ff_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(income_support_ff_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(income_support_ff_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(income_support_ff_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(income_support_ff_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(income_support_ff_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three structurally distinct readings: freedom_floor_reading (rope, low extraction, universal beneficiaries), dependency_trap_reading (work-disincentive framing), and targeting_efficiency_reading (concentrated need framing). Each reading has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
