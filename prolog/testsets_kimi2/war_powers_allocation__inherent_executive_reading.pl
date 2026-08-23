% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Reading
 *   domain: constitutional law/separation of powers/war powers
 *
 * SUMMARY:
 *   This constraint instantiates the inherent_executive_reading of the
 *   war_powers_allocation kernel: the claim that Article II's
 *   Commander-in-Chief clause grants the President an inherent,
 *   self-executing constitutional authority to deploy military force in
 *   defense of national interests without prior congressional authorization.
 *   Under this reading, legislative authorization becomes a political
 *   courtesy rather than a constitutional prerequisite, and congressional
 *   checks are reduced to post-hoc appropriations or political censure. The
 *   executive branch is the structural beneficiary of this authority shift;
 *   the legislative branch's Article I war powers are the victim. The
 *   constraint is actively enforced through executive legal opinion (OLC),
 *   fait accompli military deployments, and the political difficulty of
 *   Congress reclaiming authority once forces are deployed.
 *
 * KEY AGENTS:
 *   - executive_branch_operators: Primary beneficiary and agenda-setter (institutional/arbitrage) â claims, deploys, and enforces unilateral authority
 *   - congressional_war_powers: Primary target/payer (institutional/constrained) â constitutional authority diluted to post-hoc ratification
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â documents divergence between text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.62).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Reading").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional law/separation of powers/war powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '57df2569-957e-4526-9f04-42226c7de03b').
narrative_ontology:cs_kernel_codification('57df2569-957e-4526-9f04-42226c7de03b', fixed_text).
narrative_ontology:cs_authority_grounding('57df2569-957e-4526-9f04-42226c7de03b', lineage).
narrative_ontology:cs_interpretation_layer_present('57df2569-957e-4526-9f04-42226c7de03b').
narrative_ontology:cs_reading_relation('57df2569-957e-4526-9f04-42226c7de03b', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('57df2569-957e-4526-9f04-42226c7de03b', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('57df2569-957e-4526-9f04-42226c7de03b', foundational, executive_inherent_force_authority).
narrative_ontology:cs_axiom_status(executive_inherent_force_authority, holdable).
narrative_ontology:cs_axiom_grounding('57df2569-957e-4526-9f04-42226c7de03b', executive_inherent_force_authority, conventional).
narrative_ontology:cs_axiom('57df2569-957e-4526-9f04-42226c7de03b', foundational, authorization_as_executive_courtesy).
narrative_ontology:cs_axiom_status(authorization_as_executive_courtesy, holdable).
narrative_ontology:cs_axiom_grounding('57df2569-957e-4526-9f04-42226c7de03b', authorization_as_executive_courtesy, conventional).
narrative_ontology:cs_reference_frame('57df2569-957e-4526-9f04-42226c7de03b', inherent_executive_prerogative).
narrative_ontology:cs_drift_state('57df2569-957e-4526-9f04-42226c7de03b', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('57df2569-957e-4526-9f04-42226c7de03b', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch_operators).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congressional_war_powers).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, presidential_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises a constitutional default authority to deploy military force without prior legislative authorization. Enforces the constraint through OLC legal opinions, fait accompli deployments, and signing statements. Can shift between statutory, inherent, and Article II frameworks as doctrinal or political needs require.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, executive_branch_operators, beneficiary).

% Holds the constitutional power to declare war and authorize force, but under this reading its role is reduced to post-hoc appropriations, political protest, or impeachment. Cannot easily reclaim the gatekeeping function once the executive has acted; defunding deployed forces is politically and operationally costly.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congressional_war_powers, payer,
    institutional, generational, constrained, national).

% Analyze the divergence between the constitutional text and executive practice; document the historical erosion of legislative constraint and the expansion of unilateral presidential war-making.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch_operators).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, rapid national command authority for military response to threats without the delays, leaks, and fragmentation of legislative deliberation or divided command.
% TRANSFER_FUNCTION: Transfers effective decision-making authority over the initiation of military hostilities from the legislative branch to the executive branch, replacing statutory authorization with post-hoc political and budgetary ratification.
% ABSENT_VOICES: Strict-constructionist legislators and originalist scholars who would insist on pre-deployment authorization; also the voices of service-members and civilians in theaters where force is initiated without legislative deliberation, who bear the direct cost of the accelerated decision path.
% DISAPPEARANCE_RATIONALE: If the executive's claimed inherent authority vanished overnight, the legislative branch would need to reconstitute its authorization machinery, the pace and secrecy of military initiation would slow, and the legal-bureaucratic apparatus of the OLC and national security state would reorganize around explicit statutory mandates and congressional deliberation.
% FOUNDING_PROBLEM: The need for swift, decisive, and potentially secret military action in an interconnected world where legislative deliberation may compromise operational security and strategic advantage.
% FOUNDING_PROBLEM_CORROBORATION: Executive-branch national security officials attest the problem is live, citing strategic surprise and speed. Congressional committees and external constitutional scholars attest the problem is overstated and the arrangement persists as executive aggrandizement; independent historical studies of prolonged conflicts initiated without authorization support the shifted-function reading.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the arrangement transfers the effective initiation authority from the legislative to the executive branch, replacing a constitutional gate with political discretion. Suppression (0.62) is moderate-high: the constraint suppresses legislative alternatives by rendering authorization a fait accompli, yet Congress retains the nuclear option of defunding and impeachment, which keeps suppression below the snare threshold. Theater_ratio (0.48) reflects substantial performative work by the OLC and executive rhetoric claiming constitutional necessity, but also real military operations. Accessibility_collapse (0.50) is moderate: the congressional-primacy alternative is legally live but operationally difficult to reinstate once abandoned. Resistance (0.72) is high because Congress, scholars, and occasionally courts contest the reading. The founding problem â rapid response to security threats â is contested: the executive claims it is live, while external observers note that most post-WWII unauthorized deployments were not driven by true operational surprise.
 *
 * PERSPECTIVAL GAP:
 *   The executive-branch seat experiences this constraint as a legitimate constitutional default that enables necessary action; the congressional seat experiences it as a structural displacement of its constitutional function. The engine computes this divergence from the same structural data: the executive has agenda-setting power and arbitrage-grade exit across legal theories, while Congress is institutionally locked into a constrained position where its formal powers are reduced to budgetary leverage after the fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive_branch_operators are declared beneficiaries with institutional power and arbitrage exit options across legal frameworks and jurisdictions, placing d near the beneficiary end (low effective extraction). Congressional_war_powers are declared victims with institutional power but constrained exit options â they cannot easily leave the constitutional framework, and their tools (appropriations, impeachment) are politically costly and reactive â placing d near the target end (high effective extraction). Constitutional_scholars sit at the analytical pole with no extraction. No overrides are needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The risk here is mislabeling a genuine coordination function (rapid, unified national command) as pure extraction, or mislabeling executive aggrandizement as necessary coordination. The R5 genealogy interview checks this: the founding problem is 'swift military response without legislative delay.' The status is contested because external observers (historians, non-executive legal scholars) attest that many unauthorized deployments were not time-sensitive. If the founding problem is dead but the arrangement persists, the mandatrophy path would flag a piton. The authored metrics (theater_ratio 0.48, extractiveness 0.68) suggest the coordination function has been substantially overlaid with extraction, supporting the tangled_rope claim rather than rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the inherent_executive_reading of the war_powers_allocation kernel. Would classification change if the constitutional text were read through the congressional_primacy or functional_accommodation framings?',
    'Comparative analysis of the sibling constraints in the same kernel family; the engine computes per-reading classifications independently.',
    'If the kernel is instead read as requiring congressional primacy, the victim and beneficiary sets invert and the constraint''s extractiveness profile collapses. The functional accommodation reading would split the constraint into context-dependent sub-constraints with divergent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer uncertainty: this is one reading of a contested kernel.').

omega_variable(
    appropriation_as_authorization,
    'Does the practice of post-hoc appropriations for ongoing military operations constitute genuine legislative authorization, or a ratification theater that preserves the appearance of checks while conceding operational control?',
    'Historical case study of appropriations votes following unauthorized deployments: measure the rate of explicit restrictive riders versus clean funding, and the rate at which such restrictions are waived or ignored.',
    'If appropriations are theater, the theater_ratio is higher than structural metrics suggest and the constraint leans toward snare; if they are genuine authorization, the extraction from Congress is lower and the constraint may read as rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_as_authorization, empirical, 'Whether post-hoc budget ratification is substantive or performative.').

omega_variable(
    operational_necessity_vs_aggrandizement,
    'Is the executive''s claimed inherent authority driven by genuine operational necessities of modern national security, or by institutional aggrandizement that concentrates war-making power?',
    'Comparative analysis of response latency in authorized versus unauthorized historical deployments, paired with executive-branch legal output volume (OLC opinions, signing statements) asserting unilateral authority.',
    'If driven by necessity, the coordination function is stronger and the constraint may reclassify toward rope; if driven by aggrandizement, extraction dominates and the constraint hardens into snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_vs_aggrandizement, empirical, 'Founding problem status ambiguity: necessity or aggrandizement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(war__tr_t15, war_powers_allocation__inherent_executive_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(war__tr_t45, war_powers_allocation__inherent_executive_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__inherent_executive_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(war__tr_t75, war_powers_allocation__inherent_executive_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(war__be_t15, war_powers_allocation__inherent_executive_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(war__be_t45, war_powers_allocation__inherent_executive_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__inherent_executive_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(war__be_t75, war_powers_allocation__inherent_executive_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(war__su_t15, war_powers_allocation__inherent_executive_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(war__su_t45, war_powers_allocation__inherent_executive_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__inherent_executive_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(war__su_t75, war_powers_allocation__inherent_executive_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel, decomposed from the colloquial label 'war powers' into structurally distinct claims per the epsilon-invariance principle. The inherent_executive_reading treats authorization as optional executive courtesy; the congressional_primacy_reading treats it as a constitutional requirement; the functional_accommodation_reading splits the difference by operational context. Each carries distinct epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
