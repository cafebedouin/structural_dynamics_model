% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract under State Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract between autonomous
 *   individuals, defined and regulated solely by state law, independent of
 *   religious or customary dictates. It is one reading of the broader
 *   'family_law_authority' kernel, emphasizing individual rights, gender
 *   equality, and secular governance. The metrics reflect a relatively
 *   low-extraction, low-suppression coordination mechanism, as the state's
 *   role is primarily administrative and rights-granting, rather than
 *   extractive. The claimed type is 'rope' because it genuinely solves a
 *   coordination problem for individuals and the state, with minimal coercive
 *   overhead, and participants are net beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.25).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.3).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract under State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '5689a1be-9f8c-4f46-9a19-84175a896158').
narrative_ontology:cs_kernel_codification('5689a1be-9f8c-4f46-9a19-84175a896158', formalized).
narrative_ontology:cs_authority_grounding('5689a1be-9f8c-4f46-9a19-84175a896158', lineage).
narrative_ontology:cs_interpretation_layer_present('5689a1be-9f8c-4f46-9a19-84175a896158').
narrative_ontology:cs_reading_relation('5689a1be-9f8c-4f46-9a19-84175a896158', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('5689a1be-9f8c-4f46-9a19-84175a896158', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5689a1be-9f8c-4f46-9a19-84175a896158', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5689a1be-9f8c-4f46-9a19-84175a896158', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('5689a1be-9f8c-4f46-9a19-84175a896158', foundational, state_as_sole_arbiter_of_legal_status).
narrative_ontology:cs_axiom_status(state_as_sole_arbiter_of_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('5689a1be-9f8c-4f46-9a19-84175a896158', state_as_sole_arbiter_of_legal_status, conventional).
narrative_ontology:cs_axiom('5689a1be-9f8c-4f46-9a19-84175a896158', foundational, gender_symmetric_rights_and_obligations).
narrative_ontology:cs_axiom_status(gender_symmetric_rights_and_obligations, holdable).
narrative_ontology:cs_axiom_grounding('5689a1be-9f8c-4f46-9a19-84175a896158', gender_symmetric_rights_and_obligations, deontological).
narrative_ontology:cs_reference_frame('5689a1be-9f8c-4f46-9a19-84175a896158', enlightenment_liberal_state).
narrative_ontology:cs_drift_state('5689a1be-9f8c-4f46-9a19-84175a896158', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5689a1be-9f8c-4f46-9a19-84175a896158', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, marrying_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, children_of_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, individual_autonomy).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, gender_equality).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, secular_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals entering into marriage gain legal recognition, rights, and responsibilities defined by the state, independent of religious affiliation. They benefit from a clear, enforceable framework for property, inheritance, and parental rights. Exit is constrained by legal processes and potential financial/custodial implications.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, marrying_individuals, beneficiary,
    moderate, biographical, constrained, national).

% The state defines, registers, and enforces the terms of marriage, ensuring uniformity and non-discrimination. It benefits from a clear, secular framework for family law, reducing reliance on diverse religious or customary laws. It administers the registration and dissolution processes.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Religious bodies may perform ceremonial marriages but these are not legally binding without state registration. They are excluded from defining the legal terms of marriage, which they may view as an erosion of their traditional authority. Their members are identity-locked to their religious traditions.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    organized, civilizational, identity_locked, global).

% Children benefit from the legal protections and clear parental responsibilities established by the civil contract, ensuring their welfare regardless of parents' religious beliefs. They are trapped within the legal framework established by their parents' marital status.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, children_of_marriage, beneficiary,
    powerless, generational, trapped, local).

% Analyze the evolution and application of secular marriage law, its impact on individual rights, and its interaction with religious and customary practices. They provide critical commentary and propose reforms.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, non-discriminatory legal framework for intimate partnerships, ensuring consistent rights and responsibilities for all citizens regardless of religious belief, and simplifying legal administration for the state.
% TRANSFER_FUNCTION: Transfers legal rights and obligations (e.g., property, inheritance, parental authority) between individuals upon marriage, and from religious institutions to the state as the primary arbiter of marital status.
% ABSENT_VOICES: Religious institutions and traditional communities, particularly those whose doctrines conflict with gender equality or secular authority, are excluded from defining the legal terms of marriage. They would argue for the primacy of religious or customary law.
% DISAPPEARANCE_RATIONALE: If state-recognized civil marriage vanished, the legal landscape for intimate partnerships would fragment, leading to chaos in property rights, inheritance, and child custody. Individuals would lack clear legal recourse, and the state would lose a fundamental tool for social organization and welfare.
% FOUNDING_PROBLEM: Historically, marriage was governed by diverse religious and customary laws, leading to legal inconsistencies, discrimination (especially against women and non-adherents), and conflicts of jurisdiction, hindering the state's ability to ensure uniform civil rights.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and human rights organizations corroborate that the problem of ensuring universal, non-discriminatory rights in intimate partnerships remains live, particularly in contexts where religious or customary laws still exert significant social pressure. State legal systems also attest to the ongoing need for a clear, secular framework.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the state primarily provides a service (legal recognition and framework) rather than extracting rents; any 'cost' is administrative overhead. Suppression is low (0.3) because the constraint is largely accepted as a foundational aspect of modern governance, though some resistance from religious groups persists. Theater ratio is very low (0.05) as the state's function is direct and functional, not performative. The historical trend shows a slight decrease in extractiveness and suppression as secular civil marriage became more established and less contested over the 20th century.
 *
 * PERSPECTIVAL GAP:
 *   While this reading frames marriage as a beneficial civil contract, other religious readings of the 'family_law_authority' kernel would view the state's exclusive authority as an illegitimate imposition or an 'extraction' of sacred authority. The engine's per-seat classification would reflect this divergence, with religious institutions likely computing as targets under their own readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Marrying individuals are primary beneficiaries, gaining legal protections and a clear framework for their relationship. The state legal system is also a beneficiary, as it gains a uniform, enforceable system for family law. Religious institutions are structurally excluded from legal authority over marriage, experiencing a form of 'suppression' of their traditional role, but are not 'victims' in the sense of having resources extracted from them by this specific constraint. Children are indirect beneficiaries of the stability and legal clarity provided.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing a universal, non-discriminatory legal framework) remains live and relevant, preventing mislabeling as a piton. Its coordination function is robust, and its benefits are widely distributed, distinguishing it from a snare. The low extractiveness and suppression, coupled with clear benefits, prevent it from being classified as a tangled rope from the perspective of its direct participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_vs_religious_legitimacy,
    'Is the state''s exclusive authority over marriage universally accepted as legitimate, or is it contested by significant religious or customary groups?',
    'Sociological surveys of public opinion on state vs. religious authority in family matters, analysis of legal challenges to secular marriage laws, and observation of parallel religious/customary marriage practices.',
    'If contested by a significant portion of the population, the ''suppression'' metric might be higher from the perspective of those groups, and the ''resistance'' metric might be understated, potentially shifting the classification towards a ''tangled_rope'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_religious_legitimacy, empirical, 'Ambiguity regarding the universal legitimacy of secular state authority over marriage.').

omega_variable(
    individual_autonomy_vs_communal_norms,
    'To what extent does the emphasis on individual autonomy in secular marriage law erode or conflict with legitimate communal or religious norms of family formation?',
    'Comparative legal analysis of jurisdictions with different balances between individual rights and communal norms, and ethnographic studies of communities navigating these tensions.',
    'If the individualistic framing is found to systematically undermine legitimate communal structures without providing adequate alternatives, the ''beneficiary'' status of ''marrying_individuals'' might be nuanced, and the ''excluded'' status of ''religious_institutions'' might carry a higher ''extraction'' component from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_autonomy_vs_communal_norms, conceptual, 'Tension between individual autonomy and communal/religious norms in marriage.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''family_law_authority'' kernel, how do the structural properties (extractiveness, suppression) of this ''secular_contractual_reading'' compare to its sibling religious readings?',
    'Generate full constraint stories for each sibling reading and compare their metric profiles and stakeholder classifications. The engine''s cross-reading comparison tools will quantify the divergence.',
    'Significant divergence would highlight the ''secular_contractual_reading'' as a distinct, and potentially less extractive, interpretation of family law authority compared to readings grounded in more hierarchical or prescriptive religious doctrines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Comparison of structural properties across different readings of the family_law_authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__secular_contractual_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__secular_contractual_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__secular_contractual_reading, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__secular_contractual_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__secular_contractual_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__secular_contractual_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__secular_contractual_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__secular_contractual_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__secular_contractual_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, attachment_coordination).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'family_law_authority' kernel. Each reading represents a distinct structural claim about marriage, with different beneficiaries, victims, and operational metrics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
