% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary â Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint instantiates the balanced contestation reading of the
 *   Israeli Basic Law interpretive boundary: both the Supreme Court and the
 *   Knesset hold legitimate but bounded authority, producing an institutional
 *   dialogue rather than a hierarchy. Courts interpret within their
 *   jurisdictional domain; the legislature retains ultimate sovereign power
 *   but is constrained by international obligations and norms of judicial
 *   independence. The constraint is actively enforced through triadic
 *   negotiation among the court, executive, and legislature, with
 *   extractiveness varying by policy domain.
 *
 * KEY AGENTS:
 *   - Judicial Branch: Primary agenda-setter (institutional/constrained) â interprets Basic Laws and negotiates boundary.
 *   - Knesset Majority: Primary payer (institutional/constrained) â bears sovereignty costs of judicial review and international constraints.
 *   - Executive Government: Secondary payer (powerful/constrained) â triadic negotiator whose autonomy is bounded.
 *   - Constitutional Rights Advocates: Primary beneficiary (organized/constrained) â protected by judicial independence.
 *   - Comparative Constitutional Scholars: Analytical observer (analytical/analytical) â external analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.6).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary â Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '305c5081-02db-471e-9436-2b676c788b8b').
narrative_ontology:cs_kernel_codification('305c5081-02db-471e-9436-2b676c788b8b', formalized).
narrative_ontology:cs_authority_grounding('305c5081-02db-471e-9436-2b676c788b8b', distributed).
narrative_ontology:cs_reading_relation('305c5081-02db-471e-9436-2b676c788b8b', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('305c5081-02db-471e-9436-2b676c788b8b', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('305c5081-02db-471e-9436-2b676c788b8b', foundational, reciprocal_institutional_bounding).
narrative_ontology:cs_axiom_status(reciprocal_institutional_bounding, holdable).
narrative_ontology:cs_axiom_grounding('305c5081-02db-471e-9436-2b676c788b8b', reciprocal_institutional_bounding, conventional).
narrative_ontology:cs_axiom('305c5081-02db-471e-9436-2b676c788b8b', foundational, dialogic_legitimacy_over_finality).
narrative_ontology:cs_axiom_status(dialogic_legitimacy_over_finality, holdable).
narrative_ontology:cs_axiom_grounding('305c5081-02db-471e-9436-2b676c788b8b', dialogic_legitimacy_over_finality, conventional).
narrative_ontology:cs_reference_frame('305c5081-02db-471e-9436-2b676c788b8b', bounded_coordinate_constitutionalism).
narrative_ontology:cs_drift_state('305c5081-02db-471e-9436-2b676c788b8b', post_2023_constitutional_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('305c5081-02db-471e-9436-2b676c788b8b', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_rights_advocates).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws within a bounded jurisdictional domain; claims authority to review legislation but acknowledges institutional limits; actively negotiates interpretive boundaries with the legislature and executive through case law and constitutional doctrine.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).

% Retains formal sovereignty and the power to enact or amend Basic Laws, yet bears the structural cost of international obligation constraints and judicial independence norms that limit ordinary majority will; constrained by the interpretive boundary when legislation is reviewed or invalidated.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority, beneficiary).

% Operates within a triadic negotiation structure where policy autonomy is bounded by both judicial review and legislative oversight; implementation authority is contingent on maintaining the inter-branch interpretive equilibrium.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_government, payer,
    powerful, biographical, constrained, national).

% Benefit from the institutionalized judicial independence and rights-protective interpretation that the bounded authority arrangement sustains; rely on the constraint to prevent legislative override of core constitutional protections.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Observe and analyze the institutional dialogue from an academic seat; document how the contested boundary compares to other democratic constitutional orders; neither administer the constraint nor bear its direct costs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constitutional collapse into either unchecked parliamentary sovereignty or unchecked judicial supremacy by establishing a domain-bounded dialogue between the legislature, executive, and judiciary.
% TRANSFER_FUNCTION: Moves interpretive authority and policy control between branches in a domain-contingent manner: judicial oversight transfers power from the legislative majority to the courts in rights-protected domains, while deference transfers autonomy to the political branches in security and budgetary domains.
% ABSENT_VOICES: Absolute parliamentary sovereigntists and judicial supremacists are structurally marginalized in this reading; populist majoritarian movements that reject international obligation constraints are largely absent from the balanced institutional dialogue.
% DISAPPEARANCE_RATIONALE: If the interpretive boundary vanished, the constitutional order would likely collapse toward either Knesset supremacy with weakened judicial review or judicial supremacy with routine legislative invalidation, eliminating the current triadic negotiation structure and reallocating authority decisively to one branch.
% FOUNDING_PROBLEM: How to secure judicial review of legislation under Basic Laws without subordinating democratic self-governance to an unelected judiciary, in a constitutional order lacking a single foundational constitutional moment.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside Israel attest to the general tension between parliamentary sovereignty and judicial review as a live problem in democratic theory; domestic civil rights organizations corroborate the need for judicial independence, while populist political parties and parliamentary sovereignty advocates dispute that the problem requires this specific balanced arrangement.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the boundary shifts power domain-by-domain: courts extract authority in rights cases, while the political branches extract deference in security and fiscal policy. Suppression (0.60) reflects the active exclusion of pure parliamentary sovereignty and pure judicial supremacy as live institutional alternatives. Theater ratio (0.35) captures the genuine but partially performative nature of constitutional dialogue rhetoric. Resistance (0.65) is high because both branches actively resist overreach by the other, which is constitutive of the constraint itself. The temporal series show extraction and enforcement requirements rising as political polarization strains the balanced framework.
 *
 * PERSPECTIVAL GAP:
 *   The judicial branch experiences the constraint as a protection of its interpretive domain and a source of institutional legitimacy; the Knesset majority experiences the same constraint as an external limit on democratic sovereignty. The engine computes this divergence from the structural data: the court is a declared beneficiary (low directionality) while the Knesset is a declared victim (high directionality), despite both being institutional actors with formally equivalent democratic pedigree. The difference is their structural relationship to THIS specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents whose position is strengthened by the boundary: the judicial branch gains a secured interpretive domain, and rights advocates gain a protected check on majoritarian power. Victim declarations map to agents who pay: the Knesset majority loses unbounded legislative sovereignty, and the executive loses unilateral policy autonomy. The comparative constitutional scholars sit at the analytical seat with arbitrage-grade exit (they can adopt different comparative frameworks), giving them near-beneficiary directionality despite being non-parties.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Rope would miss the asymmetric extraction: the Knesset does not merely coordinate with the Court, it bears a systematic cost in democratic sovereignty. Classifying it as a Snare would miss the genuine coordination function: the boundary prevents both judicial tyranny and majoritarian override of minority rights. Tangled Rope is the only category that admits both the coordination (dialogue) and extraction (sovereignty transfer) without collapsing one into the other. Mandatrophy is not yet resolved â the founding problem remains contested and the arrangement is under severe political strain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_dependent_epsilon,
    'Does the extractiveness of the interpretive boundary vary so substantially by policy domain that a single scalar epsilon fails to describe the constraint?',
    'Decompose the constraint into domain-specific sub-constraints (security, budget, rights, religion-state) and compute per-domain epsilon; compare variance across domains.',
    'High domain variance would require decomposition into a constraint family; low variance would validate the single-story abstraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_dependent_epsilon, conceptual, 'Whether single scalar extraction is valid across all policy domains').

omega_variable(
    enforcement_mechanism_nature,
    'Is the triadic negotiation over constraint enforcement a genuine coordination mechanism, or a performative cover for executive capture of both judicial and legislative agendas?',
    'Trace policy outcomes across multiple domains to determine whether the executive branch consistently benefits from the dialogue structure at the expense of both other branches.',
    'If executive capture, the coordination story is cover and the constraint reclassifies toward snare; if genuine triadic balance, tangled_rope remains accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Whether triadic negotiation is genuine coordination or executive capture').

omega_variable(
    committer_coexistence_stability,
    'Can the balanced contestation reading remain stable when sibling readings (judicial supremacy and parliamentary sovereignty) command major political backing simultaneously?',
    'Observe constitutional crises and institutional breakdowns: if the balanced reading collapses into one sibling during polarization, the coexistence relation is contingent rather than structural.',
    'Would determine whether the reading relation should be coexists_with or influences, and whether the constraint is transitioning toward a different kernel attractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_coexistence_stability, empirical, 'Stability of balanced reading under sibling political mobilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basi_tr_t7, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 7, 0.22).
narrative_ontology:measurement(basi_tr_t14, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(basi_tr_t21, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 21, 0.3).
narrative_ontology:measurement(basi_tr_t28, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement(basi_tr_t35, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t7, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 7, 0.35).
narrative_ontology:measurement(basi_be_t14, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 14, 0.4).
narrative_ontology:measurement(basi_be_t21, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 21, 0.46).
narrative_ontology:measurement(basi_be_t28, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(basi_be_t35, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(basi_su_t7, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 7, 0.45).
narrative_ontology:measurement(basi_su_t14, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(basi_su_t21, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(basi_su_t28, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(basi_su_t35, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the basic_law_interpretive_boundary kernel. The kernel conflates three structurally distinct claims about the locus of constitutional interpretive authority under Israel's Basic Laws. Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
