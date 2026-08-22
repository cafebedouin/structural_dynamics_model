% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Basic Law Interpretive Boundary — Balanced Contestation Reading
 *   domain: Constitutional Law / Comparative Constitutionalism / Judicial Review Theory
 *
 * SUMMARY:
 *   This story instantiates the balanced-contestation reading of the Basic
 *   Law interpretive boundary: neither the courts nor the legislature holds
 *   final, uncontested authority over constitutional meaning. Courts
 *   interpret within a jurisdictional domain they themselves must continually
 *   redefine; the legislature retains formal sovereign power but operates
 *   under the practical shadow of judicial review and international
 *   obligation. The result is not settled hierarchy but an ongoing triadic
 *   negotiation among court, executive, and legislature, with the effective
 *   extraction (ε) varying by policy domain depending on which institution
 *   currently commands more confidence in a given area. This is a distinct
 *   constraint from the judicial_supremacy_reading (where court invalidation
 *   is treated as binding and final) and the
 *   parliamentary_sovereignty_reading (where Knesset majority action is
 *   treated as dispositive even over judicial objection) — those are separate
 *   stories with their own ε values, not alternative measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: institutional agenda-setter interpreting within a self-bounded domain
 *   - sitting_legislative_coalition: institutional agenda-setter holding formal sovereignty but politically constrained
 *   - opposition_lawmakers: organized payer relying on judicial review as their only check
 *   - minority_rights_claimants: powerless payer whose protection depends on an unstable boundary
 *   - lower_court_litigants: moderate-power payer facing doctrinal inconsistency
 *   - international_treaty_partners: institutional beneficiary of the constrained-sovereignty norm
 *   - executive_branch: institutional observer/beneficiary exploiting the ambiguity for administrative discretion
 *   - constitutional_law_scholars: analytical observer of the triadic dialogue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary — Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "Constitutional Law / Comparative Constitutionalism / Judicial Review Theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4').
narrative_ontology:cs_kernel_codification('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', distributed).
narrative_ontology:cs_authority_grounding('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', distributed).
narrative_ontology:cs_reading_relation('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', foundational, authority_is_bounded_and_mutually_constraining).
narrative_ontology:cs_axiom_status(authority_is_bounded_and_mutually_constraining, holdable).
narrative_ontology:cs_axiom_grounding('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', authority_is_bounded_and_mutually_constraining, conventional).
narrative_ontology:cs_axiom('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', foundational, sovereignty_is_qualified_by_international_obligation_and_judicial_independence_norms).
narrative_ontology:cs_axiom_status(sovereignty_is_qualified_by_international_obligation_and_judicial_independence_norms, holdable).
narrative_ontology:cs_axiom_grounding('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', sovereignty_is_qualified_by_international_obligation_and_judicial_independence_norms, conventional).
narrative_ontology:cs_reference_frame('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', dual_legitimacy_institutional_equilibrium).
narrative_ontology:cs_drift_state('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', contemporary_judicial_reform_contest, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e5d6ce1-80d6-4aa9-b5b8-5f2182805ad4', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, sitting_legislative_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, opposition_lawmakers).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, lower_court_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Basic Laws within a jurisdictional domain they themselves must continually re-bound, since no single settled text fixes the limit of judicial review. They gain institutional standing and interpretive authority from an unresolved boundary, but cannot force compliance without political cooperation and face recurring override threats from the legislature.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices, beneficiary).

% Holds formal sovereign power to legislate and amend Basic Laws by majority vote, but operates under the shadow of judicial review and international obligations that raise the political cost of some enactments. Benefits from the ambiguity when it wants deniability for contested legislation, and is constrained by it when it wants unchecked action.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, sitting_legislative_coalition, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, sitting_legislative_coalition, beneficiary).

% Depend on the courts as a check against coalition majorities they cannot outvote. When the interpretive boundary shifts toward legislative supremacy, their only recourse — judicial invalidation — weakens; they bear the cost of an unstable boundary without controlling where it settles.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, opposition_lawmakers, payer,
    organized, biographical, constrained, national).

% Bring claims that depend entirely on which institution's authority prevails in a given policy domain. Because the boundary is contested rather than fixed, the same claim may succeed or fail depending on the current balance of institutional confidence, leaving them unable to predict or rely on legal protection.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Face inconsistent application of the interpretive boundary as lower courts calibrate their deference to Supreme Court doctrine that is itself in flux. Litigation costs and outcome uncertainty rise whenever the triadic negotiation between court, executive, and legislature is unsettled in their policy domain.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, lower_court_litigants, payer,
    moderate, immediate, constrained, regional).

% Rely on the norm that domestic legislative sovereignty is constrained by international obligations to secure compliance with treaties and diplomatic commitments. Benefit whenever the interpretive boundary tilts toward honoring external constraints, without bearing direct costs from domestic institutional friction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners, beneficiary,
    institutional, civilizational, analytical, global).

% Implements policy caught between legislative mandates and judicial constraints, and can selectively invoke either institution's authority to justify or resist action. Watches the boundary's drift closely because its administrative discretion expands or contracts with it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, beneficiary).

% Document and theorize the triadic negotiation without a direct stake in any single outcome, producing the comparative record used to assess whether the contested boundary functions as genuine institutional dialogue or as cover for whichever institution currently holds more raw power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes constitutional authority across two co-equal but bounded institutions so that neither legislative majorities nor judicial doctrine alone can permanently settle contested questions of rights and sovereignty — each institution checks the other's overreach through an ongoing, unresolved dialogue.
% TRANSFER_FUNCTION: Moves the practical power to fix meaning in high-stakes policy domains back and forth between the legislative coalition and the courts, with predictability and legal certainty flowing away from litigants (especially minority claimants) and toward whichever institution currently commands more political or doctrinal momentum.
% ABSENT_VOICES: Minority rights claimants and future litigants have no seat in the triadic negotiation itself — they experience the boundary's drift as an external fact determined by court composition and coalition arithmetic, not as parties to a dialogue they are said to benefit from.
% DISAPPEARANCE_RATIONALE: If the contested-boundary equilibrium collapsed into a settled rule (either full judicial supremacy or full parliamentary sovereignty), litigation strategy, coalition legislative drafting, international treaty compliance mechanisms, and lower court deference doctrine would all reorganize immediately around the new settled hierarchy — the current triadic bargaining apparatus would become unnecessary.
% FOUNDING_PROBLEM: Absent an entrenched written constitution with a clear amendment threshold, the state needed some mechanism to prevent simple legislative majorities from unilaterally rewriting fundamental rights protections, while also preserving democratic accountability against unelected judicial overreach.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside both the sitting coalition and the sitting court attest that the underlying problem — absence of a formal constitution with entrenched amendment rules — remains structurally live; they diverge from both institutions' self-serving genealogies (the coalition's narrative of restored democratic supremacy and the court's narrative of necessary rights guardianship) in treating the current arrangement as an improvised equilibrium rather than a designed solution to either camp's stated problem.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the arrangement genuinely distributes power rather than concentrating it in either institution — but it is not zero, because the ambiguity itself is periodically exploited by whichever institution holds momentary advantage (a coalition passing rights-restricting legislation timed against a differently-composed court, or a court asserting review in a domain the legislature considered settled). Suppression is moderate-low (0.38) because neither institution can fully suppress the other's claim to authority; each retains genuine institutional leverage. Theater ratio (0.30) reflects that some 'dialogue' performances — ceremonial deference language, symbolic override threats never executed — mask an underlying reality where actual policy outcomes track political power more than principled boundary-drawing, but the coordination function (preventing unilateral capture by either institution) remains real and substantial, distinguishing this from a pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of either institution, the arrangement looks like principled checks and balances — genuine coordination preventing capture by the other branch. From the seat of a minority rights claimant or opposition lawmaker, the same structure looks like extraction: their legal protection is contingent on an ongoing power struggle they cannot participate in, and the 'balance' language obscures that outcomes still track which institution currently has more leverage. The engine should compute a divergence between the institutional agenda-setter seats and the powerless payer seats even though both nominally endorse the same coordination story.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court justices and the sitting legislative coalition both sit near the beneficiary end: the unresolved boundary is a source of institutional power and flexibility for both, even though they are nominal adversaries in the dialogue. International treaty partners benefit from the norm that sovereignty is externally constrained, at minimal cost to themselves (analytical exit). Minority rights claimants, opposition lawmakers, and lower court litigants sit near the target end: they cannot control where the boundary settles in any given case and bear the cost of unpredictability directly — minority claimants especially, since they are trapped (cannot exit the jurisdiction or wait out an unfavorable court composition) and depend entirely on a boundary neither institution is obligated to hold in their favor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing either simple majoritarianism or unelected judicial supremacy from capturing constitutional meaning outright — remains live in the abstract, but the specific triadic equilibrium that has emerged is contested rather than designed: it arose from the absence of an entrenched formal constitution, not from a deliberate choice to create ongoing institutional dialogue as a value in itself. This prevents the classification from either over-crediting the arrangement as pure coordination (it does have real extractive uses when institutions time actions strategically) or dismissing it as pure extraction (the coordination function of dual bounded authority is genuinely operative and does constrain both institutions some of the time).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_stability_vs_perpetual_contest,
    'Is the contested interpretive boundary a stable equilibrium (genuine ongoing dialogue that self-corrects) or a transitional state that will eventually collapse into either judicial supremacy or parliamentary sovereignty?',
    'Longitudinal tracking of override legislation, court invalidation rates, and international compliance patterns across multiple electoral cycles and court compositions; a stable equilibrium would show oscillation around a mean, while a transitional state would show a secular trend toward one pole.',
    'If stable, the balanced_contestation_reading is descriptively accurate and this constraint''s tangled_rope classification is durable. If transitional, this reading is a temporary snapshot and the constraint should eventually be re-authored to reflect whichever sibling reading the system converges toward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_stability_vs_perpetual_contest, empirical, 'Whether triadic contestation is a stable or transitional institutional state.').

omega_variable(
    which_reading_the_committer_structure_favors,
    'Which of the three sibling readings (balanced_contestation, judicial_supremacy, parliamentary_sovereignty) best describes the ACTUAL operative practice at any given moment, versus which is merely the rhetorical framing each institution prefers when it currently holds advantage?',
    'Compare each institution''s stated doctrine in moments of institutional weakness versus institutional strength — a reading that is only invoked when convenient for the invoking institution suggests strategic rather than principled commitment to that reading.',
    'If institutions systematically shift reading preference based on momentary advantage, the balanced_contestation_reading is best understood as the honest description of the underlying dynamic (with judicial_supremacy and parliamentary_sovereignty as competing strategic claims layered on top), which would corroborate this story''s structural account over either sibling''s claim to be the ''true'' settled doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_the_committer_structure_favors, conceptual, 'Whether sibling readings are principled positions or strategic invocations that this reading''s account best explains.').

omega_variable(
    domain_variance_in_epsilon,
    'Does effective extraction (χ) genuinely vary by policy domain (security matters versus religious-status matters versus economic regulation), or is the appearance of domain variance actually explained by which litigants have resources to bring cases in each domain?',
    'Cross-domain comparison of case outcomes controlling for litigant resources, legal representation quality, and media salience.',
    'If domain variance is genuine and independent of litigant resources, it supports the claim that different policy areas have settled into different institutional equilibria. If it is confounded by resource access, the apparent domain-specificity is actually a power-and-access effect layered on top of a more uniform underlying boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_variance_in_epsilon, empirical, 'Whether ε variance by policy domain is structural or an artifact of unequal litigant resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the basic_law_interpretive_boundary kernel. balanced_contestation_reading treats the boundary as genuinely contested and dynamically negotiated (ε=0.42, tangled_rope). judicial_supremacy_reading and parliamentary_sovereignty_reading are separate constraints asserting that one institution has effectively settled final authority; each carries its own ε, beneficiary/victim structure, and classification. The three readings are linked here for contamination-propagation analysis: a shift in the operative reading (e.g., a court decision or override vote that decisively favors one pole) would be expected to degrade or strengthen the purity of the sibling readings correspondingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
