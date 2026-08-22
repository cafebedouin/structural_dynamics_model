% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   Law interpretive boundary kernel: neither the Supreme Court nor the
 *   Knesset holds unqualified final authority over constitutional meaning.
 *   Courts interpret within a bounded jurisdictional domain (chiefly
 *   rights-compatibility review), while the legislature retains formal
 *   sovereign power to enact and amend, but that sovereignty is itself
 *   constrained by international treaty obligations and the normative
 *   expectation that judicial independence be respected rather than
 *   legislated away. The result is an ongoing triadic negotiation among
 *   court, legislature, and executive over where the boundary actually sits
 *   in any given policy domain, with the boundary's location shifting case by
 *   case rather than being fixed by a supremacy clause in either direction.
 *
 * KEY AGENTS:
 *   - supreme_court: institutional agenda_setter interpreting within a bounded domain, dependent on continued acceptance of its rulings
 *   - knesset_majority_coalition: institutional payer/agenda_setter holding formal sovereignty but bounded by international obligations and judicial independence norms
 *   - minority_rights_claimants: powerless beneficiary relying on judicial review as their primary protective channel
 *   - policy_reform_advocates: organized payer experiencing judicial review as a cost on electorally mandated policy
 *   - international_treaty_partners: institutional beneficiary of predictable rule-of-law compliance
 *   - executive_branch: institutional observer/agenda_setter negotiating the boundary through implementation and compliance choices
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
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '3505e58b-2442-442d-b3c1-16a814e5f2d3').
narrative_ontology:cs_kernel_codification('3505e58b-2442-442d-b3c1-16a814e5f2d3', formalized).
narrative_ontology:cs_authority_grounding('3505e58b-2442-442d-b3c1-16a814e5f2d3', distributed).
narrative_ontology:cs_reading_relation('3505e58b-2442-442d-b3c1-16a814e5f2d3', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3505e58b-2442-442d-b3c1-16a814e5f2d3', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('3505e58b-2442-442d-b3c1-16a814e5f2d3', foundational, dual_bounded_legitimacy).
narrative_ontology:cs_axiom_status(dual_bounded_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3505e58b-2442-442d-b3c1-16a814e5f2d3', dual_bounded_legitimacy, conventional).
narrative_ontology:cs_axiom('3505e58b-2442-442d-b3c1-16a814e5f2d3', foundational, sovereignty_constrained_by_international_norms).
narrative_ontology:cs_axiom_status(sovereignty_constrained_by_international_norms, holdable).
narrative_ontology:cs_axiom_grounding('3505e58b-2442-442d-b3c1-16a814e5f2d3', sovereignty_constrained_by_international_norms, conventional).
narrative_ontology:cs_reference_frame('3505e58b-2442-442d-b3c1-16a814e5f2d3', unresolved_dual_authority_architecture).
narrative_ontology:cs_drift_state('3505e58b-2442-442d-b3c1-16a814e5f2d3', contemporary_override_legislation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3505e58b-2442-442d-b3c1-16a814e5f2d3', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_democracy_stability).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_majority_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, policy_reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority_coalition).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, checks_and_balances_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, dialogic_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws within its jurisdictional domain, reviewing legislation for compatibility with fundamental rights and international obligations. It can strike down or read down statutes but cannot itself legislate; its authority depends on continued legislative and executive acceptance of its rulings, which is not guaranteed and is periodically threatened by override legislation proposals.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, beneficiary).

% Holds ultimate sovereign lawmaking power and can amend Basic Laws by legislative majority, but finds its preferred policies periodically invalidated or narrowed by judicial review, and its formal sovereignty is bounded by international obligations and the normative expectation of judicial independence it cannot simply legislate away without reputational and diplomatic cost.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority_coalition, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority_coalition, agenda_setter).

% Rely on judicial review as the primary channel through which majoritarian legislation that burdens them can be challenged; they have no comparable access to override the Knesset directly and depend on the court's willingness to hear and decide cases in their favor.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% Push legislative reforms that reflect a democratic mandate but find implementation delayed, narrowed, or invalidated through judicial review, experiencing the interpretive boundary as a cost imposed on policies they believe carry electoral legitimacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, policy_reform_advocates, payer,
    organized, biographical, constrained, national).

% Benefit from a predictable rule-of-law framework in which domestic legislation is checked against international obligations; they have no direct enforcement mechanism domestically but rely on the court-legislature boundary to keep the state within treaty commitments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners, beneficiary,
    institutional, generational, analytical, global).

% Sits between the two institutions, implementing legislation subject to judicial review and negotiating the practical boundary in real time through litigation strategy, compliance choices, and its own constitutional posture toward court rulings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, agenda_setter).

% Study and comment on the evolving boundary between judicial and legislative authority, without formal power to resolve the contest but shaping the normative vocabulary both institutions use to justify their positions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a working division of labor in which courts adjudicate rights-compatibility of legislation within a bounded jurisdictional domain while the legislature retains primary lawmaking authority, allowing both democratic responsiveness and rights protection to operate without either institution needing to fully subordinate the other.
% TRANSFER_FUNCTION: Moves the practical power to finalize contested policy outcomes back and forth between the legislative majority and the judiciary depending on domain and case; in rights-sensitive domains it shifts effective authority toward the court and away from legislative majorities, while in most ordinary policy domains legislative primacy holds largely undisturbed.
% ABSENT_VOICES: Neither institution's own account fully represents the affected public: minority groups who rely on judicial protection have no seat at the legislative table when statutes are drafted, and electoral majorities have no formal voice inside judicial deliberation when rulings override enacted policy. Comparative constitutional systems with clearer hierarchies are also absent from the domestic debate as a live alternative.
% DISAPPEARANCE_RATIONALE: If the interpretive boundary dissolved overnight, the consequence differs sharply by which institution captured full authority: a shift to unconstrained parliamentary sovereignty would allow rapid legislative reform but remove protection for minorities and could destabilize international treaty compliance; a shift to unconstrained judicial supremacy would entrench rights protections but risk reducing democratic responsiveness. Both institutions and their respective constituencies dispute which disappearance scenario is more consequential, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: The absence of a fully codified, entrenched constitution left open the question of how much authority courts could exercise over legislation, and the Basic Laws were enacted incrementally without an explicit supremacy clause resolving the court-legislature hierarchy — the boundary exists to manage this unresolved constitutional architecture without forcing a premature choice between systems.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside both the judiciary and the legislature (writing in international law journals and OECD rule-of-law assessments) corroborate that the underlying architectural ambiguity remains unresolved and is not merely rhetorical; no formal amendment process has closed the question, and repeated legislative override attempts and judicial pushback since the 1990s indicate the founding tension is actively, not nominally, live.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, contested).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42) and rising gently over the interval, reflecting a genuinely contested arrangement rather than a settled extraction mechanism — some policy domains see courts firmly overriding legislative preference (higher local extraction from the legislative-majority seat), others see legislative primacy essentially unchallenged (near-zero extraction). Suppression is moderate (0.38) because enforcement of the boundary depends on soft mechanisms — reputational cost, diplomatic pressure, normative expectation — rather than hard coercion; there is no single body that can compel compliance from either institution against its will. Theater ratio (0.3) captures that some boundary-drawing performs constitutional principle for public legitimacy even where the practical stakes are modest. All three metrics are authored on one shared 7-point grid (0,5,10,15,20,25,30) reflecting gradually intensifying institutional friction as override legislation attempts and judicial assertiveness have both increased over the period modeled.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, the arrangement looks like principled interpretive review operating exactly within its proper domain — a rope-like coordination function protecting rights and treaty compliance. From the Knesset majority's seat, the same structure looks like an unelected body imposing costs on a democratically mandated agenda — closer to a tangled rope where legitimate boundary-setting shades into extraction of legislative capacity. The engine computes these divergent per-seat classifications from the same structural data; the balanced-contestation reading does not resolve this divergence, it names it as the reading's central content — that both perceptions are simultaneously defensible within a genuinely contested boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority rights claimants and international treaty partners sit near the beneficiary end: the boundary's existence protects outcomes they could not secure through ordinary legislative majorities or direct enforcement. The Knesset majority coalition and policy reform advocates sit nearer the target end: they bear the cost when judicial review narrows or invalidates policies with electoral backing, though this cost is domain-variable, not uniform. The executive branch and constitutional scholars are treated as observers/dual-role because they neither straightforwardly extract from nor are extracted by the boundary — they administer and interpret it from adjacent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unresolved constitutional architecture without an explicit supremacy clause — remains live rather than dead, corroborated by scholars outside both benefiting institutions. This prevents mislabeling the boundary as either pure legislative extraction (parliamentary sovereignty reading) or pure judicial overreach (judicial supremacy reading): the balanced-contestation reading holds that the boundary continues to perform a genuine, unresolved coordination function — managing a real institutional design gap — even though its operation imposes real, unevenly distributed costs on legislative majorities in specific domains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_location_domain_variance,
    'Does the interpretive boundary between court and legislature sit at a stable, principled location across policy domains, or does its location vary opportunistically depending on political salience and institutional strength at the moment of contest?',
    'Longitudinal case-law analysis tracking judicial deference rates across policy domains (economic regulation vs. rights-sensitive legislation) combined with tracking of override-legislation attempts and their success rates over multiple Knesset terms.',
    'If the boundary is principled and stable, the balanced-contestation reading is well-supported as a genuine dialogic equilibrium. If the boundary shifts opportunistically with political strength, the arrangement is closer to an unstable power struggle temporarily described as balance, which would push toward reclassifying specific domain-instances as tangled_rope or snare depending on who currently holds leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_location_domain_variance, empirical, 'Whether the contested boundary is a stable principle or an opportunistic power equilibrium.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the balanced-contestation reading itself a stable third position, or is it better understood as the observable surface produced by an ongoing unresolved contest between the judicial-supremacy and parliamentary-sovereignty readings, neither of which has yet won?',
    'Track whether constitutional actors (justices, legislators, executive officials) explicitly endorse a balanced/dialogic theory of the boundary in their own reasoning, versus implicitly asserting one of the two competing supremacy claims while practically compromising due to insufficient power to enforce it.',
    'If actors genuinely hold a dialogic theory, this reading is a distinct, stable structural claim. If actors are simply engaged in an unresolved power struggle and the ''balance'' is an artifact of neither side yet prevailing, this reading may better be understood as a transitional snapshot rather than an independent equilibrium — with implications for whether ε should be read as stable or as tracking an ongoing power shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether balanced contestation is a genuine equilibrium reading or an artifact of an unresolved supremacy contest between the other two readings.').

omega_variable(
    international_obligation_enforcement_gap,
    'How binding are the international obligations that are said to constrain the legislature''s sovereign power, given the absence of a domestic enforcement mechanism compelling compliance?',
    'Compare instances where domestic legislation has been amended or withdrawn specifically citing international obligations against instances where such obligations were invoked rhetorically but did not alter legislative outcomes.',
    'If international obligations rarely constrain outcomes in practice, the reading''s claim that legislative sovereignty is meaningfully bounded by international norms is weaker than stated, shifting effective power further toward unconstrained parliamentary sovereignty in practice even if not in doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligation_enforcement_gap, empirical, 'Whether international obligations function as a real constraint on legislative sovereignty or as rhetorical cover with limited enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the basic_law_interpretive_boundary kernel. judicial_supremacy_reading treats the court as holding final, binding interpretive authority over the Knesset (higher extraction from the legislative seat, lower from rights-claimant seats). parliamentary_sovereignty_reading treats the Knesset as holding ultimate authority including override power (higher extraction from minority/rights-claimant seats, lower from legislative-majority seats). This balanced_contestation_reading authors a moderate, domain-variable ε reflecting genuine institutional dialogue rather than either institution's full dominance. All three share the same kernel (the unresolved Basic Law hierarchy) but are structurally distinct constraints with independently authored ε, beneficiary/victim sets, and classifications — per the ε-invariance principle they are not merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
