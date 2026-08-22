% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In constitutional systems that retain plural, community-administered
 *   personal law codes (marriage, divorce, succession) without enacting a
 *   single Uniform Civil Code, the apex judiciary has become the de facto
 *   harmonizing authority — striking down or reading down specific
 *   personal-law provisions that fail constitutional equality or dignity
 *   review, one case at a time. No legislature has authorized this as a
 *   comprehensive reform program; it accretes from individual litigation. The
 *   claimed type is scaffold — coordination meant to be transitional, filling
 *   a gap pending eventual comprehensive legislative reform — but the
 *   authored metrics show accumulating extraction and theater over time,
 *   which is exactly the divergence a scaffold that has stopped functioning
 *   as transitional support would produce.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: institutional/analytical — accretes harmonizing authority case by case
 *   - litigants_with_appellate_access: moderate/constrained — benefit when they reach the forum
 *   - unlitigated_personal_law_subjects: powerless/trapped — bear the cost of the lottery of selection
 *   - community_legal_institutions: organized/constrained — lose authority provision by provision without negotiation
 *   - legislature_as_institution: institutional/constrained — bypassed, and its own political incentive to legislate is reduced by the substitute
 *   - legal_academic_commentariat: organized/arbitrage — analytical observer with a career stake in the pathway's continuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.46).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.38).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'e1ef9a8e-a812-4db5-930e-61226b236bdf').
narrative_ontology:cs_kernel_codification('e1ef9a8e-a812-4db5-930e-61226b236bdf', distributed).
narrative_ontology:cs_authority_grounding('e1ef9a8e-a812-4db5-930e-61226b236bdf', practice).
narrative_ontology:cs_interpretation_layer_present('e1ef9a8e-a812-4db5-930e-61226b236bdf').
narrative_ontology:cs_reading_relation('e1ef9a8e-a812-4db5-930e-61226b236bdf', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('e1ef9a8e-a812-4db5-930e-61226b236bdf', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('e1ef9a8e-a812-4db5-930e-61226b236bdf', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1ef9a8e-a812-4db5-930e-61226b236bdf', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('e1ef9a8e-a812-4db5-930e-61226b236bdf', foundational, constitutional_floor_via_adjudication_is_legitimate_absent_legislation).
narrative_ontology:cs_axiom_status(constitutional_floor_via_adjudication_is_legitimate_absent_legislation, holdable).
narrative_ontology:cs_axiom_grounding('e1ef9a8e-a812-4db5-930e-61226b236bdf', constitutional_floor_via_adjudication_is_legitimate_absent_legislation, conventional).
narrative_ontology:cs_axiom('e1ef9a8e-a812-4db5-930e-61226b236bdf', secondary, incremental_case_accretion_substitutes_validly_for_comprehensive_codification).
narrative_ontology:cs_axiom_status(incremental_case_accretion_substitutes_validly_for_comprehensive_codification, holdable).
narrative_ontology:cs_axiom_grounding('e1ef9a8e-a812-4db5-930e-61226b236bdf', incremental_case_accretion_substitutes_validly_for_comprehensive_codification, instrumental).
narrative_ontology:cs_reference_frame('e1ef9a8e-a812-4db5-930e-61226b236bdf', pre_independence_personal_law_administration).
narrative_ontology:cs_drift_state('e1ef9a8e-a812-4db5-930e-61226b236bdf', contemporary_apex_court_activism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e1ef9a8e-a812-4db5-930e-61226b236bdf', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, litigants_with_appellate_access).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, legal_academic_commentariat).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, unlitigated_personal_law_subjects).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, community_legal_institutions).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, legislature_as_institution).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, judicial_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional floor case by case, deciding which personal-law provisions survive equality and dignity review. Accumulates interpretive authority over family law with each ruling, without ever having to build a legislative coalition. Its output cannot be reversed except by constitutional amendment or a contrary later bench, which entrenches its own institutional role as the harmonizing authority.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, constitutional_judiciary, beneficiary).

% Individuals who can fund or attract public-interest counsel to bring a personal-law provision before the apex court obtain a ruling that reshapes their community's law and any similarly situated litigant's, but only if they have the resources, standing, and timing to reach that forum in the first place.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, litigants_with_appellate_access, beneficiary,
    moderate, biographical, constrained, national).

% Builds careers, casebooks, and doctrinal frameworks around the accreting body of case law. Has a stake in the pathway continuing indefinitely — a completed Uniform Civil Code would collapse a rich field of doctrinal interpretation into settled statute.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legal_academic_commentariat, beneficiary,
    organized, generational, arbitrage, national).

% The overwhelming majority of people governed by personal law never bring or benefit from a constitutional case. They live under whatever floor has (or has not yet) been judicially imposed on their specific community's code, with no say in the sequence, pace, or selection of which provisions get litigated next. Protection is a lottery of which case happened to reach the court and when.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, unlitigated_personal_law_subjects, payer,
    powerless, biographical, trapped, national).

% Community bodies that administer personal law (religious courts, community councils) find their authority progressively hollowed out provision by provision through litigation they did not initiate and often cannot anticipate, without ever facing a legislature that could negotiate a comprehensive settlement with them.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, community_legal_institutions, payer,
    organized, generational, constrained, regional).

% The body constitutionally tasked with enacting family law is bypassed: the judiciary fills the vacuum the legislature's inaction (or political inability to pass a UCC) leaves open. Each ruling reduces the pressure and the political cost of eventually legislating comprehensively, since piecemeal judicial relief blunts the urgency that might otherwise force a floor debate.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislature_as_institution, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, legislature_as_institution, excluded).

% Minority religious communities whose personal law is the subject of litigation are rarely direct parties to the cases that reshape their family law; the litigation is typically brought by an individual member or an outside rights organization, and the community as a corporate voice is not represented in the courtroom that decides its internal law.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_minority_communities, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional floor below which no personal law provision — regardless of community of origin — may fall, without requiring the political coalition a comprehensive Uniform Civil Code would need. It lets the constitutional order harmonize baseline rights protections incrementally.
% TRANSFER_FUNCTION: Moves interpretive and normative authority over family law from elected legislatures and community institutions to the apex judiciary, and moves the timing and selection of which rights get vindicated from a deliberate legislative process to whichever litigant happens to reach the court.
% ABSENT_VOICES: The overwhelming majority of people living under unlitigated personal-law provisions have no voice in which cases get selected for review; community institutions whose internal law is being reshaped are frequently not parties to the litigation that reshapes it; the legislature itself is structurally sidelined by a pathway that reduces the political pressure to legislate.
% DISAPPEARANCE_RATIONALE: If case-by-case constitutional review of personal law stopped, the incremental floor-raising would halt entirely absent legislative action; unlitigated provisions would remain permanently unreviewed by any mechanism, community institutions would regain full uncontested authority over their internal codes, and pressure would either shift decisively toward legislative UCC reform or toward no reform at all — the current hybrid, partially-harmonized state would not persist.
% FOUNDING_PROBLEM: Constitutions with fundamental-rights guarantees but plural, community-administered personal law codes need SOME mechanism to prevent particular personal-law provisions from violating baseline constitutional guarantees, in political systems where legislating a single uniform code is not achievable.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the judiciary (writing on India, Israel, and similar plural-law systems) attest the underlying tension between fundamental rights guarantees and personal law pluralism remains unresolved by legislative means in most such systems; legislative leaders across multiple electoral cycles have themselves acknowledged an inability to build a UCC coalition, corroborating that judicial review is filling a genuine, persisting gap rather than a manufactured one — though the same commentators also note the pathway's own persistence increasingly serves the judiciary's and academy's institutional interests independent of the original gap.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).
:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to 0.46 over the interval because each successive ruling further entrenches judicial supremacy over family law as the default mechanism, while doing nothing to correct the fundamentally arbitrary selection process (extraction here means the diffuse cost borne by unlitigated subjects and sidelined institutions, not a rent collected by an identifiable financial beneficiary). Theater ratio rises from 0.18 to 0.42 because an increasing share of the judiciary's and commentariat's activity is framing rulings as steps toward eventual comprehensive reform — sustaining scaffold legitimacy — when the underlying legislative reform shows no corresponding movement; the 'transition' narrative persists while the transition itself stalls. Suppression rises modestly (0.25 to 0.38): community institutions and the legislature face growing structural pressure to accept the judiciary's incremental authority rather than resist it, since resisting a specific ruling risks a broader unfavorable holding.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's own seat, each ruling is principled constitutional adjudication filling an interstitial gap — a rope-like coordination function correcting an injustice. From the seat of unlitigated subjects and community institutions, the same mechanism looks like an unaccountable, unpredictable, and unequally distributed reallocation of authority that never resolves into a stable, generally-applicable settlement. The engine's per-seat computation should register this divergence structurally, from the differing power/exit/scope values authored above, without either seat's framing being privileged in the base metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a structural beneficiary despite formally being a neutral arbiter: each ruling that fills a legislative vacuum strengthens the doctrine that the judiciary may legitimately act where the legislature has not, expanding its own institutional domain (d low). Litigants who reach the court benefit concretely and individually (d low-moderate) but this is asymmetric — only those with resources and standing. Unlitigated subjects are structurally the targets: they bear whatever residual unconstitutional provisions remain unreviewed, and have no say in sequencing (d high). The legislature is a target of a different kind — its institutional relevance is being displaced by a mechanism that reduces the political cost of its own inaction (d high, though it is also complicit by omission).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling personal-law pluralism with constitutional equality guarantees absent a legislative UCC — remains genuinely live (status: live), which distinguishes this constraint from a pure mandatrophy case where the founding problem has died but the mandate persists. What has shifted is that the SPECIFIC MECHANISM (case-by-case judicial accretion, as opposed to comprehensive legislation) has itself become an entrenched institutional interest for the judiciary and legal academy, evidenced by rising theater_ratio: the scaffold's sunset condition (eventual UCC legislation, or at minimum comprehensive judicial codification) recedes rather than approaches as the pathway matures. This is not mandatrophy in the classic sense (problem solved, mandate persists) but a scaffold-drift pattern: problem unsolved, but the SPECIFIC temporary mechanism increasingly serves interests independent of solving it, which is exactly what the declared has_sunset_clause and disappearance_verdict=world_rearranges are meant to surface for scrutiny rather than let pass as permanent design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harmonization_reading_kernel_disambiguation,
    'Is the judicial_harmonization_reading a distinct normative position within the marriage_authority kernel contest, or is it better understood as a description of institutional MECHANISM that is compatible with several of the normative readings (secularist, gender_rights) simultaneously operating through it?',
    'Trace specific rulings: does the judiciary''s own reasoning invoke gender-equality doctrine (converging with gender_rights_reading), secular-uniformity doctrine (converging with secularist_reading), or a freestanding constitutional-floor doctrine independent of either? Sustained doctrinal citation patterns across a body of cases would indicate which normative reading the mechanism actually serves.',
    'If the mechanism turns out to be normatively neutral scaffolding that different readings can use instrumentally, this story''s independence from its siblings is confirmed as a mechanism-vs-normative-claim distinction. If the mechanism is found to systematically encode one sibling reading''s premises (e.g., consistently deploying gender-equality reasoning), the sibling with which it substantively merges should absorb this story via network linkage rather than remain structurally separate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_reading_kernel_disambiguation, conceptual, 'Whether the judicial harmonization pathway is a neutral mechanism or a disguised instantiation of a sibling normative reading.').

omega_variable(
    scaffold_permanence_risk,
    'Does the rising theater_ratio and extractiveness over the interval indicate the scaffold is drifting toward permanent institutional entrenchment (a piton in the making), or does it reflect a genuinely necessary lengthening of the transition given how slowly plural personal-law systems can be constitutionally harmonized?',
    'Compare the rate of legislative UCC proposal activity against the rate of judicial rulings over the same interval; a widening gap (judicial activity accelerating while legislative activity stagnates or declines) would support entrenchment; parallel movement would support genuine transitional necessity.',
    'If entrenchment is confirmed, the has_sunset_clause declaration becomes formally true but substantively hollow, and a future revision of this story might reclassify toward piton (theatrical maintenance of a transition that will not complete) rather than scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_permanence_risk, empirical, 'Whether the judicial pathway is a genuine transition or a scaffold hardening into permanent institutional practice.').

omega_variable(
    selection_arbitrariness_omega,
    'Is the sequence and selection of which personal-law provisions get litigated (and thus harmonized) meaningfully random with respect to severity of rights violation, or does it systematically track litigant resources and legal-mobilization capacity, meaning the least-resourced communities'' worst violations are least likely ever to be reviewed?',
    'Cross-reference the docket of personal-law constitutional cases against independent severity assessments of rights violations across all personal law codes in the jurisdiction, testing for correlation with litigant socioeconomic profile or represented organization''s resources.',
    'A strong resource-correlation would sharpen the case that unlitigated_personal_law_subjects bear the extraction disproportionately along lines of poverty and organizational access, strengthening the payer/target directionality already authored for that group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selection_arbitrariness_omega, empirical, 'Whether case selection for constitutional review tracks rights-violation severity or litigant resource capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(marr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(marr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(marr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.43).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(marr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(marr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(marr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints emitted from a single contested kernel (marriage_authority). Each sibling reading — communal_autonomy_reading, secularist_reading, gender_rights_reading, federalist_millet_reading — instantiates a different normative claim about who should hold marriage authority and why; this story instead describes the actual institutional mechanism (case-by-case constitutional review) through which authority is currently, contingently being reallocated in the absence of any of those normative claims being legislatively settled. Its ε (0.46, rising) reflects the mechanism's own accumulating institutional entrenchment, not the merits of any sibling's normative position. The relationship to each sibling is authored in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
