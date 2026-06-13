% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading: Equality as Self-Evident Universal Right
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story captures the expansive universalist reading of the
 *   equality clause kernel in U.S. constitutional law. Expansive universalism
 *   treats equality as a self-evident, pre-constitutional principle that
 *   applies to all humans regardless of historical exclusions. It legitimizes
 *   judicial expansion of equality rights without amendment, by reading
 *   historical exclusions as errors rather than binding boundaries of the
 *   constitutional bargain. This reading underlies major civil rights
 *   expansions (abolition, female suffrage, racial equality, LGBTQ+ rights,
 *   disability access). It conflicts directly with restrictive originalism
 *   (which treats the Framers' historical boundaries as binding) and coexists
 *   uneasily with progressive textualism (which allows expansion but requires
 *   amendment or democratic process, not judicial reinterpretation). The
 *   claim (rope) reflects the genuine coordination function: unifying diverse
 *   equality claims under a universal principle lowers the cost of rights
 *   advocacy and avoids fragmenting them claim-by-claim. The metrics reflect
 *   the extractive dimension: the reading transfers interpretive authority
 *   from legislative amendment (proper gate) to courts, suppresses
 *   originalist scholarship as morally disqualifying, and creates spillover
 *   costs for those committed to historical meaning as binding.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups (beneficiary, organized power): civil rights claimants who gain legitimacy from the self-evidence principle
 *   - civil_rights_movements (beneficiary/agenda_setter, organized power): advocacy organizations that invoke expansive universalism to lower the amendment threshold
 *   - judicial_interpreters_progressive (agenda_setter, institutional power): courts and progressive scholars who expand rights through interpretive authority
 *   - restrictive_originalist_advocates (payer, organized power): originalists and conservatives who see their interpretive approach marginalized
 *   - legislative_amendment_gatekeepers (payer, institutional power): Congress/state legislatures lose exclusive gatekeeping over constitutional rights
 *   - originalist_constitutional_scholars (excluded, moderate power): academic tradition treated as morally bankrupt rather than legitimate interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.29).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading: Equality as Self-Evident Universal Right").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0').
narrative_ontology:cs_kernel_codification('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', fixed_text).
narrative_ontology:cs_authority_grounding('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', lineage).
narrative_ontology:cs_interpretation_layer_present('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0').
narrative_ontology:cs_reading_relation('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', foundational, equality_self_evident_universal).
narrative_ontology:cs_axiom_status(equality_self_evident_universal, holdable).
narrative_ontology:cs_axiom_grounding('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', equality_self_evident_universal, deontological).
narrative_ontology:cs_axiom('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', foundational, historical_exclusions_not_binding).
narrative_ontology:cs_axiom_status(historical_exclusions_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', historical_exclusions_not_binding, deontological).
narrative_ontology:cs_reference_frame('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', universal_equal_dignity).
narrative_ontology:cs_drift_state('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', contemporary_rights_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e3be78d-1ce9-4a38-9075-c88c9ad4dbb0', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_movements).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, judicial_interpreters_progressive).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, restrictive_originalist_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, legislative_amendment_gatekeepers).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_dignity).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, moral_equality_premise).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evident_truth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups formally excluded from the Constitution's original protections (enslaved persons, women, Indigenous peoples, religious minorities) derive legitimacy for rights claims from the expansive universalist reading. This reading treats historical exclusion as error to be corrected, not binding constraint. They benefit by having their equality claims treated as self-evident rather than requiring constitutional amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Advocacy organizations and social movements cite expansive universalism to argue for judicial recognition of new equality rights (sex discrimination, sexual orientation, disability access). They invoke the self-evidence principle to bypass amendment-requirement arguments and lower the threshold for rights recognition through interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, civil_rights_movements, agenda_setter).

% Judges and constitutional scholars who adopt expansive universalism justify expanding equality rights through interpretation without waiting for formal amendment. They treat historical exclusions as violations of a pre-existing universal principle rather than legitimate boundaries of the original bargain. This reading legitimizes judicial activism in expanding rights categories.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judicial_interpreters_progressive, agenda_setter,
    institutional, generational, analytical, national).

% Constitutional originalists and conservative legal scholars argue that expansive universalism overrides the historical consensus that shaped the Constitution. They treat this reading as imposing unanticipated obligations on the constitutional framework and constraining legitimate originalist interpretation. The cost they bear is reduced influence over what the Constitution permits.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalist_advocates, payer,
    organized, generational, mobile, national).

% Congress and state legislatures find their formal amendment power supplemented (from a conservative view, undermined) by judicial expansion of rights through expansive universalist interpretation. Legislatures lose exclusive gatekeeping authority over which equality claims become constitutional law. Amendment becomes optional rather than necessary.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legislative_amendment_gatekeepers, payer,
    institutional, generational, constrained, national).

% Academic and legal scholars who defend originalist methodology are systematically marginalized by expansive universalist framing—their arguments about historical meaning are treated as morally bankrupt (defending exclusion), not as legitimate interpretive disputes. They are excluded from the conversation about which reading is authoritative.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_constitutional_scholars, excluded,
    moderate, biographical, mobile, national).

% The interpretive tradition grounded in the Framers' actual intent and the historical social contract. This is not an actor but a framing that expansive universalism marginalizes—the reading treats the Framers' historical exclusions as irrelevant to what the Constitution means rather than constitutive of it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitution_authorship_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__expansive_universalist, constitution_authorship_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a rights-claiming system that treats equality as a self-evident principle rather than a negotiated outcome of the constitutional bargain. Enables rights movements to appeal to a universal norm rather than negotiating group-by-group for amendment. Unifies diverse equality claims under a single self-evidence doctrine rather than fragmenting them into separate constitutional questions.
% TRANSFER_FUNCTION: Transfers interpretive authority from legislatures and amendment processes to courts and the doctrine of judicial review. Moves the burden of proof: historical exclusion must be justified rather than rights expansion must be justified. Shifts which groups' reading of the Constitution gets treated as authoritative—from those committed to historical meaning to those committed to moral universalism.
% ABSENT_VOICES: Originalist constitutional scholars and those committed to historical meaning as binding constraint are systematically excluded from determining which reading is legitimate. The framing that treats historical exclusion as morally disqualifying rather than as interpretively relevant silences the scholarly tradition grounded in the Framers' intent. Originalists would argue for structural deference to amendment process and reject moral universalism as a hermeneutic principle.
% DISAPPEARANCE_RATIONALE: Conservative originalists argue the world would rearrange back to the Constitution's historical meaning if expansive universalism disappeared—rights would revert to amendment-dependent status and courts would defer to legislative gatekeeping. Progressive universalists argue the self-evident principle is independent of any particular reading and would persist in moral form even if absent from judicial doctrine. The question whether equality's universality is self-evident or constructed remains fundamentally contested.
% FOUNDING_PROBLEM: The original Constitution explicitly excluded significant portions of the population from its protections and benefits (slavery, female disenfranchisement, property requirements). Successive amendments and civil rights movements corrected these exclusions. The question: are these exclusions binding boundaries of the constitutional bargain, or violations of a pre-existing universal principle that the Constitution should be read to embody?
% FOUNDING_PROBLEM_CORROBORATION: Civil rights scholars and progressive constitutional theorists attest that the founding problem persists—courts still encounter equality claims against residual historical exclusions. Originalist scholars attest that the founding problem was the Constitution's historical shape, now corrected by amendment, and that imposing post-hoc universalism onto the Framers' text violates interpretive integrity. Congressional voting records on civil rights legislation show political consensus that historical exclusions required correction, supporting the fact that they happened; but the question of whether that fact determines how to read the Constitution remains among constitutional scholars themselves.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, contested).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38 at 2026) because the constraint coordinates a real rights-expansion function that benefits historically excluded groups AND transfers interpretive power to courts. It is not a pure snare: the coordination gain is genuine. Suppression is low (0.29) because originalists maintain intellectual counter-positions and the constraint does not physically coerce adherence; it operates through framing and institutional authority. Theater ratio is also low (0.18) because the self-evidence doctrine does real interpretive work, not merely theatrical maintenance. However, extractiveness has accumulated over time (0.15 in 1787, 0.38 in 2026) as the reading's use in judicial reasoning expanded and as its authority to interpret the Constitution without amendment was consolidated. Suppression remained nearly flat because the mechanism of suppression is constant: framing originalist historical-meaning arguments as morally bankrupt rather than legitimate interpretation. The temporal series captures how a legitimating principle (self-evident equality) has concentrated interpretive power in courts over two centuries, moving from weak presence (1787—the phrase 'all men are created equal' in Declaration, not Constitution) to dominant institutional doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of historically excluded groups and civil rights advocates, the expansive universalist reading is liberatory coordination—it transforms exclusion into error and makes rights claims self-justifying. From the seat of originalists and amendment-gatekeepers, it is institutional overreach—courts imposing a principle not textually present and claiming moral authority to override historical meaning. From a judicial seat, the reading enables rights expansion aligned with moral progress. From a legislative seat, it bypasses the proper amendment gate. These are not different measurements of the same constraint; they are per-seat classifications of the same structural situation. The engine computes them separately.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: historically_excluded_groups (d near 0, full beneficiary—they gain rights and face low cost), civil_rights_movements (d near 0, full beneficiary—they gain legitimacy and lower the amendment threshold), judicial_interpreters_progressive (d near 0.2, slight beneficiary—they gain interpretive authority and institutional relevance). Payers: restrictive_originalist_advocates (d near 0.75, nearly full target—they lose influence and face delegitimization), legislative_amendment_gatekeepers (d near 0.6, moderate-to-high target—they lose exclusive gatekeeping power). The directionality spread is wide: the constraint creates sharp asymmetry. Excluded voices (originalist scholars) face suppression but carry d near 0.8 because they cannot operate within the constraint's framing without accepting its delegitimization.
 *
 * MANDATROPHY ANALYSIS:
 *   Expansive universalism does NOT show mandatrophy signs. The founding problem (historical exclusions) remains contested—its status is disputed between the readings. The constraint's function (enabling rights expansion) remains live and delivers measurable benefit (civil rights protections expanded). The disappearance verdict is correctly placed as 'contested' rather than 'dead' because originalists dispute that the founding problem persists (they argue amendment has solved it) while universalists dispute that mandatrophy has occurred (they argue self-evidence is timeless). The theater ratio is low because the principle does real work, not mere theatrical maintenance. If theater ratio rose significantly and originalist influence collapsed entirely, mandatrophy might emerge—but currently the constraint coordinates genuine expansion and faces live intellectual resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_evidence_vs_construction,
    'Is equality a self-evident universal truth that pre-exists constitutional text, or a constructed principle whose scope must be negotiated through amendment and democratic process?',
    'Analysis of competing philosophical grounding: does expansive universalism rest on claims about pre-constitutional moral reality (self-evidence) or on claims about how to interpret a living text (construction)? Contrast with progressive textualist and restrictive originalist readings of the same kernel.',
    'If self-evident, expansive universalism is a moral claim that constrains constitutional reading and justifies judicial expansion of rights. If constructed, the constraint is an imposed interpretation rather than a discovered principle, and amendment or legislative process properly gate rights expansion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_evidence_vs_construction, conceptual, 'Whether equality''s universality is pre-textual self-evidence or post-textual construction.').

omega_variable(
    judicial_vs_legislative_gatekeeping,
    'Does expansive universalism properly allocate interpretive authority to courts, or does it bypass the amendment process that the Constitution designates as the path for expanding rights?',
    'Comparison of outcomes under expansive universalism vs. amendment-based expansion: do courts using expansive universalism produce more stable, durable rights protections than legislatively-amended rights? Do they produce over-reach? Do they achieve the same protections faster?',
    'If courts achieve more durable and legitimate rights expansion, expansive universalism is a beneficial acceleration of justice. If over-reach and instability result, it is an extraction of gatekeeping power from the proper constitutional venue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_legislative_gatekeeping, empirical, 'Whether court-driven expansion via expansive universalism outperforms amendment-driven expansion.').

omega_variable(
    kernel_reading_contest,
    'Is the expansive universalist reading one defensible interpretation of the equality clause kernel, or does it foreclose or coexist with the restrictive originalist and progressive textualist readings?',
    'Examine whether the three readings rest on incompatible core premises (forecloses), can be held simultaneously by different parties (coexists_with), or whether one creates structural pressure on the others (influences). This is the committer-frame omega: which reading-relation structure best describes the actual constitutional dispute?',
    'If expansive universalism forecloses originalism, then the constitutional question is settled at the level of basic premises. If they coexist, the dispute is live and unresolved. If expansive universalism influences but does not foreclose the others, it shapes the terrain without determining the outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The structural relationship between this reading and its sibling readings.').

omega_variable(
    historical_exclusion_binding_status,
    'Are the historical exclusions in the Constitution''s original scope (slavery, female disenfranchisement, property requirements) binding evidence of what the Constitution authorized, or are they violations of what the Constitution should be read to mean?',
    'Hermeneutic analysis: does the meaning of the text depend on what it was understood to mean at ratification, or on what its language and logic commit it to in principle? This is the interpretive choice point where expansive universalism and restrictive originalism diverge most sharply.',
    'If historical exclusions are binding, restrictive originalism''s reading is correct and rights expansion requires amendment. If they are violations of pre-textual principle, expansive universalism''s reading is correct and courts can correct them through interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_exclusion_binding_status, conceptual, 'Whether historical exclusions constrain or violate constitutional meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__expansive_universalist, theater_ratio, 1787, 0.08).
narrative_ontology:measurement_basis(equa_tr_t1787, observed).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__expansive_universalist, theater_ratio, 1865, 0.12).
narrative_ontology:measurement_basis(equa_tr_t1865, observed).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__expansive_universalist, theater_ratio, 1920, 0.14).
narrative_ontology:measurement_basis(equa_tr_t1920, observed).
narrative_ontology:measurement(equa_tr_t1964, equality_clause_scope__expansive_universalist, theater_ratio, 1964, 0.16).
narrative_ontology:measurement_basis(equa_tr_t1964, observed).
narrative_ontology:measurement(equa_tr_t1990, equality_clause_scope__expansive_universalist, theater_ratio, 1990, 0.17).
narrative_ontology:measurement_basis(equa_tr_t1990, observed).
narrative_ontology:measurement(equa_tr_t2026, equality_clause_scope__expansive_universalist, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__expansive_universalist, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement_basis(equa_be_t1787, observed).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__expansive_universalist, base_extractiveness, 1865, 0.22).
narrative_ontology:measurement_basis(equa_be_t1865, observed).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__expansive_universalist, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement_basis(equa_be_t1920, observed).
narrative_ontology:measurement(equa_be_t1964, equality_clause_scope__expansive_universalist, base_extractiveness, 1964, 0.35).
narrative_ontology:measurement_basis(equa_be_t1964, observed).
narrative_ontology:measurement(equa_be_t1990, equality_clause_scope__expansive_universalist, base_extractiveness, 1990, 0.37).
narrative_ontology:measurement_basis(equa_be_t1990, observed).
narrative_ontology:measurement(equa_be_t2026, equality_clause_scope__expansive_universalist, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__expansive_universalist, suppression_requirement, 1787, 0.25).
narrative_ontology:measurement_basis(equa_su_t1787, observed).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__expansive_universalist, suppression_requirement, 1865, 0.27).
narrative_ontology:measurement_basis(equa_su_t1865, observed).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__expansive_universalist, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement_basis(equa_su_t1920, observed).
narrative_ontology:measurement(equa_su_t1964, equality_clause_scope__expansive_universalist, suppression_requirement, 1964, 0.29).
narrative_ontology:measurement_basis(equa_su_t1964, observed).
narrative_ontology:measurement(equa_su_t1990, equality_clause_scope__expansive_universalist, suppression_requirement, 1990, 0.29).
narrative_ontology:measurement_basis(equa_su_t1990, observed).
narrative_ontology:measurement(equa_su_t2026, equality_clause_scope__expansive_universalist, suppression_requirement, 2026, 0.29).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, judicial_review_scope__expansive).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, amendment_process_gatekeeping).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equality_clause_scope kernel. The three readings (expansive_universalist, progressive_textualist, restrictive_originalist) are structurally distinct constraints because they instantiate different ε values, different beneficiary/victim sets, and different institutional dynamics. They share the same kernel (the Constitution's equality principle) but decompose into separate constraint stories per the ε-invariance principle. All three are linked bidirectionally via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
