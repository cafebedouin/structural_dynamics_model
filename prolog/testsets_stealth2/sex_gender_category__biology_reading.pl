% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Legal Sex Category Membership by Immutable Reproductive Biology (Biology Reading)
 *   domain: social ontology/identity politics/legal classification
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   sex_gender_category: the biology_reading, under which category membership
 *   ('woman'/'man') is determined by immutable reproductive biology —
 *   chromosomes and anatomy at birth. Per the epsilon-referent rule, epsilon
 *   is authored for the standing biology-anchored arrangement, assessed by
 *   this reading's own lights; the sibling readings (identity_reading,
 *   hybrid_reading) are separate constraints with their own epsilon,
 *   beneficiary/victim structure, and classification, and are not averaged
 *   into this story. The structural delta this reading produces: cis women
 *   are the sole recognized victim set for sex-based harms and the primary
 *   beneficiaries of the secured boundary; trans women and trans men are
 *   excluded from their lived category with no exit path; intersex
 *   individuals are forced into the binary, historically by nonconsensual
 *   normalization; and the boundary requires active enforcement
 *   (documentation rules, eligibility testing, facility allocation) whose
 *   costs fall on the excluded. Decomposition discipline: the biological
 *   facts themselves (sexual dimorphism, chromosomal immutability) are
 *   natural facts — a mountain outside this family — while THIS constraint is
 *   the determination RULE that leverages them; the rule is enacted, revised,
 *   and enforced, which is why emerges_naturally is false despite the
 *   reading's natural-recognition framing. The claim/metric gap is
 *   deliberate: the reading CLAIMS the category as natural recognition while
 *   the authored metrics describe actively enforced, moderately extractive
 *   operation — the engine measures that divergence. KEY AGENTS (by
 *   structural relationship): - cis_women: Primary beneficiary
 *   (organized/identity_locked) — receive the secured category boundary and
 *   are the reading's sole recognized victim set for sex-based harms -
 *   trans_women: Primary target (moderate/identity_locked) — excluded from
 *   'woman', bear enforcement costs across documentation, facilities, sport -
 *   trans_men: Target (moderate/identity_locked) — excluded from 'man',
 *   mirrored exclusion costs - intersex_individuals: Target
 *   (powerless/trapped) — forced into the binary, nonconsensual normalization
 *   in infancy - legislators_and_courts: Agenda-setter
 *   (institutional/arbitrage) — choose and revise the determination rule
 *   among readings - sports_governing_bodies: Enforcement agenda-setter
 *   (institutional/arbitrage) — repeatedly re-operationalize eligibility
 *   testing - medical_establishment: Operational agenda-setter + secondary
 *   beneficiary (institutional/constrained) — birth assignment and
 *   normalization, collects authority and billing - gender_critical_advocacy:
 *   Beneficiary (organized/identity_locked) — mobilization rides on boundary
 *   defense - human_rights_bodies: Analytical observer
 *   (institutional/analytical) — reviews the rule against human-rights
 *   instruments
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.55).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.58).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Legal Sex Category Membership by Immutable Reproductive Biology (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social ontology/identity politics/legal classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '48c2a86a-6350-4ddd-be12-cf7a8ae89ab5').
narrative_ontology:cs_kernel_codification('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', formalized).
narrative_ontology:cs_authority_grounding('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', practice).
narrative_ontology:cs_interpretation_layer_present('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5').
narrative_ontology:cs_reading_relation('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', foundational, sex_category_membership_immutable_at_birth).
narrative_ontology:cs_axiom_status(sex_category_membership_immutable_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', sex_category_membership_immutable_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', foundational, sex_based_protections_require_biological_category_boundaries).
narrative_ontology:cs_axiom_status(sex_based_protections_require_biological_category_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', sex_based_protections_require_biological_category_boundaries, instrumental).
narrative_ontology:cs_reference_frame('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', immutable_binary_sex_recognition).
narrative_ontology:cs_drift_state('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', contemporary_identity_reading_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48c2a86a-6350-4ddd-be12-cf7a8ae89ab5', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, gender_critical_advocacy).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, medical_establishment).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_immutability_doctrine).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, binary_sex_dimorphism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold membership in the 'woman' category by birth biology and are the reading's recognized population for sex-based harms and protections: refuges, prison allocation, sports categories, and sex-differentiated medical protocols are secured to them by the boundary the rule draws. They do not administer the rule; they receive its boundary security. Their membership is fixed at birth and not changeable, and most experience the rule not as an imposition but as the background condition of sex-based provision.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, biographical, identity_locked, global).

% Live as women while the rule assigns them 'male' at birth and holds that assignment fixed. Depending on jurisdiction they carry documentation that mismatches presentation, are barred from women's sports categories and from sex-designated spaces and services aligned with their lives, and face eligibility testing where the boundary is policed. Their identity is not changeable and their category under this rule is not either; there is no application, transition, or record that moves them across the boundary.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, global).

% Mirror position: assigned 'female' at birth and held there by the rule regardless of transition, excluded from 'man' as a legal category. Some are placed in women's facilities against the grain of their lives, and pregnancy and gynecological care are administered to them under the birth assignment. Exit takes the same non-existent form as for trans women.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    moderate, biographical, identity_locked, global).

% Are born with chromosomes, anatomy, or hormones that do not fit the binary the rule requires. In infancy, before any consent is possible, clinical practice assigns them to one side and often surgically or hormonally normalizes them to fit; the assignment then follows them through documentation, sport, and care. They had no seat in constructing the binary they are fitted to, and the normalization is largely irreversible.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, intersex_individuals, excluded).

% Enact and adjudicate the determination rule: statutes define legal sex by birth biology, and courts apply it to documentation, facilities, and eligibility disputes. They can revise the rule — several jurisdictions have moved between determination rules, tightened or loosened marker changes, and rewritten eligibility statutes — so their position relative to the rule is one of choice among rules rather than subjection to one.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legislators_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Set and enforce eligibility for sex-designated competition. They have repeatedly re-operationalized the boundary — from chromosome testing, to hormonal thresholds, to birth-certificate requirements — revising implementation while holding the biology-anchored shape. The cost of each revision (testing regimes, eligibility litigation, athlete exclusions) falls on athletes, not on the bodies themselves.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Assigns sex at birth and operationalizes the rule clinically: birth-registration entries, normalization procedures for intersex infants, and sex-differentiated protocols. It collects professional authority and billing from the assignment and normalization work, while professional norms and liability bind it to the practice; it cannot unilaterally stop assigning, and its ethical review of infant normalization is recent and partial.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, medical_establishment, beneficiary).

% Organizes to defend the biology-anchored boundary in legislation, litigation, and public argument. Its mobilization, membership, and funding ride on the boundary remaining contested and defended; the rule's persistence is the organization's reason to exist, and its identity is fused with the reading it defends.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_critical_advocacy, beneficiary,
    organized, biographical, identity_locked, continental).

% Treaty bodies, supranational courts, and special rapporteurs that review the rule against human-rights instruments: they take testimony from all seats, publish findings on the treatment of trans and intersex people, and their conclusions pressure legislatures without themselves administering any category.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, cis_women).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the administration problem of sex-based provision: medical protocols that track biological sex, protective provision against sex-patterned violence, sex-designated sports categories, and demographic statistics all require a determinate, administrable rule for who counts as male and female. The biology reading supplies one rule: membership follows chromosomes and anatomy at birth.
% TRANSFER_FUNCTION: Moves category membership and its attached access — sex-designated spaces, sports eligibility, legal recognition, protection coverage — strictly by birth biology. The security of the boundary accrues to cis members of each category; the costs of enforcement (eligibility testing, documentation disputes, nonconsensual normalization of intersex infants) fall on trans and intersex people.
% ABSENT_VOICES: Intersex people were historically absent from the rule's construction: assignments made on infants, normalization performed without consent, no seat in the legislatures that codified the binary. Trans people appear in the contemporary contest largely as objects of the rule rather than as parties to its terms in many jurisdictions. Both would object to the binary's terms; their exclusion is partly what the enforcement machinery maintains.
% DISAPPEARANCE_RATIONALE: Legal sex is load-bearing across documentation, medicine, sport, prisons, refuges, and statistics. If the biology rule vanished overnight, every jurisdiction would need a replacement determination rule (identity or hybrid), sports bodies would rebuild eligibility frameworks, and documentation systems would re-register millions — the classification architecture of the modern state would reorganize around a different rule.
% FOUNDING_PROBLEM: The rule was built to administer sex-based distinctions with a test simple enough for a registrar to apply at birth: which patients receive which medical protocols, who is protected from sex-based violence, who competes in which category, what the census counts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: clinical medicine attests the medical relevance of biological sex (endocrinology and hematology literature independent of the category contest); sports physiology attests performance-relevant sex differences; and the identity reading's own proponents concede sex's medical relevance while disputing category membership, while trans-advocacy and intersex-advocacy organizations attest the enforcement harms while conceding the protective problems are real. No party to the contest denies the founding problems exist; the dispute is over which determination rule should serve them.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.55) because the same structure that delivers genuine sex-based provision concentrates its costs on two small populations: trans people bear exclusion and enforcement across documentation, facilities, and sport, and intersex infants bear nonconsensual normalization — costs the reading's own lights cannot fully discount even while it reads trans exclusion as category integrity rather than harm. Suppression (0.58) is legal-administrative: no individual exit exists (the assigned category is fixed), though the collective alternative — a different determination rule — remains politically live in many jurisdictions, so alternatives are narrowed rather than suppressed outright; suppression is authored as a raw structural property, unscaled. Theater (0.31) is low-moderate: most enforcement does real boundary work, but a rising share is symbolic legislation with little operational effect. Accessibility collapse (0.55): for any individual, reclassification is effectively impossible, but the identity and hybrid readings operate as live institutional alternatives, so the alternative set is contested, not closed. Resistance (0.75) is among the highest of any classification regime in contemporary politics. The epsilon authored here is reading-indexed: the referent is the standing biology-anchored arrangement, assessed by this reading's own lights; the identity reading of the same kernel would author substantially higher epsilon over the identical referent. The measurement series run on one shared time grid (0/5/10/15/20/25) so every tracked metric is authored at every point: rising extraction models the expanding enforcement surface, rising suppression_requirement models enforcement hardening (marker-change restrictions, eligibility regimes, facility statutes), and rising theater models symbolic legislation. FNL alert for identity_coordination: the reading's 'this is just biology' framing is precisely the identity-narrative cover the coordination-type guidance warns about — the structural check is whether Power x Scope coupling concentrates costs on low-power agents at large scope; here it does (intersex infants: powerless, global), which the type's complexity offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the cis_women seat the rule is protective coordination: the boundary secures provisions they rely on, and the reading's own victim-set claim places them as the protected class. From the trans_women, trans_men, and intersex seats the same rule operates as enforced exclusion with no exit — identity_locked for trans stakeholders, trapped for intersex. From the agenda-setter seats (legislators, sports bodies) the rule is a choice among determination rules; they hold arbitrage and experience the contest as a policy option, not a fate. The medical seat is genuinely dual: it administers the assignment and collects from it. The structural counterweight to the target seats is coalition potential — trans, intersex, and allied cis women acting together — though identity divergence between the seats has historically limited it. The engine computes per-seat classifications from the power/exit/role data; this divergence is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   cis_women and gender_critical_advocacy are declared beneficiaries and derive d near the beneficiary end — the rule subsidizes their category security and, for the advocacy seat, its mobilization. trans_women, trans_men, and intersex_individuals are declared victims whose identity_locked or trapped exit places them near the full-target end: trapped or identity-locked targets sit nearer full extraction than mobile ones. The agenda-setter seats are not declared beneficiaries and sit mid-range by fallback; the medical establishment's dual position (administers and collects) is recorded as secondary_role beneficiary, pulling its derived d below a pure administrator's. Scope amplification applies to the target seats: the rule operates at national-to-global scope, which raises effective extraction by making enforcement harms harder to verify, while suppression remains unscaled by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabelings. Reading the rule as pure extraction would erase the genuine coordination it performs — sex-differentiated medicine, protective provision against a sex-patterned harm profile, and administrable birth registration are real functions corroborated from outside the beneficiary set. Reading it as pure coordination (rope) or natural recognition (mountain) would erase the asymmetric extraction the same structure performs on trans and intersex people and the enforcement machinery the boundary requires. The founding problem is live (not dead), so no mandatrophy is declared: the contest is over which determination rule should serve a still-needed function, not over whether the function has atrophied. If the medical relevance of biological sex collapsed — protocols fully individualized — the founding problem would die and this arrangement would decay toward inertial persistence; the sole_victim_set and sports_boundary omegas track the empirical premises on which that trajectory turns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel sex_gender_category; what would the sibling readings (identity_reading, hybrid_reading) change structurally?',
    'Author the sibling stories as separate constraints and compare computed per-seat types, victim sets, and epsilon over the identical referent (the standing biology-anchored arrangement).',
    'Under identity_reading, trans women enter the protected beneficiary set and most enforcement machinery dissolves; under hybrid_reading, a medicalized modification path opens and the trapped exit of trans stakeholders relaxes toward constrained. Victim-set composition and enforcement-cost structure are the reading-indexed variables.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-indexed victim sets and enforcement costs across sibling readings of the category kernel.').

omega_variable(
    natural_recognition_vs_constructed_rule,
    'Is the determination rule a recognition of natural fact, as the reading''s own framing holds, or a constructed and revisable legal rule that leverages natural facts?',
    'Track operationalization history: the boundary has been implemented successively as chromosomes, anatomy, birth certificates, gametes, and hormonal thresholds while the underlying biology stayed fixed. Rule-revision under fixed biology demonstrates construction.',
    'If constructed, the arrangement classifies on the coordination-with-extraction side and the natural-recognition framing is rhetorical cover; if the rule were genuinely fixed by biology, mountain-side analysis would apply and enforcement costs would be misattributed to the rule rather than to its administrators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_recognition_vs_constructed_rule, conceptual, 'Whether the classification rule is natural recognition or constructed law.').

omega_variable(
    intersex_normalization_necessity,
    'Is fitting intersex infants into the binary a necessary cost of binary determination, or an avoidable enforcement choice?',
    'Compare jurisdictions and clinical protocols: delayed-assignment and third-marker regimes show whether birth determination can proceed without infant normalization.',
    'If avoidable, the cost concentrates as an enforcement choice rather than a structural necessity, worsening the reading''s cost profile and vindicating the excluded-seat objection of intersex advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_normalization_necessity, empirical, 'Necessity of infant normalization to the binary determination rule.').

omega_variable(
    sports_boundary_performance_basis,
    'Does the sports eligibility boundary track performance-relevant biology or administrative simplicity?',
    'Longitudinal sports-science data on performance after transition, and comparative outcomes under testosterone-threshold versus birth-certificate versus chromosome-test regimes.',
    'If the boundary exceeds what performance fairness requires, the sports instantiation is exclusion without its coordination justification; if it tracks performance, that slice of enforcement is genuine coordination cost and the extraction picture improves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sports_boundary_performance_basis, empirical, 'Empirical basis of the sports eligibility boundary.').

omega_variable(
    sole_victim_set_claim,
    'The reading holds cis women as the sole victim set for sex-based harms; do trans women bear sex-patterned harms that the category excludes them from protection against?',
    'Victimization epidemiology for trans women (rates of male violence relative to cis men and cis women) and service-access data from exclusion events.',
    'If trans women bear sex-based harms, the sole-victim-set premise fails, the protection structure is incomplete, and the reading''s coordination justification weakens materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sole_victim_set_claim, empirical, 'Robustness of the sole-victim-set premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__biology_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(sex__tr_t5, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__biology_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(sex__tr_t15, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__biology_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(sex__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__biology_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(sex__be_t5, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__biology_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(sex__be_t15, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__biology_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(sex__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__biology_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(sex__su_t5, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__biology_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(sex__su_t15, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__biology_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(sex__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'sex/gender category' covers three structurally distinct determination rules (biology_reading, hybrid_reading, identity_reading), decomposed per the epsilon-invariance principle into three stories over the same kernel, linked by affects_constraints and by cs_structure.reading_relations. Additionally, the underlying biological facts (sexual dimorphism, chromosomal immutability) are a separate mountain-side constraint outside this family: the rule references those facts but is not them — conflating the fact with the determination rule is the family's characteristic authoring error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
