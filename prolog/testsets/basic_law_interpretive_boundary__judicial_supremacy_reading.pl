% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Law Judicial Supremacy: Court-Enforced Constitutional Hierarchy
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   This constraint instantiates a specific reading of the contested kernel
 *   governing the Basic Laws' constitutional status and the Supreme Court's
 *   authority. The judicial supremacy reading holds that Basic Laws
 *   constitute a higher-order normative framework that the Supreme Court must
 *   interpret and enforce, with the power to invalidate ordinary legislation
 *   that conflicts with Basic Law principles. This reading emerged in the
 *   mid-1990s following the enactment of the first modern Basic Laws and the
 *   Court's landmark decisions asserting review authority. The reading is
 *   contested by parliamentary sovereignty advocates (who argue the Knesset
 *   retains ultimate authority to interpret Basic Laws via simple majority
 *   amendment) and by balanced contestation proponents (who argue both
 *   institutions hold legitimate but bounded authority). This JSON
 *   instantiates ONLY the judicial supremacy reading as a clean, ε-invariant
 *   constraint. The sibling readings are separate constraint stories with
 *   their own ε values and structural data.
 *
 * KEY AGENTS:
 *   - Supreme Court: institutional agenda-setter; interprets Basic Laws as constitutional hierarchy and enforces via invalidation
 *   - Knesset legislative majority: powerful payer; faces veto on legislation conflicting with judicially-interpreted Basic Law principles
 *   - Rights claimants: powerless but arbitrage-mobile beneficiaries; gain litigation access to invalidate legislation
 *   - Narrow coalitions: moderate-power payers; face heightened vulnerability to judicial invalidation of coalition-interest legislation
 *   - Political movements opposing judicial review: organized but excluded; actively contest the reading's legitimacy and seek institutional change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Law Judicial Supremacy: Court-Enforced Constitutional Hierarchy").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '1d6c581a-5f73-484e-8d57-c9564e025ad7').
narrative_ontology:cs_kernel_codification('1d6c581a-5f73-484e-8d57-c9564e025ad7', fixed_text).
narrative_ontology:cs_authority_grounding('1d6c581a-5f73-484e-8d57-c9564e025ad7', lineage).
narrative_ontology:cs_interpretation_layer_present('1d6c581a-5f73-484e-8d57-c9564e025ad7').
narrative_ontology:cs_reading_relation('1d6c581a-5f73-484e-8d57-c9564e025ad7', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1d6c581a-5f73-484e-8d57-c9564e025ad7', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('1d6c581a-5f73-484e-8d57-c9564e025ad7', foundational, basic_laws_constitutional_supremacy).
narrative_ontology:cs_axiom_status(basic_laws_constitutional_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1d6c581a-5f73-484e-8d57-c9564e025ad7', basic_laws_constitutional_supremacy, deontological).
narrative_ontology:cs_axiom('1d6c581a-5f73-484e-8d57-c9564e025ad7', foundational, judicial_review_necessary_enforcement).
narrative_ontology:cs_axiom_status(judicial_review_necessary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('1d6c581a-5f73-484e-8d57-c9564e025ad7', judicial_review_necessary_enforcement, instrumental).
narrative_ontology:cs_reference_frame('1d6c581a-5f73-484e-8d57-c9564e025ad7', constitutional_hierarchy_with_judicial_guardianship).
narrative_ontology:cs_drift_state('1d6c581a-5f73-484e-8d57-c9564e025ad7', contemporary_institutional_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1d6c581a-5f73-484e-8d57-c9564e025ad7', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institutional_authority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1992, early judicial review claiming) to 0.68 (2024, established doctrine) because the Court's authority to veto legislation has hardened into institutional practice; Knesset majorities have internalized constitutional constraint and conduct pre-screening; and the range of legislation subject to potential challenge has broadened. Theater_ratio is low but rising (0.08→0.22), reflecting that while security/legitimacy review is genuine, enforcement increasingly defends the court's interpretive supremacy rather than purely protecting rights—performative constitutional rhetoric around 'protecting democracy' has grown. Suppression_requirement rises (0.38→0.55) because the institutional machinery maintaining judicial supremacy has had to strengthen as parliamentary sovereignty advocates gain electoral leverage; the constraint is no longer self-evident and requires active institutional defense. Accessibility_collapse is moderately high (0.72) because once the court asserts authority to invalidate legislation, Knesset majorities' alternatives collapse—they can amend Basic Laws (high friction, supermajority required), ignore court orders (legitimacy cost, international pressure), or accept the veto. Resistance is moderate (0.58) because parliamentary sovereignty movements mounted strong institutional and electoral challenges, especially post-2015.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat: judicial review is a necessary institutional innovation protecting rights against majoritarian excess. The coordination problem is real—without an authoritative interpreter, each Knesset session could redefine foundational principles. Theater is minimal; enforcement is legitimate. From the Knesset majority's seat: judicial review is an institutional constraint that converts judicial preferences into veto power, narrows legislative autonomy, and requires the majority to negotiate with the court or pursue supermajority amendment. The extracted value is legislative authority; the coordination story is cover for institutional expansion. From rights claimants' seat: the constraint provides a real protection mechanism, but access is unequal—arbitrage concentrates benefits among resourced constituencies.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court benefits from this constraint (collects institutional authority, can shape policy via rights interpretation) and sets its terms; it is the primary beneficiary and agenda-setter. The Knesset legislative majority bears the cost: legislation can be invalidated, policy space is constrained, and supermajority processes (which it may not control) become necessary to override courts. Rights claimants benefit from litigation access and substantive rights protection, but face arbitrage costs (legal fees, institutional resources required to weaponize judicial review). The constraint is tangled rope because: (1) genuine coordination exists (stable constitutional framework, singular authoritative interpreter); (2) asymmetric extraction is clear (Knesset bears cost, court collects authority); (3) active enforcement is required (court must issue binding decisions, Knesset must comply despite preferring not to). The Knesset could theoretically exit by amending Basic Laws (trapped→constrained shift), but supermajority requirement and international pressure (legitimacy cost) keep exit option narrow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (lack of enforceable constitutional framework protecting rights against majoritarian erosion) was live at the constraint's inception (1992). By 2024, the problem status is contested: the Court and rights advocates argue the problem remains live (recent coalition governments have attempted legislation conflicting with Basic Law principles, invalidated by courts); parliamentary sovereignty advocates argue the problem has shifted (constitutional norms and international pressure now constrain majorities more effectively than courts, and judicial review has become the primary problem, not its solution). The theater_ratio's rise (0.08→0.22) suggests performative constitutional protection is growing relative to functional rights-safeguarding. The measured extraction (0.68) exceeding the coordination cost suggests the constraint is carrying rent-seeking by the Court beyond what the founding problem requires. This does NOT trigger automatic mandatrophy classification—the coordination function is real—but it indicates the constraint is drifting toward snare characteristics on the Knesset's seat. The divergent founding_problem_status assessment (live vs. dead) between the Court and parliamentary sovereignty advocates is itself evidence of institutional contest: the constraint's legitimacy depends on whether you believe the problem it was built to solve persists, and that question is now politically live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contention,
    'Is the Basic Law framework a higher-order constitutional structure that legitimately constrains the Knesset (judicial supremacy), or does ultimate sovereign authority remain with elected majorities (parliamentary sovereignty), or is legitimate authority genuinely shared with courts holding bounded jurisdiction (balanced contestation)?',
    'This omega documents the fundamental kernel contest itself. Sibling readings instantiate the alternative commitments (parliamentary_sovereignty_reading and balanced_contestation_reading). Resolution requires the Israeli political and constitutional system to settle which framing becomes institutionally dominant—a matter of constitutional amendment, Supreme Court doctrine shift, or regime change rather than empirical fact-finding.',
    'If this reading is displaced by parliamentary sovereignty reading, the Supreme Court loses authority to invalidate legislation; if displaced by balanced contestation, courts gain bounded but not supremacist jurisdiction. The current reading (judicial_supremacy) produces the highest ε for Knesset legislative majorities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Which framework the Israeli system instantiates: judicial supremacy, parliamentary sovereignty, or balanced contestation.').

omega_variable(
    marginal_coordination_vs_extraction,
    'What proportion of the measured extractiveness (0.68) reflects the genuine coordination cost of having a stable, independent court-enforced constitutional framework, versus extractiveness from the court wielding interpretive authority beyond the scope needed for rights protection?',
    'Comparative institutional analysis: jurisdictions with similar-strength judicial review but lower measured extractiveness (e.g., Germany, Canada) could illuminate what portion is structural necessity and what portion is Israeli-specific institutional design. Analysis of court decisions that broaden rights protection without being compelled by Basic Law text could separate genuine coordination cost from discretionary expansion.',
    'A high-coordination-cost finding would support the judicial supremacy framing as necessary for constitutional stability. A high-discretionary-expansion finding would suggest the constraint carries substantial extractive rent-seeking by the court itself, shifting classification toward snare on the coordinated seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_coordination_vs_extraction, empirical, 'Whether extractiveness reflects coordination necessity or judicial expansion beyond textual warrant.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the Knesset''s compliance with judicial invalidation structurally enforced (lacking institutional capacity to ignore court orders, bounded by international pressure and domestic legitimacy costs), or has it become internalized (the legislature now self-censors and pre-clears legislation, incorporating court preferences into drafting without awaiting invalidation)?',
    'Post-decision data: instances where the Knesset has ignored, delayed implementing, or explicitly defied a Supreme Court invalidation decision would indicate the suppression is structurally maintained. Conversely, evidence that legislation is systematically pre-screened for court defensibility and draft rejection occurs in legislative committee rather than in court would indicate internalization.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the authored 0.55 suggests, and the Knesset''s perceived exit options are more constrained than structural analysis alone would show. The theater_ratio rising (from 0.08 to 0.22) may reflect internalization: fewer cases reach court because pre-screening catches conflicts earlier, reducing visible enforcement activity while expanding the court''s effective reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether judicial suppression operates structurally or has become internalized in legislative behavior.').

omega_variable(
    rights_claimant_arbitrage_inequality,
    'The rights_claimants stakeholder is authored with exit_options=arbitrage, reflecting that litigation access requires legal resources and institutional connectivity. Does this produce a systematic pattern where wealthy, organized interests and those with media platforms gain effective veto rights via litigation, while dispersed or marginalized constituencies lack the arbitrage resources to weaponize judicial review?',
    'Empirical audit of successful constitutional litigation: classification of claimants by wealth, organizational backing, and media connectivity. High correlation between resources and successful challenges would indicate arbitrage is not equally distributed—the constraint protects some rights-claimants far more than others, concentrating the beneficiary function.',
    'High arbitrage inequality would suggest rights_claimants is not a unified beneficiary group; it should split into resourced_rights_claimants and marginalized_constituencies. The constraint would show markedly different extraction profiles depending on which claimant seat is examined, complicating seat divergence analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_claimant_arbitrage_inequality, empirical, 'Whether arbitrage-access to judicial review distributes equally among rights-claimants or concentrates among resourced constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(basi_tr_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.38).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2016, 0.52).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.18).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_process).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, israeli_constitutional_amendment_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel basic_law_interpretive_boundary. Each reading instantiates a different constitutional framing with distinct ε values and stakeholder structures. The judicial_supremacy reading (this constraint) holds that the Court is the authoritative interpreter of Basic Laws and can invalidate conflicting legislation; the parliamentary_sovereignty reading holds that the Knesset retains ultimate authority; the balanced_contestation reading holds that both institutions have legitimate bounded authority. The three readings are linked via network.affects_constraints to enable analysis of how the kernel contest shapes institutional behavior. They are NOT alternative measurements of a single constraint; they are distinct constraints grounded in different foundational commitments. Sibling readings will be authored separately with their own ε values, stakeholders, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
