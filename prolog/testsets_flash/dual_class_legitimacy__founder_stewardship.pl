% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Legitimacy: Founder Stewardship Reading
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint describes the 'founder stewardship' reading of dual-class
 *   share structures, where concentrated founder control is seen as
 *   legitimate because it enables long-horizon mission execution, benefiting
 *   all shareholders indirectly through sustained growth and strategic focus.
 *   This reading emphasizes the founder's fiduciary duty to the company's
 *   long-term health, rather than short-term share price. The dual-class
 *   structure is viewed as a coordination mechanism to protect the company
 *   from market pressures that might force suboptimal short-term decisions.
 *
 * KEY AGENTS:
 *   - founding_shareholders: Agenda setter (institutional/arbitrage) — maintains control, sets strategic direction.
 *   - class_a_shareholders: Beneficiary/Payer (powerful/mobile) — invest in the company, benefit from long-term growth, but lack proportional voting rights.
 *   - company_employees: Beneficiary (moderate/constrained) — benefit from stable, mission-driven employment and long-term company success.
 *   - institutional_investors: Observer (institutional/analytical) — evaluate governance structures, may advocate for single-class structures but also seek long-term returns.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.3).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.4).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Legitimacy: Founder Stewardship Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'a2a5d68f-0498-4d27-a079-09786dd5cfc7').
narrative_ontology:cs_kernel_codification('a2a5d68f-0498-4d27-a079-09786dd5cfc7', formalized).
narrative_ontology:cs_authority_grounding('a2a5d68f-0498-4d27-a079-09786dd5cfc7', lineage).
narrative_ontology:cs_interpretation_layer_present('a2a5d68f-0498-4d27-a079-09786dd5cfc7').
narrative_ontology:cs_reading_relation('a2a5d68f-0498-4d27-a079-09786dd5cfc7', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('a2a5d68f-0498-4d27-a079-09786dd5cfc7', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('a2a5d68f-0498-4d27-a079-09786dd5cfc7', foundational, founder_as_fiduciary_steward).
narrative_ontology:cs_axiom_status(founder_as_fiduciary_steward, holdable).
narrative_ontology:cs_axiom_grounding('a2a5d68f-0498-4d27-a079-09786dd5cfc7', founder_as_fiduciary_steward, deontological).
narrative_ontology:cs_axiom('a2a5d68f-0498-4d27-a079-09786dd5cfc7', foundational, long_term_value_creation_prioritized).
narrative_ontology:cs_axiom_status(long_term_value_creation_prioritized, holdable).
narrative_ontology:cs_axiom_grounding('a2a5d68f-0498-4d27-a079-09786dd5cfc7', long_term_value_creation_prioritized, instrumental).
narrative_ontology:cs_reference_frame('a2a5d68f-0498-4d27-a079-09786dd5cfc7', mission_driven_founder_control).
narrative_ontology:cs_drift_state('a2a5d68f-0498-4d27-a079-09786dd5cfc7', contemporary_governance_debate, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('a2a5d68f-0498-4d27-a079-09786dd5cfc7', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_term_mission_aligned_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, company_employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_term_value_creation_theory).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, mission_driven_governance_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares, maintaining control over strategic decisions and company mission. They are seen as stewards of the company's long-term vision, protected from short-term market pressures by the dual-class structure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold publicly traded shares with limited voting rights. They invest in the company expecting long-term value creation driven by the founder's vision, accepting the governance asymmetry as a trade-off for mission stability.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from a stable, mission-driven work environment and the potential for long-term career growth and equity value. Their interests are aligned with the company's sustained success under founder leadership.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, company_employees, beneficiary,
    moderate, biographical, constrained, national).

% Evaluate the governance structure for its impact on long-term returns. While some may prefer single-class structures, others invest in dual-class firms specifically for their potential to execute long-term strategies without short-term market interference.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_investors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founding_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term strategic vision and mission execution by insulating founders from short-term market pressures, thereby aligning all shareholders around sustained value creation.
% TRANSFER_FUNCTION: Transfers decision-making authority and control over strategic direction from public market shareholders to founding shareholders, in exchange for the promise of long-term mission-driven value creation.
% ABSENT_VOICES: Advocates for 'one share, one vote' principles and proponents of stronger minority shareholder rights are often excluded from the initial design and ongoing governance discussions of dual-class structures, as their views directly challenge the premise of founder control.
% DISAPPEARANCE_RATIONALE: If concentrated founder control vanished overnight, dual-class companies would immediately face pressure for short-term performance, potentially leading to changes in mission, strategy, and leadership. The market for mission-driven, long-horizon companies would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of short-term market pressures forcing founders to compromise long-term strategic vision and mission, leading to suboptimal outcomes for the company and its stakeholders.
% FOUNDING_PROBLEM_CORROBORATION: Founders and long-term investors attest that short-term market pressures remain a significant threat to mission-driven companies. Academic research on corporate governance and organizational economics, from outside the direct beneficiaries, also corroborates the existence of short-termism in public markets, supporting the rationale for structures that mitigate it.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, representing the potential for founders to capture some private benefits of control, but primarily reflecting the 'cost' of protecting the long-term mission from short-term market pressures. Suppression (0.4) is moderate, as Class A shareholders have limited ability to challenge founder decisions, but can exit the investment. Theater ratio (0.1) is low, as the structure is genuinely intended to serve its stated purpose of mission protection. The claimed type is Rope because, from this reading, the structure coordinates long-term vision and protects the company from short-termism, with all parties ultimately benefiting from mission success, even if control is asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of founding shareholders, the dual-class structure is a necessary coordination mechanism for long-term value creation. From the perspective of Class A shareholders, it is a constraint that limits their governance rights, but they accept it for the potential long-term benefits. The divergence arises when the 'stewardship' function is questioned, leading to other readings of the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are clear beneficiaries (d=0.0-0.1) as they retain control and guide the company's mission. Class A shareholders are beneficiaries (d=0.2-0.3) in this reading, as they are presumed to benefit from the long-term mission execution, despite their limited voting power. Company employees are also beneficiaries (d=0.1-0.2) due to stable employment and mission alignment. There are no direct 'victims' in this reading, as the structure is framed as serving all shareholders' long-term interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as pure extraction by emphasizing the genuine coordination function of protecting long-term mission. Mandatrophy would occur if the founder's stewardship failed to deliver long-term value, or if the mission became irrelevant, but the control structure persisted due to inertia or self-interest. The 'founding_problem_status' being 'live' supports the ongoing relevance of the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_vs_extraction_ambiguity,
    'Is concentrated founder control primarily a mechanism for long-term stewardship and mission execution, or does it enable extraction from minority shareholders?',
    'Empirical analysis of dual-class firms'' long-term performance, founder compensation, and related-party transactions compared to single-class peers, controlling for industry and firm age. Also, analysis of founder exit events and their impact on minority shareholders.',
    'If primarily stewardship, the constraint is a Rope (coordination); if primarily extraction, it is a Snare. This reading (founder_stewardship) asserts the former, but the ambiguity is irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_stewardship_vs_extraction_ambiguity, empirical, 'Ambiguity between founder stewardship and minority extraction in dual-class structures.').

omega_variable(
    dual_class_legitimacy_kernel_reading,
    'This constraint is the ''founder_stewardship'' reading of the ''dual_class_legitimacy'' kernel. What would change if the ''minority_extraction'' reading were adopted?',
    'Legal precedent or regulatory action explicitly re-interpreting fiduciary duties in dual-class structures to prioritize proportional governance over founder control.',
    'If the ''minority_extraction'' reading were adopted, the constraint would reclassify from Rope to Snare, with founder shareholders becoming primary beneficiaries of extraction and Class A shareholders becoming victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_class_legitimacy_kernel_reading, conceptual, 'Impact of adopting the ''minority_extraction'' reading of the dual_class_legitimacy kernel.').

omega_variable(
    dual_class_legitimacy_kernel_reading_disclosure,
    'This constraint is the ''founder_stewardship'' reading of the ''dual_class_legitimacy'' kernel. What would change if the ''disclosure_consent'' reading were adopted?',
    'Legal precedent or regulatory action explicitly stating that robust disclosure at IPO is sufficient to legitimize any governance structure, regardless of ongoing proportional representation.',
    'If the ''disclosure_consent'' reading were adopted, the constraint''s legitimacy would shift from ongoing founder stewardship to the initial informed consent of investors. This would likely reinforce the Rope classification by emphasizing the voluntary nature of initial investment, but might reduce pressure for ongoing founder accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_class_legitimacy_kernel_reading_disclosure, conceptual, 'Impact of adopting the ''disclosure_consent'' reading of the dual_class_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.1).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on founder stewardship. Other readings include 'minority_extraction' and 'disclosure_consent'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
