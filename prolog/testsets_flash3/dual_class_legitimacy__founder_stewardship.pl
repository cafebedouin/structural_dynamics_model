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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Founder Stewardship Justification for Dual-Class Shares
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'founder stewardship' reading of
 *   dual-class share structures, where concentrated founder control is seen
 *   as a legitimate mechanism to protect a company's long-term mission and
 *   ultimately benefit all shareholders. It argues that by insulating
 *   founders from short-term market pressures, the structure enables
 *   strategic execution that creates greater value over time. This reading
 *   emphasizes the coordination function of stable leadership and the
 *   indirect benefits to non-voting shareholders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.2).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.15).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.2).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Justification for Dual-Class Shares").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'b12c9833-dce0-4ad8-b41d-50c21def7e6a').
narrative_ontology:cs_kernel_codification('b12c9833-dce0-4ad8-b41d-50c21def7e6a', formalized).
narrative_ontology:cs_authority_grounding('b12c9833-dce0-4ad8-b41d-50c21def7e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('b12c9833-dce0-4ad8-b41d-50c21def7e6a').
narrative_ontology:cs_reading_relation('b12c9833-dce0-4ad8-b41d-50c21def7e6a', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('b12c9833-dce0-4ad8-b41d-50c21def7e6a', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('b12c9833-dce0-4ad8-b41d-50c21def7e6a', foundational, founder_vision_creates_long_term_value).
narrative_ontology:cs_axiom_status(founder_vision_creates_long_term_value, holdable).
narrative_ontology:cs_axiom_grounding('b12c9833-dce0-4ad8-b41d-50c21def7e6a', founder_vision_creates_long_term_value, empirically_contingent).
narrative_ontology:cs_axiom('b12c9833-dce0-4ad8-b41d-50c21def7e6a', foundational, insulation_from_short_termism_is_beneficial).
narrative_ontology:cs_axiom_status(insulation_from_short_termism_is_beneficial, holdable).
narrative_ontology:cs_axiom_grounding('b12c9833-dce0-4ad8-b41d-50c21def7e6a', insulation_from_short_termism_is_beneficial, empirically_contingent).
narrative_ontology:cs_reference_frame('b12c9833-dce0-4ad8-b41d-50c21def7e6a', stewardship_model_of_corporate_governance).
narrative_ontology:cs_drift_state('b12c9833-dce0-4ad8-b41d-50c21def7e6a', contemporary_market_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b12c9833-dce0-4ad8-b41d-50c21def7e6a', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_term_mission_aligned_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares, enabling them to control strategic direction and resist short-term market pressures. They are seen as stewards of the company's long-term vision and mission, benefiting from its sustained success.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold non-voting or low-voting shares. They benefit from the company's long-term value creation and mission execution, which is enabled by founder control, even if they lack direct governance input. Their benefit is indirect via mission success.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    moderate, biographical, mobile, global).

% Invest in dual-class companies, weighing the benefits of founder vision against potential governance risks. They monitor performance and may advocate for governance changes, but accept the founder control structure as a condition of investment.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_investors, observer,
    organized, biographical, constrained, global).

% Oversee the fairness and transparency of capital markets. They acknowledge the legality of dual-class structures but scrutinize disclosure and potential for abuse, balancing investor protection with corporate flexibility.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns corporate strategy with a long-term mission by insulating founders from short-term market demands, ensuring consistent execution of a vision that benefits all shareholders over time.
% TRANSFER_FUNCTION: Transfers decision-making authority and strategic stability to founding shareholders, in exchange for the potential for enhanced long-term value creation for all shareholders.
% ABSENT_VOICES: Short-term activist investors, who would prioritize immediate returns and challenge founder control, are effectively marginalized by the dual-class structure. They are present in the market but excluded from governance influence.
% DISAPPEARANCE_RATIONALE: If concentrated founder control vanished overnight, many mission-driven companies would face immediate pressure to prioritize short-term financial metrics, potentially compromising their long-term strategic goals and unique value propositions. This would fundamentally alter their operational and investment landscape.
% FOUNDING_PROBLEM: Public companies often face pressure from quarterly earnings cycles and activist investors, which can derail long-term strategic investments and mission-driven innovation.
% FOUNDING_PROBLEM_CORROBORATION: Founders and many long-term institutional investors attest that the problem of short-term market pressure remains live, citing examples of companies that lost their strategic direction after losing founder control. Academic research on corporate longevity and innovation also supports the claim that founder control can protect long-term vision.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.2) is low because, from this reading's perspective, the control premium is justified by the long-term value creation it enables for all shareholders. Suppression (0.15) is also low, as the structure is legally established and accepted by investors who choose to buy non-voting shares. Theater ratio is minimal (0.1) because the stated purpose of long-term mission execution is genuinely pursued. The claimed type is 'rope' because it is viewed as a coordination mechanism that benefits all participants, albeit with an asymmetry in governance rights.
 *
 * PERSPECTIVAL GAP:
 *   While founding shareholders perceive this as a pure coordination mechanism, minority shareholders (as per the 'minority_extraction' reading) might experience it as a form of extraction due to their lack of proportional governance rights. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are clear beneficiaries and agenda-setters, as they retain control and guide the company's mission (low d). Class A shareholders are also beneficiaries, as they are presumed to benefit from the long-term value creation, even without direct control (low d). Institutional investors and regulators are observers, analyzing the structure's effects without being direct targets or primary beneficiaries of the control mechanism itself (moderate d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_entrenchment,
    'Is concentrated founder control primarily a mechanism for long-term stewardship, or does it primarily serve to entrench founders and extract private benefits?',
    'Empirical studies tracking long-term performance, innovation, and founder compensation in dual-class vs. single-class companies, particularly after founder departure or succession events.',
    'If entrenchment is dominant, the constraint''s extractiveness would be reclassified as significantly higher, and its type would shift towards ''snare'' or ''tangled_rope'' for minority shareholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_entrenchment, empirical, 'Distinguishing genuine stewardship from self-serving entrenchment.').

omega_variable(
    mission_value_quantification,
    'How can the ''long-horizon mission execution'' value, which justifies founder control, be objectively quantified and attributed to the dual-class structure?',
    'Development of robust, non-financial metrics for mission impact and long-term innovation, correlated with dual-class structures, and compared against single-class peers.',
    'Lack of quantifiable mission value weakens the ''stewardship'' justification, potentially increasing perceived extractiveness for non-voting shareholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mission_value_quantification, conceptual, 'Quantifying the non-financial benefits of founder control.').

omega_variable(
    kernel_reading_founder_stewardship,
    'This constraint is the ''founder_stewardship'' reading of the ''dual_class_legitimacy'' kernel. How would the classification change under the ''minority_extraction'' or ''disclosure_consent'' readings?',
    'Analyzing the same structural data through the lens of the ''minority_extraction'' reading (focus on governance rights and proportional capital) or ''disclosure_consent'' reading (focus on informed investor choice).',
    'The ''minority_extraction'' reading would likely classify the constraint as a ''snare'' or ''tangled_rope'' due to high extraction from minority shareholders. The ''disclosure_consent'' reading might classify it as a ''rope'' or ''scaffold'' if disclosure is deemed sufficient for informed consent, but with higher suppression if exit options are limited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_founder_stewardship, conceptual, 'Impact of alternative readings of dual-class share legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(dual_be_t1980, dual_class_legitimacy__founder_stewardship, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(dual_be_t1990, dual_class_legitimacy__founder_stewardship, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(dual_be_t2000, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(dual_be_t2010, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2010, 0.19).
narrative_ontology:measurement(dual_be_t2024, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t1980, dual_class_legitimacy__founder_stewardship, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(dual_su_t1990, dual_class_legitimacy__founder_stewardship, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(dual_su_t2000, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2000, 0.13).
narrative_ontology:measurement(dual_su_t2010, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(dual_su_t2024, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('founder_stewardship') of the 'dual_class_legitimacy' kernel. Its sibling readings are 'minority_extraction' and 'disclosure_consent', each representing a distinct structural claim about the same underlying corporate governance mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
