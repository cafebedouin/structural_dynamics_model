% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Minority Shareholder Extraction in Dual-Class Structures
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'minority_extraction' reading of
 *   the 'dual_class_legitimacy' kernel. From this perspective, dual-class
 *   share structures, which grant disproportionate voting rights to founding
 *   or controlling shareholders, primarily function as a mechanism to extract
 *   governance value from minority shareholders. Despite claims of fostering
 *   long-term vision, the structure enables controlling parties to entrench
 *   themselves and make decisions that may not align with the broader
 *   shareholder base, while minority holders bear risk without voice. The
 *   constraint is classified as a Snare due to its high extractiveness and
 *   suppression of alternatives for minority shareholders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.85).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.9).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.85).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, snare).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Minority Shareholder Extraction in Dual-Class Structures").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '2b79870a-849f-4230-a0ad-72d4a7bfc2e2').
narrative_ontology:cs_kernel_codification('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', formalized).
narrative_ontology:cs_authority_grounding('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', extraction).
narrative_ontology:cs_interpretation_layer_present('2b79870a-849f-4230-a0ad-72d4a7bfc2e2').
narrative_ontology:cs_reading_relation('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_reading_relation('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', dual_class_legitimacy__founder_stewardship, forecloses).
narrative_ontology:cs_axiom('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', foundational, proportional_governance_right).
narrative_ontology:cs_axiom_status(proportional_governance_right, holdable).
narrative_ontology:cs_axiom_grounding('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', proportional_governance_right, deontological).
narrative_ontology:cs_axiom('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', foundational, control_as_economic_value).
narrative_ontology:cs_axiom_status(control_as_economic_value, holdable).
narrative_ontology:cs_axiom_grounding('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', control_as_economic_value, empirically_contingent).
narrative_ontology:cs_reference_frame('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', one_share_one_vote_principle).
narrative_ontology:cs_drift_state('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', contemporary_dual_class_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2b79870a-849f-4230-a0ad-72d4a7bfc2e2', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, controlling_insiders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares that grant disproportionate control over the company's governance, even with a minority economic stake. They set the board agenda, appoint directors, and can block proposals from other shareholders, effectively extracting governance value.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Often aligned with founding shareholders, these executives and board members benefit from concentrated control through executive compensation, strategic decisions that favor their interests, and entrenchment against hostile takeovers. They collect value without bearing proportional risk.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, controlling_insiders, beneficiary,
    institutional, biographical, arbitrage, global).

% Invest capital and bear economic risk but possess significantly less voting power than their equity stake would suggest. Their voice in governance is minimal, and they are subject to decisions made by controlling shareholders, often without recourse.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholders, payer,
    powerless, biographical, constrained, global).

% A broad class of individual and small institutional investors who purchase non-voting or low-voting shares. They are aware of the dual-class structure but accept it for access to the company's growth potential, effectively paying a control premium to founders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_investors, payer,
    moderate, biographical, constrained, global).

% Large pension funds, mutual funds, and asset managers who invest substantial capital in dual-class companies. Despite their size, their voting power is diluted, and their ability to influence governance is severely limited, leading to calls for one-share-one-vote reforms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_investors, payer,
    organized, biographical, constrained, global).

% Government bodies tasked with ensuring fair and transparent securities markets. While they enforce disclosure requirements, their mandate often limits their ability to intervene in corporate governance structures like dual-class shares, which are legally permissible.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Advocate for shareholder rights and one-share-one-vote principles. They publish research, engage with institutional investors, and lobby for regulatory changes to eliminate or limit dual-class structures, highlighting the extraction of value from minority holders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, corporate_governance_activists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to coordinate long-term strategic vision and stability for the company by insulating founders from short-term market pressures and activist demands.
% TRANSFER_FUNCTION: Transfers effective governance power and associated economic value (e.g., control premium, strategic flexibility) from minority shareholders (who bear proportional capital risk) to founding and controlling shareholders (who retain disproportionate voting power).
% ABSENT_VOICES: Advocates for universal one-share-one-vote principles, pension funds demanding proportional representation, and academics highlighting governance failures are often excluded from direct influence over corporate charter decisions, despite their significant economic stake or analytical insights.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight, corporate governance would fundamentally shift towards proportional representation. This would likely alter capital allocation decisions, M&A activity, executive accountability, and potentially lead to a re-evaluation of company valuations based on governance risk. The entire landscape of corporate control would reorganize.
% FOUNDING_PROBLEM: To allow founders to retain control and pursue long-term vision and mission-driven goals, even after taking their companies public, by insulating them from short-term market pressures and hostile takeovers.
% FOUNDING_PROBLEM_CORROBORATION: Founders and company boards attest that the problem of short-termism and hostile takeovers is still live, justifying dual-class structures. Minority shareholders, governance activists, and some academics attest that the founding problem is largely solved or that the structure now primarily serves entrenchment, citing lack of accountability and potential for value destruction. Legislative hearings and independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because minority shareholders contribute capital and bear risk but receive significantly less governance influence, representing a clear transfer of value. Suppression is very high (0.90) as legal and corporate charter provisions actively prevent minority shareholders from gaining proportional voice or challenging controlling interests, with few viable exit options that don't involve significant loss. The theater ratio is moderate (0.40) because while formal governance structures (e.g., shareholder meetings, board oversight) exist, their actual function for minority shareholders is often performative, masking the underlying power imbalance. The measurement series show a gradual increase in both extraction and suppression over time, reflecting the hardening of these structures and the increasing entrenchment of control.
 *
 * PERSPECTIVAL GAP:
 *   The 'minority_extraction' reading fundamentally diverges from the 'founder_stewardship' reading. From the perspective of founding shareholders, the dual-class structure is a Rope or Scaffold, enabling long-term value creation and protecting the company's mission. From the perspective of minority shareholders, it is a Snare, a mechanism for concentrated control to extract value and suppress dissent. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders and controlling insiders are clear beneficiaries (d near 0.0) as they gain disproportionate control and associated economic value. Minority shareholders, public investors, and institutional investors are targets (d near 1.0) as they bear risk and contribute capital without commensurate governance power. Securities regulators and governance activists act as observers, analyzing the constraint's operation without directly benefiting or being targeted by its primary extractive function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_premium_valuation,
    'Is the ''control premium'' embedded in dual-class shares a fair compensation for founder stewardship and long-term vision, or an uncompensated transfer of value from minority shareholders?',
    'Empirical studies comparing long-term performance and shareholder returns of dual-class vs. single-class companies, adjusted for industry and founder quality, or regulatory mandates for sunset clauses on dual-class structures.',
    'If it''s an uncompensated transfer, the constraint''s extractiveness is confirmed as high, supporting its Snare classification. If it''s fair compensation, the extractiveness might be lower, suggesting a more complex Tangled Rope or even Rope classification from a different perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_valuation, empirical, 'Whether the value of control in dual-class structures is justified or extractive.').

omega_variable(
    long_term_value_creation,
    'Does concentrated founder control, enabled by dual-class structures, actually lead to superior long-term value creation for *all* shareholders, or primarily for controlling shareholders?',
    'Longitudinal studies tracking total shareholder returns (TSR) and governance-adjusted valuations for dual-class companies over multiple decades, compared to single-class peers, with careful controls for founder quality and industry dynamics.',
    'If superior value for all shareholders is consistently demonstrated, the ''founder_stewardship'' reading gains empirical support, potentially reclassifying the constraint as a Tangled Rope (coordination + some extraction). If value accrues disproportionately to controlling shareholders, the ''minority_extraction'' reading is strengthened, reinforcing the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_value_creation, empirical, 'Assessing the actual beneficiaries of long-term value creation under dual-class structures.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of minority shareholder voice primarily structural (legal/charter provisions) or internalized (investors accepting the status quo due to perceived benefits or lack of collective action)?',
    'Analysis of investor behavior in response to governance reforms (e.g., sunset clauses, enhanced disclosure) and the efficacy of shareholder activism in challenging dual-class structures. If voice remains suppressed even with structural changes, internalized factors are more dominant.',
    'If suppression is largely internalized, the effective suppression is higher than the structural measure suggests, as investors carry the suppression with them. If it''s purely structural, legal reforms would be more effective in altering the constraint''s nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of minority shareholder voice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.3).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.35).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dual_tr_t32, dual_class_legitimacy__minority_extraction, theater_ratio, 32, 0.39).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__minority_extraction, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__minority_extraction, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__minority_extraction, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.83).
narrative_ontology:measurement(dual_be_t32, dual_class_legitimacy__minority_extraction, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__minority_extraction, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__minority_extraction, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__minority_extraction, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(dual_su_t32, dual_class_legitimacy__minority_extraction, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(dual_su_t40, dual_class_legitimacy__minority_extraction, suppression_requirement, 40, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, securities_disclosure_requirements).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, corporate_board_composition).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, corporate_takeover_defenses).

% DUAL FORMULATION NOTE:
% This constraint is the 'minority_extraction' reading of the 'dual_class_legitimacy' kernel, which also includes 'founder_stewardship' and 'disclosure_consent' readings. Each reading offers a distinct structural interpretation of the same underlying corporate governance mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
