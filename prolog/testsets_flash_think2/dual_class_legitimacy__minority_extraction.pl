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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Minority Shareholder Extraction in Dual-Class Structures
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'minority_extraction' reading of
 *   the 'dual_class_legitimacy' kernel. From this perspective, dual-class
 *   share structures, which grant disproportionate voting power to founding
 *   or controlling shareholders, are a mechanism for extracting governance
 *   value from minority shareholders. The constraint is the legal and
 *   corporate framework that permits and enforces these structures, allowing
 *   controlling parties to bear less risk while retaining control and
 *   associated economic benefits, at the expense of public investors who bear
 *   full economic risk with limited voice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.85).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.9).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.85).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, snare).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Minority Shareholder Extraction in Dual-Class Structures").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'f22c8599-d4d6-464f-b883-c5e31f4f4b70').
narrative_ontology:cs_kernel_codification('f22c8599-d4d6-464f-b883-c5e31f4f4b70', formalized).
narrative_ontology:cs_authority_grounding('f22c8599-d4d6-464f-b883-c5e31f4f4b70', extraction).
narrative_ontology:cs_interpretation_layer_present('f22c8599-d4d6-464f-b883-c5e31f4f4b70').
narrative_ontology:cs_reading_relation('f22c8599-d4d6-464f-b883-c5e31f4f4b70', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_reading_relation('f22c8599-d4d6-464f-b883-c5e31f4f4b70', dual_class_legitimacy__founder_stewardship, forecloses).
narrative_ontology:cs_axiom('f22c8599-d4d6-464f-b883-c5e31f4f4b70', foundational, proportional_governance_to_risk).
narrative_ontology:cs_axiom_status(proportional_governance_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('f22c8599-d4d6-464f-b883-c5e31f4f4b70', proportional_governance_to_risk, deontological).
narrative_ontology:cs_axiom('f22c8599-d4d6-464f-b883-c5e31f4f4b70', foundational, control_as_economic_value_transfer).
narrative_ontology:cs_axiom_status(control_as_economic_value_transfer, holdable).
narrative_ontology:cs_axiom_grounding('f22c8599-d4d6-464f-b883-c5e31f4f4b70', control_as_economic_value_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('f22c8599-d4d6-464f-b883-c5e31f4f4b70', one_share_one_vote_principle).
narrative_ontology:cs_drift_state('f22c8599-d4d6-464f-b883-c5e31f4f4b70', contemporary_dual_class_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f22c8599-d4d6-464f-b883-c5e31f4f4b70', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, controlling_insiders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, corporate_boards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares that grant disproportionate control relative to their economic stake. They set the strategic direction, appoint the board, and are insulated from market pressures, often citing long-term vision. They directly benefit from the governance premium and control over corporate assets.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_shareholders, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Are often aligned with founding shareholders, holding executive positions or board seats. They benefit from the stability of control, reduced accountability to public markets, and potential for private benefits of control, even if their economic stake is small.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, controlling_insiders, beneficiary,
    institutional, biographical, arbitrage, global).

% Own common shares with limited or no voting rights, bearing full economic risk without proportional governance influence. They are unable to meaningfully influence corporate decisions, elect directors, or challenge management, and their only 'exit' is to sell shares, often at a discount due to the control premium.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholders, payer,
    powerless, biographical, trapped, global).

% Include institutional funds and retail investors who invest in dual-class companies. While some accept the structure for growth potential, they bear the cost of diluted governance rights and potential for agency costs. Their exit is constrained by market liquidity and the control discount.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_investors, payer,
    moderate, biographical, constrained, global).

% Oversee capital markets and corporate disclosures. They grapple with balancing investor protection against corporate flexibility, often relying on disclosure as the primary mechanism to address governance imbalances, but facing calls for more substantive intervention.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Are often dominated by appointees of the controlling shareholders, limiting their independence. While ostensibly representing all shareholders, their primary accountability is often to the controlling block, leading to decisions that may not optimize for minority interests.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, corporate_boards, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, corporate_boards, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates capital formation for companies while allowing founders to retain control and pursue long-term strategic visions, ostensibly coordinating patient capital with entrepreneurial leadership.
% TRANSFER_FUNCTION: Transfers governance control and associated economic value (e.g., through related-party transactions, entrenchment of management, or lower accountability) from minority shareholders to founding and controlling shareholders.
% ABSENT_VOICES: Advocates for 'one-share, one-vote' principles, proxy advisory firms, and institutional investors who are structurally excluded from meaningful influence in dual-class companies. They would argue for governance parity and greater accountability.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight, capital markets would reprice companies based on proportional governance, controlling shareholders would lose their disproportionate power, and corporate governance practices would shift towards greater accountability to all shareholders. This would fundamentally alter the power dynamics and valuation of many public companies.
% FOUNDING_PROBLEM: To allow founders of innovative companies to raise public capital without sacrificing their long-term vision or succumbing to short-term market pressures and activist investors.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their allies attest that the problem of short-termism and activist pressure remains live. Minority shareholders, governance advocates, and some academic studies attest that the founding problem is largely a cover for entrenchment and extraction, with the arrangement persisting beyond its original justification.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because minority shareholders bear full economic risk but are systematically denied proportional governance rights, leading to a transfer of control premium and potential for agency costs. Suppression is very high (0.90) as minority shareholders are legally and structurally disempowered, with few effective avenues to challenge controlling interests or exit without penalty. Theater ratio is moderate (0.45) as the 'long-term vision' and 'founder protection' narratives often serve as a performative cover for what is, in practice, entrenchment and extraction. Accessibility collapse is high (0.80) because the legal and market structures offer very limited alternatives for minority shareholders to gain proportional governance. Resistance is moderate (0.60) due to ongoing advocacy by governance groups and some institutional investors, but this resistance faces significant structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of founding and controlling shareholders, the dual-class structure is a legitimate mechanism for long-term value creation and protection of strategic vision. From the perspective of minority shareholders, it is a clear instance of extraction where their capital and risk are not matched by governance rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding and controlling shareholders are clear beneficiaries (low d) as they retain control and associated economic value. Minority shareholders and public investors are targets (high d) as they bear the costs of diluted governance and potential agency problems. Securities regulators are observers (analytical d), tasked with balancing competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint best understood as an instance of ''minority_extraction'' within the ''dual_class_legitimacy'' kernel, or is another reading more appropriate?',
    'Analysis of empirical evidence regarding agency costs, control premia, and long-term performance in dual-class vs. single-class companies, combined with legal and economic interpretations of shareholder rights.',
    'If the ''founder_stewardship'' reading were adopted, the constraint might be reclassified as a Rope or Tangled Rope, emphasizing coordination benefits. If ''disclosure_consent'' were adopted, the focus might shift to the adequacy of disclosure rather than the structure itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the dual-class legitimacy kernel.').

omega_variable(
    long_term_vision_vs_entrenchment,
    'To what extent does concentrated control genuinely enable long-term vision and innovation, versus merely entrenching management and facilitating private benefits of control?',
    'Longitudinal studies comparing R&D investment, innovation output, and shareholder returns of dual-class vs. single-class firms, controlling for industry and firm characteristics. Analysis of governance events (e.g., related-party transactions, executive compensation) in dual-class firms.',
    'Strong evidence for entrenchment would increase the measured extractiveness and suppression, solidifying the Snare classification. Strong evidence for genuine long-term benefits might shift the classification towards a Tangled Rope, acknowledging a coordination function alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_vision_vs_entrenchment, empirical, 'Distinguishes between the claimed benefits of dual-class structures and their potential for entrenchment.').

omega_variable(
    disclosure_adequacy_vs_structural_inequality,
    'Is robust disclosure sufficient to mitigate the risks to minority shareholders in dual-class structures, or does the structural inequality of voting rights inherently lead to extraction regardless of disclosure?',
    'Empirical studies on the effectiveness of enhanced disclosure in protecting minority shareholders in dual-class firms, and legal analysis of whether ''informed consent'' can truly legitimize a structure that fundamentally denies proportional governance.',
    'If disclosure is found to be largely ineffective, it reinforces the Snare classification by highlighting the irremediable nature of the structural extraction. If disclosure is found to be highly effective, it might suggest a re-evaluation of the suppression metric and the overall extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_adequacy_vs_structural_inequality, conceptual, 'Examines whether disclosure can compensate for structural governance inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__minority_extraction, theater_ratio, 6, 0.35).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.4).
narrative_ontology:measurement(dual_tr_t18, dual_class_legitimacy__minority_extraction, theater_ratio, 18, 0.42).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.44).
narrative_ontology:measurement(dual_tr_t30, dual_class_legitimacy__minority_extraction, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__minority_extraction, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__minority_extraction, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(dual_be_t18, dual_class_legitimacy__minority_extraction, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(dual_be_t30, dual_class_legitimacy__minority_extraction, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__minority_extraction, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__minority_extraction, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(dual_su_t18, dual_class_legitimacy__minority_extraction, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(dual_su_t30, dual_class_legitimacy__minority_extraction, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
