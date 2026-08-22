% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Dual-Class Legitimacy: Minority Extraction Reading
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint story instantiates the minority_extraction reading of the
 *   dual_class_legitimacy kernel. The standing arrangement under contest is
 *   the dual-class share structure and controlled-company exemptions that
 *   allocate super-voting rights to founder-controllers while restricting
 *   Class A minority shareholders to inferior governance rights. From this
 *   reading, the constraint is a mechanism for extracting governance value
 *   and control premiums from public minority capital-bearers to insiders who
 *   retain decision-making power disproportionate to economic risk. The
 *   colloquial label 'dual-class structure' conflates three structurally
 *   distinct claimsâfounder stewardship, disclosure consent, and minority
 *   extraction; this story isolates the latter and authors its metrics
 *   independently of the sibling readings.
 *
 * KEY AGENTS:
 *   - founder_controllers (agenda_setter / beneficiary): powerful, mobile â hold super-voting shares and entrench control while bearing minority economic interest
 *   - class_a_minority_shareholders (payer): organized, mobile â bear proportionate risk without proportional governance
 *   - passive_institutional_investors (payer): institutional, constrained â bound by index mandates to hold misaligned governance structures
 *   - securities_regulators (observer): institutional, analytical â enforce disclosure and controlled-company exemptions that strip mandatory protections
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.78).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.72).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, snare).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Legitimacy: Minority Extraction Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'f729f4e8-19e0-4dcc-8995-f8743ec91fc7').
narrative_ontology:cs_kernel_codification('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', formalized).
narrative_ontology:cs_authority_grounding('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', extraction).
narrative_ontology:cs_interpretation_layer_present('f729f4e8-19e0-4dcc-8995-f8743ec91fc7').
narrative_ontology:cs_reading_relation('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', foundational, governance_proportional_to_capital_and_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_capital_and_risk, holdable).
narrative_ontology:cs_axiom_grounding('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', governance_proportional_to_capital_and_risk, deontological).
narrative_ontology:cs_axiom('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', foundational, one_share_one_vote_default).
narrative_ontology:cs_axiom_status(one_share_one_vote_default, holdable).
narrative_ontology:cs_axiom_grounding('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', one_share_one_vote_default, conventional).
narrative_ontology:cs_reference_frame('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', proportional_governance_default).
narrative_ontology:cs_drift_state('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', post_dual_class_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f729f4e8-19e0-4dcc-8995-f8743ec91fc7', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, class_a_minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, passive_institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares that entrench control over board composition, mergers, and charter amendments while economically owning a minority of cash-flow rights. They set the terms of the dual-class structure at IPO and resist sunset provisions or equal-voting mandates.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controllers, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_controllers, beneficiary).

% Hold inferior voting rights or no voting rights despite bearing proportionate economic risk and capital contribution. They receive governance disclosures and advisory votes but lack binding mechanisms to alter control structures or obtain proportional board representation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, class_a_minority_shareholders, payer,
    organized, biographical, mobile, national).

% Index funds and pension managers bound by tracking-error mandates to hold dual-class stocks even when governance is severely misaligned. They file shareholder proposals and occasionally support governance campaigns but are structurally unable to divest at scale without breaching investment mandates.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, passive_institutional_investors, payer,
    institutional, biographical, constrained, national).

% Enforce disclosure rules and controlled-company exemptions that permit dual-class structures to strip mandatory governance protections from minority holders. They evaluate listing standards and proxy rules but have not mandated proportional voting or sunset requirements.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates voting control with founders, nominally to protect long-term mission execution from short-term shareholder pressure and activist interference.
% TRANSFER_FUNCTION: Moves governance rights, board control, and merger-approval power from Class A minority shareholders to founder-controllers holding super-voting shares, while economic risk remains disproportionately borne by public capital.
% ABSENT_VOICES: Dispersed retail minority shareholders lack charter-amendment leverage; index funds are bound by tracking mandates that mute exit; proxy advisors and governance critics are heard but lack binding authority to mandate control sunsets.
% DISAPPEARANCE_RATIONALE: If dual-class control structures and controlled-company exemptions vanished overnight, founder control would collapse to economic interest, proxy contests and meaningful board elections would become possible, M&A premiums would distribute proportionally, and exchanges would face pressure to delist non-compliant control structures.
% FOUNDING_PROBLEM: How to raise public growth capital without surrendering founder control over corporate strategy, board composition, and long-term mission.
% FOUNDING_PROBLEM_CORROBORATION: Founders and venture capitalists attest the problem remains live. Independent corporate governance scholars, minority shareholder advocates, and institutional investor coalitions attest that the problem is solvable through time-limited sunsets, sunset-triggered conversion, and contractual governance mechanisms, and that the arrangement persists as control extraction rather than capital-raising necessity.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the dual-class structure decouples voting control from economic contribution, transferring governance rights to founders. Suppression (0.72) reflects the active legal and institutional machineryâcontrolled-company exemptions, charter amendment supermajorities, and exchange listing standardsâthat forecloses minority voice and protects the control premium. Theater ratio (0.48) captures the growth of performative governance (advisory say-on-pay votes, ESG committees, enhanced disclosure) that absorbs dissent without altering control allocation. Accessibility collapse (0.68) is substantial because meaningful alternatives (proportional governance, proxy contest success, or index-exclusion) are structurally unavailable once an investor holds the security. Resistance (0.55) is moderate and rising, driven by institutional investor coalitions and proxy advisor campaigns that challenge dual-class permanence.
 *
 * PERSPECTIVAL GAP:
 *   The founder_controllers seat experiences the constraint as legitimate capital-raising architecture that preserves mission integrity; the minority and institutional payer seats experience the same structure as extraction of governance value. The engine computes this divergence from structural dataâbeneficiary status, concentrated agenda-setting power, and divergent exit optionsârather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_controllers are declared beneficiaries with mobile exit and powerful position, yielding a low directionality value; the engine will treat their effective extraction as damped or inverted into subsidy. Class A minority shareholders are declared victims with organized power but mobile exit (can sell), yielding moderate-high directionality. Passive institutional investors are declared victims with constrained exit (index-tracking mandates), yielding higher directionality and thus amplified effective extraction. Securities regulators sit in an analytical seat with no beneficiary or victim tag; they default to the canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring named victims and asymmetric extraction for a snare classification. If the constraint were merely a rope, there would be no identifiable victim group paying governance value to an agenda-setter; if it were a mountain, it would emerge naturally and have no beneficiaries. The authored metrics independently describe high extraction and suppression, while the claimed type (snare) reflects the reading's structural assessment that the coordination story (founder stewardship) is cover for control extraction. A mandatrophy scenario would be a founder-stewardship claim where the problem the structure was built to solve is dead, yet the structure persists as rent extraction; this reading captures that as the current state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (minority_extraction) of the dual_class_legitimacy kernel. Would adopting the founder_stewardship or disclosure_consent reading change the epsilon referent or the beneficiary/victim structure?',
    'Compare sibling constraint stories for structural deltas in epsilon, beneficiaries, and victims.',
    'If sibling readings produce different epsilon values or beneficiary/victim mappings, the colloquial label conflates distinct constraints and the decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel reading decomposition validity').

omega_variable(
    founder_control_coordination_separability,
    'Is founder control structurally separable from the extraction of governance value, or is long-term stewardship achievable only through super-voting disproportionate to capital?',
    'Empirical analysis of firms with sunset-clause dual-class structures versus perpetual dual-class structures on long-term value creation metrics.',
    'If separable, the dual-class structure is not a necessary coordination cost and the measured extraction is pure transfer; if inseparable, part of the extraction may be coordination rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_control_coordination_separability, empirical, 'Whether founder coordination requires extraction').

omega_variable(
    disclosure_waiver_validity,
    'Does informed consent at IPO and ongoing disclosure cure the governance deficit for secondary-market minority shareholders who never directly consented?',
    'Legal and economic analysis of secondary-market pricing, investor protection doctrine, and empirical investor sophistication studies.',
    'If disclosure does not cure the deficit for subsequent holders, the legitimacy claim rests on a defective consent mechanism and extraction is intensified for trapped capital.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_waiver_validity, conceptual, 'Whether disclosure legitimates extraction for non-contracting shareholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.22).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.3).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dual_tr_t32, dual_class_legitimacy__minority_extraction, theater_ratio, 32, 0.44).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__minority_extraction, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__minority_extraction, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__minority_extraction, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(dual_be_t32, dual_class_legitimacy__minority_extraction, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__minority_extraction, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__minority_extraction, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__minority_extraction, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(dual_su_t32, dual_class_legitimacy__minority_extraction, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(dual_su_t40, dual_class_legitimacy__minority_extraction, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is the minority_extraction reading of the dual_class_legitimacy kernel, decomposed from the colloquial label 'dual-class share structure' which conflates founder stewardship, disclosure consent, and minority extraction claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
