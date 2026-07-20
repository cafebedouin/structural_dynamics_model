% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Governance Legitimacy via Disclosure and Informed Consent
 *   domain: corporate governance / securities law / organizational economics
 *
 * SUMMARY:
 *   This constraint models the dual-class equity structure as legitimated by
 *   the disclosure-consent reading of corporate governance: the Securities
 *   Act duty to disclose material facts, including voting-rights disparity,
 *   is treated as satisfying the firm's obligation to public shareholders.
 *   The structure is presented as a contractual choice in which Class A
 *   investors knowingly accept reduced governance rights. This reading
 *   competes with stewardship and minority-extraction readings of the same
 *   kernel.
 *
 * KEY AGENTS:
 *   - Founding insiders: structural beneficiaries who capture control rights disproportionate to economic ownership.
 *   - Public Class A shareholders: structural payers who bear equity risk with diluted or zero voting power.
 *   - Corporate issuer: agenda-setter that adopts and enforces the dual-class charter.
 *   - SEC regulators: analytical observer reviewing disclosure adequacy rather than substantive fairness.
 *   - Activist investors: excluded voice arguing that disclosure cannot cure governance disparity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.62).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.45).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Governance Legitimacy via Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate governance / securities law / organizational economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '28e8740d-c8f3-469d-bf3b-7bb2c946412a').
narrative_ontology:cs_kernel_codification('28e8740d-c8f3-469d-bf3b-7bb2c946412a', formalized).
narrative_ontology:cs_authority_grounding('28e8740d-c8f3-469d-bf3b-7bb2c946412a', lineage).
narrative_ontology:cs_interpretation_layer_present('28e8740d-c8f3-469d-bf3b-7bb2c946412a').
narrative_ontology:cs_reading_relation('28e8740d-c8f3-469d-bf3b-7bb2c946412a', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('28e8740d-c8f3-469d-bf3b-7bb2c946412a', dual_class_legitimacy__minority_extraction, forecloses).
narrative_ontology:cs_axiom('28e8740d-c8f3-469d-bf3b-7bb2c946412a', foundational, informed_consent_cures_control_parity_deficit).
narrative_ontology:cs_axiom_status(informed_consent_cures_control_parity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('28e8740d-c8f3-469d-bf3b-7bb2c946412a', informed_consent_cures_control_parity_deficit, conventional).
narrative_ontology:cs_axiom('28e8740d-c8f3-469d-bf3b-7bb2c946412a', secondary, governance_disparity_fully_priced).
narrative_ontology:cs_axiom_status(governance_disparity_fully_priced, holdable).
narrative_ontology:cs_axiom_grounding('28e8740d-c8f3-469d-bf3b-7bb2c946412a', governance_disparity_fully_priced, empirically_contingent).
narrative_ontology:cs_reference_frame('28e8740d-c8f3-469d-bf3b-7bb2c946412a', contractarian_disclosure_regime).
narrative_ontology:cs_drift_state('28e8740d-c8f3-469d-bf3b-7bb2c946412a', contemporary_governance_criticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28e8740d-c8f3-469d-bf3b-7bb2c946412a', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founding_insiders).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, public_class_a_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain disproportionate voting control through super-voting shares while accessing public capital via Class A issuance. Their control is insulated from ordinary market pressure by the charter, and they benefit from the legitimacy narrative that disclosure cures any governance deficit.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founding_insiders, beneficiary,
    powerful, biographical, constrained, national).

% Purchase shares with inferior or no voting rights after reviewing S-1 disclosure. They bear equity risk without proportional governance voice. Exit is technically available via public markets, but the governance discount may be imperfectly priced, and index mandates can force holding.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, public_class_a_shareholders, payer,
    moderate, biographical, mobile, global).

% Adopts and maintains dual-class charter provisions at the IPO stage. Files detailed Securities Act disclosures describing the voting-rights disparity. Asserts that informed consent and pricing fully satisfy duties to public shareholders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, corporate_issuer, agenda_setter,
    institutional, generational, constrained, national).

% Review S-1 filings for disclosure adequacy under the Securities Act. They do not adjudicate the fairness of the governance structure itself, only whether the disparity is accurately described. Their analytical seat treats the constraint through the lens of transparency rather than substantive governance parity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_regulators, observer,
    institutional, generational, analytical, national).

% Argue that no amount of disclosure legitimizes perpetual disproportionate voting rights. They are structurally overridden by the consent doctrine: courts and regulators treat their fairness objections as waived by the disclosure process, leaving them without a remedial path.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, activist_investors, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founders to access public capital while retaining decision-making control that they argue is necessary for long-term strategic execution, by offering investors a transparently disclosed governance discount.
% TRANSFER_FUNCTION: Transfers effective corporate control from public capital providers to founding insiders, while transferring capital from public markets to the corporation; the public bears equity risk with diluted governance rights.
% ABSENT_VOICES: Activist investors and governance reform advocates who argue that informed consent cannot cure a structural entitlement to proportional governance; they are present in the market but excluded from the regulatory remedy space by the disclosure-consent framework.
% DISAPPEARANCE_RATIONALE: If dual-class structures legitimated by disclosure consent disappeared overnight, founders would lose the primary mechanism for accessing public capital without surrendering control; IPO structures would shift toward single-class or sunset-governed models; and the market would reprice governance risk across the technology and media sectors.
% FOUNDING_PROBLEM: How to allow visionary founders to raise public equity capital without subjecting long-term corporate strategy to short-term market pressure or activist interference.
% FOUNDING_PROBLEM_CORROBORATION: Venture capital practitioners and certain corporate law scholars corroborate the founder-control problem as genuine. Institutional investors and governance critics corroborate that the problem is overstated and the disclosure-consent arrangement persists as entrenchment long after the founding rationale has faded.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the charter structurally transfers control from public shareholders to insiders regardless of consent formalities. Suppression (0.45) is moderate: the constraint suppresses alternatives not by hiding them but by foreclosing remedial challenges once disclosure is deemed adequate. Theater ratio (0.42) captures the performative dimension of the S-1 disclosure ritual, which produces thick documentation that may substitute for substantive governance parity. Resistance (0.35) is relatively low because the consent narrative deflects formal opposition into market exit rather than institutional challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the founder and issuer seat, the arrangement is a benign contractual choice with full transparency. From the public shareholder seat, the same structure is a governance right stripped away and replaced by a disclosure document. The engine computes this divergence from the structural data: the same charter provision yields low directionality for insiders and high directionality for public shareholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding insiders are structural beneficiaries (low d): the constraint subsidizes their control position. Public Class A shareholders are structural targets (high d): the constraint extracts governance rights from them. The SEC occupies an analytical seat with near-neutral d. Activist investors are excluded from the bargaining framework and would compute as high-d targets if seated.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function: dual-class structures do solve a real principal-agent problem by insulating long-term decision-making from short-term market pressure. The mandatrophy guard prevents misreading the entire arrangement as pure extraction. However, the consent reading risks masking the extraction half by treating disclosure as a full substitute for parity. The metrics are authored to reflect that the extraction is structurally present even when the coordination is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_genuineness,
    'Is the consent of Class A shareholders genuinely informed and rational, or do behavioral biases, complexity, and passive indexing obscure the governance discount?',
    'Empirical analysis of retail investor comprehension of S-1 governance provisions; measurement of tracking error and mandate constraints on institutional investors who cannot exit.',
    'If consent is largely illusory, the extraction metric understates the constraint''s effective force and the false-summit signature for contractual legitimacy strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_genuineness, empirical, 'Whether informed consent is structurally genuine or a performative legal fiction.').

omega_variable(
    pricing_efficiency,
    'Is the governance disparity fully priced into Class A shares, or does market segmentation and index inclusion mean investors bear unpriced governance risk?',
    'Event studies around governance shocks in dual-class firms; comparison of Class A returns versus single-class peers controlling for sector and growth profiles.',
    'If the disparity is underpriced, the extraction is partly hidden in expected returns, raising effective extraction for investors who do not consciously accept the discount.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_efficiency, empirical, 'Whether the governance discount is actually reflected in share pricing.').

omega_variable(
    kernel_reading_boundary,
    'Does the disclosure-consent reading of dual-class legitimacy foreclose the minority-extraction reading, or do both remain live in contemporary corporate law discourse?',
    'Jurisprudential analysis of judicial treatment of dual-class challenges: do courts treat disclosure as a complete defense, or do they leave room for substantive fairness review?',
    'If both readings remain live, the kernel is contested rather than settled; if disclosure-consent forecloses minority-extraction in practice, the authority of the latter is eroded despite its theoretical holdability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Committer uncertainty about the foreclosure relationship between sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__disclosure_consent, theater_ratio, 8, 0.28).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__disclosure_consent, theater_ratio, 16, 0.32).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__disclosure_consent, theater_ratio, 24, 0.36).
narrative_ontology:measurement(dual_tr_t32, dual_class_legitimacy__disclosure_consent, theater_ratio, 32, 0.39).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__disclosure_consent, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__disclosure_consent, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__disclosure_consent, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__disclosure_consent, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(dual_be_t32, dual_class_legitimacy__disclosure_consent, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__disclosure_consent, base_extractiveness, 40, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% This constraint is the disclosure_consent reading of the dual_class_legitimacy kernel. The kernel decomposes into three structurally distinct constraints because the legitimating basis for dual-class governance changes the beneficiary/victim structure and the epsilon value. This reading treats the arrangement as consensual contractual choice; sibling readings treat it as coordination (stewardship) or extraction (entitlement violation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
