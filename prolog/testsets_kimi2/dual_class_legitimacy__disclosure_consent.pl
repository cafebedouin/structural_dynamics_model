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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy via Securities Disclosure and Informed Consent
 *   domain: corporate governance / securities law
 *
 * SUMMARY:
 *   This constraint instantiates the disclosure_consent reading of the
 *   dual_class_legitimacy kernel. It treats the legitimacy of dual-class
 *   share structures as deriving from the Securities Act's mandatory
 *   disclosure regime: investors receive an S-1 that fully describes the
 *   governance disparity, price it into their investment decisions, and
 *   thereby consent to the arrangement. Under this reading, control parity is
 *   unnecessary because legitimacy is procedural and contractual rather than
 *   substantive. The reading stands in contrast to sibling readings that
 *   frame the same arrangement as founder stewardship (coordination) or
 *   minority extraction (extraction).
 *
 * KEY AGENTS:
 *   - securities_regulators (agenda_setter / institutional / analytical) â administer and enforce the disclosure framework
 *   - dual_class_issuers (beneficiary / powerful / constrained) â retain control via dual-class charters
 *   - class_a_shareholders (beneficiary / moderate / mobile) â invest with disclosed governance limits and liquid exit
 *   - governance_activists (excluded / organized / mobile) â advocate for parity but are outside the legitimacy framework
 *   - securities_law_scholars (observer / analytical / analytical) â evaluate the sufficiency of disclosure-based legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.22).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.25).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy via Securities Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate governance / securities law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '83f9c5b9-97d1-46dd-9671-6a411ec96934').
narrative_ontology:cs_kernel_codification('83f9c5b9-97d1-46dd-9671-6a411ec96934', formalized).
narrative_ontology:cs_authority_grounding('83f9c5b9-97d1-46dd-9671-6a411ec96934', lineage).
narrative_ontology:cs_interpretation_layer_present('83f9c5b9-97d1-46dd-9671-6a411ec96934').
narrative_ontology:cs_reading_relation('83f9c5b9-97d1-46dd-9671-6a411ec96934', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('83f9c5b9-97d1-46dd-9671-6a411ec96934', dual_class_legitimacy__minority_extraction, forecloses).
narrative_ontology:cs_axiom('83f9c5b9-97d1-46dd-9671-6a411ec96934', foundational, securities_disclosure_satisfies_regulatory_duty).
narrative_ontology:cs_axiom_status(securities_disclosure_satisfies_regulatory_duty, holdable).
narrative_ontology:cs_axiom_grounding('83f9c5b9-97d1-46dd-9671-6a411ec96934', securities_disclosure_satisfies_regulatory_duty, conventional).
narrative_ontology:cs_axiom('83f9c5b9-97d1-46dd-9671-6a411ec96934', foundational, governance_disparity_is_market_priced).
narrative_ontology:cs_axiom_status(governance_disparity_is_market_priced, holdable).
narrative_ontology:cs_axiom_grounding('83f9c5b9-97d1-46dd-9671-6a411ec96934', governance_disparity_is_market_priced, empirically_contingent).
narrative_ontology:cs_reference_frame('83f9c5b9-97d1-46dd-9671-6a411ec96934', securities_disclosure_efficient_market).
narrative_ontology:cs_drift_state('83f9c5b9-97d1-46dd-9671-6a411ec96934', contemporary_governance_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('83f9c5b9-97d1-46dd-9671-6a411ec96934', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, dual_class_issuers).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Securities Act disclosure regime; review S-1 registration statements for adequacy; enforce compliance through comment letters, stop orders, and penalties; they mandate transparency but do not require governance parity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Founders and executives who adopt multi-class share structures to retain voting control while raising public capital; they bear the cost of detailed S-1 disclosure and ongoing reporting but capture the control premium.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, dual_class_issuers, beneficiary,
    powerful, biographical, constrained, national).

% Public investors who purchase Class A shares with disclosed governance limitations; they rely on mandated disclosure to assess the control discount and can exit by selling into liquid secondary markets.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_shareholders, beneficiary,
    moderate, biographical, mobile, national).

% Institutional investors and proxy advisors who advocate for one-share-one-vote and view control disparity as inherently extractive; their objections are deflected by reference to S-1 disclosure rather than engaged as substantive legitimacy claims.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, governance_activists, excluded,
    organized, generational, mobile, national).

% Academics who analyze the efficiency and fairness of disclosure-based securities regulation; they assess whether informed consent is sufficient for governance legitimacy or merely a procedural screen.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, dual_class_issuers).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Securities Act disclosure regime coordinates the relationship between issuers and investors by mandating standardized information release, reducing information asymmetry so that investors can price governance disparity into their investment decisions.
% TRANSFER_FUNCTION: Moves voting control from public Class A shareholders to founding insiders, while moving capital from investors to the firm; the transfer is legitimated by the exchange of detailed disclosure for the explicit waiver of governance parity.
% ABSENT_VOICES: Governance activists and institutional investors who view proportional control as a fundamental shareholder right are structurally excluded; their voices are addressed by pointing to the S-1 disclosure rather than treated as live objections within the legitimacy framework.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures no longer rested on informed consent via disclosure, issuers would need to justify control disparity on functional grounds or eliminate it; the market for dual-class IPOs would contract and the equilibrium of control retention would reorganize toward parity or stewardship-based justifications.
% FOUNDING_PROBLEM: Information asymmetry in public securities markets, where insiders possess superior knowledge about firm value and governance risks, threatening fraud and inefficient capital allocation.
% FOUNDING_PROBLEM_CORROBORATION: Securities law historians and regulatory economists attest that the disclosure framework was designed to address information asymmetry; the SEC and federal courts corroborate that this problem remains live and is addressed through mandatory disclosure rather than substantive governance mandates.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because this reading assesses the arrangement as consensual: investors have full information and liquid exit, so the control disparity is priced rather than extracted. Suppression is low (0.25) because the constraint's persistence does not depend on coercively suppressing alternatives; investors can simply decline to purchase. Accessibility collapse is moderate (0.38) because while alternatives (not investing, index exclusion) exist, the market infrastructure is built around the disclosure paradigm. Resistance is low (0.20) because most market participants accept the framework, though activist pressure is growing. Theater ratio is low-moderate (0.18): disclosure is largely functional, but some compliance activity is performative box-checking that does not materially inform.
 *
 * PERSPECTIVAL GAP:
 *   The regulator and issuer seats experience the constraint as a functional, low-extraction coordination mechanism that sustains capital formation. The excluded activist seat experiences the same structural arrangement as a legitimacy failure that substitutes procedural disclosure for substantive fairness. The investor seat sits in between: structurally it waives control, but this reading codes it as a compensated beneficiary rather than a target because of the consent framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual-class issuers are beneficiaries (low d): they are subsidized by the legal permission to retain control while raising public capital. Class A shareholders are also treated as beneficiaries (low d) under this reading because the disclosure regime is construed as serving them; their structural waiver of control is offset by transparency and pricing. Securities regulators sit near symmetric (moderate d) as agenda-setters who neither collect nor pay. Governance activists are excluded from the framework and would carry high d if seated, but their exclusion is structural. The engine will compute a wide perspectival gap between the agenda-setter/beneficiary seats and the excluded activist seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (information asymmetry) remains live, and the disclosure apparatus is actively used. The arrangement is not a piton because the mandate has not atrophied. The risk of mandatrophy would arise if the disclosure framework became purely theatrical while the market stopped relying on it; the authored theater_ratio (0.18) and live founding problem status indicate this has not occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the disclosure_consent reading of the dual_class_legitimacy kernel. Does the contractual-choice framing survive if empirical evidence shows systematic underpricing of governance disparity in dual-class IPOs?',
    'Event-study and long-run performance analysis comparing initial pricing of dual-class IPOs against subsequent control-premium realizations; if Class A shares consistently underprice the governance discount, the informed-consent premise is weakened.',
    'If disparity is not fully priced, the reading''s low-extraction claim fails and the constraint computes as more extractive from the investor seat, potentially shifting the seat classification toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Empirical test of the informed-consent premise for dual-class legitimacy.').

omega_variable(
    minority_extraction_foreclosure_validity,
    'This reading structurally treats proportional governance entitlement as foreclosed by informed consent. Is this foreclosure valid in jurisdictions or frameworks that recognize both disclosure and substantive fairness norms?',
    'Comparative corporate-law analysis of jurisdictions with fairness-review doctrines (e.g., entire fairness in controller transactions) versus pure disclosure regimes.',
    'If both disclosure and substantive fairness can coexist as legal norms, the forecloses relation to minority_extraction should be downgraded to coexists_with or influences, altering the constraint-family topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_extraction_foreclosure_validity, conceptual, 'Whether disclosure-consent logically forecloses proportional-governance claims.').

omega_variable(
    suppression_ambiguity_activist_exclusion,
    'Is the exclusion of governance activists from the legitimacy framework a structural feature of securities law (valid omission outside the regulatory scope) or an internalized suppression of dissent?',
    'Track whether activist proposals for parity are ruled out-of-scope on jurisdictional grounds (structural) or dismissed as illegitimate within the disclosure paradigm (internalized).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, raising extraction for the excluded seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_ambiguity_activist_exclusion, conceptual, 'Structural versus internalized suppression of governance dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_disclosure_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_class_disclosure_tr_t8, dual_class_legitimacy__disclosure_consent, theater_ratio, 8, 0.1).
narrative_ontology:measurement(dual_class_disclosure_tr_t16, dual_class_legitimacy__disclosure_consent, theater_ratio, 16, 0.12).
narrative_ontology:measurement(dual_class_disclosure_tr_t24, dual_class_legitimacy__disclosure_consent, theater_ratio, 24, 0.15).
narrative_ontology:measurement(dual_class_disclosure_tr_t32, dual_class_legitimacy__disclosure_consent, theater_ratio, 32, 0.17).
narrative_ontology:measurement(dual_class_disclosure_tr_t40, dual_class_legitimacy__disclosure_consent, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(dual_class_disclosure_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(dual_class_disclosure_be_t8, dual_class_legitimacy__disclosure_consent, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(dual_class_disclosure_be_t16, dual_class_legitimacy__disclosure_consent, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(dual_class_disclosure_be_t24, dual_class_legitimacy__disclosure_consent, base_extractiveness, 24, 0.19).
narrative_ontology:measurement(dual_class_disclosure_be_t32, dual_class_legitimacy__disclosure_consent, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(dual_class_disclosure_be_t40, dual_class_legitimacy__disclosure_consent, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, information_standard).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% The dual_class_legitimacy kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. This reading (disclosure_consent) treats legitimacy as deriving from securities disclosure and market consent; the founder_stewardship reading treats the same arrangement as coordination; the minority_extraction reading treats it as extraction. Each has a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
