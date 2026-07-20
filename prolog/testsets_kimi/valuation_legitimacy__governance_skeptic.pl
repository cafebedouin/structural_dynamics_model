% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__governance_skeptic, []).

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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Control Extraction (Governance Skeptic Reading)
 *   domain: corporate finance/technology governance/space economics
 *
 * SUMMARY:
 *   This constraint is the governance_skeptic reading of the
 *   valuation_legitimacy kernel. It treats the dual-class share structure
 *   with a 10:1 voting ratio and controlled-company exemptions in
 *   Musk-controlled ventures as a mechanism that extracts private benefits of
 *   control from public Class A shareholders. The $1.75T valuation is read as
 *   pricing Musk's private benefits of control rather than shareholder value.
 *   The constraint coordinates massive capital formation (genuine rope-like
 *   function) but does so through an asymmetrically extractive structure that
 *   transfers governance rights and cross-venture allocation power to the
 *   control block. The claim is tangled_rope; the metrics are authored to
 *   reflect high and rising extraction, active suppression of minority voice,
 *   and substantial performative maintenance of public-market legitimacy.
 *
 * KEY AGENTS:
 *   - Musk control block (agenda_setter): 82.4% voting power on 42% equity; controls board, compensation, and cross-venture allocation
 *   - Early Class B investors (beneficiary): passive holders of super-voting shares who benefit from control premium
 *   - Class A public shareholders (payer): capital providers with no governance rights, dependent on sale for exit
 *   - Governance analysts (observer): document the governance discount but lack mechanism to force charter change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.82).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.78).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.82).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Control Extraction (Governance Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate finance/technology governance/space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, '8e7c1c57-47d4-4838-96e8-6c817d27cfd0').
narrative_ontology:cs_kernel_codification('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', formalized).
narrative_ontology:cs_authority_grounding('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', extraction).
narrative_ontology:cs_interpretation_layer_present('8e7c1c57-47d4-4838-96e8-6c817d27cfd0').
narrative_ontology:cs_reading_relation('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', foundational, minority_protection_required_for_legitimacy).
narrative_ontology:cs_axiom_status(minority_protection_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', minority_protection_required_for_legitimacy, deontological).
narrative_ontology:cs_axiom('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', foundational, supervoting_structure_enables_extraction).
narrative_ontology:cs_axiom_status(supervoting_structure_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', supervoting_structure_enables_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', minority_protective_governance).
narrative_ontology:cs_drift_state('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', post_scale_up_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8e7c1c57-47d4-4838-96e8-6c817d27cfd0', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% of voting power through a dual-class structure with a 10:1 vote ratio while owning 42% of equity. Controls board composition, CEO compensation, and strategic resource allocation across multiple ventures. The corporate charter renounces corporate opportunity claims against him, allowing him to direct opportunities to other entities he controls.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_control_block, agenda_setter,
    powerful, generational, arbitrage, global).

% Hold super-voting Class B shares alongside Musk, giving them disproportionate governance influence relative to economic ownership. They benefit from valuation premiums associated with insider control and from strategic decisions made by the control block, without active managerial roles.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Provide capital through non-voting or substantially diluted Class A shares in public and private vehicles. They lack independent compensation and nominating committees to review related-party transactions or CEO pay. Their only exit is sale, and the valuation may capitalize private benefits of control rather than distributable cash flows.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, constrained, global).

% Publish research on governance discounts and control premiums. They document the separation of voting rights from economic rights and the absence of standard minority protections, but their findings do not trigger charter amendment because the control block holds majority voting power.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, governance_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates billions in patient capital for multi-decade technology development in aerospace, electric transport, and energy by offering equity exposure to ventures that conventional governance might starve of long-term funding.
% TRANSFER_FUNCTION: Moves control rights, governance premiums, and private benefits of control from public capital providers (Class A) to the super-voting control block (Musk and early Class B holders), while insulating the controller from accountability over cross-venture resource allocation and compensation.
% ABSENT_VOICES: Institutional investors governed by fiduciary standards that prohibit sustained investment in controlled companies without minority protections; corporate governance scholars who would recommend sunset clauses for dual-class shares; minority shareholders who would demand independent review of related-party transactions between Musk's ventures if they had board representation.
% DISAPPEARANCE_RATIONALE: The $1.75T valuation depends on the control block's private allocation decisions. Without the super-voting structure, independent committees would review Musk's compensation and inter-company deals, the control premium would reprice as a governance discount, and capital would demand risk-adjusted returns rather than founder-optionality pricing.
% FOUNDING_PROBLEM: Funding capital-intensive, long-duration technology bets (reusable rockets, mass-market electric vehicles, satellite constellations) that conventional public markets with short-term governance might abandon during development.
% FOUNDING_PROBLEM_CORROBORATION: Entrepreneurship historians and venture capitalists outside the beneficiary set acknowledge the genuine capital need, but governance analysts and institutional investor guidelines attest that dual-class sunset provisions and independent committees could have achieved the same coordination without the extraction structure.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__governance_skeptic_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__governance_skeptic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the control block captures private benefitsâcorporate opportunity waivers, cross-venture allocation, and CEO compensationâdecoupled from proportional economic ownership. Suppression (0.78) is high because the structure actively excludes minority voice through super-voting shares, controlled-company exemptions, and the absence of independent committees. Theater ratio (0.65) reflects increasing performative maintenance of 'public company' legitimacy while actual governance remains private. Accessibility collapse (0.72) because Class A shareholders, once invested, have no internal governance exitâonly market exit at a price that may already capitalize extraction. Resistance (0.45) reflects persistent but structurally ineffectual criticism from analysts and minority shareholders who cannot force a vote.
 *
 * PERSPECTIVAL GAP:
 *   From the control block's seat, the arrangement is necessary long-term coordination that protects visionary capital allocation from short-term pressure. From the Class A seat, the same structure is extraction dressed as innovation financing. The engine computes this divergence from the structural data: agenda-setter with arbitrage exit versus powerless payer with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk control block and early Class B investors are structural beneficiaries with arbitrage-grade exit options, placing their directionality near the full-beneficiary end (low d). Class A public shareholders are structural victims with constrained exit and no voice, placing their directionality near the full-target end (high d). Governance analysts occupy an analytical seat with no economic exposure, excluded from the directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâraising patient capital for long-duration technology venturesâwas arguably live during early rounds and near-bankruptcy periods. However, the constraint's persistence at a $1.75T valuation without a sunset clause, independent committees, or corporate opportunity protections suggests the coordination mandate has atrophied. The theater ratio captures the drift: an increasing share of governance activity is performative maintenance of public-market legitimacy rather than genuine coordination. The R5 genealogy (founding_problem_status: contested) flags this as a potential zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_benefit_quantification,
    'What is the dollar value of private benefits of control (cross-venture allocation, compensation, corporate opportunity waivers) extracted by the control block, relative to the $1.75T headline valuation?',
    'Forensic accounting of related-party transactions, comparable CEO compensation analysis, and valuation of charter provisions renouncing corporate opportunities.',
    'A large quantified private benefit would confirm the extraction classification; a negligible amount would weaken the governance-skeptic reading and support the real-options or cult-believer readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_benefit_quantification, empirical, 'Whether the control block''s private benefits are material to the valuation.').

omega_variable(
    dual_class_sunset_counterfactual,
    'Would the same capital have been raised, and at what cost, if the dual-class structure included a 10-year sunset or independent committee requirements?',
    'Comparative analysis of founder-controlled companies with and without sunset provisions, and investor subscription patterns in hypothetical SpaceX/Tesla rounds with standard governance.',
    'If comparable capital is available with standard governance, the constraint is extraction riding on a coordination cover story; if not, the super-voting structure may be a necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_class_sunset_counterfactual, conceptual, 'Whether the dual-class structure is necessary for coordination or separable from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.3).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.4).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.5).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.58).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.62).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is the governance_skeptic reading of the valuation_legitimacy kernel. The colloquial 'Musk valuation' decomposes into four structurally distinct claims: dcf_fundamentalist (cash-flow basis), governance_skeptic (minority-protection requirement), real_options_technologist (technological optionality), and musk_cult_believer (founder-track-record basis). Each reading carries its own epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through cs_structure.reading_relations, not through causal network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
