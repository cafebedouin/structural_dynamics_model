% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Founder Stewardship Legitimacy
 *   domain: corporate governance/securities law/organizational economics
 *
 * SUMMARY:
 *   This constraint story instantiates the founder_stewardship reading of the
 *   dual_class_legitimacy kernel. The contested kernel is the dual-class
 *   corporate governance structure that separates voting control from
 *   economic ownership. This reading frames concentrated founder control as
 *   legitimate coordination: the founder acts as fiduciary steward,
 *   protecting long-horizon mission execution from short-term market
 *   pressures, with Class A holders benefiting indirectly through sustained
 *   enterprise value creation. The sibling readingsâminority_extraction and
 *   disclosure_consentâare linked in the constraint family. The authored
 *   metrics are independent of the claim: the structural reality includes
 *   both genuine coordination (mission protection) and asymmetric extraction
 *   (control without proportional capital), yielding a tangled_rope
 *   classification.
 *
 * KEY AGENTS:
 *   - founder_controller: Primary agenda-setter and beneficiary (powerful/identity_locked/global) â holds supervoting control, defines mission, cannot exit without destroying firm-specific identity capital.
 *   - class_a_minority_shareholders: Primary payer and secondary beneficiary (moderate/constrained/global) â bear economic risk without proportional governance voice; may benefit indirectly if stewardship succeeds.
 *   - activist_investors: Excluded seat (powerful/constrained/national) â structurally barred from governance influence by supervoting charter provisions.
 *   - institutional_governance_observers: Analytical observer (institutional/analytical/global) â proxy advisors and index providers evaluating dual-class legitimacy against one-share-one-vote norms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.58).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.45).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.58).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Founder Stewardship Legitimacy").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate governance/securities law/organizational economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6').
narrative_ontology:cs_kernel_codification('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', formalized).
narrative_ontology:cs_authority_grounding('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', lineage).
narrative_ontology:cs_interpretation_layer_present('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6').
narrative_ontology:cs_reading_relation('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', foundational, founder_control_serves_mission).
narrative_ontology:cs_axiom_status(founder_control_serves_mission, holdable).
narrative_ontology:cs_axiom_grounding('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', founder_control_serves_mission, instrumental).
narrative_ontology:cs_axiom('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', foundational, short_term_market_pressure_harmful).
narrative_ontology:cs_axiom_status(short_term_market_pressure_harmful, holdable).
narrative_ontology:cs_axiom_grounding('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', short_term_market_pressure_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', founder_mission_protection_framework).
narrative_ontology:cs_drift_state('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', contemporary_governance_activism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e2f22bb-fc1e-46eb-8eae-6f6e63ad90a6', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_controller).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supervoting shares or dual-class stock that grants disproportionate voting control relative to economic ownership. Sets strategic direction, controls board composition, and defines the company's mission. Cannot easily exit without destroying the value of their locked-in firm-specific capital and reputational identity. Collects private benefits of control and insulation from market discipline.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_controller, agenda_setter,
    powerful, generational, identity_locked, global).

% Hold inferior voting shares with proportionate economic risk but minimal governance influence. Bear the risk of misalignment between founder preferences and shareholder value. May benefit indirectly if founder stewardship enhances long-term returns, but lack structural mechanisms to discipline the founder if performance deteriorates. Exit is constrained by capital gains tax, index inclusion, or illiquidity in the control premium.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders, beneficiary).

% Would seek board representation, strategy changes, or sale of the company to unlock value. Structurally excluded from governance influence by supervoting provisions that make proxy contests unwinnable regardless of economic support from minority shareholders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_investors, excluded,
    powerful, biographical, constrained, national).

% Proxy advisors, pension funds, and stock index providers that evaluate dual-class structures against one-share-one-vote governance principles. Issue voting guidelines and exclusion criteria but lack authority to override charter provisions. Track the long-term performance and entrenchment risk of dual-class firms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_governance_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_controller).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables long-term mission execution and capital allocation by insulating management from short-term earnings pressure, activist interference, and market-driven strategy changes.
% TRANSFER_FUNCTION: Moves disproportionate governance control to the founder relative to economic capital contributed; moves the risk of misalignment and control discount to Class A minority shareholders.
% ABSENT_VOICES: Activist investors and corporate governance reform advocates who would demand one-share-one-vote and independent board control are structurally excluded by supervoting charter provisions; their objections are voiced in policy forums but not in the boardroom.
% DISAPPEARANCE_RATIONALE: If dual-class founder control vanished overnight, capital allocation time horizons would shorten, activist pressure would increase, founder-led mission-driven firms would face restructuring or sale, and the control premium in share prices would collapse or reprice.
% FOUNDING_PROBLEM: Public equity markets impose short-term performance pressure that undermines long-term value creation, mission-driven business models, and risky innovation investments.
% FOUNDING_PROBLEM_CORROBORATION: Entrepreneurship scholars and founder-affiliated boards attest the problem remains live. Index fund governance teams, public pension funds, and the Council of Institutional Investors attest the problem is overstated and used to justify entrenchment. No neutral corroborating consensus exists; empirical evidence on long-term performance of dual-class firms is mixed and contested.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the structural asymmetry of separating voting control from cash-flow rights: the founder captures private benefits of control and insulation from market discipline. Suppression (0.45) is moderate because the constraint suppresses alternative governance through charter provisions rather than overt coercion; activists can speak but cannot win votes. Theater_ratio (0.40) captures the partially performative nature of stewardship claimsâgenuine in some firms, post-hoc rationalization in others. Accessibility_collapse (0.50) indicates that while shareholders can exit by selling, governance alternatives (proxy contests, board turnover) collapse once the dual-class structure is understood. Resistance (0.55) reflects sustained institutional investor opposition and periodic regulatory scrutiny. The measurement series track a slow drift toward greater extraction and theater as firms age and succession pressures mount.
 *
 * PERSPECTIVAL GAP:
 *   The founder_controller seat perceives the arrangement as necessary coordinationâwithout it, quarterly capitalism would dismantle mission-driven investment. The class_a_minority_shareholder seat perceives a governance gap where they bear downside risk without control rights. The activist seat sees pure extraction. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_controller is declared beneficiary with identity_locked exit: they are structurally subsidized by the constraint (low d), though their exit is fused with the firm. Class_a_minority_shareholders are declared victims with constrained exit: they are structural targets (high d). The activist_investors are excluded, which the engine treats as extreme target position since they are actively suppressed by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both coordination and extraction. Pure rope is excluded because victims are structurally present (Class A holders lack proportional control). Pure snare is excluded because a genuine coordination functionâlong-horizon capital allocation shielded from short-term market pressureâis plausibly operative. The tangled_rope classification captures that the same charter provisions both coordinate (mission stability) and extract (control rents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_authenticity,
    'Is the founder''s long-horizon stewardship orientation a genuine governance feature that produces shareholder value, or a post-hoc rationalization for control entrenchment?',
    'Long-term abnormal return differential between dual-class firms with founder-CEOs versus single-class peers, controlling for sector and age; event studies around founder death or incapacitation.',
    'If stewardship produces no abnormal value, the coordination story collapses toward extraction; if positive, the tangled rope classification retains genuine coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_authenticity, empirical, 'Whether founder stewardship claim is genuine value creation or entrenchment cover.').

omega_variable(
    class_a_net_benefit,
    'Do Class A shareholders receive net benefits from founder control sufficient to offset the governance discount and misalignment risk?',
    'Comparative study of total shareholder returns, control premium pricing, and expropriation event frequency in dual-class versus single-class firms.',
    'If Class A holders are net losers, they are unambiguous victims; if net beneficiaries, the victim classification requires nuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_a_net_benefit, empirical, 'Net welfare effect on Class A shareholders from founder control.').

omega_variable(
    suppression_ambiguity,
    'Is the suppression of activist influence structural (charter-based voting powerlessness) or internalized (shareholder acceptance of the stewardship narrative)?',
    'Measure activist campaign incidence and success rates in dual-class firms versus charter-protected single-class firms; survey institutional investor acceptance of dual-class legitimacy.',
    'If primarily internalized, effective suppression exceeds structural measures and the constraint operates partly through narrative capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_ambiguity, conceptual, 'Structural versus internalized suppression mechanism in dual-class governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_fs_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dual_class_fs_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.28).
narrative_ontology:measurement(dual_class_fs_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.32).
narrative_ontology:measurement(dual_class_fs_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.36).
narrative_ontology:measurement(dual_class_fs_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.38).
narrative_ontology:measurement(dual_class_fs_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(dual_class_fs_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dual_class_fs_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dual_class_fs_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(dual_class_fs_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(dual_class_fs_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(dual_class_fs_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_fs_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dual_class_fs_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(dual_class_fs_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(dual_class_fs_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(dual_class_fs_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(dual_class_fs_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is part of the dual_class_legitimacy kernel family. The kernel (dual-class corporate governance) decomposes into three structurally distinct readings: founder_stewardship (coordination justification), minority_extraction (governance entitlement), and disclosure_consent (contractual/consent justification). Each reading carries a distinct epsilon, beneficiary/victim structure, and normative axioms. They are linked as a constraint family because they are competing normative framings of the same charter mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
