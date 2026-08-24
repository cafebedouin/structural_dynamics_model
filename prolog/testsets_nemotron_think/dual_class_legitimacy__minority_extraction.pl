% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure as Minority Extraction
 *   domain: economic/legal/organizational
 *
 * SUMMARY:
 *   Dual-class share structures have proliferated since the 1980s,
 *   particularly in media and technology, allowing founders to retain voting
 *   control with minority economic stakes. The minority_extraction reading
 *   views this as a structural transfer of governance value from capital
 *   providers to controllers, enforced through controlled-company exemptions
 *   that strip standard shareholder protections. The constraint is claimed as
 *   tangled_rope — it has a genuine coordination function (capital formation
 *   with founder vision) but operates with substantial asymmetric extraction
 *   and active enforcement (charter provisions, exchange exemptions,
 *   lobbying). The measurement series tracks rising extraction and theater as
 *   dual-class has become normalized and sunset provisions abandoned.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.78).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.82).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure as Minority Extraction").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "economic/legal/organizational").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '10711f93-4391-4379-a02d-52adb5048d39').
narrative_ontology:cs_kernel_codification('10711f93-4391-4379-a02d-52adb5048d39', formalized).
narrative_ontology:cs_authority_grounding('10711f93-4391-4379-a02d-52adb5048d39', extraction).
narrative_ontology:cs_interpretation_layer_present('10711f93-4391-4379-a02d-52adb5048d39').
narrative_ontology:cs_reading_relation('10711f93-4391-4379-a02d-52adb5048d39', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('10711f93-4391-4379-a02d-52adb5048d39', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('10711f93-4391-4379-a02d-52adb5048d39', foundational, governance_proportional_to_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('10711f93-4391-4379-a02d-52adb5048d39', governance_proportional_to_risk, deontological).
narrative_ontology:cs_axiom('10711f93-4391-4379-a02d-52adb5048d39', secondary, consent_does_not_legitimate_perpetual_extraction).
narrative_ontology:cs_axiom_status(consent_does_not_legitimate_perpetual_extraction, holdable).
narrative_ontology:cs_axiom_grounding('10711f93-4391-4379-a02d-52adb5048d39', consent_does_not_legitimate_perpetual_extraction, deontological).
narrative_ontology:cs_reference_frame('10711f93-4391-4379-a02d-52adb5048d39', proportional_governance_baseline).
narrative_ontology:cs_drift_state('10711f93-4391-4379-a02d-52adb5048d39', contemporary_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('10711f93-4391-4379-a02d-52adb5048d39', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, dual_class_architects).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_shareholders_class_a).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, institutional_investors_public).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, proportional_governance_principle).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, one_share_one_vote_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Class B super-voting shares (typically 10:1 or 20:1 voting ratio) representing minority economic interest but majority voting control. Set corporate strategy, appoint boards, control M&A decisions, and block shareholder proposals. Exit via sale of control premium or succession planning; not subject to market discipline on voting power.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controllers, agenda_setter,
    powerful, generational, arbitrage, global).

% Hold Class A shares with inferior voting rights (often 1 vote vs 10-20 for Class B) but provide the vast majority of equity capital. Bear full downside risk and dilution but cannot elect directors, approve acquisitions, or remove management. Exit via selling shares (liquidity exists but at discount to control value) or litigation; no voice in governance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_shareholders_class_a, payer,
    moderate, biographical, constrained, global).

% Large index funds and active managers hold significant Class A positions. They bear governance costs (monitoring, engagement, litigation) and valuation discounts from dual-class structures. Some benefit from liquidity and access to founder-led growth stories. Can vote with feet across portfolio but face index-inclusion constraints and fiduciary duties that limit exit.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_investors_public, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, institutional_investors_public, beneficiary).

% Corporate law firms, investment banks, and proxy advisors who design, market, and advise on dual-class structures. Collect fees for IPO structuring, charter amendments, and ongoing governance advisory. Their business model depends on the legitimacy and prevalence of dual-class; they lobby for controlled-company exemptions and against regulatory reform.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, dual_class_architects, beneficiary,
    organized, biographical, mobile, global).

% SEC and state securities regulators oversee disclosure requirements and exchange listing standards. They have permitted dual-class through controlled-company exemptions (NYSE/NASDAQ) and disclosure-based frameworks. Face political pressure from both investor protection advocates and capital-formation proponents. Can modify listing standards but rarely do.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% ISS and Glass Lewis provide voting recommendations to institutional investors. They generally oppose dual-class and advocate for sunset provisions, but their influence is limited where voting power is concentrated. Their recommendations shape institutional voting but cannot overcome structural control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, proxy_advisors, observer,
    organized, biographical, analytical, global).

% Boards of dual-class companies where independent directors are selected by founder-controllers. Nominally oversee management but structurally aligned with founder interests. Directors' professional identity and board-seats depend on founder patronage. Exit means loss of prestigious positions and future board opportunities; identity fused with 'stewardship' narrative.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, corporate_boards_controlled, agenda_setter,
    institutional, biographical, identity_locked, global).

% Hedge funds and activists who would challenge dual-class structures through proposals, proxy fights, or litigation. They are structurally excluded because super-voting shares make proxy contests unwinnable and controlled-company exemptions remove standard governance levers. Their exclusion is the enforcement mechanism that preserves the extraction.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, activist_investors, excluded,
    powerful, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dual-class structures enable founders to raise public capital while retaining control to pursue long-horizon strategies without short-term market pressure, theoretically solving a coordination problem between capital providers and visionary entrepreneurs.
% TRANSFER_FUNCTION: Transfers voting control and governance value from public shareholders (who contribute ~80-95% of equity capital) to founder-controllers (who hold ~5-20% economic stake), via super-voting shares and controlled-company exemptions that strip mandatory protections (majority voting, independent boards, say-on-pay).
% ABSENT_VOICES: Public shareholders at IPO who could not foresee decades of entrenched control; future shareholders who inherit the structure with no consent opportunity; employees, customers, and communities affected by unaccountable governance decisions; retail investors excluded from index-fund governance engagement.
% DISAPPEARANCE_RATIONALE: If dual-class structures and controlled-company exemptions vanished overnight, controlled companies would immediately face standard exchange governance requirements: majority voting for directors, independent board majorities, annual say-on-pay, and proxy access. Governance would reorganize around proportional representation; founder control would depend on negotiated contracts, not structural entrenchment.
% FOUNDING_PROBLEM: Founders in the 1980s-2000s (media, tech) needed to access public capital markets without surrendering control to pursue long-term missions that public markets allegedly undervalued (e.g., NYT, Ford, Google, Meta, Snap).
% FOUNDING_PROBLEM_CORROBORATION: Founder advocates and dual-class architects attest the problem remains live, citing ongoing short-termism in public markets. Governance scholars (Bebchuk, Hirst), institutional investor coalitions (CII, ICGN), and empirical studies (Cremers et al.) attest the founding problem has shifted: capital markets now accommodate long-term structures through loyalty shares, tenure voting, and engaged ownership — dual-class persists as control retention, not capital formation necessity.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.78) is high because the voting premium is decoupled from economic contribution; suppression (0.82) is higher because persistence depends on legal barriers (charter amendments require supermajority, controlled-company exemptions remove exit via governance reform). Theater (0.45) reflects performative independent directors and advisory votes that mask control entrenchment. Accessibility collapse (0.65) is substantial but not total — shareholders can sell, but at a control discount. Resistance (0.55) is meaningful: activist campaigns, litigation, and regulatory proposals exist but are structurally blocked.
 *
 * PERSPECTIVAL GAP:
 *   From the founder seat, the arrangement is genuine coordination enabling long-term value creation. From the public shareholder seat, the same structure operates as enforced extraction with no voice. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both functions exist; the metrics describe their relative weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder-controllers are structural beneficiaries (d near 0.0): they collect control rents, set rules, and face arbitrage-grade exit. Public Class A shareholders are targets (d near 1.0): they bear costs, have constrained exit, and are identity-locked to the 'investor' role. Institutional investors sit near symmetric (d ~0.5): they bear governance costs but gain portfolio access. Dual-class architects are beneficiaries (d low). Corporate boards are identity-locked agenda-setters: their professional identity fuses with founder stewardship narrative. Activists are trapped — excluded by the enforcement mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capital formation without control surrender) was live in the 1980s-2000s. Today it is contested: loyalty shares, tenure voting, and engaged ownership provide alternative coordination mechanisms. The arrangement persists because founder-controllers (agenda-setters) benefit enough to maintain it, while the cost to fix (charter amendment requiring supermajority vote they control) is prohibitive for payers. This cost-asymmetry — administrator could change it but won't, payers could benefit from change but can't force it — is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the minority_extraction reading structurally relate to the sibling readings founder_stewardship and disclosure_consent within the dual_class_legitimacy kernel?',
    'Map the logical space of the three readings: does any reading''s core premise logically foreclose another in a single framework? Or do they coexist as live positions held by different parties? The engine computes foreclosure from cs_structure.axioms + drift_state; this omega documents the authoring judgment.',
    'If minority_extraction forecloses founder_stewardship, no single governance framework can hold both — they are mutually exclusive legitimacy claims. If they coexist, the kernel is a persistent dispute with no structural resolution. If minority_extraction influences disclosure_consent, it creates downstream pressure on consent-based legitimacy without resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between kernel readings').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the coordination function of dual-class (long-term founder vision) genuine and separable from the extraction, or is coordination a cover story that has atrophied?',
    'Natural experiment: compare dual-class firms with sunset provisions vs. perpetual structures; measure long-term innovation, capital allocation, and shareholder returns. If sunset firms perform equally, coordination is separable from extraction.',
    'If coordination is genuine and separable, the constraint is tangled_rope (coordination + extraction). If coordination is cover for extraction, it is snare. If coordination has atrophied but structure persists, it trends toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether dual-class coordination function is genuine or cover').

omega_variable(
    suppression_mechanism_dual_class,
    'Is the suppression of minority voice structural (legal barriers: charter provisions, exchange exemptions, supermajority requirements) or internalized (investor acquiescence, index-fund passivity, ''governance is priced in'' narratives)?',
    'Track suppression trajectory after regulatory shocks (e.g., SEC proxy reform, exchange rule changes). If suppression persists after structural barriers are lowered, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure — investors carry the constraint with them. If purely structural, removal of barriers should rapidly increase voice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_dual_class, empirical, 'Structural vs. internalized suppression in dual-class governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.25).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.32).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dual_tr_t32, dual_class_legitimacy__minority_extraction, theater_ratio, 32, 0.42).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__minority_extraction, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__minority_extraction, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__minority_extraction, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(dual_be_t32, dual_class_legitimacy__minority_extraction, base_extractiveness, 32, 0.73).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__minority_extraction, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__minority_extraction, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__minority_extraction, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(dual_su_t32, dual_class_legitimacy__minority_extraction, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(dual_su_t40, dual_class_legitimacy__minority_extraction, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.08).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, controlled_company_exemptions).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, proxy_access_rules).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, say_on_pay).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, majority_voting_standards).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, board_independence_requirements).

% DUAL FORMULATION NOTE:
% Part of dual_class_legitimacy constraint family with founder_stewardship and disclosure_consent readings. This reading (minority_extraction) has the highest extractiveness and sees controlled-company exemptions as the enforcement mechanism. The founder_stewardship reading claims lower extraction (coordination benefit dominates). The disclosure_consent reading sees extraction as legitimated by initial consent. All three share the same kernel (dual-class charter provisions) but instantiate different constraints with different ε, beneficiary/victim structures, and drift profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, organized, 0.4).
constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
