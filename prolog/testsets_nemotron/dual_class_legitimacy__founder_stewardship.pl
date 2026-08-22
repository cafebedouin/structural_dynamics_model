% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Founder Stewardship Dual-Class Governance
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   Dual-class share structures concentrate voting power in founders (Class
 *   B, typically 10:1 voting) while granting economic rights equally (Class
 *   A). The founder_stewardship reading argues this is a coordination device:
 *   founders bear unique monitoring costs and reputational risk to pursue
 *   missions that public markets would truncate. Class A holders benefit
 *   indirectly through mission success; the structure is a Rope with a
 *   coordination function. But the reading must account for extraction: when
 *   missions drift or fail, Class A holders bear losses without voice — the
 *   coordination story becomes a cover for control rents. This tension makes
 *   it a Tangled Rope, not a pure Rope. The claim/metric gap is intentional:
 *   the reading CLAIMS Rope (coordination), but the authored metrics describe
 *   a structure with measurable extraction (0.28) and active enforcement
 *   (charter provisions, poison pills, staggered boards) — the engine
 *   measures the divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.28).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.15).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.28).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Dual-Class Governance").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0').
narrative_ontology:cs_kernel_codification('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', formalized).
narrative_ontology:cs_authority_grounding('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', lineage).
narrative_ontology:cs_interpretation_layer_present('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0').
narrative_ontology:cs_reading_relation('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', foundational, founder_control_enables_long_horizon_mission).
narrative_ontology:cs_axiom_status(founder_control_enables_long_horizon_mission, holdable).
narrative_ontology:cs_axiom_grounding('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', founder_control_enables_long_horizon_mission, instrumental).
narrative_ontology:cs_axiom('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', secondary, mission_success_benefits_all_shareholders).
narrative_ontology:cs_axiom_status(mission_success_benefits_all_shareholders, holdable).
narrative_ontology:cs_axiom_grounding('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', mission_success_benefits_all_shareholders, empirically_contingent).
narrative_ontology:cs_reference_frame('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', founding_bargain_horizon_insulation).
narrative_ontology:cs_drift_state('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', post_2020_governance_reform_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04b5c8a6-eb1f-4d2a-8299-d96b9fd27ab0', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_institutional_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, mission_aligned_employees).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, diversified_passive_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, activist_hedge_funds).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_term_value_creation_requires_insulation).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, founder_vision_as_public_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares that grant effective control despite minority economic ownership. Frame their control as fiduciary stewardship of a long-horizon mission that benefits all shareholders. Their personal identity, reputation, and legacy are fused to the company's mission; exit would dissolve the stewardship claim. They bear monitoring costs and reputational risk but capture control rents and mission-definition authority.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_shareholders, agenda_setter,
    powerful, generational, identity_locked, global).

% Endorse the dual-class structure because it aligns with their own long-horizon mandates (pension funds, sovereign wealth funds, endowment models). They benefit from mission stability and compounding returns without bearing governance costs. Their exit is constrained by mandate fit — switching to short-horizon alternatives violates their own investment philosophy.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_horizon_institutional_investors, beneficiary,
    institutional, generational, constrained, global).

% Experience the founder's mission as a coordination device that reduces internal politics, clarifies strategy, and enables decade-long R&D bets. They benefit from cultural coherence and career capital tied to mission success. Exit is constrained by identity investment in the mission and the scarcity of comparable mission-driven workplaces.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, mission_aligned_employees, beneficiary,
    organized, biographical, constrained, global).

% Hold index-weighted positions that include dual-class companies. They bear the governance discount (lower valuations, reduced voice) without the ability to select out of individual holdings. Their exit is trivial at the portfolio level (sell the index) but structurally meaningless — they cannot discipline any single company's governance. They pay through depressed returns when stewardship fails or control is abused.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, diversified_passive_investors, payer,
    powerless, immediate, arbitrage, global).

% Target dual-class companies for governance campaigns, arguing that concentrated control destroys value. They bear the cost of blocked interventions and governance discounts. Their exit is mobile — they can rotate capital to more governance-permissive targets. They are structural adversaries of the constraint, not incidental victims; their model depends on the constraint being classified as extraction.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_hedge_funds, payer,
    powerful, biographical, mobile, global).

% Analyze the constraint through doctrinal, empirical, and theoretical lenses. They produce the frameworks (agency theory, stewardship theory, team production) that courts and regulators use to evaluate dual-class legitimacy. Their output shapes the legitimacy conditions of the constraint itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the horizon mismatch between public markets (quarterly earnings pressure, short-term activist campaigns) and mission-critical investments (R&D, brand building, culture formation) that require decade-long commitment and insulation from capital market discipline.
% TRANSFER_FUNCTION: Moves governance authority (board control, strategic veto, capital allocation discretion) from dispersed Class A shareholders to concentrated Class B founders. Moves monitoring costs and reputational risk to founders. Moves potential upside of mission success to all shareholders; moves downside of stewardship failure disproportionately to Class A holders who lack exit.
% ABSENT_VOICES: Future shareholders who inherit the governance structure without consent; employees who join post-IPO and never chose the dual-class arrangement; retail investors who hold via index funds and have no voice in governance reform. They are structurally excluded from the founding bargain but bear its consequences.
% DISAPPEARANCE_RATIONALE: If dual-class protection vanished overnight, founder-controlled firms would face immediate activist pressure, forced monetization of long-horizon bets, and potential loss of mission coherence. The corporate landscape would rearrange toward shorter horizons, higher payout ratios, and more uniform governance — but also potentially more efficient capital allocation where missions were failing.
% FOUNDING_PROBLEM: Public markets systematically undervalue and pressure long-horizon investments; founder control is the only structural mechanism that credibly commits a firm to a mission beyond the next earnings cycle.
% FOUNDING_PROBLEM_CORROBORATION: Proponents: founder-CEOs (Zuckerberg, Page/Brin, Bezos pre-transition), long-horizon institutional investors (Berkshire, Sequoia, Founders Fund), stewardship-theory scholars (Davis, Schoorman, Donaldson). Critics: agency-theory scholars (Jensen, Fama), index fund managers (Vanguard, BlackRock governance teams), activist investors (Elliott, Pershing Square), empirical studies showing dual-class valuation discounts (Bebchuk, Kastiel). Corroboration from outside the beneficiary set: the SEC's 2020-2024 regulatory agenda on dual-class, ICGN governance principles, and the Council of Institutional Investors' multi-year campaign for sunset provisions.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.28) reflects the governance discount documented in empirical literature — dual-class firms trade at a discount to single-class peers, widening over time as the founding generation ages and succession risk rises. Suppression (0.15) is low because Class A holders CAN sell (arbitrage exit), and the constraint does not actively block exit — it blocks voice. Theater ratio (0.12) is low but rising: stewardship rhetoric increasingly decorates what may be control preservation. Accessibility collapse (0.35) is moderate — alternatives (single-class, sunset provisions) exist but are structurally difficult to adopt once dual-class is entrenched. Resistance (0.25) reflects sustained institutional investor pressure and regulatory scrutiny but not existential challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the founder seat, the constraint is a Rope: genuine coordination enabling mission execution that benefits all. From the passive investor seat, it is a Snare: extraction without voice, enforced by charter. From the activist seat, it is a Snare they campaign against. The engine computes per-seat types from the structural data — the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are structural beneficiaries (collect control rents + mission authority) — d near 0.1 (beneficiary end). Long-horizon institutions are beneficiaries with constrained exit — d ~ 0.2. Mission-aligned employees benefit from coordination — d ~ 0.3 (mildly beneficiary). Diversified passive investors are payers with arbitrage exit but no voice — d ~ 0.6 (moderate target, amplified by scope). Activist funds are payers with mobile exit — d ~ 0.7 (target, but can rotate). The engine derives these from the declared roles and exit options; the asymmetry between founder identity_lock and passive investor arbitrage is the structural driver.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (horizon mismatch) remains live but contested. If the problem is dead (markets now reward long-horizon bets via ESG, patient capital), the constraint persists as a Piton — theatrical stewardship maintaining control after the coordination function atrophied. The rising theater_ratio and extractiveness trajectory suggests early mandatrophy dynamics. The constraint resolves mandatrophy only if sunset provisions activate or if empirical evidence confirms the coordination function still exceeds extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_extraction_boundary,
    'At what point does founder stewardship become control extraction — is there a measurable threshold (tenure, succession failure, valuation discount) where the coordination function is outweighed by the extraction?',
    'Longitudinal study of dual-class firms tracking mission adherence, valuation discount, and governance outcomes across founder tenure and succession events. Compare firms that adopted sunset provisions vs. those that did not.',
    'If a clear threshold exists, the constraint is a Scaffold with a de facto sunset (the threshold). If no threshold exists, the coordination claim is unfalsifiable — the constraint may be a Snare with a coordination cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stewardship_vs_extraction_boundary, conceptual, 'Whether the stewardship/extraction boundary is structurally determinate or a judgment call').

omega_variable(
    index_fund_voice_vs_exit,
    'Do diversified passive investors genuinely lack voice, or does their collective index-weight voting (via Big Three stewardship teams) constitute a latent governance channel that the constraint suppresses?',
    'Analyze Big Three voting records on dual-class shareholder proposals, engagement campaigns, and behind-the-scenes settlements. Measure whether their opposition correlates with governance changes.',
    'If passive investors have effective voice, their exit_options shift from ''arbitrage'' toward ''constrained'' or ''mobile'', reducing their effective extraction. If voice is illusory, the extraction on passive holders is structural and the constraint is more extractive than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_fund_voice_vs_exit, empirical, 'Whether the ''arbitrage exit'' of passive investors masks suppressed voice').

omega_variable(
    committer_frame_legitimacy,
    'This reading derives legitimacy from the founder''s fiduciary stewardship claim. Is that claim structurally dependent on the founder''s personal identity (identity_locked), or does it survive founder succession as an institutionalized stewardship model?',
    'Track dual-class firms through founder succession (Page/Brin→Pichai, Zuckerberg→?, Bezos→Jassy). Measure whether the stewardship narrative and governance discount persist, weaken, or transform.',
    'If stewardship is founder-personal, the constraint is a Scaffold tied to a specific agent''s tenure — mandatrophy is structural. If stewardship institutionalizes, the constraint may be a stable Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_legitimacy, empirical, 'Whether the coordination function survives the founding agent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2004, dual_class_legitimacy__founder_stewardship, theater_ratio, 2004, 0.05).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2008, dual_class_legitimacy__founder_stewardship, theater_ratio, 2008, 0.07).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2012, dual_class_legitimacy__founder_stewardship, theater_ratio, 2012, 0.08).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2016, dual_class_legitimacy__founder_stewardship, theater_ratio, 2016, 0.09).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2020, dual_class_legitimacy__founder_stewardship, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_tr_t2024, dual_class_legitimacy__founder_stewardship, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2004, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2004, 0.15).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2008, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2008, 0.18).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2012, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2016, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2016, 0.25).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2020, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2020, 0.26).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_be_t2024, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2004, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2004, 0.08).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2008, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2012, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2016, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2016, 0.13).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2020, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2020, 0.14).
narrative_ontology:measurement(dual_class_legitimacy__founder_stewardship_su_t2024, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.08).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, corporate_governance_sunset_provisions).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, index_fund_stewardship_effectiveness).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dual_class_legitimacy kernel. The founder_stewardship reading claims control IS coordination (identity_coordination type). The minority_extraction reading claims the same structure IS extraction (snare). The disclosure_consent reading claims legitimacy derives from consent at IPO (rope/scaffold depending on sunset). They share the referent (dual-class charter provisions) but instantiate different constraints with different ε, different victims/beneficiaries, different types. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, powerful, 0.15).
constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
