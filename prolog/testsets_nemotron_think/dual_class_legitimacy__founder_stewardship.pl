% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Founder Control as Stewardship
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   Dual-class share structures concentrate voting control in founders
 *   (typically 10:1 or 20:1 super-voting shares) while minority shareholders
 *   hold low-vote shares. The founder_stewardship reading frames this as
 *   legitimate coordination: founders act as fiduciary stewards protecting
 *   long-horizon missions from short-term market pressure, benefiting all
 *   shareholders indirectly through superior value creation. The structural
 *   reality shows identifiable beneficiaries (founders, founding teams) and
 *   victims (minority shareholders, index funds) with active enforcement via
 *   charter provisions and state law. The constraint exhibits both
 *   coordination function (long-horizon insulation) and asymmetric extraction
 *   (control premium without economic proportion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.62).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.55).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Founder Control as Stewardship").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'f7077332-90bb-467c-ba80-a7bd0d10b1d5').
narrative_ontology:cs_kernel_codification('f7077332-90bb-467c-ba80-a7bd0d10b1d5', formalized).
narrative_ontology:cs_authority_grounding('f7077332-90bb-467c-ba80-a7bd0d10b1d5', extraction).
narrative_ontology:cs_interpretation_layer_present('f7077332-90bb-467c-ba80-a7bd0d10b1d5').
narrative_ontology:cs_reading_relation('f7077332-90bb-467c-ba80-a7bd0d10b1d5', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('f7077332-90bb-467c-ba80-a7bd0d10b1d5', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('f7077332-90bb-467c-ba80-a7bd0d10b1d5', foundational, founder_as_fiduciary_steward).
narrative_ontology:cs_axiom_status(founder_as_fiduciary_steward, holdable).
narrative_ontology:cs_axiom_grounding('f7077332-90bb-467c-ba80-a7bd0d10b1d5', founder_as_fiduciary_steward, deontological).
narrative_ontology:cs_axiom('f7077332-90bb-467c-ba80-a7bd0d10b1d5', foundational, long_horizon_mission_primacy).
narrative_ontology:cs_axiom_status(long_horizon_mission_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f7077332-90bb-467c-ba80-a7bd0d10b1d5', long_horizon_mission_primacy, instrumental).
narrative_ontology:cs_reference_frame('f7077332-90bb-467c-ba80-a7bd0d10b1d5', founder_led_governance_model).
narrative_ontology:cs_drift_state('f7077332-90bb-467c-ba80-a7bd0d10b1d5', contemporary_activism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f7077332-90bb-467c-ba80-a7bd0d10b1d5', '2026-06-11T14:30:00Z').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_teams).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_investors_aligned_with_founder).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, public_market_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, index_funds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, board_of_directors).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, founding_teams).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_horizon_value_creation).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, founder_led_innovation).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, mission_preservation_against_short_termism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares (typically 10:1 or 20:1) giving voting control far exceeding economic ownership. Set corporate strategy, appoint board, control M&A decisions. Their personal identity and legacy are fused with the company; exit means abandoning life's work. Justify control as necessary to pursue multi-decade missions without quarterly pressure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founders, agenda_setter,
    powerful, biographical, identity_locked, global).

% Early employees and co-founders granted high-vote shares or options. Benefit from founder's long-horizon protection which shields their equity value from short-term activism. Also bear cost: their own voting power is diluted by founder's super-voting shares; they cannot effectively challenge founder decisions even when they disagree.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_teams, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founding_teams, payer).

% Hold low-vote or single-vote shares (Class A). Provide the vast majority of capital but receive minimal voting power. Cannot elect directors, approve acquisitions, or block founder proposals. Exit option is selling shares — but dual-class structures often trade at a discount, and liquidity may be limited for controlled companies. Bear the cost of control premium without the benefit.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, minority_shareholders, payer,
    moderate, biographical, constrained, global).

% Institutional and retail investors buying Class A shares in public markets. Price in the governance discount but have no voice. Can exit by selling, but the discount persists as long as the structure exists. Their capital funds the founder's mission but they capture only residual returns after founder's control preferences are satisfied.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, public_market_investors, payer,
    organized, immediate, mobile, global).

% Forced holders via index inclusion. Cannot selectively exit dual-class companies without tracking error. Hold diversified portfolios where governance discounts across many controlled companies accumulate to meaningful drag. Lobby for reform (e.g., CII policies, Big Three voting guidelines) but lack leverage against founder-controlled boards.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, index_funds, payer,
    institutional, generational, constrained, global).

% Nominally oversee management but in practice are appointed by and accountable to founder. Benefit from prestigious, compensated directorships with minimal accountability. Their independence is structurally compromised — the founder controls re-nomination. They administer the constraint (charter provisions, voting agreements) rather than constrain it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, board_of_directors, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, board_of_directors, beneficiary).

% ISS, Glass Lewis, etc. Issue voting recommendations on shareholder proposals (e.g., sunset provisions, equal voting rights). Their influence is advisory; they cannot compel change. Provide analytical framing that shapes institutional voter sentiment but the constraint's enforcement (charter, state law) sits outside their reach.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisors, observer,
    institutional, biographical, analytical, global).

% SEC, state securities commissions. Oversee disclosure (IPO prospectuses, ongoing reporting) but not substantive governance terms. Dual-class is permitted under state corporate law (DGCL) and exchange rules (with some sunset requirements for new listings). Their role is transparency, not redistribution of control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Insulates founder-led companies from short-term capital market pressure, enabling multi-year R&D, unconventional strategies, and mission preservation that dispersed shareholders would not support. Solves the coordination problem of aligning long-horizon execution with volatile public capital.
% TRANSFER_FUNCTION: Moves voting control from capital-proportional (one share, one vote) to founder-concentrated (super-voting shares). Transfers decision rights over strategy, M&A, board composition, and charter amendments from minority shareholders to founder. Transfers the control premium (valuation discount) from all shareholders to the founder's private benefit of control.
% ABSENT_VOICES: Future shareholders who will inherit the structure without participating in the IPO; employees without equity whose livelihoods depend on founder's long-horizon bets; stakeholders (communities, customers) affected by missions that prioritize founder vision over distributed interests. They are not in the room at IPO and have no exit from consequences.
% DISAPPEARANCE_RATIONALE: If dual-class vanished overnight, founders would lose voting control premium; boards would become accountable to all shareholders; capital would re-price governance risk; some long-horizon projects would lose protection and be cut; index funds would capture governance discount elimination; the corporate governance landscape would shift toward one-share-one-vote norm.
% FOUNDING_PROBLEM: The separation of ownership and control in public corporations creates short-termism: dispersed shareholders pressure managers for quarterly results, undermining long-horizon investment. Founders with concentrated control can resist this pressure and pursue multi-decade missions (e.g., Amazon's infrastructure build-out, Meta's metaverse pivot, Google's moonshots).
% FOUNDING_PROBLEM_CORROBORATION: The founder-stewardship narrative is attested by founders themselves (Bezos, Zuckerberg, Page/Brin, Musk) and some aligned long-horizon investors (e.g., Sequoia, Founders Fund). It is contested by corporate governance scholars (Bebchuk, Coates, Fisch), institutional investor coalitions (CII, ICGN), and empirical studies showing dual-class firms underperform on governance metrics and often destroy value after founder departure. No independent corroboration exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the control premium: founders capture voting rights worth 10-20x their economic stake, enforced by charter and DGCL. Suppression (0.55) is structural — minority shareholders cannot vote out founders, cannot approve M&A, face liquidity discounts; exit is selling at a discount. Theater ratio (0.32) rises over time as governance rituals (annual meetings, advisory votes) become performative while real decisions remain founder-controlled. Accessibility collapse (0.42) is moderate: single-class alternatives exist but dual-class IPOs have proliferated, reducing choice. Resistance (0.48) grows: shareholder proposals, litigation (e.g., Snap, Facebook), regulatory pressure (SEC, exchanges), but founder control blocks structural change.
 *
 * PERSPECTIVAL GAP:
 *   From the founder seat, the constraint is genuine coordination — they built the company, bear the vision, and need protection from myopic capital. From minority shareholder seats, the same structure is extraction — they provided 90%+ of capital for <10% of votes. The board seat experiences it as comfortable administration. The engine computes per-seat types from these structural positions; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders are structural beneficiaries (d ~0.15): they collect control premium, set agenda, face identity-locked exit. Founding teams are dual-positioned: beneficiaries of mission protection but payers of diluted voice (d ~0.35). Minority shareholders, public investors, and index funds are payers (d ~0.75-0.85): they provide capital, bear governance discount, have constrained/mobile exit. Board members are agenda_setters who benefit from the arrangement (d ~0.25). Proxy advisors and regulators are analytical observers (d ~0.5). The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (short-termism) is contested, not dead. Some founders genuinely use control for long-horizon value (Amazon, Tesla early years); others entrench after mission completion (Snap, later-stage Meta). The arrangement persists past its coordination function in many cases — a mandatrophy signal. But the reading's claim is that the problem remains live; the engine will test this via founding_problem_status x disappearance_verdict mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism (rope/tangled_rope) or a constructed extraction mechanism (snare) dressed in stewardship language?',
    'Compare long-horizon outcomes of dual-class vs. single-class firms controlling for founder presence, industry, and capital intensity. Track whether control premium correlates with mission success or founder entrenchment.',
    'If coordination, the constraint is tangled_rope (coordination + asymmetric extraction). If extraction with coordination cover, it is snare. The founder_stewardship reading asserts the former; minority_extraction reading asserts the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the founder_stewardship reading''s coordination claim holds structurally or is a cover story.').

omega_variable(
    coordination_extraction_boundary,
    'How much of the voting control premium is necessary for long-horizon coordination vs. pure founder private benefit?',
    'Natural experiments: sunset provisions (e.g., 7-year auto-conversion), post-founder transitions, jurisdictions with mandatory sunset (Hong Kong, UK premium listing). Measure whether mission execution degrades when control premium shrinks.',
    'If coordination requires the full premium, the constraint is more rope-like. If partial sunset preserves coordination while reducing extraction, the current premium is partly extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'The boundary between necessary coordination cost and extractive surplus in the control premium.').

omega_variable(
    minority_consent_validity,
    'Does IPO disclosure of dual-class structure constitute meaningful consent by minority shareholders, or is consent structurally coerced by lack of alternatives?',
    'Survey IPO investors on whether they could practically decline dual-class allocations; analyze whether dual-class IPOs are priced at a discount reflecting governance risk (implying forced acceptance) or at parity (implying willing consent).',
    'Supports disclosure_consent sibling reading if consent is meaningful; undermines it if consent is illusory. The founder_stewardship reading does not depend on consent quality — it asserts coordination function suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_consent_validity, conceptual, 'Whether the disclosure_consent reading''s legitimacy condition is met in practice.').

omega_variable(
    suppression_mechanism_dual_class,
    'Is minority shareholder suppression structural (charter, law, liquidity discount) or internalized (investor resignation, index fund passivity, belief that founder control is normal)?',
    'Post-exit suppression trajectory: if minority investors who sell dual-class holdings still perceive governance risk in other dual-class companies, suppression is internalized. If activism increases after negative outcomes (e.g., Meta 2022, Snap 2017), structural suppression dominates.',
    'If internalized, effective suppression is higher than structural measure — the constraint shapes investor cognition beyond formal barriers. This affects directionality derivation for index_funds and public_market_investors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_dual_class, empirical, 'Structural vs. internalized suppression mechanism for minority shareholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.2).
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.25).
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.28).
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.3).
narrative_ontology:measurement(dual_class_founder_stewardship_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.32).

% Extraction over time
narrative_ontology:measurement(dual_class_founder_stewardship_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dual_class_founder_stewardship_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(dual_class_founder_stewardship_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(dual_class_founder_stewardship_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(dual_class_founder_stewardship_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(dual_class_founder_stewardship_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_founder_stewardship_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dual_class_founder_stewardship_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(dual_class_founder_stewardship_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(dual_class_founder_stewardship_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(dual_class_founder_stewardship_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(dual_class_founder_stewardship_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.08).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The dual_class_legitimacy kernel decomposes into three readings: founder_stewardship (this story, claims coordination), minority_extraction (claims pure extraction), disclosure_consent (claims legitimacy depends on consent quality). They share the same charter structure but differ on ε referent: founder_stewardship assesses the standing arrangement as coordination; minority_extraction assesses it as extraction; disclosure_consent assesses it as conditional on disclosure adequacy. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
