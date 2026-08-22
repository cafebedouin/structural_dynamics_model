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
 *   human_readable: Dual-Class Share Structure as Founder Fiduciary Stewardship
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This story instantiates the founder_stewardship reading of the
 *   dual_class_legitimacy kernel: the claim that concentrated founder voting
 *   control, disproportionate to economic stake, is legitimate because it
 *   functions as a coordination device protecting long-horizon mission
 *   execution on behalf of all shareholders, not merely the founder. This is
 *   one of three structurally distinct constraints sharing the same
 *   underlying arrangement (a dual-class share structure) — the
 *   disclosure_consent reading grounds legitimacy in informed consent at IPO
 *   rather than in stewardship function, and the minority_extraction reading
 *   holds that governance should track capital risk and treats the same
 *   structure as extractive. Each reading carries its own epsilon, its own
 *   beneficiary/victim structure, and its own classification; they are not
 *   merged here. This story's referent is the standing dual-class arrangement
 *   as the stewardship reading understands it — not the one-share-one-vote
 *   alternative it implicitly rejects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.58).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure as Founder Fiduciary Stewardship").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'c845add4-1773-49ac-a18d-23f8071f7682').
narrative_ontology:cs_kernel_codification('c845add4-1773-49ac-a18d-23f8071f7682', formalized).
narrative_ontology:cs_authority_grounding('c845add4-1773-49ac-a18d-23f8071f7682', lineage).
narrative_ontology:cs_interpretation_layer_present('c845add4-1773-49ac-a18d-23f8071f7682').
narrative_ontology:cs_reading_relation('c845add4-1773-49ac-a18d-23f8071f7682', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('c845add4-1773-49ac-a18d-23f8071f7682', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('c845add4-1773-49ac-a18d-23f8071f7682', foundational, control_without_proportional_capital_can_be_fiduciary).
narrative_ontology:cs_axiom_status(control_without_proportional_capital_can_be_fiduciary, holdable).
narrative_ontology:cs_axiom_grounding('c845add4-1773-49ac-a18d-23f8071f7682', control_without_proportional_capital_can_be_fiduciary, instrumental).
narrative_ontology:cs_axiom('c845add4-1773-49ac-a18d-23f8071f7682', secondary, long_horizon_insulation_serves_dispersed_shareholders_indirectly).
narrative_ontology:cs_axiom_status(long_horizon_insulation_serves_dispersed_shareholders_indirectly, holdable).
narrative_ontology:cs_axiom_grounding('c845add4-1773-49ac-a18d-23f8071f7682', long_horizon_insulation_serves_dispersed_shareholders_indirectly, empirically_contingent).
narrative_ontology:cs_reference_frame('c845add4-1773-49ac-a18d-23f8071f7682', founder_led_mission_protection_at_ipo).
narrative_ontology:cs_drift_state('c845add4-1773-49ac-a18d-23f8071f7682', post_ipo_maturity_decade, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c845add4-1773-49ac-a18d-23f8071f7682', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_public_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_index_funds).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, company_mission_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_super_voting_holder).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_horizon_stewardship_thesis).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, control_as_coordination_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B shares carrying disproportionate votes (typically 10:1) relative to economic interest. Sets strategy, board composition, and capital allocation without needing to win a majority of economic capital. Frames the arrangement as protecting the company's long-term mission from quarterly market pressure. Can sell control-class shares at a premium (a coordination-with-exit position most other seats lack).
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_super_voting_holder, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founder_super_voting_holder, beneficiary).

% Purchased Class A shares knowing the voting structure at IPO, priced accordingly. Bear full economic exposure to the company's performance with minimal say in governance. Under this reading, they receive the benefit of shielded long-horizon decision-making (R&D bets, resistance to activist short-termism) without themselves having to monitor governance; if the mission succeeds, share value reflects it. Can sell shares freely if they disagree with direction — liquidity substitutes for voice.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_public_shareholders, beneficiary,
    moderate, biographical, mobile, global).

% Hold Class A shares as part of diversified index positions; cannot easily exit single holdings without tracking-error cost. Under the stewardship reading, they benefit from the founder's insulation against short-term activist pressure that might otherwise force value-destroying buybacks or breakups. Occasionally vote (where permitted) on narrow matters but accept the control structure as priced into the index weighting.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_horizon_index_funds, beneficiary,
    organized, generational, constrained, global).

% Would ordinarily accumulate stakes and press for board seats, buybacks, or strategic change. The dual-class structure makes any such campaign mathematically unwinnable regardless of economic stake accumulated, so most do not attempt it here. Under this reading their exclusion is framed as protection of the company from precisely the short-termism they would introduce.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_investors, excluded,
    powerful, biographical, constrained, national).

% Nominally supervises management and owes fiduciary duties to all shareholders, but board composition is effectively determined by the founder's voting control. Ratifies the founder's long-horizon strategic choices and can be read either as genuine mission-fidelity oversight or as a formality that tracks the controller's preferences.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, board_of_directors, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, board_of_directors, agenda_setter).

% Permit dual-class listings subject to disclosure requirements, taking the position that informed market participants who buy Class A shares at IPO have consented to the structure. Monitor sunset provisions and disclosure adequacy but do not mandate voting parity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_super_voting_holder).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single decision-maker with a stable, undiluteable mandate can commit the firm to long-horizon investments (R&D, platform-building, mission-driven strategy) that would be difficult to sustain under a governance structure exposed to quarter-by-quarter market discipline or hostile takeover threat.
% TRANSFER_FUNCTION: Voting control is concentrated in the founder relative to economic stake; in exchange, Class A holders receive (on this reading) the benefit of shielded strategic continuity and, in principle, superior long-run value creation that a more diffusely-controlled company might not achieve under short-term pressure.
% ABSENT_VOICES: Activist investors and any Class A holder bloc that might otherwise coordinate to change strategy are structurally unable to translate economic stake into governance influence; their objection — that they bear the capital risk without commensurate voice — is not adjudicated inside the constraint, only outside it in the disclosure_consent and minority_extraction readings.
% DISAPPEARANCE_RATIONALE: If the dual-class structure were dissolved overnight (one-share-one-vote), the stewardship reading holds the company's long-horizon strategic commitments would be immediately vulnerable to activist pressure and possible short-term-value-maximizing changes — mission continuity would be at risk. The minority_extraction reading would say governance simply normalizes to match capital risk, which is not the same as the world 'rearranging' in a harmful sense. The verdict is genuinely contested between the readings, not resolved by this one.
% FOUNDING_PROBLEM: At IPO, founders sought a mechanism to prevent near-term market pressure and potential hostile acquisition from displacing a long-horizon strategic vision before it could be executed and prove itself.
% FOUNDING_PROBLEM_CORROBORATION: The founder and allied board members attest the mission-protection problem remains live (citing continued strategic bets that would not survive activist pressure). Independent corporate-governance scholars and some institutional investors attest that, years after IPO, the 'protection from short-termism' rationale increasingly functions as insulation from any accountability at all, regardless of whether mission-critical decisions are still being made — this corroboration comes from outside the founder's own governance apparatus but is itself disputed by proponents of the stewardship reading.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.42 by interval end) and rising modestly: even under the most charitable stewardship framing, disproportionate control without disproportionate capital risk carries some structural extraction — the founder's ability to entrench, self-deal on compensation, or resist accountability grows as the company matures and the original 'protect the mission' rationale becomes harder to distinguish from simple entrenchment. Suppression (0.58) is authored higher than extraction because activist and minority-shareholder alternatives are structurally foreclosed by the voting mechanics themselves, regardless of whether the founder is currently exercising control well. Theater ratio is kept low-moderate (0.22): under this reading the coordination function (insulated long-horizon decision-making) is substantially real, not merely performed, though it modestly rises over time as the 'protecting the founding mission' story is invoked for decisions increasingly distant from the original founding vision.
 *
 * DIRECTIONALITY LOGIC:
 *   Class A public shareholders and long-horizon index funds are declared beneficiaries under this reading — the stewardship claim is precisely that their economic interest is served indirectly through mission continuity, even though they hold no proportional vote. This is the load-bearing structural move of the reading: benefit flows without control. The founder is both agenda_setter and beneficiary, holding the coordination role with genuine exit-grade liquidity via control-premium share sales — a position no other seat has. Activist investors are excluded rather than victimized in this reading's own terms: the reading frames their exclusion as protective rather than extractive, which is exactly the structural claim other readings of the kernel dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading is vulnerable to mandatrophy exactly where founding_problem_status is authored contested: if the original problem (protecting a nascent, unproven strategic vision from premature market discipline) has been solved or superseded by corporate maturity, but the control structure persists and its costs (suppression of alternatives) continue to compound, the arrangement would be mislabeled coordination when its founding function has quietly become inertial protection. This story does not resolve that question — it is exactly the omega and six_questions machinery's job to keep the question open rather than let either the founder's or the critics' self-interested narrative settle it by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_entrenchment_boundary,
    'At what point does founder control shift from genuinely protecting an unproven long-horizon mission to simple entrenchment insulated from any accountability, and is that point identifiable from outside the founder''s own governance apparatus?',
    'Track strategic decisions over time against the original founding thesis: if decisions increasingly diverge from the founding mission while control remains equally concentrated, and if independent directors or minority shareholders cannot meaningfully test this divergence, entrenchment is the better explanation than stewardship.',
    'If entrenchment dominates, this reading''s premise (control serves all shareholders via mission continuity) collapses into the minority_extraction reading''s premise for the same structure — the same arrangement, differently classified because the underlying fact pattern has shifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_entrenchment_boundary, empirical, 'Whether founder control still tracks the founding mission or has become self-perpetuating.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to author this arrangement under the founder_stewardship reading rather than the minority_extraction or disclosure_consent readings itself doing evaluative work, or is it a neutral description of one live structural possibility among the three?',
    'Compare how each reading''s authored beneficiaries/victims and epsilon would classify the identical company at the identical point in time; persistent divergence in classification across readings for the same real-world facts indicates the readings are not merely different descriptions but different normative commitments about what legitimacy requires.',
    'If the three readings produce materially different classifications for the same company, the kernel itself (not any single reading) is the appropriate unit of contest — this favors treating dual-class legitimacy debates as genuinely kernel-level rather than resolvable by better facts alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether reading selection for a given company is a factual or a normative choice.').

omega_variable(
    class_a_indirect_benefit_measurability,
    'Can the claimed indirect benefit to Class A holders (superior long-run value from insulated decision-making) actually be measured against a counterfactual one-share-one-vote governance regime for the same company?',
    'Event studies comparing dual-class firms to matched one-share-one-vote peers at IPO cohort and sector, tracking long-horizon total shareholder return and strategic-investment persistence.',
    'If dual-class firms systematically underperform matched peers over the long horizon, the stewardship reading''s core empirical premise is weakened even though its normative framing remains internally coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_a_indirect_benefit_measurability, empirical, 'Whether the stewardship benefit claim is empirically supported.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__founder_stewardship, theater_ratio, 4, 0.13).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__founder_stewardship, theater_ratio, 8, 0.16).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.18).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__founder_stewardship, theater_ratio, 16, 0.2).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__founder_stewardship, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__founder_stewardship, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__founder_stewardship, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dual_su_t4, dual_class_legitimacy__founder_stewardship, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__founder_stewardship, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__founder_stewardship, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, enforcement_mechanism).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dual_class_legitimacy kernel. dual_class_legitimacy__minority_extraction treats the same share structure as extractive by default, requiring governance proportional to capital risk; dual_class_legitimacy__disclosure_consent grounds legitimacy in IPO-stage informed consent independent of any stewardship or extraction claim. Each carries its own epsilon and classification; the three are linked here via affects_constraints rather than merged into one story, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
