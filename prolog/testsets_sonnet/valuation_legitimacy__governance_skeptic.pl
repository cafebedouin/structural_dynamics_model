% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Dual-Class Voting Control as Valuation Legitimacy Failure (Governance-Skeptic Reading)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the governance-skeptic reading of the
 *   valuation_legitimacy kernel applied to a Musk-controlled entity (Terafab)
 *   carrying a dual-class share structure: 82.4% voting control derived from
 *   Class B shares on only 42% of economic equity, no independent
 *   compensation or nominating committees under controlled-company listing
 *   exemptions, a charter-level renunciation of corporate opportunities in
 *   Musk's favor, and Musk's attention divided across five-plus companies
 *   with no disclosed allocation mechanism. The reading's core claim is that
 *   valuation legitimacy is conditioned on governance structures that protect
 *   minority shareholders from expropriation of control value — and that this
 *   structure fails that condition, making the resulting $1.75T valuation
 *   partly a price on private benefits of control rather than a price on
 *   distributable shareholder value. Sibling readings (dcf_fundamentalist,
 *   real_options_technologist, musk_cult_believer) are NOT part of this
 *   story; they price the same entity through cash-flow discipline,
 *   technological option value, and founder-track-record trust respectively,
 *   and would each classify this arrangement differently. This story's ε,
 *   beneficiary/victim structure, and classification are stable and
 *   self-contained under the governance-skeptic frame only.
 *
 * KEY AGENTS:
 *   - musk: agenda_setter/beneficiary (institutional/arbitrage) — sets governance terms, captures control premium
 *   - early_class_b_holders: beneficiary (organized/arbitrage) — insulated co-beneficiaries of vote concentration
 *   - class_a_public_shareholders: payer (powerless/constrained) — bear governance risk with no binding remedy
 *   - minority_institutional_investors: payer (moderate/constrained) — formal voting weight, mathematically immaterial
 *   - tesla_spacex_shareholders: excluded (moderate/constrained) — bear allocation externalities with no standing here
 *   - corporate_governance_analysts: observer (analytical) — document the pattern without power to compel change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.79).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.71).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.79).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Control as Valuation Legitimacy Failure (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'e7e28963-15a3-43d6-9c14-03bbbf36d35b').
narrative_ontology:cs_kernel_codification('e7e28963-15a3-43d6-9c14-03bbbf36d35b', distributed).
narrative_ontology:cs_authority_grounding('e7e28963-15a3-43d6-9c14-03bbbf36d35b', distributed).
narrative_ontology:cs_reading_relation('e7e28963-15a3-43d6-9c14-03bbbf36d35b', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('e7e28963-15a3-43d6-9c14-03bbbf36d35b', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('e7e28963-15a3-43d6-9c14-03bbbf36d35b', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('e7e28963-15a3-43d6-9c14-03bbbf36d35b', foundational, governance_protection_precedes_valuation_legitimacy).
narrative_ontology:cs_axiom_status(governance_protection_precedes_valuation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e7e28963-15a3-43d6-9c14-03bbbf36d35b', governance_protection_precedes_valuation_legitimacy, conventional).
narrative_ontology:cs_axiom('e7e28963-15a3-43d6-9c14-03bbbf36d35b', foundational, control_premium_is_extraction_not_value_creation).
narrative_ontology:cs_axiom_status(control_premium_is_extraction_not_value_creation, holdable).
narrative_ontology:cs_axiom_grounding('e7e28963-15a3-43d6-9c14-03bbbf36d35b', control_premium_is_extraction_not_value_creation, empirically_contingent).
narrative_ontology:cs_reference_frame('e7e28963-15a3-43d6-9c14-03bbbf36d35b', one_share_one_vote_fiduciary_norm).
narrative_ontology:cs_drift_state('e7e28963-15a3-43d6-9c14-03bbbf36d35b', post_dual_class_ipo_wave_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e7e28963-15a3-43d6-9c14-03bbbf36d35b', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, minority_institutional_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, shareholder_primacy_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% of voting power via Class B shares carrying 10x votes despite only 42% economic equity. Sets board composition, executive compensation, and corporate-opportunity allocation across Tesla, SpaceX, X, Neuralink, and this entity (Terafab) simultaneously. The charter renounces corporate opportunities on his behalf, meaning profitable ventures he identifies need not be offered to this company first. Faces no binding accountability mechanism from public shareholders regardless of capital allocation decisions.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, musk, beneficiary).

% Hold the supervoting share class alongside Musk, insulated from the same governance exposure Class A holders face. Benefit from control concentration that suppresses activist challenges, hostile takeover premiums, and proxy contests that would otherwise discipline capital allocation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    organized, generational, arbitrage, global).

% Hold 58% of economic equity but a small minority of votes. Cannot replace the board, cannot contest executive compensation through an independent committee (none exists under the controlled-company exemption), and cannot compel disclosure of how Musk allocates time or opportunity across his five-plus companies. Exit means selling shares at whatever price the market sets for a stock structurally priced on Musk's personal brand and control premium rather than auditable cash flows; there is no internal remedy.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, constrained, global).

% Index funds and pension managers hold Class A shares as a matter of benchmark tracking or fiduciary mandate, not conviction in the governance structure. They have voting weight on paper but it is mathematically immaterial against 82.4% control. Some have publicly objected to prior Musk compensation packages at other controlled entities and lost in court or via ratification votes structurally weighted against them; their formal objection channel exists but cannot bind outcomes.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, minority_institutional_investors, payer,
    moderate, biographical, constrained, global).

% Not shareholders of this entity, but structurally implicated: if Musk's attention, capital-allocation decisions, or corporate opportunities are diverted toward this venture, Tesla and SpaceX shareholders bear an opportunity cost with no seat at this table and no visibility into the allocation decision that affects them.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_spacex_shareholders, excluded,
    moderate, biographical, constrained, global).

% Academic and institutional-investor-adjacent researchers who study dual-class structures, controlled-company exemptions, and related-party allocation problems across Musk's corporate empire. They document the structural pattern and its precedents (prior compensation litigation, corporate-opportunity waivers) without holding a seat that can compel change.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, corporate_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supervoting structure is presented as solving a founder-dilution problem: it lets Musk pursue long-horizon, high-risk technological bets (reusable rockets, AI, robotics) without being deposed by short-term-focused public shareholders during volatile periods.
% TRANSFER_FUNCTION: Moves control premium and private benefits of control — freedom from accountability on compensation, capital allocation, time allocation, and corporate-opportunity capture — from the 58%-equity-holding Class A public shareholders to Musk and Class B holders, who hold 82.4% of votes on 42% of the equity claim.
% ABSENT_VOICES: Class A shareholders have record votes on compensation and governance matters but are mathematically foreclosed from prevailing; Tesla and SpaceX shareholders bear allocation externalities from Musk's divided attention but have no standing in this entity's governance at all. Neither voice can alter the outcome regardless of how they vote or object.
% DISAPPEARANCE_RATIONALE: If the dual-class structure were unwound to one-share-one-vote, Class A shareholders would gain the capacity to replace directors, install an independent compensation committee, contest the corporate-opportunity waiver, and demand disclosure on cross-company time and capital allocation. The $1.75T valuation, built in significant part on a control premium and a personality-driven narrative rather than auditable governance-protected cash flows, would likely reprice; board composition and executive pay structures would change within one to two proxy cycles.
% FOUNDING_PROBLEM: Founder-controlled dual-class structures were originally justified by the problem of activist short-termism suppressing long-horizon R&D investment — a founder needs insulation from quarterly-earnings pressure to build capital-intensive, multi-decade technology.
% FOUNDING_PROBLEM_CORROBORATION: Musk and Class B holders attest the structure remains necessary to protect long-horizon execution from short-term shareholder pressure. Independent corporate-governance researchers, proxy advisory firms (ISS, Glass Lewis, in analogous Tesla compensation votes), and minority institutional investors who have litigated or voted against comparable Musk-controlled structures attest that the founding rationale has been substantially superseded by accountability-avoidance function — the insulation now primarily shields compensation, corporate-opportunity, and cross-company allocation decisions from any independent review, not R&D timelines specifically.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.79, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the measured interval (0.55→0.79) reflecting a widening gap between the 42% economic claim and the 82.4% control claim as the entity's valuation compounds and the control premium becomes a larger absolute-dollar transfer. Suppression (authored via suppression_requirement, 0.48→0.71) tracks the hardening of the controlled-company exemption posture — as scrutiny of comparable Musk-controlled compensation structures increased industry-wide, the entity's board correspondingly formalized the exemption rather than relaxing it, which is enforcement intensification, not decay. Theater ratio (0.22→0.42) captures growing performative governance: press-released 'independent director' additions and voluntary disclosure gestures that do not carry binding authority, rising as external criticism rose. All three series share one time grid (0,4,8,12,16,20,24) per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From Musk's seat this is efficient founder-protected long-horizon execution; the classification computed from that seat's structural position (institutional power, arbitrage exit, direct capture of the control premium) will diverge sharply from the classification computed from the class_a_public_shareholders seat (powerless, constrained exit, bears the transfer). This divergence is exactly the structure a tangled_rope claim predicts: real coordination function (insulation from short-term pressure) coexists with asymmetric extraction (control premium capture) through the same mechanism, and the engine should show both seats' computed types differing sharply rather than converging.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk and early Class B holders derive low d (beneficiary end): they hold disproportionate control, face no binding accountability mechanism, and have arbitrage-grade exit (can sell, restructure, or redeploy attention across ventures at will). Class A public shareholders and minority institutional investors derive high d (target end): they bear the economic risk of capital-allocation decisions they cannot influence, face constrained exit (sell at a control-premium-distorted price or hold), and have no internal remedy. Tesla/SpaceX shareholders are excluded rather than positioned on the d spectrum for this entity specifically — they are affected by spillover, not by this constraint's direct operation, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insulating long-horizon technological execution from short-term shareholder pressure) may still be partially live for R&D-timeline purposes, but the founding_problem_status is authored as contested because the same insulation mechanism now also shields compensation-setting, corporate-opportunity allocation, and cross-company time allocation from any independent review — functions that were never part of the original justification. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (insulation from short-termism has real value for capital-intensive R&D) while still naming the asymmetric extraction riding on the same structural mechanism — collapsing it to pure snare would erase the coordination story's partial validity; calling it rope would erase the victim set and enforcement requirement entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four valuation_legitimacy readings disagree — is it about what data counts as evidence (cash flow vs. optionality vs. track record vs. governance), or about a shared premise regarding what makes a valuation legitimate at all?',
    'Not empirically resolvable within this story; documented here as the committer-structure record per Rule 2. Would require comparing the four sibling constraint stories'' cs_structure.axioms directly to locate whether any pair genuinely forecloses another or merely coexists as different evidentiary priorities.',
    'If the disagreement is purely evidentiary (which data source to trust), the readings coexist and a synthesis valuation could in principle blend them. If it is a premise-level disagreement about what ''legitimate valuation'' even means (governance-protected value vs. option value vs. trust-based value), no synthesis is coherent and the market price reflects a bet on which premise wins culturally, not a resolved valuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates whether the four kernel readings are compatible evidentiary disputes or incompatible premise disputes.').

omega_variable(
    corporate_opportunity_waiver_scope_ambiguity,
    'How much of the $1.75T valuation is attributable to expected future ventures Musk might route into this entity under the corporate-opportunity waiver, versus attributable to disclosed current operations?',
    'Would require forensic decomposition of analyst valuation models and forward guidance to separate the discounted value of disclosed operations from the option value embedded in Musk''s undisclosed future allocation decisions.',
    'If a large share of the valuation is priced on undisclosed future allocation optionality that only Musk controls, the effective extraction is understated by this story''s ε — public shareholders would be paying today for option value they cannot compel to be exercised in their favor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_opportunity_waiver_scope_ambiguity, empirical, 'Whether the valuation prices governance-inaccessible future optionality that inflates ε beyond the authored estimate.').

omega_variable(
    control_premium_versus_coordination_value_separability,
    'Is the control premium Musk captures separable from the genuine coordination value of insulated long-horizon execution, or are they the same dollar measured two ways?',
    'Compare valuation and execution outcomes across founder-controlled versus one-share-one-vote peer companies pursuing comparably capital-intensive, long-horizon technology bets; a persistent execution premium under one-share-one-vote governance would indicate separability.',
    'If separable, the tangled_rope classification is well-founded — coordination value and extraction are distinct components. If inseparable, part of what this reading calls ''extraction'' may be the necessary price of the coordination function itself, weakening the governance-skeptic reading''s distinctiveness from the real_options_technologist reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(control_premium_versus_coordination_value_separability, conceptual, 'Whether governance-skeptic''s ''extraction'' component is empirically distinguishable from genuine long-horizon coordination value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.22).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.27).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.31).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.35).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.38).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.4).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__governance_skeptic, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__governance_skeptic, base_extractiveness, 24, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__governance_skeptic, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.1).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the valuation_legitimacy kernel applied to the same entity. Each reading is ε-invariant and self-contained: governance_skeptic (this story, tangled_rope, ε≈0.79) grounds legitimacy in minority-shareholder protection and finds the dual-class structure extractive; dcf_fundamentalist grounds legitimacy in discounted proven cash flows; real_options_technologist grounds legitimacy in technological option value; musk_cult_believer grounds legitimacy in founder track record. The four are linked via affects_constraints, not merged — no single ε or classification averages across them. This story's beneficiary/victim structure (Musk and Class B holders vs. Class A public shareholders) is specific to the governance frame and should not be imported into the sibling stories' base_properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
