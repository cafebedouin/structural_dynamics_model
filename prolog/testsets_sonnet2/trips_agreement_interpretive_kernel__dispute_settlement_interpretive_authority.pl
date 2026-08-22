% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Panel Interpretive Authority Over TRIPS Text
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint isolates the dispute-settlement interpretive-authority
 *   reading of the TRIPS kernel: the claim that WTO panels hold binding
 *   authority to say what TRIPS text means, with that authority backed by
 *   trade retaliation. It is deliberately NOT the text-level question of
 *   whether TRIPS embeds broad public-health flexibilities
 *   (public_health_flexibility_reading) or mandates strong uniform patent
 *   protection (strong_exclusivity_reading) — those are separate readings of
 *   the same kernel with their own ε and their own stakeholder sets. This
 *   reading is about who gets to settle the argument and how that settlement
 *   is enforced, which is a meta-level constraint sitting on top of the
 *   substantive dispute. Since 2019, the collapse of Appellate Body quorum
 *   has hollowed out the multilateral half of this mechanism: first-instance
 *   panel rulings can be 'appealed into the void,' making them either final
 *   by default or negotiable only through bilateral leverage, which is
 *   exactly the raw-power dynamic the mechanism was built to replace.
 *
 * KEY AGENTS:
 *   - wto_dispute_panel_secretariat: administers rulings, institutional/analytical exit
 *   - originator_pharmaceutical_exporters: organized beneficiary using panel precedent and threatened disputes to preserve exclusivity
 *   - developed_country_trade_ministries: agenda-setting beneficiary wielding retaliation threat, increasingly bypassing the stalled multilateral forum
 *   - generic_manufacturing_states: payer with constrained exit, exposed to unappealable adverse rulings
 *   - low_income_treatment_access_populations: powerless, trapped, bearing downstream cost with no standing
 *   - least_developed_country_negotiators: excluded from effective bargaining despite formal membership
 *   - international_health_law_scholars: analytical observer tracking the drift from adjudication to coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Panel Interpretive Authority Over TRIPS Text").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '83146f36-a857-4f39-9672-a1e03cdca9f3').
narrative_ontology:cs_kernel_codification('83146f36-a857-4f39-9672-a1e03cdca9f3', fixed_text).
narrative_ontology:cs_authority_grounding('83146f36-a857-4f39-9672-a1e03cdca9f3', lineage).
narrative_ontology:cs_interpretation_layer_present('83146f36-a857-4f39-9672-a1e03cdca9f3').
narrative_ontology:cs_reading_relation('83146f36-a857-4f39-9672-a1e03cdca9f3', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_reading_relation('83146f36-a857-4f39-9672-a1e03cdca9f3', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_axiom('83146f36-a857-4f39-9672-a1e03cdca9f3', foundational, binding_third_party_adjudication_legitimate_over_unilateral_action).
narrative_ontology:cs_axiom_status(binding_third_party_adjudication_legitimate_over_unilateral_action, holdable).
narrative_ontology:cs_axiom_grounding('83146f36-a857-4f39-9672-a1e03cdca9f3', binding_third_party_adjudication_legitimate_over_unilateral_action, conventional).
narrative_ontology:cs_axiom('83146f36-a857-4f39-9672-a1e03cdca9f3', secondary, retaliation_authorization_is_necessary_enforcement_not_extraction).
narrative_ontology:cs_axiom_status(retaliation_authorization_is_necessary_enforcement_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('83146f36-a857-4f39-9672-a1e03cdca9f3', retaliation_authorization_is_necessary_enforcement_not_extraction, instrumental).
narrative_ontology:cs_reference_frame('83146f36-a857-4f39-9672-a1e03cdca9f3', multilateral_panel_plus_appellate_adjudication).
narrative_ontology:cs_drift_state('83146f36-a857-4f39-9672-a1e03cdca9f3', post_appellate_body_collapse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('83146f36-a857-4f39-9672-a1e03cdca9f3', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_trade_ministries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_treatment_access_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes panels, drafts rulings on TRIPS text, and issues interpretations that become binding precedent absent consensus reversal. Since the Appellate Body's collapse in 2019 (loss of quorum), panel rulings can be appealed into a legal void, which the panel apparatus itself cannot fix but continues to operate around.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panel_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Rely on panel rulings that read TRIPS obligations expansively to secure patent exclusivity in export markets, and lobby home-state trade ministries to bring or threaten disputes against states that issue compulsory licenses. Can relocate production and licensing structures across jurisdictions to preserve exclusivity even where a single ruling goes against them.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters, beneficiary,
    organized, generational, arbitrage, global).

% Bring disputes and threaten trade retaliation (tariff suspension, market access withdrawal) against states whose IP practice departs from panel precedent. Also negotiate bilateral and plurilateral side deals that route around a stalled Appellate Body, using the credible threat of retaliation as leverage independent of any actual ruling.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_trade_ministries, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_trade_ministries, beneficiary).

% Depend on compulsory licensing and parallel importation to supply generic medicines domestically and regionally. Face the threat of formal disputes or informal trade pressure (market access conditions, aid conditionality) if their licensing practice is read by panels or by powerful states as exceeding TRIPS flexibilities. Cannot appeal a panel ruling into a functioning appellate body, so an adverse first-instance ruling is effectively final.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states, payer,
    moderate, biographical, constrained, national).

% Bear the direct consequence when interpretive rulings or retaliation threats narrow the domestic supply of affordable generic medicines. Have no standing before the panel process itself and experience the constraint only through its downstream effect on drug price and availability.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_treatment_access_populations, payer,
    powerless, immediate, trapped, national).

% Would argue for a public-health-first reading of TRIPS flexibilities but lack the legal capacity, standing, and retaliatory leverage to bring or defend disputes on equal footing with organized industry-backed trade ministries. Their absence from the effective bargaining table means the interpretive contest is settled largely between originator-state and generic-state blocs without their direct participation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_country_negotiators, excluded,
    powerless, generational, trapped, global).

% Analyze panel rulings, track the drift from multilateral adjudication toward bilateral coercion post-Appellate Body collapse, and document which reading of TRIPS text is being locked in through accumulated precedent versus raw bargaining power.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, international_health_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, in-principle-neutral forum to resolve genuine disagreements about what TRIPS text requires, so that trade relations do not collapse into unilateral retaliation every time two members read the same clause differently.
% TRANSFER_FUNCTION: Moves interpretive authority over ambiguous TRIPS provisions (especially Article 31 compulsory licensing and Article 6 exhaustion/parallel import language) from the text itself and from national legislatures toward panel precedent, and moves compliance leverage from multilateral consensus toward whichever state can credibly threaten retaliation once the Appellate Body backstop is gone.
% ABSENT_VOICES: Least-developed-country negotiators and the patients who depend on generic access are not parties to disputes and have no seat at the panel table; their interests are represented, if at all, secondhand through amicus submissions or NGO advocacy that carries no binding weight.
% DISAPPEARANCE_RATIONALE: If binding panel interpretive authority vanished, TRIPS compliance disputes would revert either to pure bilateral negotiation (already accelerating post-Appellate-Body-collapse) or to unresolved textual ambiguity that each member state would interpret unilaterally; the entire architecture of 'TRIPS-consistent' compulsory licensing assessment, which currently anchors both originator litigation strategy and generic-state legal defense, would lose its reference point.
% FOUNDING_PROBLEM: The Uruguay Round negotiators needed a credible, rules-based mechanism to prevent trade disputes over intellectual property from being resolved by raw unilateral retaliation (as under the pre-WTO Section 301 regime), and to give TRIPS obligations some enforceable teeth beyond diplomatic pressure.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars outside the pharmaceutical and IP-exporting bloc (including several who served on early panels) attest that the mechanism functioned as designed through the 1990s and 2000s, but that the 2019 US-engineered collapse of Appellate Body quorum has shifted practice back toward the bilateral coercion the mechanism was built to replace — a reading corroborated by WTO Secretariat annual reports documenting the appeal backlog and by independent legal academics tracking the resulting rise in 'appeal into the void' filings, not merely asserted by generic-state governments who have an interest in delegitimizing the forum.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by 2025) and suppression (0.72) both rose steadily and then accelerated sharply after 2019 — the theater_ratio series shows the same inflection, because post-Appellate-Body the panel process increasingly produces rulings whose finality is enforced less by legal reasoning than by which state can credibly threaten retaliation, so procedural activity increasingly substitutes for adjudicated resolution. accessibility_collapse (0.58) and resistance (0.61) reflect that generic-state defenses and health-advocacy arguments are not foreclosed outright — some space for compulsory licensing survives (Doha Declaration precedent) — but the space has narrowed and is actively contested each time it is used.
 *
 * PERSPECTIVAL GAP:
 *   From the trade-ministry seat, this looks like the operation of a rules-based system doing exactly the coordination job it was built for. From the generic-manufacturing-state seat, the same structure increasingly looks like enforced extraction, because the appellate backstop that made the earlier era feel rules-based is gone, and what remains is asymmetric retaliation leverage dressed in the procedural language of adjudication. The engine computes these as different seat-level classifications from the same structural data; this story does not decide between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Originator exporters and developed-country trade ministries sit near the beneficiary end: they set the interpretive agenda through repeat litigation and treaty-drafting leverage, and can arbitrage across jurisdictions regardless of any single ruling's outcome. Generic-manufacturing states and treatment-access populations sit near the target end: they depend on a specific reading of flexibilities surviving panel scrutiny, cannot appeal an adverse ruling into a functioning appellate body, and have no exit from the consequences once a ruling or retaliation threat lands. Least-developed-country negotiators are formally inside the WTO but structurally excluded from the bargaining that actually determines outcomes — their d is high but their voice in the process is close to zero, which is a distinct fact from directionality and is captured instead in the excluded role and absent_voices field.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing unilateral Section-301-style retaliation with rules-based adjudication — is genuinely live in the sense that trade disputes over IP still need resolving, but the specific mechanism built to solve it (a two-tier panel-plus-appeal system) has partially died since 2019 while the institutional shell persists. Classifying this as tangled_rope rather than snare preserves the fact that the mechanism still does real coordination work in the majority of disputes that never reach the appeal-void problem; classifying it as tangled_rope rather than rope preserves the fact that its enforcement machinery now serves organized beneficiaries disproportionately once the multilateral backstop is gone. A pure snare label would erase the genuine dispute-resolution function still operating in un-appealed cases; a pure rope label would erase the asymmetric retaliation leverage that has grown since 2019.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adjudication_vs_bilateral_coercion_post_appellate_body,
    'After the Appellate Body''s loss of quorum, is TRIPS dispute resolution still functioning as multilateral adjudication (with panels providing a genuine, if imperfect, neutral forum) or has it substantively reverted to bilateral coercion with panel rulings serving as a legitimating veneer?',
    'Track the ratio of disputes resolved through the Multi-Party Interim Appeal Arbitration Arrangement (MPIA, the voluntary workaround) versus disputes where a losing party appeals into the void and the outcome is instead settled through direct bilateral trade pressure; a rising void-appeal-then-bilateral-settlement pattern would indicate substantive reversion.',
    'If reversion is substantial, this reading''s classification should drift further toward snare as the interpretive authority claim becomes decreasingly backed by actual adjudication and increasingly backed by raw retaliatory capacity alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_vs_bilateral_coercion_post_appellate_body, empirical, 'Whether post-2019 TRIPS dispute resolution is genuine adjudication or coercion wearing adjudicative form.').

omega_variable(
    precedent_lock_in_across_kernel_readings,
    'Do accumulated panel rulings function as genuine precedent that progressively forecloses the public_health_flexibility_reading of the TRIPS text, or do they remain narrow case-specific holdings that leave the underlying textual contest open?',
    'Comparative analysis of panel reasoning across the major compulsory-licensing and parallel-import disputes (e.g., Canada—Pharmaceutical Patents, the Section 211 dispute, and subsequent Doha-era practice) to assess whether later panels treat earlier rulings as binding interpretive constraints on flexibility or as fact-specific applications.',
    'If rulings function as progressive lock-in, this interpretive-authority reading structurally forecloses meaningful operation of the public_health_flexibility_reading over time even without any textual amendment — the meta-constraint would be doing substantive work that its own framing (mere dispute resolution) obscures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_lock_in_across_kernel_readings, conceptual, 'Whether panel precedent progressively narrows the substantive flexibility reading independent of text.').

omega_variable(
    retaliation_capacity_asymmetry_is_structural_or_contingent,
    'Is the asymmetry in retaliatory capacity between originator-exporting and generic-manufacturing states an intrinsic feature of the WTO enforcement design, or a contingent function of current economic power distribution that could shift?',
    'Examine whether smaller or coalition-based retaliatory threats (e.g., coordinated action by multiple generic-manufacturing states) have ever been credibly deployed and what effect they had, versus cases where only large-market states'' threats produced compliance.',
    'If the asymmetry is structural (built into how retaliation authorization and market size interact), this reading''s extraction is closer to intrinsic to the mechanism; if contingent, coalition strategies could rebalance the constraint without requiring institutional reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retaliation_capacity_asymmetry_is_structural_or_contingent, empirical, 'Whether retaliation-based enforcement is inherently asymmetric or merely currently so.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.36).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_exclusivity_reading).

% DUAL FORMULATION NOTE:
% This constraint, public_health_flexibility_reading, and strong_exclusivity_reading form a three-member family reading the trips_agreement_interpretive_kernel. The two substantive readings disagree about what TRIPS text requires; this constraint is the meta-level mechanism that adjudicates between them and enforces the outcome, so it structurally influences both — its degradation (post-Appellate-Body) changes the practical operating conditions for both substantive readings without changing the text either reading is about.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
