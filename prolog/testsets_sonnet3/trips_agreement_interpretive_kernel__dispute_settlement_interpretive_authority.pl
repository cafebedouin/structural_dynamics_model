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
 *   domain: International Trade Law / Public Health Policy / Intellectual Property Regime
 *
 * SUMMARY:
 *   This story reads the TRIPS kernel through the interpretive-authority
 *   lens: the constraint is not the treaty text itself, nor either
 *   substantive reading of it (strong exclusivity vs. public health
 *   flexibility), but the meta-arrangement by which a specific institutional
 *   mechanism — WTO dispute panels backed by trade retaliation authorization
 *   — decides, case by case, which reading of the ambiguous text controls.
 *   The 2019 Appellate Body collapse (loss of quorum from U.S. blocking of
 *   appointments) is the central inflection: it did not eliminate this
 *   constraint but transformed it, since panel rulings can now be appealed
 *   'into the void' by parties who lose, effectively making the interpretive
 *   authority binding only against parties without the leverage to escape it.
 *   The theater ratio rises sharply after 2019 because much of the remaining
 *   dispute settlement activity performs adjudicative legitimacy while the
 *   substantive enforcement increasingly runs through bilateral retaliation
 *   threats that route around the panels entirely.
 *
 * KEY AGENTS:
 *   - wto_dispute_panels: institutional agenda-setter — issues binding interpretive rulings, but rulings' bindingness is now asymmetric post-Appellate-Body collapse
 *   - originator_pharmaceutical_exporters: organized beneficiary with arbitrage exit — benefits from narrow-flexibility precedent and deterrent effect on compulsory licensing
 *   - developed_member_trade_delegations: institutional beneficiary/agenda-setter — controls panel composition politics and increasingly substitutes unilateral retaliation for adjudication
 *   - generic_manufacturing_states: moderate-power payer, constrained exit — bears direct restriction of licensing latitude from adverse precedent
 *   - low_income_country_health_ministries: powerless payer, trapped exit — depends entirely on interpretations decided by others
 *   - trade_law_scholars: analytical observer — documents the precedent drift and the post-2019 asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.71).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Panel Interpretive Authority Over TRIPS Text").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "International Trade Law / Public Health Policy / Intellectual Property Regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '6a4de718-1f49-4955-b0a8-2be402ee2460').
narrative_ontology:cs_kernel_codification('6a4de718-1f49-4955-b0a8-2be402ee2460', formalized).
narrative_ontology:cs_authority_grounding('6a4de718-1f49-4955-b0a8-2be402ee2460', extraction).
narrative_ontology:cs_interpretation_layer_present('6a4de718-1f49-4955-b0a8-2be402ee2460').
narrative_ontology:cs_reading_relation('6a4de718-1f49-4955-b0a8-2be402ee2460', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('6a4de718-1f49-4955-b0a8-2be402ee2460', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('6a4de718-1f49-4955-b0a8-2be402ee2460', foundational, panel_precedent_binds_absent_formal_doctrine).
narrative_ontology:cs_axiom_status(panel_precedent_binds_absent_formal_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('6a4de718-1f49-4955-b0a8-2be402ee2460', panel_precedent_binds_absent_formal_doctrine, conventional).
narrative_ontology:cs_axiom('6a4de718-1f49-4955-b0a8-2be402ee2460', secondary, retaliation_authorization_is_legitimate_enforcement_not_power_politics).
narrative_ontology:cs_axiom_status(retaliation_authorization_is_legitimate_enforcement_not_power_politics, holdable).
narrative_ontology:cs_axiom_grounding('6a4de718-1f49-4955-b0a8-2be402ee2460', retaliation_authorization_is_legitimate_enforcement_not_power_politics, instrumental).
narrative_ontology:cs_reference_frame('6a4de718-1f49-4955-b0a8-2be402ee2460', multilateral_binding_two_tier_adjudication).
narrative_ontology:cs_drift_state('6a4de718-1f49-4955-b0a8-2be402ee2460', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a4de718-1f49-4955-b0a8-2be402ee2460', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_member_trade_delegations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_country_health_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue binding rulings on TRIPS text meaning when a member state brings a complaint. Rulings become precedent that subsequent panels treat as authoritative even though the WTO has no formal stare decisis doctrine. Since the Appellate Body's collapse (no quorum since 2019), panel rulings can be appealed into a legal void, which some large members exploit to block adverse findings while continuing to invoke favorable ones.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% File complaints or lobby home governments to file complaints when a member state issues a compulsory license or permits parallel importation. Benefit from panel readings that narrow the flexibilities available under Articles 30/31, and from the deterrent effect of retaliation threats on states considering compulsory licenses. Can relocate manufacturing and licensing arrangements across jurisdictions to preserve exclusivity regardless of any single ruling.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters, beneficiary,
    organized, biographical, arbitrage, global).

% Bring or threaten disputes on behalf of domestic industries, shape panel composition through appointment politics, and increasingly bypass multilateral adjudication entirely by threatening unilateral tariff retaliation (Section 301-style actions) when a panel outcome is unfavorable or unavailable due to Appellate Body paralysis.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_member_trade_delegations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_member_trade_delegations, agenda_setter).

% Depend on generic pharmaceutical export revenue and domestic supply capacity; a panel ruling narrowing compulsory licensing conditions (as in the India-related disputes) directly reduces their license-granting latitude and exposes them to retaliation threats if they act on flexibilities the dispute settlement system has not affirmatively cleared. Cannot exit the WTO without catastrophic loss of market access for unrelated exports.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states, payer,
    moderate, biographical, constrained, national).

% Need affordable medicines during health crises but rarely have standing, legal capacity, or diplomatic leverage to bring or defend disputes themselves; their access depends on what larger generic-producing states are permitted to supply under panel-shaped interpretations of Article 31bis, decided in fora where they have no independent voice.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_country_health_ministries, payer,
    powerless, immediate, trapped, national).

% Would argue for expansive, permanent flexibility interpretations but lack the legal and diplomatic capacity to bring cases or intervene meaningfully as third parties; their transition-period extensions are negotiated by others and their substantive interests are represented, if at all, through NGO amicus submissions panels are not obligated to weigh.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_country_bloc, excluded,
    powerless, biographical, trapped, global).

% Document the health impact of restrictive readings and submit amicus briefs but have no standing to initiate or be party to a dispute; their evidence enters the record only at panel discretion.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ngo_coalitions, excluded,
    organized, generational, constrained, global).

% Analyze panel reasoning, track the drift of TRIPS jurisprudence, and document the growing gap between the treaty text's negotiated ambiguity and the narrowing case law that has accumulated through selective litigation by well-resourced parties.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, binding forum to resolve genuine disagreements about what an intentionally ambiguous multilateral text (TRIPS) requires, so 164 members are not each unilaterally interpreting their own obligations and retaliating ad hoc.
% TRANSFER_FUNCTION: Moves interpretive settlement power from the negotiated text itself (which embeds deliberate ambiguity and flexibility) to whichever party has the legal capacity, resources, and standing to litigate — moving practical latitude away from states that cannot afford to litigate or defend a dispute and toward states and industries that can repeatedly bring or threaten one.
% ABSENT_VOICES: Least-developed countries and public health NGO coalitions would argue for maximal, durable flexibility readings but have no party standing; their interests enter only through third-party submissions or amicus briefs a panel may disregard, and through the diplomatic advocacy of larger states with their own agendas.
% DISAPPEARANCE_RATIONALE: If binding panel interpretive authority disappeared overnight, TRIPS obligations would revert to unilateral self-interpretation by each member subject only to diplomatic pressure and bilateral retaliation threats — which, notably, is already increasingly the operative reality post-Appellate Body collapse. States would litigate less and negotiate/threaten more; large economies with retaliation capacity would gain relative power, small economies would lose the (already thin) protection of a rules-based forum.
% FOUNDING_PROBLEM: TRIPS text was deliberately negotiated with ambiguous language (particularly around compulsory licensing conditions and the scope of exceptions) to secure agreement across states with conflicting interests; a mechanism was needed to resolve disputes about what the ambiguous language meant in concrete cases without each member unilaterally retaliating.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars and several WTO Secretariat retrospectives attest the dispute settlement mechanism has drifted from neutral text-clarification toward accumulation of precedent that systematically favors well-resourced repeat litigants — a reading corroborated by empirical case-outcome studies (e.g., analyses of the India compulsory-licensing and Australia plain-packaging disputes) conducted by academics outside both the pharmaceutical industry and government trade delegations. Developed-member delegations themselves attest the mechanism still performs its founding coordination function faithfully; this is a self-interested attestation from a benefiting party and is weighted accordingly.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.38 at TRIPS inception to 0.68 by 2025 because the coordination function (a shared forum for resolving genuine textual ambiguity) has been progressively supplemented, and after 2019 partially supplanted, by an enforcement structure whose bindingness runs one direction: against states without retaliation capacity of their own, and increasingly optional for states that do have it. Suppression (0.71) reflects that the deterrent threat of trade retaliation shapes behavior even absent a completed dispute — generic manufacturers and health ministries adjust policy pre-emptively to avoid triggering a process they cannot survive, whether or not a panel ever rules. Theater ratio's post-2019 climb to 0.42 captures the growing gap between the appearance of rules-based multilateral adjudication and the practical substitution of bilateral power politics, especially where a losing party appeals into the nonfunctional Appellate Body and simply stalls compliance indefinitely.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a developed-member trade delegation, this system is functioning exactly as designed: a rules-based forum that has, over three decades, produced a coherent and predictable body of interpretive law. From the seat of a generic manufacturing state or health ministry, the same structure looks like a one-way ratchet — every consequential ruling since TRIPS entry into force has narrowed rather than expanded flexibility, and the 2019 collapse removed the appellate check specifically at the moment it might have started correcting course. The engine's per-seat computation is expected to diverge sharply between the institutional agenda-setter/beneficiary seats and the powerless/moderate payer seats — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed-member delegations and originator pharmaceutical exporters sit near the beneficiary end: they shape panel composition, can afford repeat litigation, and increasingly hold a credible unilateral retaliation option that operates whether or not the multilateral mechanism functions — this is why their exit_options is coded arbitrage rather than merely mobile. Generic manufacturing states and low-income health ministries sit near the target end: their obligations are given concrete, binding content by rulings they had limited capacity to shape, and their behavior is constrained by retaliation threats regardless of formal panel outcomes. The powerless health ministries in particular experience suppression as anticipatory compliance — a chilling effect operating upstream of any actual dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding unilateral self-interpretation and ad hoc retaliation in a text negotiated with deliberate ambiguity) is genuinely contested as live or dead: the underlying need for SOME adjudicative mechanism persists (states do still disagree about what TRIPS requires), so this is not a pure zombie mandate. But the specific FORM the mechanism now takes — binding panels with a broken appellate check, functioning asymmetrically because retaliation capacity is unevenly distributed — has drifted from the founding design (a two-tier system meant to constrain error and correct one-sided readings) toward something closer to its own critique: unilateral power politics, now merely laundered through a partially-functioning multilateral shell. This is precisely the tangled-rope signature: real coordination function persists (the forum reduces some unilateral chaos) while asymmetric extraction operates through the same structure, and active enforcement (retaliation authorization) is required to hold it together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_as_law_vs_ad_hoc_ruling,
    'Do WTO panel rulings function as genuine binding precedent (stare decisis in practice, even without formal doctrine) or as a series of ad hoc, case-specific findings that later panels are free to depart from?',
    'Systematic citation analysis across panel reports: track how often panels cite and follow prior rulings on the same TRIPS provisions versus distinguish or ignore them, and whether the rate has changed post-2019.',
    'If rulings function as de facto binding precedent, the interpretive-authority constraint is substantially more consequential and more clearly tangled-rope (real coordination plus locked-in asymmetric extraction); if panels routinely depart from prior findings, the constraint is closer to a weak coordination mechanism whose apparent narrowing is coincidental rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_as_law_vs_ad_hoc_ruling, empirical, 'Whether panel rulings operate as binding precedent or ad hoc findings.').

omega_variable(
    appellate_body_collapse_intentionality,
    'Was the U.S. blockade of Appellate Body appointments (producing the 2019 quorum collapse) a deliberate strategy to convert binding multilateral adjudication into a system where only unilateral retaliation capacity is decisive, or a defensive response to perceived judicial overreach by the Appellate Body itself?',
    'Diplomatic history analysis: USTR position papers, congressional testimony, and comparison of stated overreach grievances against the pattern of which rulings were and were not appealed into the void afterward.',
    'If deliberate strategy, the current asymmetric-enforcement structure is better read as an intended feature of the interpretive-authority constraint rather than an unintended side effect, strengthening the tangled_rope classification and its coupling to the strong_exclusivity_reading. If defensive, the resulting asymmetry is better modeled as an emergent, unintended drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appellate_body_collapse_intentionality, conceptual, 'Whether the enforcement asymmetry is designed or emergent.').

omega_variable(
    kernel_framing_locus_of_disagreement,
    'Is the location of disagreement across the three kernel readings best modeled as a dispute over WHAT the TRIPS text means (substantive readings) or over WHO has final authority to fix that meaning (this reading) — and does treating them as fully separable understate how precedent from this reading retroactively determines which substantive reading counts as ''the text''?',
    'Track whether panel rulings under this constraint are cited in subsequent substantive policy debates as settling the strong_exclusivity vs. public_health_flexibility question, versus treated by public health advocates as one contestable interpretation among others still open to renegotiation.',
    'If panel rulings are treated as settling the substantive question, this reading''s classification as a separate meta-constraint understates its causal weight — it would functionally BE the mechanism deciding the sibling readings'' practical fate, not merely a procedural layer above them. If panel rulings are widely treated as contestable and non-final by public health advocates and some member states, the three-way decomposition holds cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_locus_of_disagreement, conceptual, 'Whether decomposing interpretive authority from substantive reading understates the authority reading''s causal centrality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(trip_tr_t2012, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2012, 0.29).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(trip_be_t2012, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(trip_su_t2012, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2012, 0.63).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is the interpretive-authority reading in a three-member kernel family under trips_agreement_interpretive_kernel. strong_exclusivity_reading and public_health_flexibility_reading each author their own ε for the substantive content of the TRIPS text as their reading sees it; this story authors ε for the separate question of WHO gets to adjudicate that contest and HOW enforcement operates. The three do not share ε — this reading's extractiveness (0.68) describes asymmetric enforcement capacity, not textual content, and is not to be averaged with or substituted for either sibling's ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
