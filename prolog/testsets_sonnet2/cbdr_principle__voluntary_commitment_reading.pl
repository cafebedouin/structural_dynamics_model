% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__voluntary_commitment_reading, []).

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
 *   constraint_id: cbdr_principle__voluntary_commitment_reading
 *   human_readable: CBDR as Voluntary NDC Regime with Technology-Transfer Discharge
 *   domain: international_governance/climate/development_economics
 *
 * SUMMARY:
 *   This story instantiates the voluntary_commitment_reading of the Common
 *   But Differentiated Responsibilities (CBDR) principle within the
 *   UNFCCC/Paris Agreement architecture: developed nations discharge their
 *   CBDR obligation through non-binding, nationally determined contributions
 *   and technology-transfer pledges, rather than through emissions reductions
 *   calibrated to cumulative historical responsibility. Under this reading,
 *   developed nations exit the victim set entirely for binding mitigation
 *   exposure — no enforceable target constrains their pledged trajectory.
 *   Developing nations, particularly small island states and least-developed
 *   countries, enter the victim set for uncompensated adaptation costs: they
 *   bear the physical consequences of historical emissions without a
 *   corresponding guaranteed compensation or binding mitigation mechanism
 *   forcing the reduction of the emissions causing that harm. The sibling
 *   historical_responsibility_reading treats the same kernel text (Article
 *   3.1 UNFCCC / CBDR-RC) as requiring binding, proportionate reductions plus
 *   loss-and-damage financing — under THAT reading, developed nations remain
 *   squarely inside the victim/obligor set for binding mitigation exposure,
 *   and the extraction and victim structure inverts. The two readings are not
 *   the same constraint measured differently; they are different constraints
 *   sharing a textual kernel, which is why they are authored as separate
 *   files linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.61).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.42).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR as Voluntary NDC Regime with Technology-Transfer Discharge").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_governance/climate/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, 'ed6ebdde-28f4-47ce-b1bd-355b24e54e11').
narrative_ontology:cs_kernel_codification('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', fixed_text).
narrative_ontology:cs_authority_grounding('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', practice).
narrative_ontology:cs_interpretation_layer_present('ed6ebdde-28f4-47ce-b1bd-355b24e54e11').
narrative_ontology:cs_reading_relation('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', foundational, sovereignty_over_development_pathway_is_primary).
narrative_ontology:cs_axiom_status(sovereignty_over_development_pathway_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', sovereignty_over_development_pathway_is_primary, conventional).
narrative_ontology:cs_axiom('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', secondary, technology_transfer_discharges_differentiated_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_discharges_differentiated_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', technology_transfer_discharges_differentiated_obligation, instrumental).
narrative_ontology:cs_reference_frame('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', kyoto_binding_annex_baseline).
narrative_ontology:cs_drift_state('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', post_paris_ndc_ratchet_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed6ebdde-28f4-47ce-b1bd-355b24e54e11', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, fossil_fuel_dependent_industries_in_annex_i_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, technology_licensing_firms_in_developed_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, small_island_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_coastal_populations).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, developing_nation_taxpayers_funding_adaptation).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, national_sovereignty_over_development_pathway).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, differentiated_but_non_binding_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate the NDC architecture at COP sessions and hold veto power over whether contributions become binding. They discharge their CBDR obligation primarily through technology-transfer pledges and voluntary finance commitments that carry no enforcement mechanism if unmet. They retain full discretion to revise their own pledged targets downward without penalty, and their historical emissions stock continues to accrue no compensatory liability under this reading.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, beneficiary).

% Benefit directly from the absence of binding reduction targets, which preserves investment horizons for existing carbon-intensive infrastructure. They lobby their governments to keep NDCs nationally determined and non-binding, framing any move toward binding historical-responsibility obligations as sovereignty infringement.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, fossil_fuel_dependent_industries_in_annex_i_states, beneficiary,
    powerful, biographical, mobile, national).

% Supply the clean-technology transfer that developed states count as their principal CBDR discharge. They retain IP control and licensing revenue rather than granting free technology access, so the 'transfer' obligation often converts into a new revenue stream for them rather than a genuine cost borne by their home governments.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, technology_licensing_firms_in_developed_states, beneficiary,
    organized, biographical, arbitrage, global).

% Face existential sea-level and storm risk despite having contributed negligibly to cumulative emissions. Under the voluntary reading they cannot compel developed-state mitigation or secure guaranteed loss-and-damage financing; they must fund adaptation domestically or seek ad hoc, non-binding pledges that are frequently underdelivered.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, small_island_developing_states, payer,
    powerless, civilizational, trapped, global).

% Depend on technology transfer and finance pledges that arrive inconsistently and often as loans rather than grants. They bear rising adaptation costs — irrigation collapse, crop failure, infrastructure loss — with no enforceable claim against the states whose historical emissions produced the warming they now absorb.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, least_developed_countries, payer,
    powerless, generational, trapped, global).

% Live the direct physical consequences of a regime that does not bind the largest historical emitters. Displacement, salinization, and livelihood loss occur on timescales the voluntary pledge cycle does not match; their exit options are internal migration or informal cross-border movement, neither backed by any treaty right.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_coastal_populations, payer,
    powerless, biographical, trapped, regional).

% Finance domestic adaptation infrastructure — sea walls, drought-resistant agriculture, relocation programs — through their own national budgets because international climate finance pledges are non-binding and routinely fall short of stated targets. They effectively subsidize the adaptation cost that historical-responsibility framings would assign to developed emitters.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developing_nation_taxpayers_funding_adaptation, payer,
    moderate, generational, constrained, national).

% Administers the NDC reporting and stocktake process without power to compel compliance. Compiles the gap between pledged and delivered finance/technology transfer, producing the evidentiary record that the voluntary regime's discharge obligations are frequently unmet.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, unfccc_secretariat, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:fixing_cost_class(cbdr_principle__voluntary_commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows nearly two hundred states with vastly different development levels, energy systems, and political capacities to remain inside a single global climate framework by letting each set its own contribution level, avoiding the treaty collapse that a uniform binding standard might trigger.
% TRANSFER_FUNCTION: Nominally moves clean technology and climate finance from developed to developing states as the primary discharge of differentiated responsibility; in practice moves the cost of adaptation and loss from historical high emitters onto states and populations that emitted comparatively little, because the technology/finance flow is voluntary and routinely under-delivered while physical climate damage is not.
% ABSENT_VOICES: Small island states and least developed countries participate formally in negotiations but hold no veto or binding claim; their preferred historical-responsibility reading is voted down or diluted at every negotiating round by the same developed-state bloc that benefits from the voluntary reading remaining dominant. Youth and future generations bearing civilizational-timescale costs have no seat at all.
% DISAPPEARANCE_RATIONALE: Developed-state negotiators and technology firms would say the treaty framework itself unravels without the voluntary, nationally-determined architecture — sovereignty-sensitive states would exit rather than accept binding targets. Vulnerable states and independent policy analysts would say the physical climate trajectory is largely unchanged either way at current pledge levels, meaning the arrangement mainly protects developed-state discretion rather than producing outcomes different from a binding regime's floor.
% FOUNDING_PROBLEM: The 1992 Rio negotiations needed a formula that could secure near-universal participation despite radically unequal historical contributions to atmospheric carbon and radically unequal capacity to reduce emissions without sacrificing development.
% FOUNDING_PROBLEM_CORROBORATION: Developed-state negotiators and technology-transfer firms attest the voluntary architecture is still necessary to keep major emitters inside the framework. Independent bodies outside the beneficiary set — the IPCC's cumulative emissions accounting, UNFCCC's own adaptation finance gap reports, and multiple small-island-state coalition statements — attest that the founding problem of securing adequate mitigation and finance from historical emitters remains substantially unsolved, and that the voluntary reading has become the mechanism by which that non-solution persists rather than a transitional step toward one.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, contested).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__voluntary_commitment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__voluntary_commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) and theater_ratio (0.58) are both authored moderately-high and rising over the interval: the technology-transfer and finance-pledge discharge mechanism has increasingly substituted symbolic commitments (announced funds, MOUs, capacity-building workshops) for the delivered mitigation or finance that would actually reduce vulnerable-state exposure — this is the metric-substitution signature the theater_ratio trajectory is meant to surface. Suppression (0.42) is moderate rather than high: developing states are not coercively silenced, but the consensus-based negotiating structure and the absence of any binding enforcement mechanism functionally suppress the historical-responsibility alternative by requiring unanimous or near-unanimous agreement that developed-state blocs can withhold. accessibility_collapse (0.35) is deliberately low — vulnerable states have not lost the ABILITY to argue for a binding reading; that argument remains live and is renewed at every COP, which is exactly why resistance (0.68) is authored high.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed-nation governments and their fossil-fuel and technology-licensing sectors sit near the full-beneficiary end of directionality: they set the negotiating agenda, retain discretion over their own pledge levels, and capture licensing revenue from the technology transfer counted as their discharge. Small island states, least-developed countries, and climate-vulnerable coastal populations sit near the full-target end: trapped exit options (no alternative venue exists to compel binding obligations outside this treaty regime), powerless power atom, and civilizational or biographical time horizons that make delayed mitigation catastrophic rather than merely costly. Developing-nation taxpayers occupy an intermediate position — moderate power, constrained exit — because they can at least shift domestic budget allocation, unlike populations with no institutional lever at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (near-universal participation despite unequal historical contribution and unequal capacity) was genuinely live in 1992 and remains partially live: a treaty architecture that immediately imposed binding, proportionate reductions might well have failed to secure US and other major-emitter participation at all, and the voluntary NDC structure did produce Paris Agreement ratification breadth that a binding CBDR-RC formula arguably would not have. But the founding_problem_status is authored as contested rather than dead because the coordination function (universal participation) has been substantially achieved while the extraction function (uncompensated adaptation cost shifted onto historical non-emitters) has not correspondingly diminished — the mismatch between founding_problem_status=contested and disappearance_verdict=contested, rather than a clean world_rearranges/dead pairing, is itself the signal that this is genuinely tangled rather than pure extraction dressed as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_binding_textual_ambiguity,
    'Does the UNFCCC Article 3.1 / Paris Agreement text''s CBDR-RC language admit a binding-obligation reading as legitimately as the voluntary reading, or does the negotiating history (particularly the shift from Kyoto''s binding annex structure to Paris''s NDC architecture) settle the question in favor of voluntarism?',
    'Comparative treaty-drafting history analysis: examine whether the Paris Agreement''s negotiators intended NDCs as a genuine narrowing of CBDR''s binding force or as a pragmatic participation mechanism explicitly understood as transitional toward eventual binding ratcheting.',
    'If the negotiating history supports a transitional reading, the voluntary_commitment_reading is better modeled as a scaffold (with an implicit sunset toward binding commitments) rather than a stable tangled_rope; if it supports genuine textual settlement in favor of voluntarism, the tangled_rope classification with no sunset is the more defensible structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_binding_textual_ambiguity, conceptual, 'Whether Paris Agreement drafting history settles or leaves open the voluntary/binding CBDR question.').

omega_variable(
    technology_transfer_genuine_cost_or_revenue,
    'Does technology transfer, as actually delivered under NDC pledges, represent a genuine cost borne by developed states/firms, or does IP-licensed transfer convert the ''obligation'' into a net revenue stream for developed-state technology firms?',
    'Financial analysis of technology-transfer agreements under the UNFCCC Technology Mechanism: compare licensing revenue captured by transferring firms against any measure of below-market or subsidized transfer terms.',
    'If transfer is substantially revenue-positive for developed-state firms, the ''primary developed nation obligation'' framing is closer to a false summit — a claimed cost that is structurally a benefit — strengthening the extraction reading; if transfer is genuinely subsidized/below-market, the coordination framing gains more support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_genuine_cost_or_revenue, empirical, 'Whether technology transfer functions as a real cost to developed states or as captured revenue.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where precisely does the historical_responsibility_reading and voluntary_commitment_reading disagreement locate — is it a disagreement about what the treaty text REQUIRES (a legal/interpretive question) or about what a JUST allocation of mitigation burden would be (a normative question dressed as interpretation)?',
    'This is the committer-structure question the kernel framing exists to hold open rather than resolve within a single reading; it would require international law scholarship on CBDR-RC''s legal status combined with normative political philosophy on historical emissions liability, neither of which this constraint story adjudicates.',
    'If the disagreement is purely normative (both readings agree on what the text technically permits but disagree on what would be just), the sibling relationship is better modeled as coexists_with with no foreclosure; if the disagreement is genuinely interpretive with one reading textually superior, one reading could in principle forecloses the other, which this story''s cs_structure declines to claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel readings diverge on legal interpretation or on normative justice, and what that implies for their structural relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(cbdr_tr_t2005, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(cbdr_tr_t2020, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2020, 0.53).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1997, 0.36).
narrative_ontology:measurement(cbdr_be_t2005, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(cbdr_be_t2020, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1997, 0.28).
narrative_ontology:measurement(cbdr_su_t2005, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(cbdr_su_t2020, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__voluntary_commitment_reading, 0.1).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, historical_responsibility_reading).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, unfccc_loss_and_damage_fund_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, paris_agreement_ndc_ratchet_mechanism).

% DUAL FORMULATION NOTE:
% This story and historical_responsibility_reading form a two-member constraint family reading the same cbdr_principle kernel (UNFCCC Article 3.1 / Paris Agreement CBDR-RC text). They are NOT the same constraint measured two ways: this reading's ε (0.61) and beneficiary/victim structure are authored independently from the sibling's, per the ε-invariance principle. Both files must link to each other via affects_constraints; a downstream reader tracing contamination or classification drift should be able to walk from either file to its sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
