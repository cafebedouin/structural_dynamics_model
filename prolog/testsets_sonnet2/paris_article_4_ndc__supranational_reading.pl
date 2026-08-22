% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Article 4 NDCs as Binding Ratchet with Supranational Accountability
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the supranational reading of the Paris
 *   Agreement's Article 4 NDC kernel: the position, held by multilateral
 *   climate institutions and climate-vulnerable state coalitions, that NDCs
 *   constitute a binding ratcheting trajectory toward net-zero enforced
 *   through international accountability mechanisms (transparency framework,
 *   global stocktake, reputational and financial consequences for
 *   non-compliance). This is NOT the sovereigntist reading (voluntary,
 *   nationally self-determined pledges preserving energy sovereignty) or the
 *   equity reading (CBDR-differentiated obligations between developed and
 *   developing states) — those are separate constraints with their own ε
 *   values and stakeholder structures, linked here via
 *   network.affects_constraints. Under this reading's own lights, the
 *   standing arrangement (the accountability architecture as it presently
 *   operates and is being strengthened cycle-over-cycle) is substantially
 *   extractive: it converts carbon-intensive industry and fossil-dependent
 *   state fiscal bases into sanctioned, stranded liabilities, and
 *   institutionalizes wealth transfer obligations from North to South, while
 *   claiming the mantle of scientific necessity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDCs as Binding Ratchet with Supranational Accountability").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '0902ee68-297d-468f-b340-968111d49a61').
narrative_ontology:cs_kernel_codification('0902ee68-297d-468f-b340-968111d49a61', fixed_text).
narrative_ontology:cs_authority_grounding('0902ee68-297d-468f-b340-968111d49a61', extraction).
narrative_ontology:cs_interpretation_layer_present('0902ee68-297d-468f-b340-968111d49a61').
narrative_ontology:cs_reading_relation('0902ee68-297d-468f-b340-968111d49a61', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('0902ee68-297d-468f-b340-968111d49a61', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('0902ee68-297d-468f-b340-968111d49a61', foundational, ndc_pursuit_creates_binding_trajectory_obligation).
narrative_ontology:cs_axiom_status(ndc_pursuit_creates_binding_trajectory_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0902ee68-297d-468f-b340-968111d49a61', ndc_pursuit_creates_binding_trajectory_obligation, conventional).
narrative_ontology:cs_axiom('0902ee68-297d-468f-b340-968111d49a61', secondary, international_accountability_mechanisms_are_legitimate_enforcement).
narrative_ontology:cs_axiom_status(international_accountability_mechanisms_are_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('0902ee68-297d-468f-b340-968111d49a61', international_accountability_mechanisms_are_legitimate_enforcement, instrumental).
narrative_ontology:cs_reference_frame('0902ee68-297d-468f-b340-968111d49a61', post_kyoto_universal_participation_bargain).
narrative_ontology:cs_drift_state('0902ee68-297d-468f-b340-968111d49a61', post_paris_ratchet_hardening_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0902ee68-297d-468f-b340-968111d49a61', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, multilateral_climate_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, emerging_economies_with_coal_grids).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, emerging_economies_with_coal_grids).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developed_state_treasuries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_state_treasuries).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, collective_action_necessity_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, science_based_target_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ratchet mechanism, the global stocktake, and the transparency framework, treating each NDC cycle as a step toward a binding net-zero trajectory. Sets expectations that non-compliance carries reputational and financing consequences, and pushes for legal interpretations that treat the 'binding obligation to pursue' language as functionally compulsory.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, multilateral_climate_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Face existential climate risk regardless of their own emissions and depend on a strong, enforceable ratchet plus resulting adaptation and loss-and-damage finance to survive. They have no exit from the physical consequences and campaign hardest for treating NDCs as binding.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_states, beneficiary,
    moderate, civilizational, trapped, global).

% Gain market certainty and investment flows as the ratchet's enforcement credibility strengthens; can relocate capital across jurisdictions to capture regulatory tailwinds wherever compliance pressure is strongest.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Face regulatory extinction risk as ratcheting targets tighten: stranded assets, carbon border adjustments, and financing exclusion. They have capital and lobbying power but cannot fully exit the trajectory once treaty mechanisms and domestic law incorporate binding targets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Depend on fossil extraction and export revenue for fiscal survival. A binding ratchet with international accountability threatens to convert their core industry into a sanctioned liability, with no viable near-term substitute economy and no meaningful exit from the treaty architecture without reputational and financial isolation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_states, payer,
    moderate, generational, trapped, national).

% Rely on coal for affordable electrification and industrial growth. Binding trajectory obligations pressure early grid retirement before development needs are met, though some also receive concessional finance and technology transfer tied to compliance, creating a mixed position.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, emerging_economies_with_coal_grids, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, emerging_economies_with_coal_grids, beneficiary).

% Bear the fiscal cost of climate finance transfers and domestic decarbonization mandates justified by the binding-trajectory reading, while benefiting from first-mover advantage in green industrial policy and from avoided catastrophic climate costs.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_state_treasuries, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developed_state_treasuries, beneficiary).

% Argue NDCs were negotiated as nationally determined and voluntary, and that treating them as binding exceeds the treaty's actual legal text. Their objection is treated as bad-faith obstruction within supranational institutional forums rather than engaged as a live legal question.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, national_sovereignty_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze whether Article 4's text ('shall pursue domestic mitigation measures') supports a binding-obligation reading or only a binding-process, voluntary-substance reading. Their conclusions differ depending on interpretive method (textualist vs. purposive), which is part of what makes this a contested kernel rather than a settled question.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, treaty_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective global emissions-reduction trajectory across nearly 200 states with heterogeneous interests, using periodic ratcheting cycles and a transparency framework to prevent free-riding on the shared atmospheric commons.
% TRANSFER_FUNCTION: Moves compliance costs, stranded-asset risk, and reputational/financial exposure from carbon-intensive industries and fossil-dependent states toward climate-vulnerable states and green-industry beneficiaries, mediated by finance transfers and market access consequences administered by multilateral institutions.
% ABSENT_VOICES: National sovereignty advocates and fossil-dependent state governments contest the binding characterization in domestic legislatures and in treaty-interpretation forums, but the supranational reading's institutional venues (COP technical bodies, UNFCCC secretariat processes) are structured around treating the ratchet as functionally obligatory, sidelining the voluntarist counter-reading procedurally rather than resolving it on the merits.
% DISAPPEARANCE_RATIONALE: If the binding-trajectory reading collapsed and NDCs reverted to pure self-determined pledges with no accountability consequence, climate finance conditionality would weaken, carbon-intensive industries would face materially reduced regulatory extinction risk, and vulnerable states would lose their strongest lever for compelling mitigation and finance from major emitters — the entire enforcement architecture built around the ratchet (stocktakes, transparency reporting, border carbon measures justified by non-compliance) would lose its normative anchor.
% FOUNDING_PROBLEM: The Paris Agreement was built to solve the Kyoto Protocol's failure: rigid binding targets that induced non-ratification and withdrawal (the US under Kyoto) versus a framework flexible enough for universal participation, while still needing a mechanism to prevent the resulting voluntarism from producing collectively inadequate ambition.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral climate institutions and vulnerable-state coalitions attest the ratchet-as-binding reading is necessary because voluntary pledges to date are collectively insufficient to meet 1.5C, citing UNEP emissions gap reports (an outside-institution technical source). Independent treaty law scholars outside the beneficiary set are split: some corroborate that the legal text supports only a binding process obligation, not a binding substantive outcome, undercutting the strong supranational reading; others find the good-faith pursuit language does create enforceable minimum-effort standards over time.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) reflects the accumulating regulatory-extinction pressure on carbon-intensive industries and the involuntary fiscal exposure of fossil-dependent states as the ratchet mechanism hardens cycle-over-cycle — the same trajectory tracked in the measurement series, where base_extractiveness rises from 0.42 to 0.71 as the transparency framework and stocktake process matured from aspirational to consequential. Theater ratio falls over the interval (0.55 to 0.40) because early NDC cycles were substantially performative (weak targets, no enforcement teeth) while later cycles increasingly translate into real financial and market consequences (carbon border adjustments, green finance conditionality) — less theater, more genuine bite, which is exactly what makes the reading's extraction claim credible rather than merely aspirational. Suppression (0.62) captures the institutional pressure applied to states and industries that resist the binding characterization — reputational sanctioning within COP forums, exclusion from green finance, and diplomatic isolation — which is a raw structural feature, not scaled by scope or power in the underlying metric.
 *
 * PERSPECTIVAL GAP:
 *   From the multilateral-institution and vulnerable-state seats, this reading is coordination under existential threat: a genuine collective-action mechanism holding free-riders accountable. From the carbon-intensive-industry and fossil-dependent-state seats, the identical enforcement architecture is experienced as extraction backed by reputational and financial coercion, imposed on actors that structurally cannot exit the treaty regime without catastrophic isolation costs. The engine computes these divergent per-seat classifications from the same structural data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable states and renewable energy industries sit near the beneficiary end: the former because a hardened ratchet is their primary lever against actors far larger than themselves, the latter because enforcement credibility drives investment their way. Carbon-intensive industries and fossil-fuel dependent states sit near the target end: they bear stranded-asset risk and fiscal exposure directly caused by the binding-trajectory interpretation, with constrained or trapped exit (a state cannot easily replace an oil-export economy; an incumbent energy major cannot easily exit its asset base). Emerging economies with coal grids and developed-state treasuries are dual-positioned — genuinely paying compliance costs while also receiving some of the finance and technology transfer the same architecture generates, which the secondary_role field captures rather than forcing a single directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Kyoto's binding-target failure inducing non-ratification) is contested as either live or dead: proponents of the supranational reading argue the emissions gap proves voluntary pledges remain collectively inadequate, so the ratchet's bindingness is still solving the original coordination failure. Critics argue the treaty text was deliberately drafted to avoid Kyoto-style binding substantive obligations, and that the supranational reading is a legal reinterpretation layered onto the original bargain after the fact — a mandate that has outgrown its textual warrant even if the underlying climate problem remains live. This divergence is exactly why the reading is authored as a separate constraint from the sovereigntist reading rather than resolved within one story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_textual_warrant,
    'Does Article 4''s ''shall pursue domestic mitigation measures'' language create a binding obligation of result (the supranational reading), a binding obligation of process only, or a purely voluntary aspiration (the sovereigntist reading)?',
    'Authoritative interpretation by the International Court of Justice or a treaty-body ruling on the legal character of NDC obligations; alternatively, consistent state practice and non-objection over multiple compliance cycles could establish customary interpretation.',
    'If interpreted as binding-of-result, this reading''s extraction claims are legally well-grounded and the classification of carbon-intensive industries as victims of enforceable sanction is structurally accurate. If interpreted as process-only or voluntary, this reading substantially overclaims bindingness relative to the text, and the extractiveness measured here reflects institutional practice exceeding legal warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_voluntary_textual_warrant, conceptual, 'Whether the treaty text itself supports the binding-trajectory reading or only a weaker process obligation.').

omega_variable(
    enforcement_capacity_vs_rhetoric,
    'Do the reputational and financial ''sanctions'' this reading claims actually possess enforcement teeth, or does the accountability framework remain substantially aspirational despite institutional rhetoric of bindingness?',
    'Track actual compliance consequences imposed on states that miss NDC targets across multiple stocktake cycles: withdrawal of finance, market access restriction, or purely rhetorical censure.',
    'If enforcement remains largely rhetorical, the high extractiveness and suppression scores authored here overstate the constraint''s actual bite and the theater_ratio trajectory should not be falling as fast as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_rhetoric, empirical, 'Whether claimed accountability mechanisms translate into material consequences.').

omega_variable(
    north_south_transfer_institutionalization,
    'Is the North-to-South wealth transfer this reading institutionalizes a genuine restitution mechanism for historical emissions responsibility, or an extraction narrative imposed asymmetrically on developing economies under the guise of binding science-based obligation?',
    'Comparative analysis of finance flows actually delivered against pledged amounts, and assessment of whether conditionality attached to that finance constrains recipient-state sovereignty beyond what CBDR principles would require.',
    'Determines whether developing-economy stakeholders in this reading are correctly coded as payers/victims of an extractive binding mechanism, or whether the equity_reading''s differentiated-responsibility framing better captures their structural position — this is precisely the disagreement that separates this constraint from its equity_reading sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_south_transfer_institutionalization, preference, 'Whether institutionalized North-South transfers under this reading are restitutive coordination or asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pari_tr_t4, paris_article_4_ndc__supranational_reading, theater_ratio, 4, 0.5).
narrative_ontology:measurement(pari_tr_t8, paris_article_4_ndc__supranational_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__supranational_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement(pari_tr_t16, paris_article_4_ndc__supranational_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pari_be_t4, paris_article_4_ndc__supranational_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(pari_be_t8, paris_article_4_ndc__supranational_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__supranational_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(pari_be_t16, paris_article_4_ndc__supranational_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pari_su_t4, paris_article_4_ndc__supranational_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(pari_su_t8, paris_article_4_ndc__supranational_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(pari_su_t12, paris_article_4_ndc__supranational_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(pari_su_t16, paris_article_4_ndc__supranational_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the same Article 4 NDC kernel. paris_article_4_ndc__sovereigntist_reading treats NDCs as voluntary self-determined pledges with low ε and minimal enforcement; paris_article_4_ndc__equity_reading treats NDCs as CBDR-differentiated obligations with a distinct beneficiary/victim structure organized around North/South responsibility distinctions rather than binding-vs-voluntary status. This story (supranational_reading) authors the highest ε of the three because it is the only reading that treats non-compliance as triggering enforceable reputational and financial consequence. The three stories share the same treaty text but are not the same constraint — per the ε-invariance principle, each reading's classification is computed independently and the readings are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
