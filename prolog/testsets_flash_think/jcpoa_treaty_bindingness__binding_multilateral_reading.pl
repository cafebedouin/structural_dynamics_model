% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'binding multilateral treaty'
 *   reading of the Joint Comprehensive Plan of Action (JCPOA). In this
 *   reading, the JCPOA is understood as a legally binding international
 *   agreement requiring consensus-based modification or dissolution, with
 *   robust multilateral enforcement mechanisms. It emphasizes the high bar
 *   for unilateral withdrawal and the necessity of UN Security Council
 *   consensus for sanctions reimposition or significant changes. This reading
 *   prioritizes the stability of the non-proliferation regime and
 *   multilateral diplomacy over unilateral state action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.75).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '201b8606-1b2f-41ce-a579-92dd73be2389').
narrative_ontology:cs_kernel_codification('201b8606-1b2f-41ce-a579-92dd73be2389', formalized).
narrative_ontology:cs_authority_grounding('201b8606-1b2f-41ce-a579-92dd73be2389', lineage).
narrative_ontology:cs_interpretation_layer_present('201b8606-1b2f-41ce-a579-92dd73be2389').
narrative_ontology:cs_reading_relation('201b8606-1b2f-41ce-a579-92dd73be2389', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('201b8606-1b2f-41ce-a579-92dd73be2389', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('201b8606-1b2f-41ce-a579-92dd73be2389', foundational, pacta_sunt_servanda).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda, holdable).
narrative_ontology:cs_axiom_grounding('201b8606-1b2f-41ce-a579-92dd73be2389', pacta_sunt_servanda, deontological).
narrative_ontology:cs_axiom('201b8606-1b2f-41ce-a579-92dd73be2389', foundational, unsc_primacy_in_security).
narrative_ontology:cs_axiom_status(unsc_primacy_in_security, holdable).
narrative_ontology:cs_axiom_grounding('201b8606-1b2f-41ce-a579-92dd73be2389', unsc_primacy_in_security, conventional).
narrative_ontology:cs_reference_frame('201b8606-1b2f-41ce-a579-92dd73be2389', vienna_convention_on_treaty_law).
narrative_ontology:cs_drift_state('201b8606-1b2f-41ce-a579-92dd73be2389', post_us_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('201b8606-1b2f-41ce-a579-92dd73be2389', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_china_russia).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, international_community).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, states_seeking_unilateral_action).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_hardliners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and precedent set by a successfully implemented, binding multilateral agreement preventing nuclear proliferation. Its legitimacy is reinforced by the treaty's adherence.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).

% Institutions like the IAEA and UNSC are tasked with monitoring compliance, facilitating dispute resolution, and enforcing the treaty's provisions. Their authority is central to this reading's operation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from sanctions relief and international legitimacy for its civilian nuclear program, but pays by accepting stringent monitoring and limits on enrichment, as well as being subject to multilateral dispute resolution mechanisms.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer).

% These states (France, Germany, UK, EU, China, Russia) are parties to the agreement and advocate for its continued binding nature, benefiting from regional stability and the preservation of the non-proliferation architecture. They actively participate in the Joint Commission and diplomatic efforts.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_china_russia, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_china_russia, beneficiary).

% These states (e.g., the US under the Trump administration) bear the cost of constrained unilateral action, as the treaty's binding nature requires multilateral consensus for significant changes or sanctions reimposition, limiting their ability to act independently. They are effectively excluded from unilateral policy choices.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, states_seeking_unilateral_action, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, states_seeking_unilateral_action, excluded).

% Bear the cost of the treaty's constraints on Iran's nuclear program and its integration into the international system, which they view as an infringement on national sovereignty and a betrayal of revolutionary ideals. They advocate for non-compliance and withdrawal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_hardliners, payer,
    powerful, biographical, constrained, national).

% Benefits from the reduced risk of nuclear proliferation and regional conflict, contributing to global security and stability. This benefit is diffuse but widely acknowledged.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_community, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prevent nuclear proliferation by establishing verifiable limits on Iran's nuclear program, providing a framework for monitoring, and linking compliance to sanctions relief.
% TRANSFER_FUNCTION: Transfers Iranian nuclear program transparency and limitations to the international community in exchange for economic sanctions relief and diplomatic normalization. It also transfers the burden of enforcement and dispute resolution to multilateral bodies.
% ABSENT_VOICES: States advocating for unilateral military action against Iran or immediate regime change, who believe the treaty is insufficient or illegitimate. Also, Iranian factions who believe the treaty compromises national sovereignty and should be abandoned.
% DISAPPEARANCE_RATIONALE: If the JCPOA as a binding multilateral treaty vanished overnight, Iran would likely resume full-scale enrichment, international monitoring would cease, and regional tensions would escalate rapidly, increasing the risk of military conflict and nuclear proliferation.
% FOUNDING_PROBLEM: Iran's accelerating nuclear enrichment program and the international community's desire to prevent nuclear proliferation without resorting to military intervention.
% FOUNDING_PROBLEM_CORROBORATION: IAEA reports, UN Security Council resolutions, statements from the E3/EU+2 nations, and analyses by non-proliferation experts consistently corroborate the ongoing threat of proliferation and the need for a diplomatic solution.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the treaty imposes significant constraints on Iran's nuclear program and limits the unilateral foreign policy options of other states. Suppression is also high (0.75) due to the active enforcement mechanisms (IAEA monitoring, Joint Commission dispute resolution, potential UNSC action) that prevent non-compliance or easy withdrawal. Theater ratio is low (0.15) as this reading emphasizes the genuine functional aspects of the treaty, with minimal performative maintenance. Accessibility collapse is high (0.80) because, for adherents of this reading, alternatives to multilateral engagement (e.g., unilateral military action or sanctions) are largely foreclosed. Resistance is high (0.70) from actors who prefer unilateral action or reject the treaty's premises.
 *
 * PERSPECTIVAL GAP:
 *   Actors committed to this reading (e.g., E3/EU+2) experience the JCPOA as a vital Rope or Tangled Rope, providing essential coordination for non-proliferation. However, actors who reject its binding nature or seek unilateral action (e.g., US unilateralists, Iranian hardliners) experience it as a Snare, extracting sovereignty or freedom of action through multilateral coercion. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The non-proliferation regime, multilateral institutions, and states committed to the treaty (E3/EU+2) are beneficiaries, gaining stability and a framework for managing proliferation risks. Iran is a beneficiary through sanctions relief and legitimacy, but also a payer through its compliance obligations. States seeking unilateral action (e.g., US unilateralists) and Iranian hardliners are victims, as their preferred actions are suppressed by the treaty's binding nature and enforcement mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_vs_political_commitment_ambiguity,
    'Is the JCPOA a legally binding treaty under international law (e.g., Vienna Convention on the Law of Treaties), or a non-binding political commitment?',
    'International Court of Justice ruling on the legal status of the JCPOA, or a definitive consensus among international legal scholars.',
    'If legally binding, this reading''s emphasis on consensus-based modification and high bar for withdrawal is reinforced. If non-binding, unilateral withdrawal becomes legally less problematic, weakening the constraint''s structural integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_vs_political_commitment_ambiguity, conceptual, 'Ambiguity regarding the JCPOA''s legal status as a treaty versus a political commitment.').

omega_variable(
    unsc_consensus_viability,
    'Can UN Security Council consensus for sanctions snapback or modification be reliably achieved, given persistent geopolitical divisions among its permanent members?',
    'Observation of future UNSC votes on JCPOA-related matters, or a shift in the geopolitical landscape that alters P5 dynamics.',
    'If consensus is consistently blocked, the enforcement mechanism of this reading is weakened, potentially leading to a reclassification towards a more performative or atrophied type. If consensus is achieved, the binding nature is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unsc_consensus_viability, empirical, 'Uncertainty about the practical viability of UNSC consensus for enforcement actions.').

omega_variable(
    unilateral_withdrawal_legitimacy,
    'Does international law permit unilateral withdrawal from an agreement like the JCPOA without multilateral consensus, or does such an action constitute a breach?',
    'Precedent set by international legal rulings or a widely accepted interpretation of relevant international law by a majority of states.',
    'If unilateral withdrawal is deemed legitimate, the constraint''s ability to suppress such actions is reduced, shifting its classification towards a less binding type. If it''s deemed a breach, the constraint''s enforcement legitimacy is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legitimacy, conceptual, 'Ambiguity regarding the legal legitimacy of unilateral withdrawal from the JCPOA.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.14).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.15).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.65).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.67).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.75).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.75).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, nuclear_non_proliferation_treaty).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_nuclear_program_limits).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
