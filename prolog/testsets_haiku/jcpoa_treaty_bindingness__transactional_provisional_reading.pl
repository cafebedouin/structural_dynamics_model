% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA Transactional Provisional Framework with Unilateral Withdrawal Right
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action, 2015) is a multilateral
 *   nuclear agreement between Iran and the P5+1 (US, UK, France, Russia,
 *   China, Germany, plus EU). This constraint story instantiates ONE READING
 *   of the contested kernel 'jcpoa_treaty_bindingness': the
 *   transactional_provisional_reading frames the JCPOA as a time-limited,
 *   revocable commercial arrangement voidable by any signatory upon a
 *   unilateral determination of Iranian bad faith. Under this reading, no
 *   party is bound by consensus; the most powerful signatories (especially
 *   the US) retain authority to reimpose sanctions without multilateral
 *   agreement and to exit the framework at will. This contrasts sharply with
 *   the binding_multilateral_reading (JCPOA as a fixed treaty requiring
 *   unanimous consent to modify) and the graduated_compliance_reading (JCPOA
 *   as a scaled reciprocal commitment with graduated enforcement tied to
 *   proportional compliance assessment). The transactional reading emerges
 *   from domestic US political debate (especially opposition to the deal) and
 *   has been operationalized through US Executive action (2018 withdrawal,
 *   sanctions reimposition). It benefits individual state sovereignty and
 *   domestic political coalitions opposing the deal while imposing asymmetric
 *   constraints on Iran and undermining the multilateral treaty framework.
 *
 * KEY AGENTS:
 *   - United States Executive: agenda-setter, institutional power; retains unilateral withdrawal authority and bad-faith determination; collects political gains from exit (satisfying domestic opposition)
 *   - Iranian State: institutional power, trapped exit; bears sanctions if declared in bad faith; must perform restrictions without reciprocal guarantee
 *   - European Signatories: institutional power, constrained exit; benefit from stability but forced to choose between US pressure and continued Iranian engagement; cannot prevent unilateral US withdrawal
 *   - Domestic Political Opposition Blocs: organized power, mobile exit; benefit from provisional framework because it enables exit framing as terminating a failed commercial deal rather than repudiating multilateral law
 *   - Non-Aligned States: moderate power, constrained exit; depend on framework stability but excluded from withdrawal decisions
 *   - Multilateral Treaty Institution: analytical observer; erosion of binding-treaty precedent when major powers treat agreements as revocable transactions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.71).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA Transactional Provisional Framework with Unilateral Withdrawal Right").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '4bbad23c-ade7-4f85-bce6-b2d54646c29a').
narrative_ontology:cs_kernel_codification('4bbad23c-ade7-4f85-bce6-b2d54646c29a', fixed_text).
narrative_ontology:cs_authority_grounding('4bbad23c-ade7-4f85-bce6-b2d54646c29a', extraction).
narrative_ontology:cs_interpretation_layer_present('4bbad23c-ade7-4f85-bce6-b2d54646c29a').
narrative_ontology:cs_reading_relation('4bbad23c-ade7-4f85-bce6-b2d54646c29a', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('4bbad23c-ade7-4f85-bce6-b2d54646c29a', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('4bbad23c-ade7-4f85-bce6-b2d54646c29a', foundational, unilateral_state_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(unilateral_state_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4bbad23c-ade7-4f85-bce6-b2d54646c29a', unilateral_state_sovereignty_supremacy, deontological).
narrative_ontology:cs_axiom('4bbad23c-ade7-4f85-bce6-b2d54646c29a', foundational, transactional_temporality_over_binding_permanence).
narrative_ontology:cs_axiom_status(transactional_temporality_over_binding_permanence, holdable).
narrative_ontology:cs_axiom_grounding('4bbad23c-ade7-4f85-bce6-b2d54646c29a', transactional_temporality_over_binding_permanence, instrumental).
narrative_ontology:cs_axiom('4bbad23c-ade7-4f85-bce6-b2d54646c29a', secondary, executive_bad_faith_determination_authority).
narrative_ontology:cs_axiom_status(executive_bad_faith_determination_authority, holdable).
narrative_ontology:cs_axiom_grounding('4bbad23c-ade7-4f85-bce6-b2d54646c29a', executive_bad_faith_determination_authority, conventional).
narrative_ontology:cs_reference_frame('4bbad23c-ade7-4f85-bce6-b2d54646c29a', individual_state_sovereignty_framework).
narrative_ontology:cs_drift_state('4bbad23c-ade7-4f85-bce6-b2d54646c29a', post_2018_us_withdrawal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4bbad23c-ade7-4f85-bce6-b2d54646c29a', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, individual_state_sovereignty_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_opposition_blocs).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_treaty_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains unilateral authority to declare Iranian bad faith and reimpose sanctions without consensus from other signatories. Can exit the framework at will, treating the agreement as a transactional exchange contingent on continuous Iranian performance rather than as a binding legal commitment. Sets the terms of 'acceptable compliance' and judges Iranian adherence unilaterally.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Bears sanctions if declared in bad faith by any major signatory, even if other parties believe compliance is met. Must perform continuous nuclear restrictions under the threat of unilateral sanctions reimposition. Has no equivalent unilateral exit right; departure triggers automatic return to pre-2015 sanctions regime. Trapped because isolation intensifies if it exits, but compliance offers no guarantee of sanctions relief if the framework dissolves.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state, payer,
    institutional, generational, trapped, global).

% Benefit from nuclear stability and Iranian sanctions relief that supports their trade and diplomatic relationships. Become payers if forced to choose between US sanctions pressure and continued Iranian engagement; secondary rejection of their compliance judgments creates pressure to align with US determinations or absorb secondary sanctions. Constrained: they can protest unilateral US withdrawal but cannot prevent it or enforce the agreement's provisions on the US.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, european_signatories, beneficiary).

% Benefits from the framework's provisional status because it creates a legal argument for exit without violating binding treaty law. Opposition coalitions leverage the 'transactional' framing to justify withdrawal as terminating a failed commercial arrangement rather than repudiating a multilateral commitment. Their political leverage improves when the framework is read as revocable at will rather than as binding law.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_opposition, beneficiary,
    organized, biographical, mobile, national).

% Depend on the JCPOA's stability for regional security and non-proliferation precedent, but have no seat at withdrawal decisions. Can be drawn into sanctions reimposition or compliance disputes without having adjudicated Iranian performance. Would advocate for binding multilateral modification procedures if included; their exclusion from the unilateral-withdrawal determination is structurally embedded in this reading.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, non_aligned_states, excluded,
    moderate, generational, constrained, global).

% Treated as victim because the transactional reading erodes the legal presumption that multilateral treaties bind all parties equally and require consensus modification. Unilateral withdrawal rights undermine precedent for binding treaty frameworks and empower the powerful to rewrite agreements unilaterally. The institution itself has no agency but absorbs reputational cost when major powers treat treaties as revocable transactions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_treaty_institution, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_treaty_institution).

% Observes how this framework's legal status influences state behavior on nuclear commitments. Unilateral withdrawal rights reduce the credibility of future non-proliferation agreements because states anticipate that powerful parties will exit if political conditions shift domestically. The regime's analytical seat measures whether the transactional reading accelerates proliferation or compliance-dodging among threshold states.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_nonproliferation_regime, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__transactional_provisional_reading, international_nonproliferation_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mutual verification and phased sanctions-relief problem: Iran accepts intrusive IAEA inspections and nuclear restrictions; signatories lift targeted sanctions in reciprocal stages. Creates a mechanism for trust-building and incremental de-escalation without requiring all-or-nothing capitulation from either party. Under the transactional reading, this coordination function is subordinate to state sovereignty — the agreement coordinates only insofar as states choose to maintain it.
% TRANSFER_FUNCTION: Moves nuclear materials restrictions from Iran to international inspectors; moves sanctions relief from Western states to the Iranian state in staged tranches contingent on compliance certification. Also moves political authority: from multilateral consensus toward the most powerful signatory's unilateral judgment of bad faith and capacity to withdraw and reimpose sanctions without partner consent.
% ABSENT_VOICES: Non-signatories and non-aligned states would object to the unilateral-withdrawal provision if they had seats; they depend on nuclear stability but cannot block withdrawal. The multilateral treaty institution itself would advocate for binding dispute-resolution and consensus-modification procedures instead of unilateral exit rights. These voices are structurally excluded by the transactional reading's framework. The IAEA, despite its role as technical verifier, is not given formal authority to adjudicate compliance disputes or forestall unilateral US withdrawal.
% DISAPPEARANCE_RATIONALE: If unilateral withdrawal authority vanished overnight, the framework would revert to binding multilateral status with mandatory dispute resolution. Sanctions could not be reimposed by a single signatory; Iranian violations would trigger negotiations rather than automatic exit. The power asymmetry disappears, and the agreement becomes more durable but also more constraining on the powerful party. Iran would have greater security that compliance brings lasting sanctions relief, and European states would regain negotiating weight equal to the US.
% FOUNDING_PROBLEM: 2015: Iran's nuclear program posed proliferation risk; the US and P5+1 needed a verification mechanism that Iran would accept despite its distrust of Western intentions, and Iran needed sanctions relief without surrendering state sovereignty entirely. JCPOA solved this as a time-limited, inspectable compromise with the explicit understanding (debated) that the agreement was designed to be reviewed and renewed at fixed intervals or abandoned if performance was unsatisfactory.
% FOUNDING_PROBLEM_CORROBORATION: The US Executive framing asserts the founding problem persists: Iranian violations and bad-faith behavior justify continuous withdrawal readiness. IAEA inspectors, non-aligned states, and European signatories attest the founding problem is substantially addressed: Iranian nuclear program is demonstrably restricted and under verification; the founding problem's resolution justifies treaty continuation and binding modification. US domestic opposition asserts the founding problem was poorly diagnosed and the deal betrays broader strategic interests by failing to address Iran's regional behavior and ballistic missile development. No external corroboration exists from outside the benefiting parties (US Executive, domestic opposition) for the 'bad faith' claim — the IAEA certifies compliance; European states affirm Iran's adherence to the nuclear limits.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 by interval end (2024) because the constraint transfers nuclear compliance burden to Iran while preserving unilateral US authority to declare compliance inadequate and reimpose sanctions. The extraction emerges from asymmetric exit options: Iran is trapped (exit means automatic return to pre-2015 sanctions), while the US has arbitrage-grade mobility (can exit and impose secondary sanctions on other signatories). Suppression is 0.71 because the framework's persistence depends on active enforcement of the exclusion of non-signatories from modification authority and continuous threat of unilateral withdrawal if Iranian compliance is judged insufficient. Theater is 0.42 (moderate-high): the IAEA inspection regime is substantive, but an increasing share of dispute framing after 2017 became political theater (US rejecting IAEA compliance certifications; Iranian 'countermeasures' that were partly performative; European statements of support for the deal while absorbing secondary sanctions pressure). The measurement series shows extraction rising sharply from 2015 (0.35, when the deal was new and cooperative framing was strong) to 2018 (0.61, when US withdrew and reframed the agreement as failed transaction) and stabilizing at 0.68 thereafter. Suppression and theater rise in lockstep, suggesting that as extraction became explicit and unilateral, enforcement machinery had to harden (suppression) and performative legitimation increased (theater). The shared time grid captures this progression at historical inflection points (2017 = initial implementation; 2018 = US withdrawal; 2020 = nuclear escalation; 2024 = current state).
 *
 * PERSPECTIVAL GAP:
 *   The perspective from the US Executive is that the agreement is a provisional transaction dependent on Iranian performance, and unilateral withdrawal authority is necessary to protect national interest. The perspective from Iranian officials and European states is that the agreement is a binding multilateral commitment that requires consensus modification, and the US withdrawal and sanctions reimposition violated the agreement's terms. These perspectives are incommensurable within the transactional reading because the reading's core premise (unilateral withdrawal right) forecloses the binding-multilateral perspective. The engine should detect this incommensurability as a high perspectival gap driven by the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Executive benefits from the provisional reading because it preserves unilateral authority (d ≈ 0.2–0.3, strong beneficiary). Iranian state bears the cost of asymmetric compliance burden and sanctions threat (d ≈ 0.85–0.95, strong target). European signatories sit between (d ≈ 0.55–0.65, constrained payers with some coordination benefit). Domestic opposition coalitions benefit from the revocable framing even if they do not directly collect rents (d ≈ 0.15–0.25, beneficiaries of the authority asymmetry). No directionality overrides are required: the structural derivation from beneficiary/victim + exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Iran's nuclear program + need for verification-based trust-building) remains contested in status, but the transactional reading obscures the question by treating mandatrophy as irrelevant: under the transactional framing, the mandate need not persist because the agreement is commercial, not constitutional. If the founding problem is dead (Iran's program is now constrained and verified), the transactional reading reframes the constraint as protecting state sovereignty rather than as persistence of unnecessary extraction. If the founding problem remains live (Iran is a persistent proliferation threat), the transactional reading justifies continuous withdrawal readiness as prudent. The omega on sibling-reading empirical precedent tests which reading the parties' actual behavior supports: if Europe enforces the agreement against US withdrawal and all parties respect multilateral modification procedures, the binding reading is operationalized despite the transactional claim. If Iran complies only when reciprocal US compliance is visible, the graduated reading is operationalized. If the US withdraws unilaterally and other parties absorb the costs, the transactional reading is operationalized (current state). Mandatrophy resolution here hinges on whether the framework persists as constraint or dissolves: if Iran restarts nuclear activities in response to US withdrawal, the constraint fails; if Iran maintains restrictions despite sanctions reimposition, the constraint persists via suppression (threat of escalating sanctions), which is the piton dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_transactional_kernel_contest,
    'Is the JCPOA structurally a binding multilateral treaty requiring consensus modification, or a provisional transactional arrangement revocable upon unilateral bad-faith determination?',
    'The International Court of Justice could adjudicate the treaty''s legal character; UN General Assembly resolutions could declare the framework binding; sustained multilateral enforcement of the agreement against unilateral withdrawal attempts would operationalize the binding reading.',
    'If binding, extraction falls sharply (d moves toward symmetric, constraint type shifts toward rope); if transactional, extraction sustains at current levels and asymmetry remains embedded in the framework. This is the core kernel ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_transactional_kernel_contest, conceptual, 'Whether the JCPOA''s legal status is binding or revocable at unilateral will.').

omega_variable(
    bad_faith_determination_opacity,
    'Who adjudicates whether a party is acting in bad faith, and by what standard? Is bad faith determined by objective multilateral assessment or by unilateral state judgment?',
    'Establishment of an independent verification panel with binding arbitration; adoption of explicit bad-faith criteria in an amended protocol; ICJ reference or UN Security Council authorization for determinations.',
    'Objective standards reduce suppression (d becomes more symmetric) and enable European and non-aligned states to contest US determinations. Unilateral judgment preserves the asymmetry and keeps suppression high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bad_faith_determination_opacity, empirical, 'Whether bad-faith determination is objective or unilateral.').

omega_variable(
    iranian_exit_symmetry,
    'Does Iran have equivalent unilateral withdrawal rights, or is the withdrawal right asymmetrically held by the powerful signatories?',
    'Explicit protocol amendment granting all parties equal exit rights with notice periods; or mutual agreement to binding dispute resolution before either party can withdraw.',
    'Symmetrical exit rights would reduce extraction substantially and reframe the constraint as genuine rope coordination. Current asymmetry is the defining feature of this reading and the primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iranian_exit_symmetry, empirical, 'Whether exit rights are symmetric across all parties.').

omega_variable(
    compliance_verification_authority_asymmetry,
    'Does the transactional reading rest on asymmetric verification authority — where the US Executive can reject IAEA compliance certification and declare Iran in breach unilaterally?',
    'IAEA compliance determinations become binding absent Security Council override; or an independent arbitral body adjudicates compliance disputes before sanctions reimposition.',
    'If verification authority is truly independent (IAEA), the constraint becomes closer to rope; if verification authority is subordinate to unilateral state judgment, extraction and suppression sustain at current levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_verification_authority_asymmetry, empirical, 'Whether IAEA compliance determinations override unilateral state declarations of breach.').

omega_variable(
    sibling_reading_empirical_precedent,
    'Which reading do the signatories'' actual behavior patterns support: treating JCPOA as binding (binding_multilateral_reading), as scaled reciprocal (graduated_compliance_reading), or as revocable transactional (this reading)?',
    'Historical analysis of enforcement: Did Europe enforce the agreement against US withdrawal? Did Iran comply conditionally on reciprocal US compliance? Did all parties accept multilateral modification procedures, or did the US act unilaterally?',
    'If actual practice diverges from the transactional reading, the constraint''s claimed type requires revision. The measurement series captures this empirically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_precedent, empirical, 'Whether empirical behavior matches the transactional reading''s structural predictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(jcpo_tr_t2015, observed).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement_basis(jcpo_tr_t2017, observed).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement_basis(jcpo_tr_t2018, observed).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t2020, observed).
narrative_ontology:measurement(jcpo_tr_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2022, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t2022, observed).
narrative_ontology:measurement(jcpo_tr_t2024, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement_basis(jcpo_be_t2015, observed).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t2017, observed).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(jcpo_be_t2018, observed).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(jcpo_be_t2020, observed).
narrative_ontology:measurement(jcpo_be_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement_basis(jcpo_be_t2022, observed).
narrative_ontology:measurement(jcpo_be_t2024, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(jcpo_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(jcpo_su_t2015, observed).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement_basis(jcpo_su_t2017, observed).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2018, 0.66).
narrative_ontology:measurement_basis(jcpo_su_t2018, observed).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(jcpo_su_t2020, observed).
narrative_ontology:measurement(jcpo_su_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement_basis(jcpo_su_t2022, observed).
narrative_ontology:measurement(jcpo_su_t2024, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(jcpo_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, nuclear_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, us_iran_strategic_asymmetry).

% DUAL FORMULATION NOTE:
% The JCPOA's legal status is contested across three structurally distinct readings: binding_multilateral (low extraction, cooperative reciprocity) vs. graduated_compliance (moderate extraction, proportional enforcement) vs. transactional_provisional (high extraction, unilateral authority). Each reading has different ε, different beneficiary/victim structure, and different type predictions. The three are linked via network.affects_constraints: changes in one reading's empirical status propagate to influence the others. The transactional reading (this file) asserts individual state sovereignty and domestic coalition benefit as the primary beneficiaries, which forecloses binding-multilateral cooperation on equal terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
