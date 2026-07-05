% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework, Voidable Upon Unilateral Bad-Faith Determination
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the JCPOA bindingness
 *   kernel: the transactional-provisional reading, under which the 2015
 *   nuclear agreement is a non-treaty political commitment that either party
 *   may treat as voided upon its own unilateral determination that the other
 *   side is acting in bad faith, without requiring recourse to the
 *   agreement's own Joint Commission dispute-resolution process. This is NOT
 *   a story about whether the JCPOA is 'really' binding — that question is
 *   exactly what the kernel contest is about, and averaging across readings
 *   would violate epsilon-invariance. Under THIS reading specifically, the
 *   low-constraint exit mechanism is the structurally load-bearing feature:
 *   it explains both the 2018 U.S. withdrawal and the reimposition of
 *   sanctions absent any Joint Commission finding of Iranian noncompliance at
 *   that time, and it explains the domestic political logic that made the
 *   provisional design attractive to a withdrawing executive in the first
 *   place. The rising extractiveness and theater-ratio trajectory from ~2015
 *   to ~2018 tracks the shift from cooperative implementation to unilateral
 *   exit and 'maximum pressure' sanctions.
 *
 * KEY AGENTS:
 *   - withdrawing_state_executive: agenda_setter, holds unilateral bad-faith determination power
 *   - iranian_civilian_population: primary payer, bears reimposed sanctions with no voice in the determination
 *   - compliant_treaty_partner_states: secondary payer, absorbs extraterritorial sanctions exposure
 *   - domestic_deal_opposition_coalition: beneficiary, gains durable veto leverage
 *   - joint_commission_dispute_mechanism: excluded non-agent, the bypassed adjudication procedure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.42).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework, Voidable Upon Unilateral Bad-Faith Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca').
narrative_ontology:cs_kernel_codification('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', fixed_text).
narrative_ontology:cs_authority_grounding('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', extraction).
narrative_ontology:cs_reading_relation('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', foundational, unilateral_sovereign_determination_sufficient_for_exit).
narrative_ontology:cs_axiom_status(unilateral_sovereign_determination_sufficient_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', unilateral_sovereign_determination_sufficient_for_exit, conventional).
narrative_ontology:cs_axiom('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', secondary, non_ratified_commitments_bind_only_at_will).
narrative_ontology:cs_axiom_status(non_ratified_commitments_bind_only_at_will, holdable).
narrative_ontology:cs_axiom_grounding('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', non_ratified_commitments_bind_only_at_will, conventional).
narrative_ontology:cs_reference_frame('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', executive_political_commitment_non_treaty_status).
narrative_ontology:cs_drift_state('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', post_2018_withdrawal, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e2691db5-0fd2-45bf-b7e8-c0dd3ab9deca', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opposition_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, compliant_treaty_partner_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, international_verification_regime).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the unilateral determination power: can declare the counterparty in bad faith and reimpose sanctions without requiring multilateral consensus or an adjudicated breach finding. Treats the JCPOA as an executive-level political commitment rather than a ratified treaty, which is precisely what makes exit low-cost. Captures domestic political credit for 'toughness' and reopens sanctions leverage that had been traded away.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Legislators, lobbying blocs, and allied governments who opposed the original deal gain a durable veto point: any future arrangement can be framed as similarly voidable, which lets them extract concessions or block re-entry without having to defeat the agreement on its substantive terms.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opposition_coalition, beneficiary,
    organized, biographical, mobile, national).

% Regional adversaries of the constrained state benefit from renewed sanctions pressure and from the precedent that multilateral nuclear agreements can be unwound by one party's unilateral judgment, which they can invoke against future arrangements they dislike.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states, beneficiary,
    powerful, generational, mobile, regional).

% Bears the reimposed sanctions' economic costs — currency collapse, medical shortages, trade collapse — despite having no role in either the original compliance bargain or the unilateral bad-faith determination that reversed it. Cannot exit the jurisdiction whose sanctions relief was withdrawn.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Co-signatory states (and the EU as a bloc) that continued compliance and built commercial relationships premised on JCPOA stability absorb reputational and economic costs when the framework is voided unilaterally by one participant, since secondary sanctions extend extraterritorially to their firms. They cannot compel the withdrawing state back into compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, compliant_treaty_partner_states, payer,
    institutional, generational, constrained, global).

% The IAEA's inspection architecture, built and calibrated for JCPOA verification, loses operational relevance and access once a party exits and the counterparty scales back cooperation in response; the technical monitoring infrastructure erodes as a direct cost of the framework's voidability.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_verification_regime, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, international_verification_regime, observer).

% Made verifiable nuclear concessions in exchange for sanctions relief; under this reading, that relief can be unilaterally revoked by the counterparty's own bad-faith finding without a joint dispute-resolution process ruling against it. Iran's own subsequent reduction of compliance commitments is both a response to and evidence cited for this reading's low-constraint structure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state, agenda_setter).

% The JCPOA's own dispute resolution process (Joint Commission, Advisory Board) was designed to adjudicate compliance disputes collectively. This reading treats it as bypassable: unilateral bad-faith determination substitutes for use of the mechanism, sidelining the procedure the text itself specifies for exactly this disagreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, joint_commission_dispute_mechanism, excluded,
    institutional, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__transactional_provisional_reading, joint_commission_dispute_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its transactional reading, the JCPOA solves a real short-horizon problem: it exchanges verifiable nuclear rollback for sanctions relief without requiring either side to make an irrevocable, treaty-ratified commitment neither party's domestic politics could sustain. Provisionality is what made the exchange possible at all.
% TRANSFER_FUNCTION: Moves sanctions relief and market access to Iran in exchange for enrichment limits and IAEA access; when this reading's exit clause is invoked, it reverses that flow unilaterally, moving punitive costs back onto Iranian civilians and secondary-sanctions exposure onto third-party firms and states, while moving political capital and negotiating leverage to the withdrawing executive and domestic opposition coalition.
% ABSENT_VOICES: The Joint Commission dispute mechanism that the text designates for adjudicating compliance disagreements is not consulted before the unilateral determination is made. Iranian civilians bearing the reimposed sanctions have no voice in either the original bad-faith finding or any review of it. The compliant co-signatories' preference for continued multilateral adjudication is overridden rather than argued against.
% DISAPPEARANCE_RATIONALE: If unilateral voidability were foreclosed and only multilateral or graduated-compliance exit were available, sanctions could not be reimposed without either a Joint Commission finding or proportional evidence review — Iran's negotiating position, the domestic coalition's leverage, and regional rivals' ability to invoke precedent would all be substantially altered. The current low-friction exit path is precisely the vulnerable-to-removal object.
% FOUNDING_PROBLEM: Neither the U.S. executive nor the Iranian government could secure ratification of a formal treaty (two-thirds Senate advice-and-consent was not obtainable; a fully binding instrument was not domestically saleable in Iran either), so the JCPOA was designed as an executive-level political commitment — the provisional, non-treaty character was the mechanism that made any deal possible.
% FOUNDING_PROBLEM_CORROBORATION: Independent international law scholars and former State Department legal advisers attest the JCPOA's status as a political commitment (not a treaty) was a documented, intentional design choice from 2015, corroborating the low-bindingness reading structurally. However, the same scholars are divided on whether that non-treaty status was ever meant to license unilateral bad-faith determination absent Joint Commission process, or whether it was meant only to avoid domestic ratification requirements while still expecting multilateral dispute resolution in practice — so the corroboration supports the framework's provisionality but not decisively this reading's low-constraint exit mechanism specifically.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (moderate-high) because the transactional reading's core function — a low-cost, reversible bargain — does produce a genuine coordination gain (verified rollback for relief) but the same low-constraint structure permits one party to capture the benefit of relief-then-withdrawal, extracting negotiating value without symmetric exposure to reciprocal penalty. Suppression is moderate (0.42): there is no direct coercive apparatus over Iran's population beyond the sanctions mechanism itself, but that mechanism's extraterritorial reach constrains third-party firms and states from alternative commercial arrangements. Theater ratio (0.4) reflects that 'bad faith' determinations under this reading are substantially performative political findings rather than technical compliance assessments — the IAEA's own verification reporting is frequently sidelined in favor of political declaration.
 *
 * DIRECTIONALITY LOGIC:
 *   The withdrawing state's executive sits at the beneficiary end: it accrues sanctions leverage and domestic political capital from the ability to declare bad faith without external adjudication. Iranian civilians sit at the full-target end: trapped, bearing sanctions costs, with zero voice in the triggering determination. Compliant co-signatory states occupy an intermediate but genuinely victimized position — institutionally powerful but unable to prevent the unilateral action, and exposed to extraterritorial secondary sanctions as a direct consequence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into either 'the deal was pure extraction all along' or 'the deal remains a live binding commitment regardless of the 2018 exit' by treating the coordination function (verified rollback for relief) and the extraction vector (asymmetric unilateral exit) as coexisting within the SAME provisional-transactional structure. The Tangled Rope classification captures that duality: the framework genuinely coordinated a real nonproliferation problem for roughly three years, and the same design feature that enabled that coordination (non-treaty status, no ratification requirement) is what permitted the asymmetric extraction that followed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_determination_legitimacy,
    'Does a state''s unilateral finding of counterparty bad faith constitute a legitimate exit trigger under the JCPOA''s own text and under general principles of treaty-adjacent political commitments, or does legitimate exit require exhausting the Joint Commission/Advisory Board dispute mechanism first?',
    'Textual and travaux-preparatoires analysis of JCPOA paragraphs 36-37 (dispute resolution mechanism) combined with UNSC Resolution 2231''s endorsement language; comparative analysis of how similar political-commitment frameworks (as opposed to ratified treaties) have handled unilateral versus adjudicated exit in state practice.',
    'If unilateral determination is found illegitimate under the framework''s own design, this reading''s core claim collapses toward the binding_multilateral_reading; if legitimate, the transactional_provisional_reading''s low-constraint exit structure is vindicated as the framework''s actual operative design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_determination_legitimacy, conceptual, 'Whether unilateral bad-faith determination was ever a legitimate exit path under the JCPOA''s own design, independent of political convenience.').

omega_variable(
    provisional_design_intent_ambiguity,
    'Was the JCPOA''s non-treaty, executive-agreement status intended by its drafters ONLY to avoid domestic ratification hurdles (a procedural convenience compatible with expected multilateral dispute resolution in practice), or was it intended to also preserve each party''s right to unilateral exit on its own determination (a substantive feature of the bargain)?',
    'Declassified negotiating records, contemporaneous statements from P5+1 negotiators, and comparison with how other executive agreements of similar structure have been treated when one party sought exit.',
    'If procedural-only, this reading over-claims a substantive feature from what was merely an avoidance-of-ratification tactic — the graduated_compliance_reading would better capture drafter intent. If substantive, this reading accurately identifies the framework''s actual bargained-for structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provisional_design_intent_ambiguity, empirical, 'Whether unilateral voidability was a drafting choice or an emergent interpretation imposed after the fact.').

omega_variable(
    beneficiary_structure_naturalness,
    'Is the concentration of benefit in the withdrawing executive and domestic opposition coalition an incidental byproduct of a genuinely necessary provisional design, or was the provisional/voidable structure itself shaped in anticipation of enabling exactly this later unilateral reversal?',
    'Historical analysis of domestic political coalition statements from 2015 (opposition characterizations of the deal''s non-bindingness at time of signing) cross-referenced against 2018 withdrawal justifications for continuity or discontinuity of argument.',
    'Continuity would suggest the provisional structure was always understood by key domestic actors as a future exit ramp, sharpening the tangled_rope classification toward extraction; discontinuity would suggest the 2018 exit was an opportunistic use of an incidentally available design feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_naturalness, empirical, 'Whether the beneficiary structure was anticipated in the original design or exploited after the fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t16, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(jcpo_tr_t16, observed).
narrative_ontology:measurement(jcpo_tr_t32, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement_basis(jcpo_tr_t32, observed).
narrative_ontology:measurement(jcpo_tr_t48, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 48, 0.55).
narrative_ontology:measurement_basis(jcpo_tr_t48, observed).
narrative_ontology:measurement(jcpo_tr_t64, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 64, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t64, observed).
narrative_ontology:measurement(jcpo_tr_t80, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t80, observed).
narrative_ontology:measurement(jcpo_tr_t96, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 96, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t96, projected).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t16, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(jcpo_be_t16, observed).
narrative_ontology:measurement(jcpo_be_t32, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement_basis(jcpo_be_t32, observed).
narrative_ontology:measurement(jcpo_be_t48, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement_basis(jcpo_be_t48, observed).
narrative_ontology:measurement(jcpo_be_t64, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 64, 0.6).
narrative_ontology:measurement_basis(jcpo_be_t64, observed).
narrative_ontology:measurement(jcpo_be_t80, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(jcpo_be_t80, observed).
narrative_ontology:measurement(jcpo_be_t96, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 96, 0.58).
narrative_ontology:measurement_basis(jcpo_be_t96, projected).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t16, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement_basis(jcpo_su_t16, observed).
narrative_ontology:measurement(jcpo_su_t32, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement_basis(jcpo_su_t32, observed).
narrative_ontology:measurement(jcpo_su_t48, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(jcpo_su_t48, observed).
narrative_ontology:measurement(jcpo_su_t64, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 64, 0.45).
narrative_ontology:measurement_basis(jcpo_su_t64, observed).
narrative_ontology:measurement(jcpo_su_t80, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(jcpo_su_t80, observed).
narrative_ontology:measurement(jcpo_su_t96, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 96, 0.42).
narrative_ontology:measurement_basis(jcpo_su_t96, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_sanctions_snapback_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints instantiating the jcpoa_treaty_bindingness kernel from a single stabilized text (the 2015 JCPOA and UNSC Resolution 2231). The binding_multilateral_reading treats the framework as requiring consensus dissolution; the graduated_compliance_reading ties exit to proportional evidence-based assessment; this transactional_provisional_reading treats unilateral bad-faith determination as a sufficient trigger. Each reading has a distinct beneficiary/victim structure and a distinct epsilon value — they are not the same constraint viewed from different angles, per the epsilon-invariance principle. This reading shows substantially higher extractiveness (0.58) than a hypothetical binding_multilateral_reading would, because the low-constraint exit path is precisely what enables the asymmetric extraction this story documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
