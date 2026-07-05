% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint isolates the reading of the JCPOA that treats it as a
 *   binding multilateral treaty whose modification or dissolution legally
 *   requires consensus among the negotiating parties, operationalized through
 *   UNSC Resolution 2231's snapback and dispute-resolution mechanics. Under
 *   this reading, no single party — including the United States — possesses
 *   unilateral legal authority to void the agreement; withdrawal without
 *   consensus is a material breach rather than a lawful exit. This is one of
 *   three structurally distinct constraints emitted from the same underlying
 *   kernel (jcpoa_treaty_bindingness): the transactional_provisional_reading
 *   treats the deal as voidable on unilateral bad-faith determination, and
 *   the graduated_compliance_reading treats enforcement as scaled to
 *   proportional compliance rather than binary consensus-or-breach. These are
 *   not the same constraint measured differently — they have different
 *   beneficiary sets, different victim exposure, and different persistence
 *   logics, and are linked here only through network.affects_constraints, per
 *   the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - unsc_permanent_members: institutional agenda-setters who hold procedural veto power over modification under the binding reading
 *   - iranian_civilian_population: powerless, trapped payers who bear sanctions costs regardless of which reading prevails among great powers
 *   - united_states_executive: structurally a co-holder of the binding framework who demonstrated in 2018 that unilateral exit was practically available despite the reading's theoretical constraint
 *   - iaea_verification_regime: institutional beneficiary whose relevance depends on the treaty's continued operation as a formally binding, verifiable instrument
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'a3749613-002f-488b-a510-e0d5c738fc97').
narrative_ontology:cs_kernel_codification('a3749613-002f-488b-a510-e0d5c738fc97', formalized).
narrative_ontology:cs_authority_grounding('a3749613-002f-488b-a510-e0d5c738fc97', lineage).
narrative_ontology:cs_interpretation_layer_present('a3749613-002f-488b-a510-e0d5c738fc97').
narrative_ontology:cs_reading_relation('a3749613-002f-488b-a510-e0d5c738fc97', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('a3749613-002f-488b-a510-e0d5c738fc97', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('a3749613-002f-488b-a510-e0d5c738fc97', foundational, consensus_required_for_lawful_modification).
narrative_ontology:cs_axiom_status(consensus_required_for_lawful_modification, holdable).
narrative_ontology:cs_axiom_grounding('a3749613-002f-488b-a510-e0d5c738fc97', consensus_required_for_lawful_modification, conventional).
narrative_ontology:cs_axiom('a3749613-002f-488b-a510-e0d5c738fc97', foundational, unilateral_withdrawal_constitutes_material_breach).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_constitutes_material_breach, holdable).
narrative_ontology:cs_axiom_grounding('a3749613-002f-488b-a510-e0d5c738fc97', unilateral_withdrawal_constitutes_material_breach, conventional).
narrative_ontology:cs_reference_frame('a3749613-002f-488b-a510-e0d5c738fc97', unsc_resolution_2231_incorporation).
narrative_ontology:cs_drift_state('a3749613-002f-488b-a510-e0d5c738fc97', post_2018_unilateral_withdrawal, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a3749613-002f-488b-a510-e0d5c738fc97', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_treaty_architecture).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_diplomatic_apparatus).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_reformist_faction).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_states_outside_agreement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_consensus_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively hold the procedural keys to modification or dissolution under Resolution 2231's snapback architecture. Can invoke or block reimposition of sanctions through Security Council mechanics, and each retains an independent veto that lets any single permanent member frustrate collective action while still citing the treaty's binding multilateral character as the governing frame.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Administers inspections and verification reporting that the binding-treaty reading treats as the technical backbone of compliance determination. Its institutional relevance and budget line are sustained by the treaty's continued operation as a verifiable multilateral instrument; a collapse into unilateral withdrawal would strip its findings of binding procedural weight.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_regime, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_regime, agenda_setter).

% The broader NPT regime is not an actor with agency but its stability is invoked repeatedly to justify the binding-treaty framing: a durable JCPOA is presented as precedent that negotiated nonproliferation commitments hold. Listed here for completeness though it collects no rents directly; treated as non-agent.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_treaty_architecture, beneficiary,
    institutional, civilizational, analytical, global).

% The E3 and EU coordination mechanism (INSTEX and successor structures) derive diplomatic standing and a distinct seat at the table from the treaty's multilateral, consensus-bound character. Their leverage evaporates if any single party can unilaterally declare the deal void, so they invest heavily in defending the binding-consensus frame even when enforcement stalls.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_diplomatic_apparatus, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_diplomatic_apparatus, agenda_setter).

% Accepted enrichment caps and inspection access in exchange for sanctions relief that a unilateral U.S. withdrawal in 2018 substantially reversed while the binding-multilateral reading held that dissolution required consensus. Iran's government now argues it faces sanctions pressure without the promised relief, while other parties insist the treaty's multilateral obligations still bind Iran's own compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government, beneficiary).

% Bears the economic cost of sanctions regimes that persist or snap back regardless of which reading of the treaty prevails among great powers. Has no seat in the negotiation and no mechanism to compel either faithful multilateral enforcement or a clean unilateral exit; absorbs currency collapse, medical shortages, and trade isolation as the framework's consensus requirement stalls resolution in either direction.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Domestic political actors who staked credibility on the deal's durability under a binding multilateral framework. When a unilateral withdrawal proved possible in practice despite the binding-treaty theory, their internal position weakened relative to hardline factions who argued the treaty was never truly binding on the most powerful party.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_reformist_faction, payer,
    moderate, biographical, constrained, national).

% States such as Gulf Cooperation Council members and Israel were not parties to the JCPOA but are structurally affected by its enrichment ceilings and sanctions architecture. They have no formal voice in the consensus-modification process the binding reading privileges, and have historically lobbied outside powers to exit or renegotiate a framework they had no hand in drafting.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_states_outside_agreement, excluded,
    powerful, generational, constrained, regional).

% As a permanent Security Council member and original signatory, the U.S. is structurally positioned as a co-holder of the binding-consensus framework, yet in 2018 unilaterally withdrew and reimposed sanctions without securing multilateral consensus, demonstrating that the binding reading's procedural constraints could be bypassed by the single most powerful party rather than dissolved through it.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_executive, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states_executive, excluded).

% Analyze whether Resolution 2231's incorporation of the JCPOA created binding international legal obligations or merely endorsed a political framework. Their assessments diverge sharply along the same lines as the kernel's contested readings, with some treating unilateral withdrawal as a material breach and others treating it as within the discretion any sovereign retained.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a verifiable, multilaterally-monitored mechanism to cap Iranian nuclear enrichment in exchange for phased sanctions relief, coordinated through IAEA inspections and a Joint Commission dispute-resolution process intended to prevent any single party from unilaterally collapsing the arrangement.
% TRANSFER_FUNCTION: Moves sanctions relief and international legitimacy toward Iran conditional on verified restraint, while moving verification authority and procedural leverage toward the P5+1 institutional apparatus and the IAEA; when enforcement fractures, the costs of stalled relief and reimposed sanctions transfer disproportionately onto the Iranian civilian population rather than onto the negotiating governments.
% ABSENT_VOICES: Regional states excluded from the negotiating table (Gulf states, Israel) have no procedural standing in the consensus-modification mechanism despite bearing direct security consequences; ordinary Iranian citizens have no voice in either the compliance disputes or the sanctions calculus that determines their economic conditions.
% DISAPPEARANCE_RATIONALE: Proponents of the binding-multilateral reading argue that if the treaty's consensus requirement disappeared, the nonproliferation architecture would fragment and unilateral action would become normalized precedent for future arms-control agreements. Critics note that the 2018 U.S. withdrawal already demonstrated the binding character could be bypassed without formal consensus, so the practical world may not rearrange as much as the reading's proponents claim — the dispute over whether disappearance would matter is itself part of the kernel contest.
% FOUNDING_PROBLEM: Iran's advancing enrichment capacity in the early 2010s created escalating proliferation risk and regional security instability that neither unilateral sanctions nor military options had resolved; the JCPOA was built to cap that capacity through negotiated, verifiable, reciprocal constraints.
% FOUNDING_PROBLEM_CORROBORATION: IAEA verification reports (an institution with some independence from the negotiating parties, though also a beneficiary of the framework's continuation) attested Iranian compliance through 2018. Independent nonproliferation research institutes outside the P5+1 governments, such as arms-control monitoring NGOs, corroborate that enrichment levels rose substantially only after the U.S. withdrawal and subsequent sanctions reimposition — suggesting the founding problem re-emerged as a consequence of the binding reading's practical failure rather than remaining continuously live throughout.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness spikes sharply at t=3 (0.55) reflecting the 2018 U.S. withdrawal and sanctions reimposition, which the binding-multilateral reading's own logic treats as a breach rather than a lawful modification — the spike measures the gap between the reading's procedural claim and what actually happened. Theater ratio follows the same spike (0.5 at t=3): continued invocation of Resolution 2231's consensus language by remaining parties after a unilateral breach is substantially performative, since the underlying enforcement mechanism had already been circumvented. Suppression is elevated (0.55-0.68 across the interval) because the reading's persistence depends on remaining parties continuing to treat non-consensus action as illegitimate even when a powerful party demonstrated it could act outside that frame without effective consequence. All three tracked metrics share one time grid (t=0,2,3,5,7,10) so no metric's value is silently substituted from an unaligned point.
 *
 * PERSPECTIVAL GAP:
 *   From the UNSC permanent members' and IAEA's seats, the binding-multilateral reading looks like functioning tangled-rope coordination: a real verification function wrapped in enforceable consensus procedure. From the Iranian civilian population's seat, the same structure computes as extraction with a broken enforcement promise — the coordination benefit (sanctions relief) was withdrawn unilaterally while the extraction cost (economic isolation) persisted, and the 'binding' character did not compel restoration. The engine is expected to register this divergence rather than resolve it in favor of either seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (UNSC P5, IAEA, EU diplomatic apparatus) sit near the beneficiary end of directionality because the binding-multilateral frame is precisely what preserves their procedural relevance and leverage — a shift to a transactional-provisional reading would strip much of that standing. The Iranian civilian population and reformist faction sit near the full-target end: trapped exit options, no seat in modification proceedings, and direct exposure to sanctions costs that persist regardless of which great power invokes or ignores the consensus requirement. The United States executive is directionally ambiguous by design — nominally a co-holder of the binding framework (low d under the reading's own theory) but empirically demonstrated mobile exit in 2018, which is why an override is warranted rather than relying on pure structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capping Iranian enrichment to prevent proliferation and regional instability) has an ambiguous status specifically because the binding-multilateral reading's central mechanism failed empirically in 2018 without formal consensus-based dissolution ever occurring — the treaty was neither properly modified nor properly ended under its own terms, it was simply abandoned by one party and partially maintained by others. This is the mandatrophy signature: the mandate (binding multilateral consensus) persists in diplomatic language and IAEA reporting structures even though the practical enforcement function it was meant to guarantee no longer operates as designed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolution_2231_binding_status,
    'Does UNSC Resolution 2231''s incorporation of the JCPOA create a legally binding obligation on all parties equivalent to a treaty, or does it merely endorse a political framework that any signatory retains sovereign discretion to exit?',
    'An authoritative international judicial ruling (ICJ advisory opinion or contentious case) interpreting Resolution 2231''s binding character, or a UNSC-level consensus reaffirmation/repudiation of the binding reading following the 2018 precedent.',
    'If authoritatively confirmed as binding, the 2018 U.S. withdrawal constitutes an unremedied material breach with unresolved legal consequences, strengthening this reading''s claim to being the correct legal characterization. If confirmed as non-binding political framework, this reading is revealed as aspirational rather than descriptive, and the transactional_provisional_reading better describes the treaty''s actual operative status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resolution_2231_binding_status, conceptual, 'Whether the binding-multilateral characterization is legally authoritative or aspirational framing.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence would distinguish which of the three sibling readings (binding_multilateral, transactional_provisional, graduated_compliance) best describes the JCPOA''s actual operative logic, given that all three are consistent with the same underlying text?',
    'Track which reading''s predicted behavior actually manifested at each contested juncture (2018 withdrawal, subsequent E3 dispute-resolution invocations, Iranian enrichment escalation responses) and assess which reading''s procedural predictions were borne out versus falsified by state practice.',
    'If state practice consistently followed transactional-provisional logic (unilateral exit without consensus, no formal dissolution proceeding), this reading''s claim to structural accuracy weakens relative to its sibling, even though its beneficiaries continue to invoke its language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether observed state practice validates or falsifies the binding-consensus reading''s procedural claims.').

omega_variable(
    beneficiary_capture_of_naturalness_claim,
    'Do the institutions that benefit from the binding-multilateral reading (IAEA, EU diplomatic apparatus, UNSC P5) have an interest in maintaining this characterization independent of its accuracy, such that their continued invocation of it constitutes motivated reasoning rather than neutral legal assessment?',
    'Compare legal assessments from actors without institutional stake in the framework''s continuation (independent international law scholars, non-signatory states'' legal analyses) against assessments from the framework''s institutional beneficiaries.',
    'If independent assessments diverge substantially from beneficiary-institution assessments, that divergence supports treating this reading as partly a legitimating narrative for institutions whose relevance depends on the binding characterization, rather than a purely descriptive legal fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_naturalness_claim, conceptual, 'Whether beneficiary institutions'' advocacy for the binding reading reflects motivated interest rather than neutral legal analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(jcpo_tr_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 7, 0.4).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jcpo_be_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 7, 0.44).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 10, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(jcpo_su_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the jcpoa_treaty_bindingness kernel and the same underlying JCPOA text plus Resolution 2231. This story (binding_multilateral_reading) treats the instrument as requiring consensus for modification/dissolution, with unilateral action as breach. The transactional_provisional_reading treats it as voidable on unilateral bad-faith determination — the reading the U.S. executive effectively acted on in 2018. The graduated_compliance_reading treats enforcement as scaled to proportional compliance rather than binary. Each carries its own ε, beneficiaries, and victims; they are linked here rather than merged because merging would violate ε-invariance — the three readings do not share a stable extraction value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
