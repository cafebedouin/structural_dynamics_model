% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Article VI as Aspirational Disarmament Goal (NWS Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The NWS reading of the NPT treats Article VI's disarmament obligation as
 *   an aspirational long-term goal rather than a binding, time-bound
 *   commitment. This reading leverages the 'at an early date' ambiguity to
 *   maintain nuclear arsenals while demanding strict non-proliferation
 *   compliance from NNWS. The IAEA safeguards budget and verification
 *   architecture overwhelmingly target horizontal proliferation (NNWS
 *   compliance) while vertical proliferation (NWS arsenal modernization)
 *   faces minimal verification. This creates a structurally extractive
 *   arrangement: NWS extract security benefits and status from nuclear
 *   retention while transferring the verification burden and constraint costs
 *   to NNWS. The coordination function (preventing nuclear war through
 *   managed deterrence) is real but asymmetrically enforced — active
 *   enforcement against NNWS, theatrical compliance by NWS.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.82).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Article VI as Aspirational Disarmament Goal (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '5226bfee-cbf8-4dbe-b555-4b9d06d1a9af').
narrative_ontology:cs_kernel_codification('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', fixed_text).
narrative_ontology:cs_authority_grounding('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', lineage).
narrative_ontology:cs_interpretation_layer_present('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af').
narrative_ontology:cs_reading_relation('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', foundational, disarmament_aspirational_not_binding).
narrative_ontology:cs_axiom_status(disarmament_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', disarmament_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', foundational, strategic_stability_requires_nuclear_retention).
narrative_ontology:cs_axiom_status(strategic_stability_requires_nuclear_retention, holdable).
narrative_ontology:cs_axiom_grounding('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', strategic_stability_requires_nuclear_retention, instrumental).
narrative_ontology:cs_reference_frame('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', npt_original_bargain_1968).
narrative_ontology:cs_drift_state('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', post_2010_action_plan_failure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5226bfee-cbf8-4dbe-b555-4b9d06d1a9af', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_military_industrial_complexes).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_alliances).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, npt_review_conference_consensus).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, disarmament_verification_architecture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, iaea_secretariat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized NWS (US, Russia, UK, France, China) set the interpretive agenda for Article VI, control the pace and scope of disarmament diplomacy, and maintain nuclear arsenals through modernization programs. They benefit from non-proliferation constraints on others while bearing minimal verification burden. Their exit options include arsenal modernization, flexible interpretation of 'good faith,' and alliance vetoes over verification regimes.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Defense contractors, nuclear laboratories, and associated industrial bases in NWS receive sustained funding for arsenal modernization, life-extension programs, and new warhead development. The aspirational disarmament reading preserves their revenue streams while non-proliferation constraints on NNWS create no reciprocal industrial constraint on NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nws_military_industrial_complexes, beneficiary,
    powerful, biographical, mobile, national).

% NATO and other nuclear-allied states benefit from extended deterrence guarantees underwritten by NWS arsenals. They participate in nuclear sharing arrangements and support NWS interpretive positions in diplomatic forums. Their security doctrines depend on the credibility of nuclear deterrence, which the aspirational reading preserves.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nws_alliances, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nws_alliances, agenda_setter).

% 186 NNWS parties bear full IAEA safeguards costs, accept intrusive verification, and forego the nuclear option. They receive aspirational disarmament commitments that have not materialized into time-bound reductions. Their exit option (Article X withdrawal) carries extreme security, economic, and diplomatic costs — effectively constrained. They organize in NAM and regional blocs to resist but lack leverage over NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% The NPT Review Conference consensus mechanism is structurally victimized by the NWS reading: consensus documents (1995, 2000, 2010) articulate binding disarmament benchmarks that NWS subsequently ignore or reinterpret. The consensus process itself becomes theater — producing documents that legitimate the regime while the structural asymmetry persists.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, npt_review_conference_consensus, payer,
    organized, biographical, constrained, global).

% Verification mechanisms for disarmament (warhead dismantlement monitoring, fissile material declarations, irreversibility verification) remain undeveloped because NWS block their negotiation. The IAEA's mandate and budget concentrate on NNWS safeguards; vertical proliferation verification is excluded. This institutional gap is not technical but political — maintained by NWS beneficiaries.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_verification_architecture, payer,
    institutional, generational, trapped, global).

% The IAEA Secretariat operates the asymmetric verification mandate: comprehensive safeguards on NNWS, minimal access to NWS facilities (voluntary offer agreements only). It bears the operational cost of this asymmetry — credibility gaps, resource allocation distortions, and political pressure from both NWS and NNWS blocs.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, payer,
    institutional, biographical, constrained, global).

% Sees the full structural asymmetry: a treaty bargain where one pillar (non-proliferation) is binding and enforced while the other (disarmament) is aspirational and unenforced. The coordination function (preventing nuclear war) is real but asymmetrically distributed — the engine computes per-seat classifications from this structural data.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear war by limiting the number of nuclear-armed states and providing a managed framework for nuclear deterrence among recognized NWS. The IAEA safeguards system provides verification confidence that NNWS are not diverting materials.
% TRANSFER_FUNCTION: Transfers verification burden, nuclear latency option value, and security autonomy from NNWS to NWS. NWS retain nuclear capabilities and status while NNWS accept intrusive verification and forego the nuclear option in exchange for aspirational disarmament promises.
% ABSENT_VOICES: Future generations who inherit the nuclear risk without participating in the bargain; states that remained outside the NPT (India, Pakistan, Israel, North Korea) and developed nuclear weapons without the constraint; civil society movements for nuclear abolition excluded from diplomatic conferences.
% DISAPPEARANCE_RATIONALE: If the NWS reading vanished overnight, NNWS would demand binding, verified disarmament timelines as the price of continued non-proliferation compliance. The verification architecture would need to expand to vertical proliferation. NWS would lose interpretive control and face enforceable disarmament obligations. The nuclear order would reorganize around either a strengthened bargain or regime collapse.
% FOUNDING_PROBLEM: Prevent nuclear war by limiting nuclear weapons spread while providing a pathway to elimination. The 1968 bargain: NNWS forego nuclear weapons; NWS pursue disarmament in good faith; all share peaceful nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: NWS attest the problem is live (security environment prevents disarmament). NNWS and civil society (ICAN, IPPNW, UNIDIR analyses) attest the disarmament pillar has atrophied while non-proliferation enforcement intensified — corroborated by 2017 TPNW adoption (122 states) as evidence of NNWS judgment that the founding bargain is broken. No independent corroboration supports the NWS reading's claim that current arrangements fulfill the founding problem.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) reflects the structural transfer: NWS retain nuclear capabilities while NNWS bear the full verification burden and forego the nuclear option. Suppression (0.82) is high because the constraint's persistence depends on actively suppressing alternative interpretations (NNWS reading) and blocking verification mechanisms for vertical proliferation. Theater ratio (0.45) captures the gap between NWS declaratory commitments (action plans, unequivocal undertakings) and actual disarmament trajectory (arsenal modernization, new warhead programs). Accessibility collapse (0.65) reflects that NNWS cannot exit the constraint without severe security consequences, but NWS maintain exit options through alliance structures. Resistance (0.55) is moderate: NNWS coalitions (NAM, NPT Review Conference blocs) resist but lack leverage to change NWS behavior.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the constraint is a rope: genuine coordination preventing nuclear proliferation while managing strategic stability. From the NNWS seat, it is a snare: binding non-proliferation extracted in exchange for disarmament promises that are structurally unenforceable. The engine computes this divergence from the beneficiary/victim declarations and power/exit asymmetries — the NWS reading's claimed_type (tangled_rope) acknowledges the coordination function while the metrics reveal its extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural beneficiaries (d ≈ 0.15): they collect security guarantees, great power status, and alliance cohesion while bearing minimal verification cost. Their exit options are arbitrage-grade (can modernize arsenals, interpret obligations flexibly, use alliance vetoes). NNWS are structural targets (d ≈ 0.85): they bear full verification costs, forego nuclear latency, and face sanctions for non-compliance while receiving aspirational disarmament promises. Exit options are constrained (withdrawal under Article X carries extreme security and diplomatic costs). IAEA verification architecture is an institutional payer (bears operational costs of asymmetric verification mandate). NWS alliances (NATO, etc.) are secondary beneficiaries through extended deterrence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear war through non-proliferation) remains live but the disarmament bargain has atrophied. NWS treat disarmament as aspirational cover for nuclear retention — the mandate has outlived its disarmament function but persists because NWS benefit from the non-proliferation constraint on others. This is not a piton (which would require no concentrated beneficiary); NWS actively maintain the arrangement. The mandatrophy_resolved flag is false: the constraint's mandate has not been formally resolved or replaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness,
    'Is Article VI a binding legal obligation requiring specific disarmament actions, or a hortatory aspiration?',
    'ICJ advisory opinion on nuclear weapons legality (1996) found obligation to pursue in good faith; NPT Review Conference consensus documents (1995, 2000, 2010) treat it as binding. NWS consistently interpret as aspirational. Resolution requires authoritative interpretation accepted by all parties — currently absent.',
    'If binding, NWS are in persistent material breach, transforming the constraint from tangled_rope toward snare. If aspirational, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_bindingness, conceptual, 'Legal character of Article VI disarmament obligation').

omega_variable(
    verification_asymmetry_purpose,
    'Does IAEA verification concentration on NNWS reflect technical necessity (horizontal proliferation is verifiable, vertical is not) or political choice (NWS block vertical verification)?',
    'Technical assessment of verification capabilities for declared vs. undeclared arsenals, fissile material cut-off verification, and warhead dismantlement monitoring. Political analysis of NWS opposition to FMCT and verification protocols.',
    'If technical necessity, the asymmetry is coordination cost (lowering extractiveness). If political choice, it is extractive design (supporting current high extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_asymmetry_purpose, empirical, 'Origin of verification asymmetry between horizontal and vertical proliferation').

omega_variable(
    committer_frame_ambiguity,
    'Does this reading''s structural advantage derive from legitimate interpretive authority or from the power to impose its interpretation on the treaty regime?',
    'Analyze whether NWS interpretation is sustained by textual support, negotiating history, and subsequent practice — or by NWS control of Security Council, IAEA governance, and alliance structures that suppress alternative readings.',
    'If interpretive authority, the reading''s beneficiaries are legitimate coordinators. If imposed interpretation, the constraint is a snare with coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether NWS reading''s dominance is interpretive or coercive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt_nws_tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(npt_nws_tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(npt_nws_tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(npt_nws_tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(npt_nws_tr_t2025, npt_treaty_text__nws_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_nws_be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt_nws_be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(npt_nws_be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(npt_nws_be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(npt_nws_be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(npt_nws_be_t2025, npt_treaty_text__nws_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(npt_nws_su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(npt_nws_su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(npt_nws_su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(npt_nws_su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(npt_nws_su_t2025, npt_treaty_text__nws_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_verification_architecture).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_review_conference_consensus_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, fissile_material_cutoff_treaty_negotiations).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nuclear_weapon_free_zone_treaties).

% DUAL FORMULATION NOTE:
% NPT kernel decomposition: nws_reading (this story) extracts via Article VI ambiguity; nnws_reading treats Article VI as binding exchange condition; withdrawal_threshold_reading contests Article X. All three share the treaty text kernel but instantiate different constraints with different epsilon values and beneficiary/victim structures. This reading's high epsilon derives from NWS interpretive control of 'at an early date' and verification asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.15).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
