% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain: Reciprocal Article IV/VI Obligations
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT's 'grand bargain' reading holds that Article IV (peaceful use)
 *   and Article VI (disarmament) are legally and morally reciprocal: NNWS
 *   acceptance of permanent nonproliferation restraint is conditioned on NWS
 *   making genuine disarmament progress. When NWS fail to deliver — measured
 *   by stalled reductions, modernization programs, and refusal to negotiate
 *   disarmament timetables — the legitimacy of demanding continued NNWS
 *   restraint erodes. This reading makes Article VI justiciable and
 *   enforceable, not aspirational. It treats verification reciprocity (NWS
 *   transparency matching NNWS safeguards) as structural requirement. The
 *   constraint is claimed as tangled_rope: genuine coordination function
 *   (preventing horizontal proliferation) coexists with asymmetric extraction
 *   (NNWS pay continuously, NWS pay conditionally).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.62).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.74).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain: Reciprocal Article IV/VI Obligations").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'ce3a1ab7-a7d0-4c1e-bed2-509d23052098').
narrative_ontology:cs_kernel_codification('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', formalized).
narrative_ontology:cs_authority_grounding('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', lineage).
narrative_ontology:cs_interpretation_layer_present('ce3a1ab7-a7d0-4c1e-bed2-509d23052098').
narrative_ontology:cs_reading_relation('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', foundational, article_vi_justiciable_obligation).
narrative_ontology:cs_axiom_status(article_vi_justiciable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', article_vi_justiciable_obligation, conventional).
narrative_ontology:cs_axiom('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', foundational, reciprocity_conditionality_of_article_iv).
narrative_ontology:cs_axiom_status(reciprocity_conditionality_of_article_iv, holdable).
narrative_ontology:cs_axiom_grounding('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', reciprocity_conditionality_of_article_iv, conventional).
narrative_ontology:cs_axiom('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', secondary, verification_reciprocity_required).
narrative_ontology:cs_axiom_status(verification_reciprocity_required, holdable).
narrative_ontology:cs_axiom_grounding('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', verification_reciprocity_required, empirically_contingent).
narrative_ontology:cs_reference_frame('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', id_1970_grand_bargain_textual_bargain).
narrative_ontology:cs_drift_state('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', post_2010_action_plan_stalemate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ce3a1ab7-a7d0-4c1e-bed2-509d23052098', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, iaea_safeguards_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, treaty_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the disarmament pace and define compliance metrics for Article VI while controlling access to Article IV technology transfers. They benefit from nonproliferation restraint by NNWS but face declining legitimacy when disarmament stalls. Their exit is effectively arbitrage-grade: they can reinterpret obligations, leverage alliance structures, or withdraw from verification regimes without existential cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the cost of permanent nonproliferation restraint (Article III safeguards, Article IV technology limits) in exchange for promised disarmament progress. Their exit is constrained: withdrawal from NPT triggers sanctions and isolation; staying means accepting widening asymmetry. Some NNWS (NATO members) have dual role as beneficiaries of extended deterrence, but the structural position remains payer.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% Institutionalized verification machinery whose mandate and budget depend on NPT continuity. Gains authority and resources from Article III inspections but faces credibility erosion when Article VI stalls. Exit is analytical: the institution cannot leave the treaty, but its interpretive latitude expands or contracts with political winds.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_safeguards_regime, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, iaea_safeguards_regime, agenda_setter).

% The abstract normative asset that the NPT regime depends on. Every year of visible Article VI non-compliance by NWS without consequence degrades the treaty's claim to reciprocal legitimacy. Not an actor — listed here because the extraction and suppression metrics are largely measured against this asset's depletion.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, treaty_credibility, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__grand_bargain, treaty_credibility).

% Coalition of NNWS that consistently demand Article VI implementation as condition for Article IV cooperation. Their objections are recorded in Review Conference documents but structurally excluded from decision-making on disarmament timelines. They can threaten withdrawal or bloc non-cooperation, but lack individual leverage to change NWS behavior.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nonaligned_movement_states, excluded,
    organized, biographical, constrained, regional).

% States and civil society actors advancing the Treaty on the Prohibition of Nuclear Weapons as alternative framework. They reject the grand bargain's conditional logic entirely. Their exit is mobile: they have built a parallel treaty regime. They are excluded from NPT decision-making but exert external pressure on its legitimacy.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, tpnw_proponents, excluded,
    organized, generational, mobile, global).

% Track compliance, verify declarations, model breakout scenarios. They see the full structure but hold no operational role. Their assessments feed into Review Conference debates but do not bind parties.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, security_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of horizontal proliferation by exchanging permanent NNWS restraint for NWS disarmament progress and peaceful-use technology access — a reciprocal bargain that makes nonproliferation individually rational for NNWS.
% TRANSFER_FUNCTION: Moves nonproliferation restraint (forgone weapons capability, safeguards acceptance, technology limits) from NNWS to NWS in exchange for disarmament steps (reductions, transparency, cessation of testing) and technology transfer commitments. The transfer is asymmetric: NNWS pay upfront and continuously; NWS pay incrementally and conditionally.
% ABSENT_VOICES: Future generations who inherit the proliferation/disarmament equilibrium; populations in nuclear-weapon-free zones whose security rests on the bargain's durability; scientific communities in NNWS denied dual-use technology access without reciprocal disarmament verification.
% DISAPPEARANCE_RATIONALE: If the reciprocal obligation vanished overnight, NNWS would have no binding reason to maintain Article III safeguards or forego enrichment/reprocessing. NWS would lose the legal basis for demanding nonproliferation compliance. The treaty would likely fracture into competing blocs: some NNWS withdrawing to pursue hedging, others seeking security guarantees elsewhere, NWS losing verification access.
% FOUNDING_PROBLEM: 1960s recognition that uncontrolled horizontal proliferation would make nuclear use inevitable, while vertical arms racing made disarmament politically impossible for NWS. The bargain traded NNWS permanent restraint for NWS Article VI pursuit and Article IV technology sharing.
% FOUNDING_PROBLEM_CORROBORATION: NWS and NATO allies attest the problem remains live (proliferation risks persist, disarmament conditions unmet). NNWS coalitions (NAM, G-21) and TPNW proponents attest the founding problem has mutated: horizontal proliferation is managed but vertical disarmament has stalled, making the original bargain asymmetrical. Independent arms control scholars (e.g., Carnegie, SIPRI) corroborate the asymmetry from outside beneficiary positions.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the widening gap between NNWS compliance costs (safeguards, technology foregone, security dependence) and NWS delivery (disarmament steps that have slowed since 1990s). Suppression (0.74) is high because the regime actively suppresses exit: withdrawal triggers sanctions, enrichment technology is denied, and Review Conferences produce consensus documents that paper over non-compliance. Theater ratio (0.38) captures the growing performative share: Review Conferences, action plans, and working groups that generate process without binding disarmament outcomes. Accessibility collapse (0.48) is moderate — alternatives exist (TPNW, regional treaties, hedging) but are costly. Resistance (0.58) reflects sustained NNWS diplomatic pushback and civil society pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the constraint is coordination: they built the nonproliferation regime, fund IAEA, and reduce arsenals (from Cold War peaks). From the NNWS seat, the same structure is extraction: they froze their capabilities permanently while NWS modernize. From the IAEA seat, it is institutional survival dependent on a bargain whose reciprocity is visibly failing. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural beneficiaries: they collect nonproliferation restraint while controlling disarmament pace and verification terms — directionality near 0.15 (beneficiary end). NNWS are structural targets: they pay continuous compliance costs with constrained exit — directionality near 0.85 (target end). IAEA sits near symmetric (d ~0.5): gains institutional mandate but bears credibility costs. Treaty credibility as abstract victim has no directionality of its own; its depletion is the extraction's measurement. NAM states and TPNW proponents are excluded voices — their structural position is outside the bargain's enforcement machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation while managing vertical arms race) has partially succeeded on the first limb and visibly failed on the second. The arrangement persists because NWS benefit from the nonproliferation payoff without paying the disarmament price, and NNWS lack credible exit. Mandatrophy is unresolved: the constraint's mandate (reciprocal bargain) has outlived its function (actual disarmament), but no party can force revision. This is not a piton — NWS actively maintain it, not through inertia but through calculated asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI a legally enforceable obligation with measurable benchmarks, or a political aspiration without justiciable content?',
    'ICJ advisory opinion on Article VI interpretation; NPT Review Conference consensus on disarmament benchmarks; state practice on withdrawal triggers.',
    'If justiciable, NWS non-compliance becomes treaty breach licensing NNWS countermeasures (withdrawal, Article IV expansion). If aspirational, the grand bargain''s reciprocity claim collapses into the nonproliferation_primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Legal status of Article VI disarmament obligation').

omega_variable(
    disarmament_measurement_protocol,
    'What constitutes ''disarmament progress'' sufficient to sustain Article IV legitimacy? Reductions from Cold War peaks? Cessation of modernization? Entry into force of FMCT? Time-bound elimination?',
    'Negotiated disarmament verification protocol; agreed metrics in Review Conference final documents; independent technical assessment of modernization vs. reduction.',
    'Without agreed metrics, NWS can claim compliance through any favorable metric while NNWS see stagnation — the measurement ambiguity IS the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disarmament_measurement_protocol, empirical, 'Absence of agreed disarmament benchmarks as structural extraction enabler').

omega_variable(
    verification_reciprocity_gap,
    'Can NNWS safeguards (Article III) be reciprocal without NWS accepting equivalent transparency (declarations, inspections, fissile material cut-off verification)?',
    'Negotiation of NWS transparency measures; FMCT with verification; IAEA access to NWS facilities beyond voluntary offer agreements.',
    'If reciprocity is structurally required but absent, the verification regime itself becomes extractive — NNWS transparency is the price of admission, NWS opacity is the privilege of status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_reciprocity_gap, conceptual, 'Asymmetric verification as extraction mechanism').

omega_variable(
    committer_frame_ambiguity,
    'Does the grand_bargain reading foreclose the nonproliferation_primary reading within a single state''s legal framework, or do they coexist as competing interpretations held by different parties?',
    'Analysis of NWS official statements: do they explicitly reject Article VI justiciability (forecloses) or merely decline to operationalize it while paying lip service (coexists)?',
    'If NWS legally foreclose the grand bargain, the constraint is a snare (coordination story is pure cover). If they coexist, it is a genuine tangled_rope with contested legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Commitment-system framing: forecloses vs. coexists_with for sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_grand_bargain_tr_t1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(npt_grand_bargain_tr_t1985, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(npt_grand_bargain_tr_t1995, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(npt_grand_bargain_tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(npt_grand_bargain_tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(npt_grand_bargain_tr_t2015, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(npt_grand_bargain_tr_t2020, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(npt_grand_bargain_tr_t2025, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(npt_grand_bargain_be_t1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(npt_grand_bargain_be_t1985, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(npt_grand_bargain_be_t1995, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(npt_grand_bargain_be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(npt_grand_bargain_be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(npt_grand_bargain_be_t2015, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement(npt_grand_bargain_be_t2020, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(npt_grand_bargain_be_t2025, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt_grand_bargain_su_t1970, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(npt_grand_bargain_su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(npt_grand_bargain_su_t1995, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(npt_grand_bargain_su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(npt_grand_bargain_su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(npt_grand_bargain_su_t2015, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(npt_grand_bargain_su_t2020, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(npt_grand_bargain_su_t2025, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iii_safeguards).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_review_conference_process).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, tpnw_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, fmct_negotiations).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nuclear_modernization_programs).

% DUAL FORMULATION NOTE:
% Part of the npt_article_iv_vi_pairing constraint family. This reading (grand_bargain) treats reciprocity as enforceable condition; nonproliferation_primary treats Article VI as aspirational; abolitionist treats Article IV as illegitimate without complete disarmament. The three readings have different ε values and victim structures — they are distinct constraints linked by the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, institutional, 0.2).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
