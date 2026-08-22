% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Obligation (Reciprocal Disarmament Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the reciprocal_disarmament_reading of
 *   the NPT Treaty 1970 kernel. Under this reading, Article VI is a binding
 *   legal obligation with temporal urgency — not aspirational, not contingent
 *   on the security environment — and horizontal nonproliferation (Articles
 *   I-II) and vertical nonproliferation (Article VI) form a reciprocal
 *   bargain: NNWS forgo nuclear acquisition in exchange for NWS pursuing
 *   disarmament in good faith. The constraint's coordination function is the
 *   grand bargain itself; its extraction function is the NWS failure to
 *   deliver verified disarmament while demanding full NNWS compliance. The
 *   enforcement gap — Articles I-II have IAEA safeguards verification;
 *   Article VI has no verification mechanism — becomes structural injustice
 *   rather than implementation detail. NWS strategic autonomy (modernization
 *   programs, doctrinal expansion) enters the victim set as constrained
 *   autonomy; the NNWS coalition gains normative leverage through the Treaty
 *   on the Prohibition of Nuclear Weapons (TPNW) and RevCon consensus
 *   language.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary agenda_setter (treaty depositaries, Security Council P5) and victim (constrained modernization under reciprocal reading) — institutional/constrained
 *   - nnws_coalition: Primary beneficiary (normative leverage, legal equality) and payer (compliance costs, security foregone) — organized/constrained
 *   - nws_modernization_establishments: Victim (constrained by Article VI good faith) — institutional/constrained
 *   - international_verification_bodies: Beneficiary (IAEA safeguards regime, potential Article VI verification mandate) — institutional/analytical
 *   - nonproliferation_norm_entrepreneurs: Beneficiary (TPNW, humanitarian initiative, RevCon consensus) — organized/mobile
 *   - withdrawal_sovereignty_advocates: Excluded (Article X reading contested) — powerful/trapped
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.67).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Obligation (Reciprocal Disarmament Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '232238b3-361c-492c-91c3-0267da76fd68').
narrative_ontology:cs_kernel_codification('232238b3-361c-492c-91c3-0267da76fd68', formalized).
narrative_ontology:cs_authority_grounding('232238b3-361c-492c-91c3-0267da76fd68', lineage).
narrative_ontology:cs_interpretation_layer_present('232238b3-361c-492c-91c3-0267da76fd68').
narrative_ontology:cs_reading_relation('232238b3-361c-492c-91c3-0267da76fd68', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('232238b3-361c-492c-91c3-0267da76fd68', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('232238b3-361c-492c-91c3-0267da76fd68', foundational, article_vi_binding_temporal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('232238b3-361c-492c-91c3-0267da76fd68', article_vi_binding_temporal_obligation, conventional).
narrative_ontology:cs_axiom('232238b3-361c-492c-91c3-0267da76fd68', foundational, horizontal_vertical_reciprocity_constitutive).
narrative_ontology:cs_axiom_status(horizontal_vertical_reciprocity_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('232238b3-361c-492c-91c3-0267da76fd68', horizontal_vertical_reciprocity_constitutive, conventional).
narrative_ontology:cs_reference_frame('232238b3-361c-492c-91c3-0267da76fd68', id_1968_grand_bargain_reciprocity).
narrative_ontology:cs_drift_state('232238b3-361c-492c-91c3-0267da76fd68', post_tpnw_2017, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('232238b3-361c-492c-91c3-0267da76fd68', '2026-06-12T14:23:17Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, international_verification_bodies).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nonproliferation_norm_entrepreneurs).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear weapon states (US, Russia, UK, France, China) are treaty depositaries and UN Security Council P5. They set the disarmament agenda, control verification discussions, and define 'good faith' and 'conditions.' Under this reading, they are also payers: Article VI constrains their modernization programs, force postures, and doctrinal expansions — the strategic autonomy they would exercise absent the reciprocal bargain. Their exit is constrained: withdrawal (Article X) carries immense normative and strategic cost; they are locked into the regime they created.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer).

% The 186 non-nuclear-weapon states party to the NPT, coordinated through the Non-Aligned Movement, New Agenda Coalition, and humanitarian initiative. They benefit from the legal equality the reciprocal bargain promises (disarmament as the quid pro quo for their forswearing) and from normative leverage (TPNW, RevCon consensus, ICJ opinions). They pay through full safeguards compliance, export control regimes, security assurances foregone, and the opportunity cost of nuclear latency. Their exit is constrained: withdrawal triggers security dilemmas, sanctions risk, and loss of peaceful-use cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition, payer).

% The nuclear weapons complexes, strategic commands, and defense industrial bases of the NWS. They bear the direct constraint: Article VI's good-faith obligation (under this reading) limits life-extension programs, new warhead development, delivery system modernization, and doctrinal mission expansion. Their exit is constrained: they are state institutions, cannot leave the treaty, and their funding depends on the strategic autonomy the constraint limits.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_modernization_establishments, payer,
    institutional, biographical, constrained, national).

% IAEA (Articles I-II safeguards), CTBTO (test ban verification), and potential future Article VI verification bodies. They benefit because the constraint's reciprocal logic creates demand for their verification mandate — without the disarmament obligation, there is no verification mission for Article VI. They are analytical observers of the enforcement gap (safeguards exist for horizontal, nothing for vertical).
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_verification_bodies, beneficiary,
    institutional, generational, analytical, global).

% Civil society (ICAN, IPPNW, arms control NGOs), middle-power states (Ireland, Mexico, Austria, South Africa), and diplomatic entrepreneurs who use the reciprocal bargain's logic to advance normative projects (TPNW, humanitarian initiative, RevCon action plans). They benefit from the constraint's normative structure; they are mobile because their leverage comes from coalition-building, not institutional position.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nonproliferation_norm_entrepreneurs, beneficiary,
    organized, biographical, mobile, global).

% States and strategists who read Article X as an unqualified sovereignty right and treaty obligations as contingent on the security environment (the withdrawal_sovereignty_reading). They are excluded from the reciprocal reading's framework because that reading treats Article VI as non-contingent — withdrawal does not extinguish the disarmament obligation's moral force. They are trapped: they cannot instantiate their reading without breaking the regime, but their reading remains live in national security discourses.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, withdrawal_sovereignty_advocates, excluded,
    powerful, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear war by solving a dual collective-action problem: NNWS commit to not acquiring nuclear weapons (horizontal nonproliferation); NWS commit to pursuing nuclear disarmament in good faith (vertical nonproliferation). The reciprocal bargain makes both commitments credible — neither side would comply without the other's commitment.
% TRANSFER_FUNCTION: Moves compliance costs and security foregone from NNWS (safeguards, export controls, nuclear latency opportunity cost, security assurances not received) to NWS (disarmament obligation, constrained modernization, verification acceptance). The transfer is asymmetric: NNWS have delivered their side (verified non-acquisition); NWS have not delivered verified disarmament.
% ABSENT_VOICES: Nuclear-armed states outside the NPT (India, Pakistan, Israel, North Korea) — they would object to the reciprocal bargain's universalizing logic but are structurally excluded from the NPT framework. Nuclear modernization establishments within NWS — their institutional interests are constrained but they have no independent voice in treaty governance. Future generations — they bear the risk of bargain collapse but have no seat.
% DISAPPEARANCE_RATIONALE: If the reciprocal bargain vanished overnight: NWS would accelerate modernization and doctrinal expansion without Article VI constraint; NNWS would face a choice between accepting permanent nuclear inequality or pursuing hedging/acquisition; the nonproliferation regime would lose its constitutional logic; TPNW would gain normative ground but lose its NPT anchor; the global nuclear order would reorganize around explicit power politics rather than legal reciprocity.
% FOUNDING_PROBLEM: Preventing nuclear war in a world where technology makes nuclear weapons increasingly accessible. The 1968 NPT founding problem: how to stop horizontal proliferation (more states getting the bomb) while managing vertical proliferation (existing arsenals growing). The reciprocal bargain was the solution: NNWS forswear; NWS disarm.
% FOUNDING_PROBLEM_CORROBORATION: NWS (depositary statements, RevCon positions) attest the problem is live but disarmament is conditions-dependent. NNWS coalition (NAM statements, TPNW preamble, ICJ 1996 Advisory Opinion) attest the problem is live and the bargain is broken — disarmament is overdue. Independent corroboration: ICJ 1996 unanimously found Article VI a binding obligation; 2010/2015/2022 RevCon consensus documents record 'unease' and 'concern' at disarmament pace; the TPNW (2017, 70+ states) exists because NNWS judge the founding problem unsolved under NPT. No corroboration from NWS modernization establishments — they operate as if the problem is managed by deterrence, not disarmament.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the structural asymmetry: NNWS bear full compliance costs (safeguards, export controls, security assurances foregone, opportunity cost of nuclear latency) while NWS deliver zero verified warhead eliminations and pursue modernization. The 0.58 is indexed to the reciprocal reading's lights — from the oligopoly reading the same arrangement would show lower extraction because Article VI is read as aspirational. Suppression (0.67) captures the active enforcement of Articles I-II (sanctions, interdiction, Security Council resolutions) against the absent enforcement of Article VI. Theater ratio (0.42) measures the growing performative share: NWS statements at RevCons, step-by-step approaches, 'conditions-based' framing — activity that maintains the appearance of compliance without approaching elimination. Accessibility collapse (0.71) is high because the reciprocal bargain's logic makes alternatives (unilateral acquisition, withdrawal) structurally costly for NNWS; the constraint is experienced as a closing horizon. Resistance (0.54) reflects NNWS coalition building (NAM, NPT conferences, TPNW) and NWS pushback (modernization, doctrinal resistance, verification refusal). The measurement grid shows extraction accumulation (0.32→0.58 over 50 years), theater creep (0.18→0.42), and suppression intensification (0.45→0.67) — all on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The oligopoly_enforcement_reading experiences this same constraint as a Rope (low extraction, genuine coordination against proliferation). The withdrawal_sovereignty_reading experiences it as a Snare (coercive, exit-suppressing). The reciprocal_disarmament_reading experiences it as a Tangled Rope (genuine coordination + asymmetric extraction). The engine computes per-seat classifications from the structural data; the divergence across readings is the measurement the kernel structure exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural agenda_setters (depositary role, Security Council veto, control of disarmament pace) but under this reading they are also victims — their strategic autonomy is constrained by the Article VI good-faith obligation they signed. The directionality derivation would place NWS near d=0.5 (symmetric: they administer but are also bound), but the reading's structural premise says they experience the constraint as a binding limit on modernization — hence the omega on modernization_as_extraction. NNWS coalition are beneficiaries (normative equality, legal leverage) and payers (compliance costs, security foregone) — their d sits near symmetric but tilted toward target because the verification gap means they pay first and receive nothing verified. Modernization establishments are pure victims (d→1.0) — their autonomy is directly constrained. Verification bodies are beneficiaries (d→0.0) — the constraint justifies their mandate. Norm entrepreneurs are beneficiaries (d→0.0). Withdrawal advocates are excluded — their reading is not seated at the table.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear war through nonproliferation + disarmament) is contested: NWS say the security environment prevents disarmament; NNWS say the bargain is broken. The mandate has not atrophied — the coordination function (preventing horizontal proliferation) remains live and effective — but the extraction function (NWS non-delivery) has accumulated. This is not mandatrophy (where function disappears but structure persists); it is mandate betrayal where one side's performance is extracted to sustain the other's compliance. The constraint persists because neither side can afford collapse: NWS need the nonproliferation barrier; NNWS need the disarmament promise. The Tangled Rope classification captures this: it is not a Piton (no one benefits from the theater) and not a Snare (the coordination is real).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the npt_treaty_1970 kernel, and does the reciprocal_disarmament_reading structurally foreclose, coexist with, or influence the oligopoly_enforcement_reading and withdrawal_sovereignty_reading?',
    'Commitment-system analysis: map each reading''s foundational axioms and drift_state; compute logical relations (forecloses/coexists_with/influences) from the structural premises. This constraint is the reciprocal_disarmament_reading instantiation.',
    'If this reading forecloses the oligopoly_enforcement_reading, no single legal framework can hold both Article VI as binding and Article VI as aspirational. If they coexist, different parties legitimately hold each. If this reading influences the oligopoly reading, it creates normative pressure without logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system structure of the NPT kernel: this reading''s relations to sibling readings').

omega_variable(
    article_vi_verifiability_gap,
    'Does the absence of an Article VI verification mechanism (unlike Articles I-II safeguards) constitute a structural extraction from NNWS, or is it an implementation gap that does not alter the constraint''s classification?',
    'Compare NNWS compliance costs (safeguards, export controls, security assurances foregone) against NWS disarmament delivery (zero verified elimination, modernization programs ongoing). Quantify the compliance asymmetry.',
    'If the verification gap is structural extraction, the constraint''s effective extraction from NNWS rises and the Tangled Rope classification strengthens. If it is merely an implementation delay, the coordination function remains dominant and the constraint trends toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_verifiability_gap, empirical, 'Whether the Article VI verification asymmetry is structural extraction or implementation lag').

omega_variable(
    nws_modernization_as_extraction,
    'Do NWS nuclear modernization programs (life-extension, new warheads, delivery systems) constitute extraction from the reciprocal bargain, or are they compatible with Article VI''s ''good faith'' standard?',
    'Legal analysis of ICJ 1996 Advisory Opinion ''pursue negotiations in good faith'' against documented modernization budgets, timelines, and doctrinal statements. Track whether modernization expands or contracts nuclear mission sets.',
    'If modernization is extractive, NWS strategic autonomy enters the victim set as constrained modernization (the reading''s expected delta) and effective extraction rises. If compatible, the constraint''s coordination function absorbs modernization as legitimate deterrence maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_modernization_as_extraction, conceptual, 'Whether NWS modernization violates the reciprocal bargain''s good-faith obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(npt_reciprocal_disarmament_tr_t50, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(npt_reciprocal_disarmament_be_t50, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(npt_reciprocal_disarmament_su_t50, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 50, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_2017_humanitarian_ban).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, ctbt_verification_regime).

% DUAL FORMULATION NOTE:
% NPT kernel family: three readings of the same treaty text. reciprocal_disarmament_reading (this story) = Article VI binding + reciprocity. oligopoly_enforcement_reading = Articles I-II binding, Article VI aspirational. withdrawal_sovereignty_reading = Article X sovereignty, obligations contingent. All three link via affects_constraints. The ε values differ: oligopoly reading ε≈0.25 (coordination dominant); reciprocal reading ε≈0.58 (extraction accumulated); withdrawal reading ε≈0.72 (coercion dominant from NNWS perspective). This decomposition follows the ε-invariance principle: each reading is a distinct constraint with its own ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__reciprocal_disarmament_reading, institutional, 0.48).
constraint_indexing:directionality_override(npt_treaty_1970__reciprocal_disarmament_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
