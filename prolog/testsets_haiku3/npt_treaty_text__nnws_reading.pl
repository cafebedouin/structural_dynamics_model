% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint embodies the Non-Nuclear Weapons States' reading of NPT
 *   Article VI: that disarmament is a binding legal obligation on the five
 *   Nuclear Weapons States, and that NNWS non-proliferation compliance is
 *   conditional restraint—purchasing NWS movement toward elimination. The
 *   NNWS reading asserts that the treaty's symmetry is not an illusion; it is
 *   enforced through the Review Conference system, diplomatic pressure, and
 *   the threat of TPNW defection. This reading competes directly with the NWS
 *   reading (disarmament as long-term aspiration, non-proliferation as
 *   unconditional), creating a fundamental interpretive divide over whether
 *   the treaty contains a binding bargain or a permission structure. The
 *   constraint is one reading of a contested kernel (the NPT treaty text
 *   itself); sibling readings instantiate different structural relationships
 *   between disarmament obligation and non-proliferation compliance.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Weapons States bloc (organized, coordination power through Review Conferences)
 *   - Five Nuclear Weapons States (institutional power, trapped between treaty text and strategic doctrine)
 *   - NPT Review Conference system (institutional architecture for NNWS collective enforcement)
 *   - Treaty on the Prohibition of Nuclear Weapons coalition (alternative regime providing NNWS exit option)
 *   - Verification bodies (IAEA, CTBTO; provide data supporting NNWS non-compliance claims)
 *   - Emerging nuclear aspirants (structurally excluded from enforcement, subject to asymmetric obligation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.38).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.42).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'ea3f237b-79ac-4c03-a307-7423380c08a5').
narrative_ontology:cs_kernel_codification('ea3f237b-79ac-4c03-a307-7423380c08a5', formalized).
narrative_ontology:cs_authority_grounding('ea3f237b-79ac-4c03-a307-7423380c08a5', lineage).
narrative_ontology:cs_interpretation_layer_present('ea3f237b-79ac-4c03-a307-7423380c08a5').
narrative_ontology:cs_reading_relation('ea3f237b-79ac-4c03-a307-7423380c08a5', npt_treaty_text__nws_reading, forecloses).
narrative_ontology:cs_reading_relation('ea3f237b-79ac-4c03-a307-7423380c08a5', npt_treaty_text__withdrawal_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('ea3f237b-79ac-4c03-a307-7423380c08a5', foundational, article_vi_legal_bindingness).
narrative_ontology:cs_axiom_status(article_vi_legal_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('ea3f237b-79ac-4c03-a307-7423380c08a5', article_vi_legal_bindingness, deontological).
narrative_ontology:cs_axiom('ea3f237b-79ac-4c03-a307-7423380c08a5', foundational, non_proliferation_conditional_on_disarmament_progress).
narrative_ontology:cs_axiom_status(non_proliferation_conditional_on_disarmament_progress, holdable).
narrative_ontology:cs_axiom_grounding('ea3f237b-79ac-4c03-a307-7423380c08a5', non_proliferation_conditional_on_disarmament_progress, deontological).
narrative_ontology:cs_reference_frame('ea3f237b-79ac-4c03-a307-7423380c08a5', npt_1968_symmetric_bargain).
narrative_ontology:cs_drift_state('ea3f237b-79ac-4c03-a307-7423380c08a5', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea3f237b-79ac-4c03-a307-7423380c08a5', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, npt_review_conference_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_aligned_movement).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapons_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NNWS coordinate through the Non-Aligned Movement (NAM) and bloc voting in the NPT Review Conference to assert that Article VI is a binding disarmament obligation. They frame non-proliferation restraint as conditional leverage: as long as NWS demonstrate concrete disarmament progress (arms reductions, verification protocols, timelines), NNWS will maintain non-weaponization commitments. They set the agenda by proposing disarmament benchmarks in Review Conference draft outcome documents and voting as a bloc. Their enforcement mechanism is the threat of regime withdrawal or TPNW accession—if NWS do not deliver progress, NNWS will defect. They benefit from a reading that imposes obligation on NWS because it gives them leverage to shape NWS behavior. Their exit option is TPNW accession, which is costly (losing IAEA technical support, security guarantees from NWS) but available if NPT negotiations collapse.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapons_states, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapons_states, beneficiary).

% Five recognized NWS (US, Russia, China, UK, France) experience the NNWS reading as imposing diplomatic obligation to justify arsenal maintenance and show disarmament progress. They interpret Article VI as a commitment to 'pursue negotiations' toward disarmament—permitting indefinite timelines so long as diplomatic effort continues. They maintain arsenals as strategic deterrents and resist temporal benchmarks or verification regimes that would limit their flexibility. They are trapped: leaving the NPT would collapse the regime and trigger universal proliferation (catastrophic for NWS security interests); staying means accepting NNWS pressure and having to justify arsenals through diplomatic theater. They pay through reputational cost (being branded as treaty-breakers), constraints on arsenal modernization programs (requiring nonproliferation justification), and diplomatic friction at Review Conferences. Their exit is impossible without regime collapse; their alternative to engagement is public non-compliance and confrontation with NNWS, which raises proliferation risk. They are beneficiaries of the NWS reading (which treats non-proliferation as unconditional) but locked into a treaty that contains the NNWS reading as a live interpretation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapons_states, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapons_states, observer).

% The Review Conference meets every five years to assess NPT implementation and issue collective consensus outcomes. The conference's procedural structure gives majority voting authority to NNWS (190+ of 193 parties are non-nuclear). Under the NNWS reading, the Review Conference is the legitimate venue for interpreting Article VI and establishing binding disarmament benchmarks. NWS participate but cannot veto outcomes (requiring consensus, not unanimity, makes consensus-breaking costlier for NWS than for NNWS blocs). The conference system generates soft-law precedent: outcomes are not legally binding in the ICJ sense, but they carry diplomatic weight and establish interpretive authority. The system persists because both NNWS and NWS value maintaining an institutionalized forum for non-proliferation coordination over unilateral exit. The conference benefits from NNWS participation (which gives it legitimacy and covers negotiation costs); it is vulnerable to NWS non-engagement or unilateral redefinition of the treaty's meaning.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, npt_review_conference_system, agenda_setter,
    institutional, generational, mobile, global).

% States that have pursued or are pursuing nuclear capabilities (Iran, North Korea, Libya historically; Japan, South Korea, Turkey as latent aspirants) are nominally subject to NNWS non-proliferation obligation but excluded from disarmament bargaining. They cannot participate in Review Conference disarmament negotiations and have no voice in interpreting Article VI. They would object to an asymmetric reading that requires them to forgo weapons while NWS indefinitely retain and modernize arsenals. Their situation illustrates the constraint's power asymmetry: they are bound by it but cannot contest its terms. Their exclusion from the constraint is the constraint's structural weakness—if disarmament obligation were genuinely binding, it would be negotiated with non-nuclear aspirants' consent or compensation (enrichment technology, security guarantees). Instead, aspirants are subject to non-proliferation surveillance and sanctions without reciprocal NWS obligation to disarm.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, emerging_nuclear_aspirants, excluded,
    moderate, biographical, constrained, regional).

% The TPNW (2017, in force 2021) is a separate legal regime that prohibits nuclear weapons categorically, without temporal indefiniteness. It is signed by 150+ NNWS and zero NWS. TPNW provides an alternative compliance structure for states dissatisfied with NPT disarmament progress. Every TPNW accession is a statement that the NPT disarmament bargain has failed. The regime benefits from NNWS defection threats (the threat to accede to TPNW is the leverage mechanism that pressures NWS in Review Conferences). TPNW is not a real agent but an institutional structure (treating it as agent=false); it is listed as a stakeholder for narrative completeness because the constraint story must account for it as a competing regime.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, treaty_on_prohibition_of_nuclear_weapons, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__nnws_reading, treaty_on_prohibition_of_nuclear_weapons).

% The IAEA monitors non-proliferation compliance through safeguards inspections and reports to the Board of Governors and the UN Security Council. It provides technical data on whether NNWS are developing weapons (negative verification) and has no mandate to verify NWS disarmament progress. The IAEA sits in an asymmetric position: it can report NWS non-compliance if commissioned to inspect their arsenals (which they have not authorized), but it can report NNWS non-compliance through routine safeguards. This asymmetry reflects the reading divide: the NNWS reading would require IAEA verification of NWS disarmament; the NWS reading treats NWS arsenals as outside the IAEA's scope. The IAEA's actual role (verifying non-proliferation only) supports the NWS reading. The IAEA's analytical function is to provide data that NNWS use to claim NWS non-compliance with aspirational disarmament goals (measuring whether arsenal reductions occur, not whether reductions are sufficient by any binding standard).
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% The CTBTO operates verification systems to detect nuclear explosions, supporting the CTBT (signed 1996, not yet in force; ratification blocked by US, China, Egypt, Iran, Israel). The CTBT is presented as an Article VI disarmament step (halting the development of new warhead designs). The CTBTO's analytical role is to provide evidence whether NWS are complying with the test moratorium (held since 1996 by major NWS, except North Korea). The organization supports the NNWS reading by providing technical data on NWS disarmament progress; it has no enforcement power but supplies information NNWS can cite in Review Conferences as evidence of either NWS compliance (test abstinence) or non-compliance (arsenal modernization programs that circumvent the test ban through subcritical experiments).
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, comprehensive_test_ban_treaty_organization, observer,
    institutional, biographical, analytical, global).

% The NAM (120+ developing and non-aligned states) provides organizational continuity for NNWS pressure on NWS. The NAM coordinates Review Conference voting blocs, proposes disarmament language, and mobilizes diplomatic support for the NNWS reading. The NAM benefits from a binding interpretation of Article VI because it increases NWS negotiation costs and gives the NAM leverage in broader geopolitical negotiations (trade, development, security guarantees). The NAM's power is coalition-based (mobile, can fracture if large states defect) but substantial because it controls Review Conference majorities.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_aligned_movement, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, npt_review_conference_system).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT coordinates two asymmetric commitments: NNWS forgo nuclear weapons development and accept inspection; NWS commit to pursue disarmament under Article VI and refrain from proliferation assistance. This reading asserts that both commitments are binding, creating mutual restraint—NNWS compliance is contingent on NWS demonstrable progress toward disarmament, not indefinite acceptance of permanent NWS arsenals.
% TRANSFER_FUNCTION: The constraint moves diplomatic legitimacy and enforcement authority from individual NWS to the NNWS-majority Review Conference system. NNWS bloc transfer their non-proliferation forbearance (accepting permanent vulnerability to NWS arsenals) in exchange for NWS commitment to disarmament benchmarks and codified timelines. The transfer is conditional: if NWS do not deliver disarmament progress, NNWS are released from restraint and can pursue TPNW accession or rearmament.
% ABSENT_VOICES: Emerging nuclear aspirants (Iran, North Korea in past context) are structurally excluded: they must forgo nuclear weapons but cannot participate in defining disarmament obligations on NWS or in negotiating the terms of asymmetry. They would argue the bargain is unjust and unenforceable against them. Subnational actors (terrorist groups, non-state armed groups seeking WMD) are entirely absent from the treaty framework but would object to their exclusion from compliance verification and to the unresolved proliferation risks that indefinite NWS arsenals create.
% DISAPPEARANCE_RATIONALE: If this reading of Article VI as binding vanished—if the international community accepted the NWS reading that disarmament is aspirational and indefinitely deferred—the NPT would transform from a conditional non-proliferation bargain into a permanent permission structure for NWS arsenals. NNWS would have no enforcement leverage; defections to TPNW would accelerate; regional security dilemmas would trigger new proliferation (Japan, South Korea, Turkey, Saudi Arabia would face unbalanced deterrence calculus); and the treaty's legitimacy would erode within a generation. The regime would persist structurally but lose the normative force that sustains NNWS compliance with inspection and non-weaponization.
% FOUNDING_PROBLEM: In the 1960s, nuclear weapons were spreading and the superpowers faced mutual annihilation risk. The NPT was designed to lock in the existing asymmetry—five NWS would retain arsenals, all others would forgo them—but make the asymmetry tolerable by binding NWS to pursue disarmament. Without the disarmament commitment, the treaty would be a permanent license for the five to dominate the rest.
% FOUNDING_PROBLEM_CORROBORATION: NNWS governments, the Non-Aligned Movement, and disarmament NGOs assert the founding problem is still live: proliferation risks persist because NWS broke the disarmament bargain. NWS governments assert the problem has partially shifted: disarmament is now constrained by strategic stability considerations (Russia, China rising powers; renewed great-power competition) that the 1960s treaty framers did not anticipate. Independent expert bodies (International Commission on Nuclear Nonproliferation and Disarmament, 2009) and academic consensus support the NNWS reading that NWS have underperformed on Article VI. The corroboration comes primarily from NNWS and non-benefiting expert bodies; NWS corroborate only the softer claim that disarmament remains a long-term goal, not a binding near-term obligation.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the NNWS reading imposes pressure on NWS through diplomatic obligation and conditional compliance, but lacks coercive enforcement—Review Conference pressure is binding in soft-law sense only; NWS can ignore outcomes without legal penalty (though reputational cost rises). Suppression is moderate (0.42) because the constraint's persistence depends on NNWS bloc coordination and willingness to threaten TPNW defection; NWS suppress dissent by controlling disarmament pace and framing indefinite arsenals as 'strategic necessity,' but this suppression is not absolute—NNWS public criticism is protected by diplomatic immunity and review-conference forums. Theater is high (0.48) because much of the visible activity around Article VI consists of diplomatic theater: joint disarmament declarations without concrete timelines, review-conference consensus language that papers over disagreement, and confidence-building measures (CTBT signature but not ratification, arms-reduction declarations without verification) that signal intention without changing operational arsenals. The measurement series shows extractiveness rising from 1970–2010 (NNWS demands intensify, NWS resistance hardens) then plateauing (TPNW regime solidifies as safety valve, preventing further escalation). Theater ratio rises sharply 1970–2010 (more diplomatic ceremony, fewer concrete outcomes) then stabilizes (the constraint finds a working equilibrium between pressure and theater).
 *
 * PERSPECTIVAL GAP:
 *   The NNWS perspective: the NPT was a bargain where both sides accepted binding commitments; NWS have violated Article VI by maintaining permanent arsenals while the NNWS upheld their end; the constraint enforces a legitimate obligation, and NNWS conditional restraint is justified leverage to compel compliance. The NWS perspective: Article VI is a good-faith aspiration toward long-term disarmament, but strategic competition, verification impossibility, and deterrence logic make current disarmament infeasible; NNWS pressure is treating a reasonable long-term goal as a binding near-term obligation and unfairly constrains legitimate defense needs. The Review Conference system's perspective (institutional position): the conference is the forum for collective interpretation of the treaty, and its consensus outcomes are binding in legitimacy terms even if not legally enforceable; NNWS pressure is the proper function of the conference, and NWS resistance undermines the treaty's authority. The engine computes different effective-extraction values from each seat because they have different power atoms (NWS institutional vs. NNWS organized), different exit options (NWS trapped by regime-collapse risk vs. NNWS mobile through TPNW), and different structural relationships to the constraint's enforcement (NNWS set the agenda through bloc voting; NWS can delay but not block outcomes).
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS bloc: d ≈ 0.25–0.35 (beneficiary-leaning; they set the agenda, control the review process majority, and frame the treaty interpretation; they benefit from NWS compliance with disarmament benchmarks; their exit to TPNW is mobile but costly, so they remain engaged with NPT pressure). NWS: d ≈ 0.65–0.75 (target-leaning; they bear the diplomatic cost of defending indefinite arsenals, face public pressure for non-compliance, are structurally trapped by proliferation-collapse risk, and cannot exit without regime-wide consequences). Review Conference system: d ≈ 0.50 (symmetric; it coordinates both NNWS pressure and NWS defense, and persists because both sides value avoiding regime collapse more than winning a unilateral advantage). Emerging nuclear aspirants (excluded from the constraint): d ≈ 0.85 (full targets; they are subject to non-proliferation obligation but cannot negotiate disarmament terms and have no voice in Review Conference decisions that affect them). The asymmetry in d-values reflects the constraint's extractive core: NNWS benefit from a reading that imposes obligation on NWS; NWS bear the cost of a reading that interprets their arsenal maintenance as treaty violation.
 *
 * MANDATROPHY ANALYSIS:
 *   The NNWS reading avoids simple mandatrophy because the founding problem—preventing proliferation in the context of NWS deterrence—remains live and contestable. The constraint's mandate has not become obsolete in a straightforward sense; rather, the mandate has bifurcated: NNWS treat Article VI as an active commitment to pursue disarmament benchmarks; NWS treat it as a long-term aspiration compatible with indefinite arsenal maintenance. The constraint persists because both sides want to avoid the coordination failure (proliferation race) that would result if the treaty collapsed. Theater ratio's rise to 0.48 signals partial mandatrophy—a growing gap between disarmament theater (review conference language, symbolic declarations) and operational arsenals (which have modernized, not diminished). However, the constraint does not meet the piton threshold (high theater + diffuse gains + no concentrated beneficiary) because the NNWS bloc is a concentrated beneficiary that actively maintains the constraint through review conference coordination and TPNW threats. The constraint is better classified as a 'contested rope'—NNWS argue it is genuine coordination with binding terms; NWS argue it is coordination that has acquired excessive theater; both maintain the constraint because the alternative (treaty collapse) is worse for both. Mandatrophy would resolve only if both sides agreed the founding problem is dead (global security no longer threatened by proliferation, NWS arsenals pose no risk)—a verdict neither side endorses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_aspirational_interpretation,
    'Is Article VI a binding legal obligation imposing specific disarmament benchmarks on NWS, or an aspirational commitment permitting indefinite implementation timelines subject to strategic necessity?',
    'International Court of Justice advisory opinion on Article VI interpretation (could be requested by UN General Assembly or NPT Review Conference). Systematic analysis of preparatory works (travaux préparatoires), state practice during treaty drafting, and subsequent consistent interpretation by the treaty parties.',
    'Binding interpretation: NWS would face legal exposure for non-compliance; NNWS would have standing to claim breach; mandatrophy analysis would focus on whether NWS comply with judicially-determined benchmarks. Aspirational interpretation: NWS would retain discretion over implementation pace; NNWS pressure would be political, not legal; the constraint would reframe as soft coordination rather than enforceable obligation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binding_vs_aspirational_interpretation, conceptual, 'The interpretive divide over Article VI''s legal force—the core disagreement between NNWS and NWS readings.').

omega_variable(
    conditionality_of_non_proliferation,
    'Is NNWS non-proliferation compliance conditional on demonstrated NWS disarmament progress, or is NNWS non-proliferation an unconditional obligation independent of NWS behavior?',
    'State practice: observe whether NNWS formally withdraw from the treaty or accelerate TPNW accession when NWS fail to meet disarmament benchmarks; or whether NNWS continue non-proliferation compliance despite NWS intransigence. Survey statements from NNWS governments on whether they view their non-proliferation restraint as permanent or contingent.',
    'If conditional: the NNWS reading is vindicated; the constraint is a binding bargain; NWS non-compliance creates grounds for NNWS defection. If unconditional: NNWS reading is weakened; the constraint becomes a one-way non-proliferation obligation on NNWS; NWS have effectively secured non-proliferation at no binding cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_of_non_proliferation, empirical, 'Whether NNWS treat their non-proliferation restraint as contingent leverage or absolute commitment.').

omega_variable(
    review_conference_interpretive_authority,
    'Does the NPT Review Conference system have the authority to establish binding interpretations of Article VI that NWS must follow, or is the conference a deliberative body whose outputs are recommendations only?',
    'Legal analysis of the treaty''s text and institutional practice: does the convention on treaty interpretation (Vienna Convention on Law of Treaties) grant treaty bodies authority to issue binding interpretations? Have Review Conferences historically issued outcomes that NWS accepted as binding? What happens when a Review Conference outcome contradicts NWS stated positions—do NWS comply or ignore the outcome?',
    'Binding authority: Review Conference consensus on Article VI benchmarks would be enforceable; NWS non-compliance would constitute treaty breach. Recommendatory authority: Review Conference outcomes would be political pressure only; NWS could ignore them without legal consequence. This omega determines whether the constraint''s enforcement mechanism (Review Conference pressure + TPNW threat) is legally grounded or purely political.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_conference_interpretive_authority, conceptual, 'The institutional authority of the Review Conference system to bind NWS to specific disarmament interpretations.').

omega_variable(
    tpnw_regime_viability_as_exit,
    'Is the TPNW regime a credible alternative that NNWS can defect to, or does defection carry sufficient institutional and geopolitical costs that it remains a threat rather than an actual exit?',
    'Empirical observation of TPNW accession rates relative to NNWS satisfaction with NPT Review Conference outcomes; analysis of economic and security costs that NNWS incur by leaving the NPT for TPNW (loss of IAEA technical support, security guarantees from NWS, etc.); state statements about conditions under which they would formally withdraw from NPT.',
    'If TPNW is credible exit: NNWS threat to defect is real leverage; NWS will invest in disarmament theater to prevent TPNW acceleration. If TPNW is not credible: NNWS are locked into NPT compliance regardless of NWS behavior; the constraint''s enforcement mechanism collapses and extractiveness from NWS drops sharply (they can ignore NNWS pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_regime_viability_as_exit, empirical, 'Whether TPNW accession is a genuine threat or a toothless protest option.').

omega_variable(
    asymmetry_legitimacy_under_geopolitical_change,
    'As great-power competition (Russia, China) intensifies and the strategic rationale for NWS deterrence arsenals shifts, does the NPT''s permanent asymmetry remain justified or does it become increasingly illegitimate in the eyes of rising powers and non-aligned states?',
    'Long-term observation of NNWS attitudes toward NPT legitimacy as geopolitical conditions shift. Track voting patterns in Review Conferences, statements at UN forums, and defection trends (TPNW accessions, withdrawal threats). Monitor whether revisionist powers (China, Russia) move toward challenging the treaty or defending the status quo.',
    'If asymmetry remains legitimate: the constraint persists as rope (both sides cooperate on non-proliferation despite disagreement on disarmament). If asymmetry becomes delegitimized: the constraint could collapse (NNWS mass TPNW defection, emerging powers pursue weaponization); mandatrophy would accelerate, theater would be exposed, and the regime would fracture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetry_legitimacy_under_geopolitical_change, empirical, 'Whether the NPT''s permanent five-power asymmetry retains normative legitimacy as geopolitics evolve.').

omega_variable(
    kernel_reading_decomposition,
    'Are the NNWS, NWS, and withdrawal-threshold readings three genuinely separate constraints with different epsilon values, or are they three perspectives on a single constraint whose epsilon is stable and independent of reading?',
    'Structural test: measure disarmament obligation, non-proliferation compliance requirement, and withdrawal conditions under each reading independently. If the measured epsilon differs significantly across readings (one reading gives ε=0.22 binding obligation, another gives ε=0.50 aspirational obligation), the readings instantiate different constraints. If epsilon is stable (all readings agree on the factual extraction structure, disagreeing only on its interpretation), they are perspectives on one constraint.',
    'If separate constraints: each reading is a valid story with its own metrics, stakeholders, and classification; the kernel is a nexus of related but distinct constraint structures. If perspectives on one constraint: epsilon is reading-independent; the debate is normative (how to interpret/enforce), not structural (what extraction occurs). This omega determines whether the committer-frame rules (separate stories for separate readings) apply at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the NPT readings are separate constraints (epsilon-invariant per ε-invariance principle) or perspectives on one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nnws_reading, theater_ratio, 2020, 0.49).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nnws_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nnws_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nnws_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, treaty_on_prohibition_of_nuclear_weapons).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, comprehensive_test_ban_treaty_enforcement).

% DUAL FORMULATION NOTE:
% The NPT treaty text is a contested kernel with multiple readings. The NNWS reading treats disarmament as binding obligation backed by Review Conference pressure and TPNW threat. The NWS reading treats disarmament as aspiration and non-proliferation as unconditional restraint. The withdrawal-threshold reading disputes whether Article X exit is high-threshold (regime-stability priority) or low-threshold (sovereignty-preservation priority). These readings are not alternate measurements of one constraint—they instantiate different structural relationships between disarmament obligation and non-proliferation compliance. Each reading produces a different epsilon, different stakeholder extraction, different enforcement mechanism. They are linked through the kernel and through state practice: NWS favor the NWS reading; NNWS favor the NNWS reading; both avoid explicit litigation of the reading difference by maintaining institutional ambiguity. The constraint stories share a single text (Article VI) but decompose into distinct structures per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, institutional, 0.72).
constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
