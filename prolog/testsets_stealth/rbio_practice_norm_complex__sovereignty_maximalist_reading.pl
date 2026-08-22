% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Absolute Sovereignty Shield (Sovereignty-Maximalist Reading of the RBIO Norm Complex)
 *   domain: international relations/international law/political economy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_maximalist_reading of the
 *   rbio_practice_norm_complex kernel as a clean, epsilon-invariant
 *   constraint: the operative arrangement under which state sovereignty is
 *   treated as absolute, RBIO norms are legitimate only insofar as they
 *   protect sovereignty against external interference, and humanitarian
 *   exceptions are diagnosed as regime-change pretexts. The arrangement
 *   coordinates states around a shared bar on cross-border force while
 *   extracting external recourse from the people governed by shielded
 *   governments — the structural delta's victim class. Per the
 *   epsilon-referent rule, the metrics below are authored over this standing
 *   arrangement itself, not over the liberal or rights-protective order the
 *   sibling readings endorse or the reading's own self-description. The
 *   claim/metric independence rule is honored: claimed_type is tangled_rope
 *   because the structure genuinely possesses both a coordination function
 *   (real, historically consequential, supplied with majorities by small
 *   states) and asymmetric extraction (borne by trapped populations,
 *   collected by shielded governments), sustained by active enforcement (veto
 *   threats, bloc discipline, counter-mobilization against interveners). The
 *   metrics are authored descriptively; the engine computes per-seat
 *   classifications and any divergence from the claim is the datum.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: primary beneficiary and active enforcer (institutional/constrained) — collects the accountability shield, supplies the veto and bloc-discipline labor that maintains it
 *   - small_state_governments: secondary beneficiary (organized/constrained) — collects protection against great-power predation they could not purchase militarily
 *   - intervention_capable_democracies: dual-positioned payer/beneficiary (institutional/mobile) — absorbs restraint costs, draws reciprocal shelter, retains partial unilateral exit
 *   - populations_under_repressive_rule: primary victim (powerless/trapped) — bears total foreclosure of external recourse
 *   - atrocities_facing_minorities: acute victim (powerless/trapped) — protection depends on action the reading defines as illegitimate
 *   - exiled_dissident_networks: excluded voice (moderate/mobile) — physically outside, politically voiceless in the fora that renew the shield
 *   - international_human_rights_machinery: observer (institutional/analytical) — documents what it cannot compel
 *   - r2p_advocacy_coalition: observer (organized/analytical) — won rhetorical adoption in 2005, lost operational content since
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Absolute Sovereignty Shield (Sovereignty-Maximalist Reading of the RBIO Norm Complex)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international relations/international law/political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e965433e-1208-4e6e-9e6c-0561936ae6c4').
narrative_ontology:cs_kernel_codification('e965433e-1208-4e6e-9e6c-0561936ae6c4', fixed_text).
narrative_ontology:cs_authority_grounding('e965433e-1208-4e6e-9e6c-0561936ae6c4', lineage).
narrative_ontology:cs_interpretation_layer_present('e965433e-1208-4e6e-9e6c-0561936ae6c4').
narrative_ontology:cs_reading_relation('e965433e-1208-4e6e-9e6c-0561936ae6c4', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('e965433e-1208-4e6e-9e6c-0561936ae6c4', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('e965433e-1208-4e6e-9e6c-0561936ae6c4', foundational, external_intervention_limited_to_self_defense).
narrative_ontology:cs_axiom_status(external_intervention_limited_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('e965433e-1208-4e6e-9e6c-0561936ae6c4', external_intervention_limited_to_self_defense, conventional).
narrative_ontology:cs_axiom('e965433e-1208-4e6e-9e6c-0561936ae6c4', secondary, humanitarian_exception_is_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_exception_is_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('e965433e-1208-4e6e-9e6c-0561936ae6c4', humanitarian_exception_is_regime_change_pretext, empirically_contingent).
narrative_ontology:cs_reference_frame('e965433e-1208-4e6e-9e6c-0561936ae6c4', westphalian_absolute_sovereignty_order).
narrative_ontology:cs_drift_state('e965433e-1208-4e6e-9e6c-0561936ae6c4', contemporary_post_r2p_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e965433e-1208-4e6e-9e6c-0561936ae6c4', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_state_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocities_facing_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, intervention_capable_democracies).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, intervention_capable_democracies).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_non_intervention_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_charter_article_2_7_domestic_jurisdiction).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_consent_basis_of_international_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the absolute-sovereignty reading to defeat scrutiny resolutions, sanction proposals, and referral attempts aimed at their domestic conduct. Enforce the reading through Security Council veto threats, General Assembly bloc discipline, and mutual-shielding agreements with similarly positioned governments. What flows to them is immunity from external accountability for internal repression; what they contribute is the voting and diplomatic labor that keeps the reading operative. Leaving the arrangement would mean accepting the intervention and conditionality precedents they exist to block.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, agenda_setter).

% Depend on the non-interference guarantee as their principal protection against larger neighbors and former colonial powers. Supply consistent General Assembly majorities for sovereignty-affirming resolutions. They receive protection they could not purchase militarily; they also extend that same protection to governments whose conduct they privately deplore, because weakening the guarantee anywhere threatens it everywhere. Exit would mean relying on great-power goodwill or collective-security mechanisms they judge unreliable.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_state_governments, beneficiary,
    organized, generational, constrained, global).

% Hold military and economic capacity to act beyond their borders but accept a standing bar on forceful humanitarian action without Security Council authorization. They absorb the restraint cost — foregone rescue operations, sanctions-only responses to atrocities — while retaining unilateral capacity they occasionally exercise, as in 1999, at the price of legal controversy. They also draw reciprocal protection: the same bar shields their domestic arrangements from external challenge. Their exit is partial: they can act alone, but each unilateral act spends legitimacy capital and invites reciprocity.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, intervention_capable_democracies, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, intervention_capable_democracies, beneficiary).

% Live under governments the reading insulates from external pressure. When their government jails opponents, shells cities, or engineers famine, the available remedies are domestic institutions controlled by the government itself, or nothing. External channels — referral, coercive inspection, protective intervention — are exactly what the reading forecloses. Physical emigration is the main exit, and it is rationed by wealth, visas, and border enforcement.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule, payer,
    powerless, biographical, trapped, global).

% Sit at the sharp edge of the reading: groups facing mass killing or expulsion whose protection depends on external action the reading defines as illegitimate. Their fate is decided largely by whether their government's patrons hold veto power. Exit is flight to refugee camps or across borders under fire.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocities_facing_minorities, payer,
    powerless, immediate, trapped, regional).

% Have physically left the jurisdictions the reading shields but remain its political outsiders: state-to-state fora treat them as partisan claimants, and the reading's logic brands their testimony as foreign-backed subversion. They lobby capitals, brief journalists, and testify to human rights bodies, but hold no vote in the diplomatic processes that renew the shield.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, exiled_dissident_networks, excluded,
    moderate, biographical, mobile, global).

% Treaty bodies, special procedures, and commissions of inquiry document violations and issue findings that name and shame but cannot compel. Their reach stops at consent-based limits the reading polices; referrals to political bodies die in veto. They see the full structure from inside the system whose enforcement teeth the reading removes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_human_rights_machinery, observer,
    institutional, generational, analytical, global).

% States, NGOs, and scholars campaigning since 2001 to redefine sovereignty as responsibility. Won rhetorical adoption in 2005; lost operational content afterward as veto holders narrowed application case by case. They remain inside the conversation — the 2005 outcome proves entry is possible — but their program is the thing this reading exists to prevent.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, r2p_advocacy_coalition, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interstate conduct around a shared bar on cross-border force and coercion directed at domestic arrangements: invasion, punitive intervention, and regime-change operations are removed from every state's option set simultaneously. This lowers the security dilemma between states with incompatible domestic systems and lets diplomatically isolated governments participate in ordinary interstate relations.
% TRANSFER_FUNCTION: Moves decision-rights over domestic governance wholly to incumbent governments, and moves impunity along with them; correspondingly it removes external recourse — protection, adjudication, rescue — from the people those incumbents govern. Restraint costs fall on intervention-capable states; the safety dividend is collected by all governments, weighted toward those with the most to hide.
% ABSENT_VOICES: Subject populations, opposition movements, and atrocity-facing minorities are never party to the bargain: it is struck and maintained among governments. Exiled dissidents attend as petitioners without votes. Even within beneficiary states, civil society opposed to their rulers has no seat at the diplomatic table where the shield is renewed.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen intervention decisions everywhere: protection gaps for atrocity-facing groups would be contested anew, intervention-capable states would face immediate pressure to act or explain inaction, shielded governments would scramble for substitute deterrents (patron alliances, deterrence-by-punishment, covert weapons programs), and the General Assembly's sovereignty bloc would fracture as protection-seekers and scrutiny-targets split. Interstate relations would reorganize around whatever replacement threshold emerged.
% FOUNDING_PROBLEM: The wars of religion, colonial 'civilizing missions,' and twentieth-century ideological interventions in which powerful states invaded weaker ones under universal mandates. The Westphalian inheritance and the post-1945 settlement built the non-interference bar to take regime-change warfare off the table, above all for newly decolonized states.
% FOUNDING_PROBLEM_CORROBORATION: Small-state diplomatic services and the non-aligned tradition attest the intervention danger from outside the authoritarian-beneficiary set; international legal historians corroborate the founding record (Charter travaux preparatoires, General Assembly Resolutions 2131 and 2625). No corroborating voice outside the beneficiary set attests that the arrangement's present-day cost allocation is acceptable: human rights bodies and survivor communities document the foreclosure of recourse and dispute the reading's account of it — their attestation covers the founding problem, not its current administration.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state scalars are read off the terminal points of one shared time grid (decades 0-60; every tracked metric is authored at every examined point, so no metric borrows another's timeline). Extractiveness 0.72 reflects near-total foreclosure of external recourse for the delta's victim class, net of the real protection the same bar affords potential targets of great-power predation. Suppression 0.62 is structural and institutional — veto threats, bloc discipline, counter-mobilization against interveners — not physical coercion, and it is authored as a raw property unscaled by scope; only extractiveness rides the directionality and scope modifiers in the engine's computation. Theater_ratio 0.45 reflects the growing share of sovereignty invocation that is selective and retaliatory (outrage at rivals' interventions, silence at allies') rather than principled maintenance of the bar. Accessibility_collapse 0.55: alternatives (R2P, humanitarian-corridor doctrines, consent-based coercive tools) survive formally but collapse in practice once veto politics are understood. Resistance 0.60: the R2P coalition, human rights machinery, and dissident networks contest continuously and won the 2005 rhetorical adoption — evidence the bar is defended, not self-executing. The series is two-phase rather than cyclically oscillating: a thinning of the shield through the humanitarian-intervention decade (trough at t30) followed by post-Libya consolidation with interest; no intermittent-reinforcement dynamic is claimed, so no extended cycle grid is authored. Coalition potential among the trapped is real on paper — diaspora networks, transnational advocacy — but the arrangement's own logic strips their testimony of standing inside the fora that matter, which is why their effective power stays at the floor despite their numbers.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the authoritarian-regime seat the arrangement is existential protection and the reading is simply correct law; from the trapped-population seat it is the closure of every external door, experienced not as law but as abandonment; from the small-state seat it is the equal-sovereignty guarantee that substitutes for armies; from the intervention-capable-democracies seat it is a binding restraint they half-honor (1999) and half-fund rhetorically while drawing reciprocal shelter from it. Inter-institutionally, the Security Council experiences the reading as veto leverage, the General Assembly as a majority weapon, the human rights machinery as a ceiling on mandate, and the International Court as a jurisdictional limit. Among nominally equal actors — all formally sovereign — exit differs sharply: two states of equal formal standing diverge according to whether they hold a veto, host a patron, or possess intervention capacity, and the structural data encode those differences so per-seat classification can legitimately diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality seats: authoritarian_regimes collect the shield directly (near the beneficiary pole) and deepen their subsidy by supplying enforcement labor as agenda-setters; small_state_governments collect protection without administering anything. Victim declarations drive the high-directionality seats: populations_under_repressive_rule and atrocities_facing_minorities bear the entire recourse-foreclosure with trapped exit, sitting nearest the full-target pole. intervention_capable_democracies are genuinely dual-positioned — payer on the restraint-cost ledger, beneficiary on the reciprocal-protection ledger — and the secondary-role declaration lets the derivation place them near symmetric rather than at the target pole. Observers carry analytical exit and no structural stake in the flow. No directionality_overrides are authored: the beneficiary/victim/exit data already locate every seat correctly, and the one ambiguous seat is handled by the dual-role declaration rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as pure coordination (its self-description) would erase the victims the structural delta names; classifying it as pure extraction would erase the genuine small-state protection function that supplies its General Assembly majorities and its real historical achievement — taking regime-change warfare off the table among great powers. The tangled-rope classification keeps both faces legible and directs attention to the ratio between them, which the measurement series tracks: the t30 trough marks the humanitarian-intervention decade when the shield thinned, and the post-t40 climb marks its restoration. The founding problem remains live — great-power intervention appetite did not expire — so no mandatrophy_resolved flag is authored. The open question is whether the arrangement's persistence now serves the founding problem or chiefly its beneficiaries; that question is carried by the protection_extraction_net_effect omega rather than resolved by fiat, and the rising theater_ratio alongside rising extractiveness is the drift signature a future dead-mandate finding would ride on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'This constraint is one reading (sovereignty_maximalist_reading) of the rbio_practice_norm_complex kernel. Does this reading''s instantiation — absolute sovereignty, no intervention authority beyond self-defense, humanitarian exceptions as pretext — capture the operative norm complex, or do the sibling readings (liberal_institutional_reading, hegemonic_extraction_reading) better describe actual practice?',
    'Comparative episode coding: classify the same enforcement episodes (Kosovo 1999, Iraq 2003, Libya 2011, Syria 2011-present, Ukraine-era measures) under each reading''s predictions and test which reading''s victim/beneficiary structure matches observed outcomes.',
    'If a sibling reading better fits practice, this constraint''s epsilon misattributes extraction — the liberal reading would relocate victims to states denied capacity-building, the hegemonic reading to Global South policy autonomy — and the classification computed from this file would describe a constraint that is not the operative one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'Committer-frame uncertainty: which reading of the RBIO kernel the operative practice instantiates.').

omega_variable(
    protection_extraction_net_effect,
    'Is the non-interference bar''s net effect on vulnerable populations protective (against external predation and great-power intervention) or extractive (a shield for domestic predation), and in what proportion?',
    'Population-outcome comparisons across matched cases: high-norm-adherence versus intervention episodes, controlling for regime type and prior violence levels; survey and mortality data from populations under shielded versus exposed governments.',
    'Determines how much of the measured extraction is intrinsic to the norm versus attributable to the regimes it shields. A strongly protective net effect would pull the classification toward the coordination pole; a negligible or negative net effect would pull it toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_extraction_net_effect, empirical, 'Whether the arrangement''s coordination dividend reaches the people it costs.').

omega_variable(
    humanitarian_exception_pretext_status,
    'Is the reading''s diagnostic claim — that humanitarian exceptions operate as pretexts for regime change — empirically warranted, or a motivated generalization from selected cases?',
    'Systematic comparison of stated mandates against executed operations and post-operation governance outcomes across the full intervention record (ECOWAS in the 1990s, Kosovo 1999, Iraq 2003, Libya 2011), coded blind to the reading''s commitments.',
    'If the pretext claim fails systematic testing, the reading loses its evidentiary core: the foreclosure of humanitarian channels would stand revealed as pure shield-maintenance rather than prudent guard against abuse, shifting the constraint''s profile decisively toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_exception_pretext_status, empirical, 'Empirical status of the reading''s foundational diagnostic claim about humanitarian exceptions.').

omega_variable(
    costless_conditionality_existence,
    'The reading accepts external conditionality only when the target state can exit without cost. Given the structural coercion inherent in asymmetrical offers to weak states, does a genuinely costless exit ever exist — or is the reading''s own conditionality standard vacuously empty?',
    'Game-theoretic and empirical analysis of exit-cost distributions under real conditionality offers: whether any accepted offer left the target''s reservation welfare unchanged, and whether rejected offers were priced above reservation welfare by construction.',
    'If costless exit never exists, the reading permits no external leverage whatsoever and its conditionality concession is decorative — tightening the classification toward the shield-pole. If costless exits are real, the reading contains a live channel for legitimate pressure and the constraint carries more coordination content than the end-state metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(costless_conditionality_existence, conceptual, 'Whether the reading''s own standard for acceptable conditionality ever bites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(rbio_tr_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(rbio_tr_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(rbio_be_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(rbio_be_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(rbio_su_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(rbio_su_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'rules-based international order' decomposes into three structurally distinct constraints corresponding to the three declared readings of the rbio_practice_norm_complex kernel. This file is the sovereignty_maximalist_reading member. Its epsilon (0.72) is authored over the absolute-sovereignty arrangement itself — victims: populations denied external recourse; beneficiaries: shielded governments. The liberal_institutional_reading member authors low epsilon over a consent-based, revisable order; the hegemonic_extraction_reading member authors high epsilon over a practically frozen hegemonic project with a different victim set (Global South policy autonomy). The Charter-text core is the upstream claim all three cite; enforcement-selectivity episodes are the contested evidence each reads differently. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
