% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain: Article IV–VI Reciprocity Constraint
 *   domain: international law/nuclear governance
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty pairs two promises: Article IV gives
 *   non-weapon states access to peaceful nuclear technology; Article VI
 *   commits the weapon states to negotiate toward disarmament. This story
 *   instantiates the grand_bargain reading of that pairing: the two articles
 *   are reciprocal obligations, non-weapon-state restraint is conditional on
 *   weapon-state disarmament progress, and persistent Article VI breach
 *   corrodes the legitimacy of the restraint demand. The standing arrangement
 *   under assessment is the operated bargain as it stands at interval end:
 *   verified restraint and full-scope safeguards on the non-weapon-state
 *   side, minimal and contested disarmament progress on the weapon-state
 *   side, and a review-cycle process that records the gap without closing it.
 *   The claim/metrics split is deliberate: the claimed type (tangled_rope — a
 *   genuine bargain structure with asymmetric, enforced extraction) is
 *   authored from what I believe structurally true of this reading's
 *   constraint; the metrics are authored from what I believe descriptively
 *   true of its actual operation. The engine computes per-seat
 *   classifications from the structural data; divergence between claim and
 *   computed type is signal, not error.
 *
 * KEY AGENTS:
 *   - weapon_states_p5: agenda-setting beneficiaries (institutional/arbitrage) — hold the frozen arsenals, police the regime, decide what counts as progress
 *   - nonaligned_nonweapon_states: primary payers (organized/constrained) — bear restraint and verification costs, judge reciprocity absent
 *   - extended_deterrence_allies: beneficiaries with payer costs (powerful/identity_locked) — receive the umbrella, locked into non-nuclear alliance identity
 *   - tpnw_coalition_states: payers who built an exit (organized/mobile) — constructed the TPNW as a parallel venue
 *   - iaea_secretariat and nuclear_export_control_regimes: administrative agenda-setters (institutional/constrained) — enforce the non-weapon-state side, gate the technology side
 *   - never_party_nuclear_states: excluded (powerful/arbitrage) — the standing counterexample outside the bargain
 *   - disarmament_diplomacy_analysts: analytical observer — tracks the compliance record both sides invoke
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.58).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain: Article IV–VI Reciprocity Constraint").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international law/nuclear governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '9201020c-fe87-4ab6-b755-5d348d7a4c6f').
narrative_ontology:cs_kernel_codification('9201020c-fe87-4ab6-b755-5d348d7a4c6f', fixed_text).
narrative_ontology:cs_authority_grounding('9201020c-fe87-4ab6-b755-5d348d7a4c6f', lineage).
narrative_ontology:cs_interpretation_layer_present('9201020c-fe87-4ab6-b755-5d348d7a4c6f').
narrative_ontology:cs_reading_relation('9201020c-fe87-4ab6-b755-5d348d7a4c6f', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('9201020c-fe87-4ab6-b755-5d348d7a4c6f', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('9201020c-fe87-4ab6-b755-5d348d7a4c6f', foundational, restraint_requires_reciprocal_disarmament_progress).
narrative_ontology:cs_axiom_status(restraint_requires_reciprocal_disarmament_progress, holdable).
narrative_ontology:cs_axiom_grounding('9201020c-fe87-4ab6-b755-5d348d7a4c6f', restraint_requires_reciprocal_disarmament_progress, deontological).
narrative_ontology:cs_axiom('9201020c-fe87-4ab6-b755-5d348d7a4c6f', foundational, article_vi_breach_undermines_article_iv_legitimacy).
narrative_ontology:cs_axiom_status(article_vi_breach_undermines_article_iv_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9201020c-fe87-4ab6-b755-5d348d7a4c6f', article_vi_breach_undermines_article_iv_legitimacy, conventional).
narrative_ontology:cs_reference_frame('9201020c-fe87-4ab6-b755-5d348d7a4c6f', reciprocal_bargain_framing).
narrative_ontology:cs_drift_state('9201020c-fe87-4ab6-b755-5d348d7a4c6f', post_tpnw_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9201020c-fe87-4ab6-b755-5d348d7a4c6f', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states_p5).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, extended_deterrence_allies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nonweapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, developing_peaceful_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, extended_deterrence_allies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, tpnw_coalition_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, reciprocal_bargain_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, interdependent_obligations_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the arsenals the treaty freezes and sit on the Security Council that polices the regime. They negotiate the review-cycle outcome documents, decide what counts as disarmament progress, and modernize their forces while reporting on steps taken. No body verifies their Article VI conduct with anything like the rigor applied to non-weapon-state facilities; leaving the arrangement would cost them the legitimacy the treaty lends their status, so they reinterpret rather than exit.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, weapon_states_p5, beneficiary).

% Administers the verification system: inspects non-weapon-state facilities, reports noncompliance to the Security Council, and runs the Additional Protocol regime. Its budget and mandate are set by member states; it verifies the restraint side of the bargain in detail while holding no standing mandate over arsenal policy on the other side. Its institutional survival depends on the arrangement continuing.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Supplier states coordinate licensing of nuclear and dual-use exports, setting the practical terms on which Article IV access is granted. They tightened conditions after the India waiver debate and the A.Q. Khan revelations; their guidelines operate outside the treaty text but gate the technology promise non-weapon states were given.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_export_control_regimes, agenda_setter,
    institutional, generational, constrained, global).

% The majority bloc of treaty parties: they forgo weapons, fund and host safeguards, and accept full-scope verification, and in exchange receive peaceful-use cooperation that export controls hedge and disarmament progress they judge absent. Their annual statements and review-conference interventions demand reciprocity; withdrawal is available on paper, but the DPRK precedent shows the price.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nonweapon_states, payer,
    organized, generational, constrained, global).

% Non-weapon states inside the security alliances of the weapon states: Japan, South Korea, and NATO members. They receive the security umbrella without fielding their own arsenals, accept the non-nuclear identity the alliance is built on, and host or underwrite the forward posture. Their latency (Japan's reprocessing capacity, European delivery modernization) keeps a theoretical exit visible, but alliance identity and extended-deterrence dependence make exercising it unthinkable in practice.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, extended_deterrence_allies, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, extended_deterrence_allies, payer).

% The states that built the Treaty on the Prohibition of Nuclear Weapons: Austria, Ireland, Mexico, South Africa, New Zealand and others. They pay the same restraint and verification costs as other non-weapon states, judge the reciprocity condition broken, and answered by constructing an alternative legal venue rather than by leaving the treaty. Their mobility comes from the TPNW itself: a parallel instrument they can accede to without exiting the NPT.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, tpnw_coalition_states, payer,
    organized, generational, mobile, global).

% States pursuing reactors, fuel, and medical isotope programs under full-scope safeguards: they carry the inspection burden and finance their own verification while their technology access is conditioned by supplier-state guidelines the weapon states dominate. The Article IV expectation is the part of the bargain they can measure directly, and it is the part most often hedged.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, developing_peaceful_nuclear_states, payer,
    moderate, biographical, constrained, regional).

% India, Pakistan, and Israel never signed the treaty and hold arsenals outside its verification and legitimacy structure. They have no seat where the bargain's terms are reviewed; their existence is the standing counterexample non-weapon states cite when arguing the arrangement entrenches a hierarchy rather than ending one.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, never_party_nuclear_states, excluded,
    powerful, generational, arbitrage, regional).

% Verification specialists, treaty lawyers, and think-tank analysts who track arsenal counts, safeguard coverage, and review-cycle outcomes. They publish the compliance record both sides invoke, and their assessments of what counts as progress under Article VI shape the legitimacy argument without themselves collecting from the arrangement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, disarmament_diplomacy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, weapon_states_p5).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Freezes the number of nuclear-armed states by giving non-weapon states a verified security-and-technology settlement: common verification standards, peaceful-use access, and a mutual restraint commitment that prevents regional arms-race cascades among the treaty's majority.
% TRANSFER_FUNCTION: Moves restraint (permanently forgone weapons options), verification costs, and safeguard burdens from non-weapon states to the regime; moves strategic stability and the legitimacy of retained arsenals to the weapon states and their allies; moves peaceful nuclear technology toward non-weapon states in the amounts supplier-state guidelines permit.
% ABSENT_VOICES: The never-party states (India, Pakistan, Israel) live outside the bargain entirely and have no seat where its terms are set; the withdrawn DPRK is present only as a cautionary tale; and the publics of the weapon states, who have never consented to indefinite arsenal retention, are represented only by the governments that retain them. Within the review cycle the TPNW majority speaks but is outmaneuvered by consensus procedure.
% DISAPPEARANCE_RATIONALE: Safeguards architecture, export-control regimes, extended-deterrence commitments, and the review-cycle diplomacy all presuppose the bargain's structure. Overnight disappearance would reopen the weapons option for latent states, unwind verification arrangements built on treaty authority, and force the alliance systems to renegotiate their nuclear foundations.
% FOUNDING_PROBLEM: The 1960s proliferation forecast: intelligence assessments projected dozens of nuclear-armed states within decades. The bargain was built to freeze the club at five by making restraint worth more than armament — verified peaceful access plus a disarmament commitment in exchange for forgone weapons.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: Non-Aligned Movement summit documents and the 2013–2014 Humanitarian Initiative conference series (159 states) attest both that proliferation risk remains and that disarmament reciprocity is unmet; SIPRI arsenal data and IAEA safeguards reports document weapon-state modernization and non-weapon-state verification burdens independently. No source outside the weapon states attests that the reciprocity condition has been satisfied.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.65 because the payer side performs fully — restraint, safeguards, financing — while the receiving side's Article VI performance is minimal and contested: arsenals persist, all five weapon states are modernizing, and the test-ban treaty has not entered force decades after adoption. Suppression is authored at 0.58 as a raw structural property, unscaled by power or scope: the enforcement machinery (Additional Protocol, Security Council sanctions, supplier-state denial) is real and hardened over the interval, but it aims almost entirely at the restraint side of the bargain. Theater rises to 0.52 because review-cycle output — consensus action plans, unequivocal undertakings, sixty-four-step roadmaps — increasingly substitutes for performance. Accessibility collapse is moderate (0.45): withdrawal (the DPRK precedent), TPNW accession, and latency hedging remain visible alternatives, each priced. Resistance is substantial (0.62) and rising, expressed through the Humanitarian Initiative and the TPNW rather than through treaty exit. The measurement series run on one shared grid (treaty years 0/11/22/33/44/55): the 1992 dip in extractiveness records the real early-1990s reductions, and the post-1995 rise records indefinite extension without enforcement teeth. Suppression_requirement is tracked because enforcement capacity is this story's dynamic: it matured and hardened (0.35 to 0.60) on one side of the bargain only — the asymmetry is the point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the weapon-state seat the arrangement is a stability architecture it built and polices: restraint is delivered, verification is rigorous, Article VI is a direction, not a deadline. From the non-aligned payer seat the same structure operates as an unreciprocated demand: full-scope verification for a technology promise hedged by supplier guidelines, in exchange for disarmament progress that arsenal data contradict. The TPNW seat computes a third position: the bargain's legitimacy failed so long ago that the remedy lies outside the treaty. Identity-lock dynamics matter at the alliance seat: the extended-deterrence allies' non-nuclear status is not a policy but a constituted identity — the alliance is built on it, and the latency that would make exit feasible is politically unthinkable to exercise. Break that frame (a credibility collapse in the umbrella, for instance) and the alliance seat's position shifts abruptly toward the payer side, which is why its exit atom is identity_locked rather than constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as weapon_states_p5 and extended_deterrence_allies: the first collects restraint and legitimacy directly; the second collects security without weapons cost, which is why formally non-weapon alliance states sit at the beneficiary end despite paying real costs (their secondary payer role records the hosting and underwriting burden). Victims are nonaligned_nonweapon_states and developing_peaceful_nuclear_states — the parties who pay the full restraint and verification price while the reciprocal good is withheld. No directionality overrides are authored: the derivation from role declarations plus exit options is the finest instrument available here, because overrides bind at power-atom granularity and this story has three institutional seats (the P5, the IAEA secretariat, the supplier regimes) whose directionalities legitimately differ; a power-atom-level override would misfire across all of them. The IAEA and export regimes carry no beneficiary declarations, so they derive toward symmetric instrument positions rather than collector positions — which matches their situations: they administer the arrangement without capturing its gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 1960s proliferation-cascade forecast — is live, so this is not a mandatrophy case, and the classification guards both adjacent mislabels. Reading the arrangement as a pure extraction mechanism would erase the real coordination goods: the cascade the treaty forestalled is measurable in the small number of armed states, and peaceful-use cooperation is real. Reading it as pure coordination would erase the asymmetry: the reciprocity condition is unmet, enforcement is one-sided, and the payer bloc's resistance is organized and growing. Tangled rope holds both facts: genuine coordination function, active enforcement, identifiable beneficiaries and identifiable victims. The R5 interview corroborates: founding problem live, corroborated from outside the beneficiary set, and disappearance would rearrange the world — the arrangement is load-bearing and unexpired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the grand_bargain reading of kernel npt_article_iv_vi_pairing; the sibling readings instantiate different constraints over the same referent. Which reading does regime practice converge on, and what would convergence change structurally?',
    'Track review-conference outcome documents, TPNW accession trajectory, and weapon-state posture statements: convergence toward nonproliferation_primary appears as verification-tightening without disarmament benchmarks; toward abolitionist as TPNW universality pressure; toward this reading as binding reciprocity benchmarks adopted in RevCon consensus.',
    'Under nonproliferation_primary, Article VI non-performance extracts nothing and effective extractiveness collapses toward coordination cost; under abolitionist, the victim set expands to all non-weapon states exposed to use risk and extractiveness rises; under this reading, extraction equals unmet reciprocity as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-reading underdetermination: three readings of the Article IV–VI pairing author different epsilon over the same arrangement.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI''s obligation (good-faith negotiations on cessation and disarmament) capable of a breach finding that any body could actually adjudicate, as this reading''s enforcement logic requires?',
    'An ICJ advisory opinion or a RevCon-adopted compliance benchmark set (test-ban entry into force, FMCT commencement, verified arsenal ceilings) would convert the standard from rhetorical to adjudicable.',
    'If no adjudicable standard emerges, this reading''s breach claim is unenforceable and the constraint drifts toward the nonproliferation_primary structure; if one emerges, weapon states become citable breach actors and the withdrawal and Article IV-expansion licensing consequences follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether Article VI breach is adjudicable or inherently rhetorical.').

omega_variable(
    withdrawal_licensing_status,
    'Does demonstrated Article VI breach actually license non-weapon-state withdrawal under Article X or expansion of Article IV demands (enrichment and reprocessing rights), or is the license rhetorical with no state willing to exercise it?',
    'State practice: any non-weapon state formally invoking Article VI breach in a withdrawal notification or a supplier-state dispute over enrichment access; the Iranian enrichment dispute and future review-cycle walkouts are the natural test cases.',
    'If the license is real, the constraint''s reciprocity has enforcement teeth and the bargain reading holds; if no state ever exercises it, the reciprocity is cover and effective extractiveness is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_licensing_status, empirical, 'Whether the reading''s central licensing consequence exists in state practice.').

omega_variable(
    extended_deterrence_net_position,
    'Are the extended-deterrence allies net beneficiaries of the arrangement, or covert payers whose hosting, underwriting, and targeting exposure exceed the umbrella''s value?',
    'Alliance burden-sharing audits and host-nation support accounting set against independent estimates of umbrella value and forward-posture risk exposure.',
    'If the allies are net payers, the beneficiary structure narrows to the five weapon states, extraction concentrates, and the payer coalition''s latent power grows substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extended_deterrence_net_position, empirical, 'Net position of alliance non-weapon states under the bargain.').

omega_variable(
    indefinite_extension_bargain_drift,
    'Did the 1995 indefinite extension convert a conditional bargain into an unconditional perpetuity — decoupling non-weapon-state restraint from any enforceable reciprocity deadline — such that the interval contains two structurally different regimes?',
    'Comparative legal reading of the 1995 Decision and Principles-and-Objectives documents against subsequent practice: whether the extension was conditioned on the disarmament program it contained, and whether any body has treated non-performance as vitiating consent.',
    'If the extension decoupled the obligations, post-1995 measurements describe a different constraint than pre-1995, and the extractiveness series should be read as a step change rather than drift, raising classification severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_bargain_drift, conceptual, 'Whether the 1995 indefinite extension restructured the bargain mid-interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_gb_pairing_tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t0, observed).
narrative_ontology:measurement(npt_gb_pairing_tr_t11, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 11, 0.25).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t11, observed).
narrative_ontology:measurement(npt_gb_pairing_tr_t22, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 22, 0.3).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t22, observed).
narrative_ontology:measurement(npt_gb_pairing_tr_t33, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 33, 0.38).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t33, observed).
narrative_ontology:measurement(npt_gb_pairing_tr_t44, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 44, 0.45).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t44, observed).
narrative_ontology:measurement(npt_gb_pairing_tr_t55, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 55, 0.52).
narrative_ontology:measurement_basis(npt_gb_pairing_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_gb_pairing_be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t0, observed).
narrative_ontology:measurement(npt_gb_pairing_be_t11, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 11, 0.46).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t11, observed).
narrative_ontology:measurement(npt_gb_pairing_be_t22, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 22, 0.4).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t22, observed).
narrative_ontology:measurement(npt_gb_pairing_be_t33, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 33, 0.5).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t33, observed).
narrative_ontology:measurement(npt_gb_pairing_be_t44, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 44, 0.58).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t44, observed).
narrative_ontology:measurement(npt_gb_pairing_be_t55, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 55, 0.65).
narrative_ontology:measurement_basis(npt_gb_pairing_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_gb_pairing_su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t0, observed).
narrative_ontology:measurement(npt_gb_pairing_su_t11, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 11, 0.38).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t11, observed).
narrative_ontology:measurement(npt_gb_pairing_su_t22, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 22, 0.45).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t22, observed).
narrative_ontology:measurement(npt_gb_pairing_su_t33, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 33, 0.55).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t33, observed).
narrative_ontology:measurement(npt_gb_pairing_su_t44, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 44, 0.58).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t44, observed).
narrative_ontology:measurement(npt_gb_pairing_su_t55, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 55, 0.6).
narrative_ontology:measurement_basis(npt_gb_pairing_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, resource_allocation).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, tpnw_prohibition_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the NPT bargain' decomposes into three readings of one kernel (npt_article_iv_vi_pairing), each a separate file with its own epsilon over the same referent. nonproliferation_primary authors low epsilon (Article VI aspirational, so non-disarmament extracts nothing); abolitionist authors high epsilon (Article IV itself illegitimate where it perpetuates dual-use risk); this grand_bargain file authors intermediate epsilon (extraction equals unmet reciprocity). This reading is downstream of the nonproliferation_primary reading in practice — weapon states operate under it — and upstream of the abolitionist reading in legitimacy argument: every demonstrated Article VI shortfall feeds the TPNW coalition's case. All three files link each other through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
