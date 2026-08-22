% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading
 *   domain: international law / nuclear governance / treaty interpretation
 *
 * SUMMARY:
 *   This story instantiates the ABOLITIONIST reading of the NPT Article IV/VI
 *   pairing kernel. The standing arrangement under contest — and therefore
 *   the referent of epsilon — is the NPT regime as it actually operates:
 *   Article IV's 'inalienable right' to peaceful nuclear technology exercised
 *   by safeguarded programs, Article VI's disarmament obligation unfulfilled
 *   for fifty-six years, and the armed five's arsenals treaty-tolerated
 *   throughout. Read through the humanitarian-law lens that produced the
 *   TPNW, that arrangement is one in which a real nonproliferation
 *   coordination function carries a permanent arsenal caste, an export
 *   economy sheltered by 'peaceful use' language, and a review-cycle ritual
 *   that performs commitment while delivering nothing on the disarmament
 *   limb. Per the epsilon-invariance principle, the colloquial label 'the NPT
 *   bargain' decomposes into three structurally distinct readings (this one,
 *   grand_bargain, nonproliferation_primary); each is a separate file with
 *   its own epsilon over the SHARED referent, linked via
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   reading's own rhetoric pushes toward calling the arrangement pure
 *   extraction, but the honest structural call retains the genuine
 *   coordination function — the engine measures that tension rather than the
 *   author reconciling it.
 *
 * KEY AGENTS:
 *   - - nuclear_weapon_states: Agenda-setting seat (institutional/arbitrage) — administers the regime, retains and modernizes the arsenals, absorbs the arrangement's principal gains
 *   - - nuclear_export_industry: Secondary beneficiary (powerful/mobile) — collects commerce rents under the Article IV peaceful-use guarantee
 *   - - nuclear_alliance_states: Dual-positioned beneficiary/payer (institutional/constrained) — subsidized by extended deterrence, paying in basing, exposure, and forgone sovereignty
 *   - - non_weapon_states_parties: Primary target (organized/trapped) — permanent restraint and financed verification against an undelivered promise
 *   - - hibakusha_and_downwind_communities: Harm-bearing seat (powerless/trapped) — the humanitarian referent, seated nowhere
 *   - - tpnw_states_parties: Dissenting coalition (organized/constrained) — built the alternative and pays pressure costs for it
 *   - - nonparty_armed_states: Excluded seat (powerful/arbitrage) — shaped by enforcement they never consented to
 *   - - icj_and_ihl_jurists: Analytical observer (analytical/analytical) — adjudicates textual meaning without enforcement power
 *   - - future_generations_under_existential_risk: Non-agent harm-bearing class (listed for completeness; excluded from derivation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.76).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.64).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.76).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international law / nuclear governance / treaty interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '808bca8d-886b-4b04-a09a-8101a18100e0').
narrative_ontology:cs_kernel_codification('808bca8d-886b-4b04-a09a-8101a18100e0', fixed_text).
narrative_ontology:cs_authority_grounding('808bca8d-886b-4b04-a09a-8101a18100e0', lineage).
narrative_ontology:cs_interpretation_layer_present('808bca8d-886b-4b04-a09a-8101a18100e0').
narrative_ontology:cs_reading_relation('808bca8d-886b-4b04-a09a-8101a18100e0', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('808bca8d-886b-4b04-a09a-8101a18100e0', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('808bca8d-886b-4b04-a09a-8101a18100e0', foundational, nuclear_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('808bca8d-886b-4b04-a09a-8101a18100e0', nuclear_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('808bca8d-886b-4b04-a09a-8101a18100e0', foundational, no_peaceful_military_program_distinction).
narrative_ontology:cs_axiom_status(no_peaceful_military_program_distinction, holdable).
narrative_ontology:cs_axiom_grounding('808bca8d-886b-4b04-a09a-8101a18100e0', no_peaceful_military_program_distinction, empirically_contingent).
narrative_ontology:cs_reference_frame('808bca8d-886b-4b04-a09a-8101a18100e0', humanitarian_law_categorical_prohibition).
narrative_ontology:cs_drift_state('808bca8d-886b-4b04-a09a-8101a18100e0', contemporary_tpnw_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('808bca8d-886b-4b04-a09a-8101a18100e0', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_export_industry).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_alliance_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_weapon_states_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, hibakusha_and_downwind_communities).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, tpnw_states_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_alliance_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states recognized under the treaty as armed run the regime's decision machinery: they set review-conference agendas, hold Security Council vetoes over enforcement, and control the pace of disarmament diplomacy. They retain their arsenals indefinitely while calling on others to forgo weapons, and they continue modernizing warheads and delivery systems. Leaving the arrangement is not contemplated — they can reinterpret, delay, or reshape its terms from inside.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% State-owned and private firms sell reactors, enrichment services, and fuel under the treaty's peaceful-use guarantee. The Article IV language gives their commerce a legal footing and a legitimacy shield, and their customer base comes from safeguarded programs worldwide. They can shift between markets and jurisdictions freely.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_export_industry, beneficiary,
    powerful, biographical, mobile, global).

% States under extended nuclear deterrence — Japan, South Korea, NATO members — receive security protection without owning weapons. They host basing and command arrangements, absorb political exposure for alliance posture, and forgo an independent deterrent. Stepping out from the umbrella would mean rebuilding national security from scratch against neighbors' arsenals.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_alliance_states, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_alliance_states, payer).

% The majority of treaty members forgo weapons entirely, accept intrusive inspections of their facilities, and help finance the verification system. In exchange they were promised disarmament negotiations and access to peaceful technology; the disarmament promise has gone undelivered for over five decades while their restraint is treated as permanent. Withdrawal carries suspicion, sanctions risk, and the cautionary example of North Korea's isolation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_weapon_states_parties, payer,
    organized, generational, trapped, global).

% Survivors of Hiroshima and Nagasaki and communities exposed to weapons-testing fallout bear the humanitarian harm the legal arguments respond to. They held no seat in 1968 and hold none in the review conferences; they enter as witnesses. Radiation injury cannot be exited — there is no leaving exposure already received, and the survivor population thins each year.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, hibakusha_and_downwind_communities, payer,
    powerless, biographical, trapped, global).

% States that joined the 2017 prohibition treaty accepted a categorical ban and now face diplomatic pressure, alliance friction, and in some cases aid or trade complications from nuclear-armed patrons. They gave up whatever latency or umbrella benefits the older arrangement offered, and they hold a legal instrument the armed states refuse to engage with.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_states_parties, payer,
    organized, generational, constrained, global).

% India, Pakistan, Israel, and North Korea hold arsenals outside the treaty. The regime's export controls and sanctions target them, yet they never agreed to its terms and have no vote in its conferences. Their existence is cited both as proof the regime fails and as the reason to keep it.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nonparty_armed_states, excluded,
    powerful, generational, arbitrage, regional).

% The International Court of Justice, UN legal offices, and academic international lawyers analyze what the treaty text and humanitarian law actually require. They produce advisory opinions and scholarship that both camps cite; they hold no enforcement power and no material stake in the outcome.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, icj_and_ihl_jurists, observer,
    analytical, civilizational, analytical, universal).

% People not yet born will inherit whichever arsenals, waste streams, and accident risks survive this century. The humanitarian argument names them as the class owed precaution, but they hold no representation anywhere in the regime's institutions.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, future_generations_under_existential_risk, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__abolitionist, future_generations_under_existential_risk).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: without a common verification standard and legal firebreak, each state's arming decision would rationally track its neighbors', producing a cascade. The treaty supplies inspection machinery through IAEA safeguards, a shared legal boundary between lawful and unlawful programs, and a public forum where restraint commitments are made and monitored.
% TRANSFER_FUNCTION: Moves restraint, inspection access, and permanently forgone weapons options from the non-armed majority to the armed minority and its export sector; moves security subsidy from armed patrons to alliance clients; and on the disarmament limb moves essentially nothing — the warhead reductions that did occur came from bilateral US-Soviet/Russian agreements, not from the multilateral Article VI process.
% ABSENT_VOICES: The people who bear the humanitarian consequences — hibakusha, downwind communities, future generations — hold no seats; states speak for them. Civil society campaigns gained access only in the 2010s and remain outside decision rooms. The armed non-parties shape the regime's enforcement environment without ever having consented to or voted on its terms.
% DISAPPEARANCE_RATIONALE: If the Article IV/VI pairing vanished overnight, the safeguards system would lose its treaty foundation, export-control regimes would lose their legal anchor, alliance deterrence postures would need renegotiation, and the armed states' arsenals would shift from treaty-tolerated to being judged solely against customary law — the entire nuclear-order architecture would have to be rebuilt around whatever replaced it.
% FOUNDING_PROBLEM: The mid-1960s proliferation cascade: intelligence projections of twenty-plus nuclear states within two decades after China's 1964 test. The treaty traded the non-armed states' permanent renunciation for the armed five's promise to negotiate disarmament, plus everyone's access to peaceful nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the 1996 ICJ advisory opinion records the disarmament obligation as unfinished; the Oslo, Nayarit, and Vienna humanitarian-conference proceedings (2013-14) document consequences the regime had not reckoned with; hibakusha testimony before the TPNW negotiations attests the harm limb; UN General Assembly First Committee voting records show the majority's persistent dissatisfaction. Only the armed states themselves attest that the proliferation problem requires retaining arsenals — no external seat corroborates the arrangement's sufficiency.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.76 at interval end) because the reading's referent includes the core asymmetry: the many permanently forgo weapons and fund verification while the few retain arsenals the same text legitimizes, and the 'peaceful use' guarantee shelters dual-use capability growth. Suppression (0.64) is authored as a RAW structural property — unscaled by power or scope; only extractiveness is scaled by the engine. It reflects export-control tightening, withdrawal penalties, and the post-2017 counter-pressure campaign against TPNW joiners, tempered by the fact that much participation remains formally consensual. Theater_ratio (0.58) tracks the review-cycle ritual: consensus final documents rehearsing 'categorical formulations' while arsenals grow — performative output now rivals functional output, though safeguards verification remains genuinely functional, keeping this below piton territory. Accessibility_collapse (0.40) is deliberately LOW: the TPNW path is visible, ratified, and in force — the arrangement has NOT collapsed its alternative, which is precisely why this reading exists as a live position. Resistance (0.72) is high: a treaty coalition, a Nobel-winning campaign, bloc walkouts, and advisory-opinion litigation. The measurement series run on ONE shared eight-point grid (every tracked metric authored at every point); the suppression dip at t=24 records the real post-Cold War enforcement relaxation before re-hardening, not grid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute opposite classifications from the same treaty text. From the weapon-state seat (beneficiary, arbitrage exit, agenda control), the arrangement presents as coordination it built and administers — effective extraction near zero or inverted into subsidy. From the non-weapon-party seat (trapped, organized) and the hibakusha seat (trapped, powerless, full-target directionality), the same articles present as enforced extraction with maximal chi. Alliance states sit between: subsidized but constrained, part-beneficiary part-payer. TPNW parties experience the identical text as a rival order they have already exited politically. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: nuclear_weapon_states (d near 0.0 — the arrangement subsidizes them), nuclear_export_industry (d near 0.05 — pure commerce rent, mobile exit), nuclear_alliance_states (d roughly 0.3 — genuine security subsidy offset by real cost-bearing). Victim declarations drive high directionality: non_weapon_states_parties (d near 1.0 — trapped by withdrawal costs), hibakusha_and_downwind_communities (d at the full-target end — harm without exit or seat), tpnw_states_parties (high d — they pay pressure costs for refusing the arrangement's terms). Global spatial scope amplifies effective extraction modestly for target seats since verification of the disarmament limb is effectively impossible at that scope. No directionality overrides are authored: the beneficiary/victim plus exit data derive the relationships correctly, and the override mechanism keys on power atoms, which would collide here (two institutional seats with opposite positions, two powerful seats likewise).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is split-brained. The nonproliferation limb (the 1960s cascade fear) remains live in altered form — new proliferators emerged, and great-power arms racing has returned — while the disarmament limb is dead in practice: fifty-six years of review cycles, zero multilateral deliveries. Because the status is authored CONTESTED rather than dead, the mismatch consumer should not fire the zombie flag here; the honest finding is partial mandatrophy — one half of the mandate atrophied, the other half still functioning. This is exactly what blocks misclassification in both directions: the live coordination function (safeguards, the firebreak) prevents flattening the arrangement into pure extraction however strong the abolitionist rhetoric, and the accumulating extraction series (0.44 to 0.76) plus rising theater prevents excusing it as pure coordination cost. The piton question is also answered negatively: theater is high and rising, but the functional core (verification, the non-cascade record) is real, so the arrangement is not mostly performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'Which structural relation between Articles IV and VI does the treaty text actually instantiate — categorical subordination of IV to a binding disarmament command (this reading), reciprocity between conditional obligations (grand_bargain), or conditionality without justiciability (nonproliferation_primary)?',
    'Comparative doctrinal analysis applying each reading to the same fixed facts — indefinite extension (1995), the 13 Practical Steps, the 2010 Action Plan, TPNW adoption, ICJ 1996 — and asking which reading predicts the regime''s actual behavior without ad hoc repair.',
    'Sibling readings restructure the victim set and epsilon materially: nonproliferation_primary renders Article VI non-justiciable and dissolves the unfulfilled-promise grievance; grand_bargain keeps the arrangement repairable; this reading makes possession itself the standing violation and raises effective extraction for every target seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this constraint is one reading of the npt_article_iv_vi_pairing kernel; the sibling readings would reassign beneficiaries, victims, and epsilon over the shared referent.').

omega_variable(
    article_vi_legal_force,
    'Does Article VI impose an enforceable legal obligation to ACHIEVE disarmament, or only an aspirational commitment to PURSUE negotiations?',
    'ICJ jurisprudence (the 1996 advisory opinion''s obligation to pursue in good faith and bring to a conclusion), state practice, and doctrine distinguishing programmatic clauses from pacta sunt servanda obligations.',
    'If merely aspirational, the extraction claim loses its legal spine and this reading collapses toward nonproliferation_primary; if obligatory and breached, the arrangement''s persistence is a standing violation and every target seat''s effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_force, conceptual, 'Whether the disarmament limb is law or preamble — the hinge on which this reading''s extraction claim turns.').

omega_variable(
    peaceful_military_separability,
    'Can safeguards verifiably separate civilian nuclear activity from weapons-relevant capability, or is every enrichment and reprocessing capability inherently dual-use?',
    'IAEA breakout-time assessments tested against historical cases: Iraq pre-1991, Iran''s Fordow program, the DPRK''s transition from safeguarded to weapons activity — evaluated on detection probability and timeline arithmetic.',
    'If separable, Article IV can be legitimated by verification and this reading''s no-distinction axiom weakens toward the nonproliferation_primary position; if inseparable, the axiom holds and Article IV as written perpetuates exactly the risk the reading condemns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peaceful_military_separability, empirical, 'Empirical test of the reading''s second foundational axiom: the possibility of a meaningful peaceful/military program distinction.').

omega_variable(
    tpnw_customary_status,
    'Does the TPNW''s prohibition bind states that never joined it, or only its parties?',
    'Tracking state practice and opinio juris: ratification counts, weapon-state and alliance voting patterns, and whether non-party conduct shifts measurably after entry into force.',
    'If customary, the prohibition norm constrains Article IV globally and this reading''s authority claim extends over the armed states themselves; if purely contractual, the reading governs only its coalition and the standing arrangement''s operation continues unimpeded for everyone else.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_customary_status, empirical, 'Whether the humanitarian-law authority this reading invokes reaches the states it condemns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.22).
narrative_ontology:measurement(npt__tr_t8, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 8, 0.28).
narrative_ontology:measurement(npt__tr_t16, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 16, 0.36).
narrative_ontology:measurement(npt__tr_t24, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 24, 0.4).
narrative_ontology:measurement(npt__tr_t32, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 32, 0.47).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 40, 0.52).
narrative_ontology:measurement(npt__tr_t48, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 48, 0.56).
narrative_ontology:measurement(npt__tr_t56, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 56, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(npt__be_t8, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(npt__be_t16, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(npt__be_t24, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(npt__be_t32, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(npt__be_t48, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 48, 0.73).
narrative_ontology:measurement(npt__be_t56, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 56, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(npt__su_t8, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(npt__su_t16, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(npt__su_t24, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(npt__su_t32, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(npt__su_t48, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 48, 0.61).
narrative_ontology:measurement(npt__su_t56, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 56, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the NPT bargain' conflates three structurally distinct claims about the Article IV/VI relation. This file instantiates the abolitionist reading (categorical subordination; possession illegal; no peaceful/military distinction; authority from humanitarian-law lineage through the TPNW). The upstream sibling nonproliferation_primary (higher empirical confidence inside the regime, weapon-state-endorsed) is the reading this one attacks head-on — its premise that Article VI is non-justiciable is what the abolitionist reading's foundational axiom forecloses. The grand_bargain sibling is the mainstream critical position sharing much of this reading's diagnosis while stopping short of categorical displacement. Each member authors its own epsilon over the shared referent; cross-file contamination analysis should expect this reading's purity to degrade if the nonproliferation_primary reading regains interpretive ground, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
