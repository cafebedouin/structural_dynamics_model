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
 *   The standing arrangement under contest is the treaty regime built on the
 *   Article IV/VI pairing: non-possessing states receive peaceful nuclear
 *   technology under inspection and forswear weapons, while the five
 *   possessors retain their arsenals under a clause committing them to
 *   negotiate disarmament. Assessed by this reading's own lights —
 *   humanitarian law and the weapons-prohibition lineage culminating in the
 *   2017 prohibition treaty — the arrangement extracts a five-state
 *   possession monopoly from universal forbearance: the possessors alone
 *   enjoy the lawful-status privilege, the dual-use commerce channel spreads
 *   latency under a peaceful/military distinction this reading holds
 *   technically untenable, and the disarmament clause is renewed rhetorically
 *   each cycle while every arsenal modernizes. KEY AGENTS (by structural
 *   relationship): - nuclear_weapon_states_p5: Agenda-setter and principal
 *   beneficiary (institutional/arbitrage) — administers the regime, sets the
 *   disarmament pace, collects the monopoly -
 *   non_nuclear_weapon_states_parties: Principal target
 *   (organized/constrained) — pays forbearance, verification burden, and
 *   ambient risk for a renewed promise - commercial_nuclear_export_industry:
 *   Secondary beneficiary (powerful/arbitrage) — monetizes the dual-use
 *   channel regardless of disarmament trajectory -
 *   extended_deterrence_host_states: Dual-positioned client
 *   (powerful/constrained) — receives protection, pays subordination and
 *   targetability - tpnw_states_coalition: Resisting target
 *   (organized/identity_locked) — rejects the bargain's premise and pays
 *   diplomatic isolation for it - civilian_populations_under_nuclear_threat:
 *   Silent target (powerless/trapped) — bears catastrophic risk with no seat
 *   - future_generations_bearing_fallout_risk: Silent target
 *   (powerless/trapped) — inherits waste and posture decisions made without
 *   them - downwind_testing_communities: Excluded harmed party
 *   (powerless/trapped) — carries realized test and mining harms, marginal to
 *   the review machinery - icj_and_humanitarian_law_jurists: Analytical
 *   observer (institutional/analytical) — supplies the prohibition standard
 *   the other seats invoke or evade
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.8).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.66).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.8).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international law / nuclear governance / treaty interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '7dddffd9-1809-4362-be4d-8a71a7345467').
narrative_ontology:cs_kernel_codification('7dddffd9-1809-4362-be4d-8a71a7345467', fixed_text).
narrative_ontology:cs_authority_grounding('7dddffd9-1809-4362-be4d-8a71a7345467', lineage).
narrative_ontology:cs_interpretation_layer_present('7dddffd9-1809-4362-be4d-8a71a7345467').
narrative_ontology:cs_reading_relation('7dddffd9-1809-4362-be4d-8a71a7345467', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('7dddffd9-1809-4362-be4d-8a71a7345467', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_axiom('7dddffd9-1809-4362-be4d-8a71a7345467', foundational, weapon_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(weapon_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('7dddffd9-1809-4362-be4d-8a71a7345467', weapon_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('7dddffd9-1809-4362-be4d-8a71a7345467', foundational, article_iv_invalid_if_perpetuating_dual_use_risk).
narrative_ontology:cs_axiom_status(article_iv_invalid_if_perpetuating_dual_use_risk, holdable).
narrative_ontology:cs_axiom_grounding('7dddffd9-1809-4362-be4d-8a71a7345467', article_iv_invalid_if_perpetuating_dual_use_risk, instrumental).
narrative_ontology:cs_axiom('7dddffd9-1809-4362-be4d-8a71a7345467', secondary, no_peaceful_military_program_distinction).
narrative_ontology:cs_axiom_status(no_peaceful_military_program_distinction, holdable).
narrative_ontology:cs_axiom_grounding('7dddffd9-1809-4362-be4d-8a71a7345467', no_peaceful_military_program_distinction, empirically_contingent).
narrative_ontology:cs_reference_frame('7dddffd9-1809-4362-be4d-8a71a7345467', complete_disarmament_mandate_under_prohibition_lineage).
narrative_ontology:cs_drift_state('7dddffd9-1809-4362-be4d-8a71a7345467', contemporary_tpnw_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7dddffd9-1809-4362-be4d-8a71a7345467', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states_p5).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, commercial_nuclear_export_industry).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_host_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, civilian_populations_under_nuclear_threat).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, future_generations_bearing_fallout_risk).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, downwind_testing_communities).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, tpnw_states_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_host_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the five recognized arsenals and administer the regime that shelters them: they set the review agenda, interpret the disarmament clause's pace, shield one another through Security Council vetoes, and fund modernization programs that extend possession across coming decades. What flows to them is the sole lawful-status monopoly on the ultimate weapon; what flows from them is the open-ended promise of future disarmament. Giving up that monopoly is the one loss they face nowhere else in the system.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states_p5, beneficiary).

% Builds and sells reactors, fuel-cycle services, and enrichment capacity under the peaceful-use assurances of the technology-sharing article. Revenue depends on the dual-use channel staying open, and profits accrue whether or not the disarmament promise is ever kept. The same firms serve civil and military customers across jurisdictions and can reprice or redirect toward any buyer the rules admit.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, commercial_nuclear_export_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Host forward-deployed weapons or extended nuclear guarantees from one or more possessors. They receive protection they could not cheaply replicate alone, and they pay by hosting targetable installations, accepting operational subordination, and forgoing indigenous arsenals their industrial base would permit. Leaving the umbrella means immediate exposure to regional adversaries; building their own weapons means breaking their commitments and triggering cascade responses from neighbors.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_host_states, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_host_states, payer).

% The large majority of treaty parties. They forswear acquisition, accept intrusive verification of their facilities, and bear the ambient risk of others' arsenals, in exchange for assured access to peaceful technology and a disarmament promise renewed at each review cycle. Collectively they wield voting blocs and built an alternative prohibition treaty; individually each faces sanctions, fuel cutoff, and security deterioration if it walks out.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_parties, payer,
    organized, generational, constrained, global).

% The bloc of states that joined the 2017 prohibition treaty. They regard their own forbearance as unconditional and reject the premise that possession is tolerable pending reciprocity. Their exit is peculiarly closed: abandoning the prohibition coalition would mean discarding the commitment their diplomacy and domestic politics are now built around, while remaining inside the older regime exposes them to pressure from possessors and their protégés alike.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_states_coalition, payer,
    organized, generational, identity_locked, global).

% Live under targeting plans, accident corridors, test legacies, and the statistical possibility of use. They contribute nothing to the arrangement and hold no seat in its councils; their main channel of expression is civil-society testimony. Nowhere to exit: the risk attaches to geography and atmosphere, not to membership in any regime.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, civilian_populations_under_nuclear_threat, payer,
    powerless, biographical, trapped, global).

% Will inherit whatever waste, contaminated sites, and destabilized deterrence postures the present arrangement leaves behind. They cannot consent, appear, or bargain; every decision that extends possession or expands fissile stocks is taken on their account without their voice.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, future_generations_bearing_fallout_risk, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, future_generations_bearing_fallout_risk, excluded).

% Communities downwind of test sites, uranium-mining regions, and accident zones already carry the arrangement's realized harms — cancers, displacement, poisoned land. Absent from the original bargain and peripheral to its review machinery ever since, their testimony enters chiefly through civil-society interventions.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, downwind_testing_communities, excluded,
    powerless, biographical, trapped, regional).

% International lawyers and humanitarian-law bodies that assess the arrangement against prohibition precedents — the Geneva Protocol, the chemical and biological weapons conventions, the 2017 prohibition treaty — and against the International Court of Justice's holdings on threat and use. They adjudicate nothing directly but supply the doctrinal standard the other seats invoke or evade.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, icj_and_humanitarian_law_jurists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states_p5).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves three real collective problems at once: verifying that non-possession pledges are kept through inspection and material accounting; keeping peaceful nuclear commerce available to compliant states through assured supply and technology transfer; and stabilizing relations among the states that already hold weapons. Whatever else the pairing does, it coordinates these.
% TRANSFER_FUNCTION: Moves security subordination and open-ended catastrophic risk from non-possessing states and the world's populations to the account of the five possessors' monopoly; moves peaceful-nuclear market access from supplier states to compliant recipients, priced in forswearance; and moves the waiting cost of disarmament onto everyone except the possessors, who alone set its pace.
% ABSENT_VOICES: Affected communities — downwinders, uranium-mining regions, test-site populations — hold no formal seat in the review machinery and enter only through civil-society interventions; future generations appear through no one's mandate; the prohibition-coalition states stopped attending the consensus table after 2022, carrying their objection outside the room. Unanimous outcome documents therefore reflect the seats that stayed, not agreement among all affected.
% DISAPPEARANCE_RATIONALE: Without the pairing, verification of forswearance lapses, assured-supply contracts lose their legal frame, several latency states reassess hedging within months, and the possessors lose the treaty shelter that distinguishes their possession from outlaw possession — procurement, alliances, and doctrine all rearrange around the vacuum.
% FOUNDING_PROBLEM: Early-1960s projections showed dozens of states on course to build weapons within two decades; the arrangement was built to freeze possession at the existing five, guarantee everyone else peaceful nuclear technology under inspection, and bind the five to negotiate their arsenals away.
% FOUNDING_PROBLEM_CORROBORATION: Attestation from outside the benefiting parties: the International Court of Justice's 1996 advisory opinion records the disarmament limb as a standing legal undertaking; the 2017 prohibition-treaty negotiating conference and the humanitarian-initiative conference record attest that limb unfulfilled after five decades; longitudinal arms-acquisition studies (SIPRI-series) attest the anti-cascade limb succeeded. No attestation rests solely on the possessors or their clients.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored high (0.80 at interval end) because the referent is the standing Article IV/VI arrangement assessed by this reading's own lights — never the prohibition order this reading would install. Suppression (0.66) is a raw structural property, deliberately unscaled: it reflects enforcement machinery actually deployed — intrusive safeguards concentrated on the non-possessed, export-denial coordination, withdrawal-punishment norms, extended-deterrence discipline over clients, and a veto shield over the possessors themselves. Theater (0.55) splits the regime's activity: material accounting and inspection are real work, while the review-conference cycle has produced five decades of consensus prose without a single warhead eliminated under the clause it celebrates. Accessibility collapse is moderate (0.40): alternatives exist and partly work — the prohibition treaty, lawful withdrawal, sovereign hedging — but each carries heavy sanctioned or security cost. Resistance is high (0.72): a 120-plus-state prohibition bloc, sustained civil-society campaigns, and litigation attempts confront the arrangement directly. Temporal series run on one shared eight-point grid, every tracked metric authored at every point. The 1988 extractiveness dip tracks the intermediate-range-treaty thaw — an external relaxation, not an internal cycle — and the long climb afterward tracks accumulation: indefinite extension without benchmarks (1995), a comprehensive test ban signed but never in force, visible modernization, and open possessor repudiation of the prohibition instrument.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergently from identical structural data. From the possessors' chair the pairing looks like stewardship they built and police — a coordination they subsidize and control, with directionality pinned near the beneficiary pole. From the constrained non-possessors' chairs the same structure operates as enforced subordination: forbearance, verification burden, and ambient risk paid against a promise whose pace the counterparties alone set. Client states under extended deterrence compute a genuinely mixed position — protected and hostage at once — while the prohibition coalition's fusion with the abolition commitment pins its members at the extreme target pole regardless of material cost. The jurist seat sees the whole surface and certifies none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The possessors and the export industry anchor the beneficiary pole (low directionality; one subsidizes itself through the arrangement, the other is insulated from its costs). The non-possessors, threatened populations, future generations, downwind communities, and the prohibition coalition anchor the target pole (high directionality, amplified by trapped and identity-locked exits). One approximation is documented rather than overridden: extended-deterrence hosts are declared beneficiaries (primary role) yet sit materially nearer the symmetric midpoint, since they pay real subordination and host targetable assets — but the override mechanism keys on power atom alone, and correcting the hosts would simultaneously drag the export industry off its true near-zero pole. The derived value errs mildly generous to the hosts; this commentary records that error instead of laundering it through a blunt override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits in two: the anti-cascade limb is live and visibly served; the disarmament limb is dead in practice — pursued ritually, achieved never. Because the authored founding-problem status is contested rather than dead, the mismatch consumer raises no automatic zombie flag; the honest signal is the theater series crossing 0.5 around 2015, marking proxy activity (milestone-free extension, consensus prose) displacing substantive pursuit on the disarmament limb while the safeguards limb keeps doing real work. Classifying the pairing as pure coordination would erase the extraction the victim seats register; classifying it as pure extraction would erase the verified forbearance that genuinely suppressed a projected twenty-five-state cascade. The hybrid classification keeps both halves visible — and marks the transition this reading demands: Article IV disciplined by the prohibition norm, Article VI converted from promissory ritual into enforceable obligation. Receipt and cost are recorded separately: the gains demonstrably accrue to the possessors' seat, while dismantling the arrangement is prohibitive for the only actors positioned to dismantle it, since it would cost them their core strategic asset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the npt_article_iv_vi_pairing kernel; would instantiating the grand_bargain or nonproliferation_primary reading change the structural classification?',
    'Generate and compile the sibling reading files; compare per-seat classifications and epsilon under identical structural inputs.',
    'Under nonproliferation_primary the arrangement reads as legitimate verification-conditioned commerce and epsilon falls sharply; under grand_bargain epsilon becomes conditional on demonstrated disarmament progress; only under this reading is possession itself the extracted privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Classification is indexed to the abolitionist reading of the Article IV/VI kernel.').

omega_variable(
    article_vi_determinate_content,
    'Does Article VI carry a determinate, justiciable disarmament obligation, or only a good-faith aspiration?',
    'Authoritative adjudication — an ICJ referral or advisory request — or adoption of dated, verifiable milestones at a review conference.',
    'A determinate obligation puts the possessors in standing breach and pushes the arrangement toward the pure-extraction end; an indeterminate one raises the theater share and stabilizes the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_determinate_content, conceptual, 'Justiciability of the disarmament clause determines whether extraction reads as breach or as deferral.').

omega_variable(
    dual_use_separability,
    'Can enrichment and reprocessing capability be verifiably confined to peaceful ends, or is the dual-use channel inherently proliferative?',
    'Safeguards performance data: detection probabilities, conversion and breakout timelines, comparative cases of diverted and undiverted programs.',
    'If separable, Article IV retains coordination legitimacy and epsilon moderates; if inseparable, the peaceful/military distinction collapses, Article IV licensing becomes latency distribution, and this reading''s second axiom hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_separability, empirical, 'Whether the peaceful-use channel is technically separable from weapons capability.').

omega_variable(
    catastrophic_risk_weighting,
    'How much of the arrangement''s extraction consists of diffuse catastrophic risk borne by all, versus direct costs borne by identifiable seats?',
    'No purely empirical resolution: the weight assigned to low-probability existential harm is a value choice; humanitarian law answers one way, deterrence-security framings another.',
    'Weighting catastrophic risk heavily drives epsilon toward the maximum; discounting it leaves a moderate extraction profile dominated by subordination and verification burdens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_risk_weighting, preference, 'Value-dependent weight of existential risk in the epsilon assessment.').

omega_variable(
    tpnw_authority_capture,
    'Will the prohibition lineage capture authority over possession from the treaty kernel, or remain a parallel track?',
    'Track prohibition-treaty ratification growth, divestment and financing effects, client-state defection from extended deterrence, and possessor engagement or continued repudiation.',
    'Capture strengthens this reading''s authority grounding and raises suppression as enforcement systems collide; durable parallelism freezes the current hybrid classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_authority_capture, empirical, 'Whether the prohibition norm displaces or merely accompanies the treaty regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(npt__tr_t1978, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1978, 0.3).
narrative_ontology:measurement(npt__tr_t1988, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1988, 0.22).
narrative_ontology:measurement(npt__tr_t1998, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1998, 0.38).
narrative_ontology:measurement(npt__tr_t2008, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2008, 0.42).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2020, 0.52).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement(npt__be_t1978, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1978, 0.64).
narrative_ontology:measurement(npt__be_t1988, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(npt__be_t1998, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(npt__be_t2008, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.32).
narrative_ontology:measurement(npt__su_t1978, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement(npt__su_t1988, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1988, 0.36).
narrative_ontology:measurement(npt__su_t1998, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement(npt__su_t2008, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).

% DUAL FORMULATION NOTE:
% The colloquial label 'NPT Article IV/VI relationship' covers at least three structurally distinct arrangements. This file instantiates the abolitionist reading: possession categorically illegitimate, the pairing read as enforced extraction riding real safeguards coordination, epsilon high. The grand_bargain sibling instantiates reciprocal conditional legitimacy (epsilon contingent on demonstrated disarmament progress); the nonproliferation_primary sibling instantiates verification-conditioned commerce with a non-justiciable disarmament clause (epsilon low-to-moderate). The readings differ in epsilon because they differ in what they hold the standing arrangement to BE — not because epsilon is observer-relative within any one of them. The upstream fact all three share is the treaty text itself; files are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
