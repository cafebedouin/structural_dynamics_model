% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Claim on Hagia Sophia's Christian Founding Legitimacy
 *   domain: cultural heritage / religious authority / sovereignty
 *
 * SUMMARY:
 *   This story authors ONE reading (orthodox_restitution_reading) of the
 *   contested Hagia Sophia kernel: the claim that the site's founding as
 *   Justinian's cathedral establishes a legitimacy that either warrants
 *   Orthodox ecclesiastical restitution or, failing that, obligates a return
 *   to religiously neutral status. This is not a story about the site's
 *   material control (Turkish state sovereignty is total and undisputed) but
 *   about a normatively active, materially dormant claim that circulates in
 *   diaspora, diplomatic, and commentariat discourse. The sibling readings —
 *   islamic_sovereignty_reading (legitimacy from 1453 conquest and continuous
 *   waqf) and universal_heritage_reading (legitimacy from shared human
 *   heritage transcending confession) — are separate constraint stories with
 *   their own ε and stakeholder structures, not alternate framings folded
 *   into this one. Per the ε-invariance principle, this story's ε is authored
 *   for the standing arrangement this reading contests (Turkish state control
 *   legitimated via 1934 secularization / 2020 reconversion), assessed by the
 *   orthodox_restitution reading's own lights — not for the reading's
 *   endorsed alternative (restitution or neutrality), which would trivially
 *   yield ε≈0.
 *
 * KEY AGENTS:
 *   - eastern_orthodox_diaspora: Primary symbolic beneficiary (organized/identity_locked) — draws cohesion from the claim, bears no material cost
 *   - greek_state_diplomatic_apparatus: Instrumental beneficiary (institutional/mobile) — deploys and withdraws the claim as diplomatic leverage
 *   - ecumenical_patriarchate: Constrained beneficiary (moderate/identity_locked) — benefits abroad, must disavow domestically to survive
 *   - turkish_state_sovereignty: Primary target (institutional/arbitrage) — bears recurring diplomatic cost, no material cost
 *   - muslim_worship_congregation: Secondary target (organized/trapped) — present use recast as historical interruption
 *   - local_istanbul_muslim_residents: Diffuse target (powerless/trapped) — no voice, no exit, framing attaches to their neighborhood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.22).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.15).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on Hagia Sophia's Christian Founding Legitimacy").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural heritage / religious authority / sovereignty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '3a11b3f5-1c01-4883-8af0-5bb42dd68112').
narrative_ontology:cs_kernel_codification('3a11b3f5-1c01-4883-8af0-5bb42dd68112', distributed).
narrative_ontology:cs_authority_grounding('3a11b3f5-1c01-4883-8af0-5bb42dd68112', distributed).
narrative_ontology:cs_reading_relation('3a11b3f5-1c01-4883-8af0-5bb42dd68112', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3a11b3f5-1c01-4883-8af0-5bb42dd68112', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('3a11b3f5-1c01-4883-8af0-5bb42dd68112', foundational, founding_consecration_confers_enduring_legitimacy).
narrative_ontology:cs_axiom_status(founding_consecration_confers_enduring_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3a11b3f5-1c01-4883-8af0-5bb42dd68112', founding_consecration_confers_enduring_legitimacy, deontological).
narrative_ontology:cs_axiom('3a11b3f5-1c01-4883-8af0-5bb42dd68112', secondary, conquest_does_not_extinguish_prior_sacred_title).
narrative_ontology:cs_axiom_status(conquest_does_not_extinguish_prior_sacred_title, holdable).
narrative_ontology:cs_axiom_grounding('3a11b3f5-1c01-4883-8af0-5bb42dd68112', conquest_does_not_extinguish_prior_sacred_title, conventional).
narrative_ontology:cs_reference_frame('3a11b3f5-1c01-4883-8af0-5bb42dd68112', byzantine_cathedral_consecration_537ce).
narrative_ontology:cs_drift_state('3a11b3f5-1c01-4883-8af0-5bb42dd68112', post_2020_reconversion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3a11b3f5-1c01-4883-8af0-5bb42dd68112', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomatic_apparatus).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worship_congregation).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, local_istanbul_muslim_residents).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_founding_priority_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, continuous_christian_consecration_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diaspora Orthodox communities outside Turkey draw symbolic cohesion and grievance-narrative continuity from the claim that Hagia Sophia's founding as a cathedral is the site's true legitimating fact. They hold no property interest and press no litigable claim; what they receive is identity reinforcement and a rallying point for diaspora institutions, church fundraising, and political lobbying in host countries.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% The Greek state periodically raises the restitution framing in diplomatic and EU-adjacent contexts as leverage in bilateral disputes with Turkey (Aegean rights, Cyprus, migration). It does not seek actual restitution but deploys the claim opportunistically; it can drop or revive the framing entirely at will depending on the state of Greek-Turkish relations, which is itself the mechanism by which the claim persists without ever needing to be resolved.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomatic_apparatus, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomatic_apparatus, agenda_setter).

% Based in Istanbul itself and dependent on continued toleration by the Turkish state for its own survival, the Patriarchate benefits symbolically from the restitution narrative circulating abroad but must publicly distance itself from it domestically, since open advocacy would jeopardize its precarious legal status inside Turkey. It profits from the claim's existence without being able to endorse it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, identity_locked, global).

% The Turkish state holds full effective control of the site (converted to a mosque in 2020 after decades as a museum) and treats any Byzantine-priority restitution claim as an affront to national sovereignty and a relitigation of the 1453 conquest. It bears no material cost — the claim has no enforcement mechanism — but absorbs a recurring diplomatic and domestic-political cost each time the narrative resurfaces, and uses it as a foil to rally domestic nationalist sentiment.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty, payer,
    institutional, generational, arbitrage, national).

% The congregation that now worships at the site under its restored mosque status has no say in the geopolitical framing contest; the restitution reading implicitly treats their present use as a historical interruption to be reversed rather than a settled fact, which colors international commentary about the site's status even though it changes nothing about their daily access to worship.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worship_congregation, payer,
    organized, biographical, trapped, local).

% Residents of the surrounding district experience the site as a living neighborhood mosque and tourist landmark; the restitution framing, when it surfaces in international press, recasts their neighborhood's religious life as a contested historical wrong, a framing they have no voice in shaping and cannot exit since it attaches to the physical building they live beside.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, local_istanbul_muslim_residents, payer,
    powerless, biographical, trapped, local).

% As the site's UNESCO World Heritage status predates the 2020 reconversion, heritage bodies have expressed concern about site management but are institutionally positioned as neutral custodians of universal heritage, not adjudicators of confessional restitution claims. Their voice on the specifically Orthodox-restitution question is structurally excluded from the framing, which treats the dispute as bilateral (Christian-founding vs. Islamic-sovereignty) rather than universal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_heritage_bodies, excluded,
    institutional, civilizational, analytical, global).

% Historians of Byzantium and Ottoman studies document the site's construction under Justinian, its nearly six centuries as a cathedral, and its subsequent Ottoman transformation, without adjudicating which era's status should govern present sovereignty. Their scholarship is selectively invoked by the restitution reading to establish founding priority while omitting that priority-of-founding is not a recognized principle of international sovereignty law.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_historical_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine present-tense coordination problem this reading solves for anyone who actually uses or administers the site; its real function is to coordinate diaspora and diplomatic actors around a shared grievance narrative that can be invoked or shelved as political convenience dictates.
% TRANSFER_FUNCTION: The claim transfers symbolic capital and diplomatic leverage to Greek state actors and Orthodox diaspora institutions, and transfers reputational and diplomatic cost to Turkish state sovereignty each time the narrative resurfaces internationally — no material property, money, or physical access ever actually moves.
% ABSENT_VOICES: The Muslim congregation currently worshipping at the site and the local Istanbul residents for whom it is a living neighborhood mosque are almost never consulted in international commentary invoking the restitution reading; UNESCO's universal-heritage framing is also structurally excluded, since the restitution reading requires treating the dispute as a two-sided confessional contest rather than a shared-heritage question.
% DISAPPEARANCE_RATIONALE: If the restitution claim vanished overnight, the Turkish state's control of the site would not change at all — nothing physically rearranges. But Greek diplomatic rhetoric would lose a recurring leverage point, and Orthodox diaspora institutions would lose a mobilizing symbol; whether that counts as 'the world rearranging' depends on whether one weighs material control (unchanged) or symbolic-political ecology (which does shift). The parties themselves would disagree about which effect matters.
% FOUNDING_PROBLEM: The claim was built to preserve a continuous Orthodox Christian institutional memory and moral standing after the 1453 conquest ended six centuries of the site's function as the seat of Eastern Christendom's chief cathedral, and to keep that historical fact politically live rather than settled by conquest.
% FOUNDING_PROBLEM_CORROBORATION: Independent Byzantine historians outside both the Greek state and the Ecumenical Patriarchate corroborate the historical facts of the cathedral's founding and centuries of Christian liturgical use, but do not corroborate that founding priority confers a present-day restitution entitlement — that normative leap is asserted only by the beneficiary parties themselves (Greek diplomatic apparatus, diaspora institutions), with no corroboration from international law scholars, who generally treat the 1934 secularization and 2020 reconversion as internal matters of Turkish sovereign discretion.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because there is no realistic implementation pathway — no court, treaty, or enforcement body that could transfer the site, so no material value actually moves. What is extracted is diplomatic and reputational cost imposed on Turkish sovereignty and background dignitary cost imposed on the site's current Muslim users, both of which are real but non-material. Theater ratio is authored moderately high and rising (0.45 to 0.62) because the claim's primary observable activity is performative — diaspora commemorations, diplomatic statements timed to bilateral tensions, op-eds — rather than any functional legal or institutional process toward the stated goal. Suppression is low (0.15): no one is coerced into silence about the claim; it circulates freely in international discourse. Accessibility collapse is moderate (0.35): alternative framings (universal heritage, Islamic sovereignty) remain fully articulable and are actively voiced by other parties — this claim has not foreclosed them. Resistance is fairly high (0.58) because Turkish state and domestic Turkish commentary actively and vocally contest the claim whenever it surfaces.
 *
 * PERSPECTIVAL GAP:
 *   From the Orthodox diaspora and Greek diplomatic seats, this reads as a Rope — a genuine, low-cost coordination mechanism for preserving historical memory and diplomatic leverage, since almost nothing is actually extracted from anyone in material terms. From the Turkish sovereignty and local Muslim resident seats, the same claim reads as low-grade but real extraction: a recurring cost imposed without consent, riding on a historical-priority framing that could never be adjudicated by any process they'd recognize as legitimate, and which periodically recasts their present, settled religious use of the site as illegitimate. The engine should compute this divergence from the structural power/exit data rather than from either party's own characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   The Orthodox diaspora and Greek diplomatic apparatus sit near the beneficiary end: they collect symbolic and leverage value from the claim's mere existence and can invoke or shelve it costlessly. The Ecumenical Patriarchate is a beneficiary with an unusual exit constraint — it profits from the narrative abroad but is identity-locked into public non-endorsement domestically, since its own survival depends on Turkish state toleration; this asymmetry is exactly why it is listed as beneficiary rather than agenda_setter, despite institutional proximity to the claim's substance. Turkish state sovereignty is the clearest target — institutional power gives it an arbitrage-grade exit from any single instance of the claim (it can simply not respond, or respond with countervailing nationalist mobilization), but the claim's persistence as a recurring diplomatic irritant is itself the cost, regardless of any single episode's material stakes. The Muslim congregation and local residents are targets with no institutional power and no exit at all — trapped by geography and by their absence from the discourse that reframes their daily religious life as a contested historical wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving Orthodox Christian institutional memory against erasure after 1453 — was arguably most live in earlier centuries when the Ottoman/Turkish state actively suppressed Christian minority institutions; today the Ecumenical Patriarchate persists (precariously) under formal toleration, and Orthodox communities worldwide practice freely. The claim's founding problem has substantially attenuated even as the claim's political utility (as diplomatic leverage and diaspora mobilization) has not — a classic mandatrophy signature where the mandate (restitution or neutrality) outlives clear evidence that its founding problem remains acute, while the mechanism persists because it serves an adjacent, undeclared function (diplomatic leverage) rather than the declared one (restitution). This is why the six_questions module records founding_problem_status as contested rather than dead: Orthodox advocates maintain the erasure risk is ongoing and symbolic, while outside historians and international law scholars see the claim as serving present political ends more than addressing a live historical injury.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_priority_as_legitimating_principle,
    'Does historical founding priority (which confession built and first consecrated the site) constitute a legitimate present-day sovereignty or restitution claim, or is it merely a historical fact with no normative force under any recognized framework of international law?',
    'No formal resolution mechanism exists — this is a conceptual dispute about which historical facts carry present normative weight. It could be informed by comparative analysis of how other contested religious sites (e.g., Cordoba''s Mezquita-Catedral, Ayodhya) have or have not treated founding priority as legally operative, but no binding adjudicating body exists for Hagia Sophia specifically.',
    'If founding priority is accepted as a legitimating principle even informally, the restitution reading gains normative traction independent of its zero implementation pathway; if rejected, the reading is purely symbolic-political with no claim to normative force beyond diaspora and diplomatic utility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_priority_as_legitimating_principle, conceptual, 'Whether historical founding priority is a legitimating sovereignty principle or merely a historical fact.').

omega_variable(
    diplomatic_leverage_vs_genuine_grievance,
    'Is the Greek state''s deployment of the restitution claim primarily genuine historical-cultural grievance, or primarily an opportunistic diplomatic instrument decoupled from any real restitution intent?',
    'Track correlation between claim invocation frequency/intensity and unrelated bilateral disputes (Aegean maritime rights, Cyprus, migration policy); a tight correlation with unrelated disputes would support the instrumental reading over the genuine-grievance reading.',
    'If primarily instrumental, this reading functions closer to a Tangled Rope riding on a genuine underlying historical grievance for diplomatic cover; if primarily genuine, the diplomatic leverage function is a side effect rather than the reading''s actual operative mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diplomatic_leverage_vs_genuine_grievance, empirical, 'Whether the claim''s diplomatic deployment is genuine grievance or opportunistic instrumentalization.').

omega_variable(
    sibling_reading_framing_asymmetry,
    'The kernel could be framed as a three-way symmetric contest (as declared) or as a two-way contest (Islamic sovereignty vs. universal heritage) with the Orthodox restitution reading as a historically residual, practically dormant third position kept alive mainly by diaspora and diplomatic actors rather than by any live ecclesiastical claimant with standing.',
    'Compare institutional weight: does any Orthodox ecclesiastical body (as opposed to diaspora lay organizations and the Greek state) formally and currently assert a restitution claim with any procedural mechanism attached, however symbolic? If not, the reading may be better modeled as a sub-current within Greek-Turkish diplomatic friction rather than a fully independent kernel reading.',
    'If the framing is asymmetric, this reading''s classification and stakeholder weight should be read as more marginal/symbolic relative to the other two readings, which does not change this story''s own ε but affects how the kernel-level contest should be weighted in cross-story analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_asymmetry, conceptual, 'Whether the three-reading kernel framing is symmetric or whether this reading is structurally more marginal than its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hagi_tr_t8, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(hagi_tr_t16, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(hagi_tr_t32, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(hagi_tr_t40, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hagi_be_t8, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(hagi_be_t16, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(hagi_be_t32, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(hagi_be_t40, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the single natural-language concept 'Hagia Sophia's legitimate status' per the ε-invariance principle. The islamic_sovereignty_reading and universal_heritage_reading are separate files with their own ε, stakeholders, and classification; they share the same kernel_id (hagia_sophia_substrate) but are not merged here. This reading's foundational axiom (founding_consecration_confers_enduring_legitimacy) directly forecloses the islamic_sovereignty_reading's core premise (that the 1453 conquest and continuous waqf establish sovereign legitimacy) within any single normative framework — a framework cannot simultaneously hold that founding priority is dispositive AND that conquest-plus-continuous-use is dispositive when they name different confessions as legitimate title-holder. It coexists_with the universal_heritage_reading because a party could hold both 'the Byzantine founding matters morally' and 'no single claim should govern the site' without contradiction, even though the political thrust of each differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
