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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Reading: Hagia Sophia Substrate
 *   domain: cultural_heritage/religious_authority/geopolitical
 *
 * SUMMARY:
 *   The Hagia Sophia substrate is a 1,700-year-old edifice that functions as
 *   the focal point of a contest over religious, cultural, and geopolitical
 *   legitimacy. Built as a Christian cathedral (330 CE), it was converted to
 *   a mosque after the 1453 Ottoman conquest, stood as a museum (1935–2020)
 *   under Turkish state neutrality, and was reconverted to a mosque in 2020.
 *   The Orthodox restitution reading asserts that the site's foundational
 *   identity as a Christian cathedral should determine its present status: it
 *   should be returned to Orthodox ecclesiastical control or at minimum
 *   remain neutral (non-religious or museum). This reading is held primarily
 *   by the Eastern Orthodox diaspora, Greek state interests, and some
 *   international heritage advocates. It is directly opposed by Islamic
 *   believers and Turkish authorities who read the site's legitimacy as
 *   grounded in the 1453 Ottoman conquest and continuous Islamic endowment. A
 *   third reading, the universal heritage reading, frames the site as
 *   transcending any single religious or national claim and belonging to all
 *   humanity. This story instantiates ONLY the Orthodox restitution reading —
 *   not the Islamic sovereignty reading, not the universal heritage reading.
 *   The ε, stakeholders, and structural analysis describe this reading's own
 *   logic, not a synthesis or compromise.
 *
 * KEY AGENTS:
 *   - Eastern Orthodox diaspora: globally dispersed beneficiary, holds symbolic claim but cannot enforce it
 *   - Greek state: beneficiary and secondary payer, derives diplomatic leverage but incurs Turkish resentment
 *   - Turkish sovereignty (national state): primary victim, faces external legitimacy challenge to its control
 *   - Islamic believers and Turkish Islamic authorities: victims of the restitution claim, would face interrupted continuity if the reading were enforced
 *   - UNESCO and international heritage bodies: observers who advocate the universal heritage reading, not the Orthodox restitution reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.42).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.18).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Reading: Hagia Sophia Substrate").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/religious_authority/geopolitical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'db43b072-dee6-4718-80a7-88984e5929af').
narrative_ontology:cs_kernel_codification('db43b072-dee6-4718-80a7-88984e5929af', fixed_text).
narrative_ontology:cs_authority_grounding('db43b072-dee6-4718-80a7-88984e5929af', lineage).
narrative_ontology:cs_interpretation_layer_present('db43b072-dee6-4718-80a7-88984e5929af').
narrative_ontology:cs_reading_relation('db43b072-dee6-4718-80a7-88984e5929af', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('db43b072-dee6-4718-80a7-88984e5929af', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('db43b072-dee6-4718-80a7-88984e5929af', foundational, foundational_christian_legitimacy_determinative).
narrative_ontology:cs_axiom_status(foundational_christian_legitimacy_determinative, holdable).
narrative_ontology:cs_axiom_grounding('db43b072-dee6-4718-80a7-88984e5929af', foundational_christian_legitimacy_determinative, deontological).
narrative_ontology:cs_axiom('db43b072-dee6-4718-80a7-88984e5929af', foundational, conquest_invalidates_possession_transfer).
narrative_ontology:cs_axiom_status(conquest_invalidates_possession_transfer, overridden).
narrative_ontology:cs_axiom_grounding('db43b072-dee6-4718-80a7-88984e5929af', conquest_invalidates_possession_transfer, conventional).
narrative_ontology:cs_reference_frame('db43b072-dee6-4718-80a7-88984e5929af', byzantine_orthodox_primacy).
narrative_ontology:cs_drift_state('db43b072-dee6-4718-80a7-88984e5929af', contemporary_mosque_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('db43b072-dee6-4718-80a7-88984e5929af', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_ulama_and_islamic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A globally dispersed community of Eastern Orthodox Christians whose symbolic and spiritual claim to Hagia Sophia derives from the edifice as the founding cathedral of Orthodox Christendom. They benefit from the legitimacy narrative this reading provides — a grounding for diaspora identity and grievance recovery — without bearing enforcement costs. They can advocate, lobby, and petition but cannot unilaterally change the site's status. Their investment is primarily symbolic and memorial.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, civilizational, mobile, global).

% Derives diplomatic and cultural leverage from advancing the Orthodox restitution claim in international forums and bilateral negotiations with Turkey. The claim functions as a bargaining asset in broader geopolitical contests over Cyprus, Aegean maritime boundaries, and regional influence. Greece bears the cost of maintaining the claim rhetorically and diplomatically (tension with Turkey) while gaining symbolic capital and the possibility of future negotiating leverage. The constraint keeps the site as a live issue in Greek-Turkish relations rather than a settled historical fact.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, payer).

% The Turkish state faces an external legitimacy claim that, if accepted, would cede sovereign authority over a nationally-significant cultural asset. The constraint operates by asserting that Turkey's control is illegitimate — that the site should 'return' to Orthodox authority or become neutral. Turkey cannot exit this constraint without abandoning Hagia Sophia or capitulating to an external claim. The mere assertion of this reading creates diplomatic friction and keeps Turkish sovereignty under challenge in certain international contexts.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, generational, trapped, national).

% Turkish Muslims and Islamic believers for whom Hagia Sophia, reconverted to a mosque in 2020 after 86 years as a museum, represents the culmination of the 1453 Ottoman conquest and Islamicization. The Orthodox restitution reading threatens to interrupt that continuity again by asserting the site 'should' return to Christian control or become neutral — effectively delegitimizing the present arrangement. Islamic worshippers cannot simply relocate their attachment to the site; their identity and faith practice are fused with it. The constraint extracts from them by perpetually contesting the legitimacy of their current use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    moderate, generational, identity_locked, regional).

% Islamic scholarly and religious authorities in Turkey who have grounded the mosque's legitimacy in waqf endowment and continuous Islamic custodianship since 1453. The Orthodox restitution reading attacks this grounding directly by asserting priority of the Byzantine Christian founding. They cannot compromise on the foundational claim without surrendering their interpretive authority over Islamic legitimacy.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_ulama_and_islamic_authority, payer,
    organized, civilizational, identity_locked, regional).

% International cultural heritage bodies that frame Hagia Sophia as universal human patrimony transcending religious and national claims. They observe the contest between the Orthodox restitution reading and Islamic sovereignty readings, and advocate for the universal heritage reading as the legitimate frame. They have no enforcement mechanism over the site but can stigmatize decisions that violate their endorsed frame.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_and_international_heritage, observer,
    institutional, generational, analytical, global).

% The Greek state administers and articulates the Orthodox restitution claim as a matter of national cultural policy and diplomatic strategy. It is the primary seat that can frame the constraint in international discourse, though it cannot unilaterally enforce the claim against Turkish sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_government, agenda_setter,
    institutional, generational, constrained, national).

% The physical and symbolic structure — as an abstract entity — cannot hold a role, but is listed here for narrative completeness: the site is a beneficiary insofar as the contested ownership status keeps it at the center of geopolitical discourse and ensures continued international attention, resource allocation to preservation, and symbolic importance that would fade if any single claim were universally accepted.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_site_itself, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_site_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function. The Orthodox restitution reading does not solve a genuine collective-action problem; it asserts a legitimacy claim that divides rather than unites. There is no coordinated outcome that satisfies the reading's core premise while also satisfying the Islamic sovereignty reading or maintaining present Turkish control.
% TRANSFER_FUNCTION: Transfers symbolic and diplomatic capital from Turkish sovereignty to Greek state and Orthodox diaspora interests. Transfers historical-legitimacy claims from Islamic endowment narratives to Christian foundational narratives. No material goods or services flow; the transfer is authority, narrative control, and grievance recognition.
% ABSENT_VOICES: Contemporary Muslims in Turkey who use the mosque for worship are structurally excluded from this reading's discourse. The reading does not ask what present-day Islamic believers want; it asserts what the site 'should' be. Secular Turks who preferred the museum era are also absent. The voices present are Greek state, Orthodox diaspora, and their international allies (some heritage advocates). The voices absent are those who benefit from or defend the present mosque status.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution reading and the restitution claim it grounds disappeared (if Greek state advocacy ceased and the Orthodox diaspora stopped asserting the claim), the geopolitical landscape would shift: one persistent source of Turkish-Greek friction would evaporate, international heritage discourse would consolidate around either the universal heritage reading or the Islamic sovereignty reading (likely the former as a diplomatic compromise), and the site would no longer be a live issue in culture-war rhetoric. The physical building would remain unchanged, but its symbolic and diplomatic meaning would stabilize. Turkey's control would no longer be perpetually contested by an external Orthodox claim.
% FOUNDING_PROBLEM: The founding problem, from the Orthodox restitution reading's perspective, is the 1453 Ottoman conquest and the resulting displacement of Christian worship from the site. The reading asserts that a 1,100-year-old Christian cathedral was illegitimately expropriated by military conquest and Islamicized, severing the continuous Orthodox tradition and the site's Christian function. The restitution reading frames this historical injustice as something that should be remedied in the present, either by returning the site to Orthodox control or by neutralizing it (museum status) to honor its foundational Christian identity rather than its present Islamic function.
% FOUNDING_PROBLEM_CORROBORATION: The Orthodox restitution reading is corroborated by internal sources only: Orthodox theological tradition, Greek state historical narratives, and diaspora community memory. The founding problem (1453 conquest and Islamicization) is historically real and independently documented by Byzantine and Ottoman chronicles. However, the reading's claim that this historical injustice should be remedied in the present is NOT corroborated by any external authority. Turkey rejects the restitution premise as a violation of present sovereignty. UNESCO advocates neutrality/universal heritage framing, not Orthodox restitution. International law privileges present sovereignty and does not require rectification of historical conquest. No external independent authority supports the reading's corrective mandate. The founding problem is dead in the sense that the conquest is 570 years past and Turkey has consolidated Islamic and national authority over the site for the entire interval; the mandate for restitution has no enforcement pathway and is maintained only as a narrative and diplomatic assertion by the reading's own beneficiaries.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures how much the constraint extracts from those it governs. Under the Orthodox restitution reading, the constraint extracts from Turkish sovereignty (by contesting its legitimacy over the site) and from Islamic worship continuity (by asserting the site should not be Muslim). The measurement series shows a striking pattern: extractiveness was highest at 1453 (0.85, when the conquest actively displaced Christians) and declined through the Ottoman centuries as the conquest became historical fact. It dropped to near-zero at 1935 when Turkey converted the site to a museum (neutralizing the conflict). It has risen again since 1980 as the Orthodox restitution claim has been revived in international discourse, reaching 0.42 by 2024 under the mosque reconversion. Theater ratio is the inverse: high (0.92) during the museum era when the restitution claim had no realistic enforcement pathway and was maintained purely as a narrative assertion; it has declined (0.72 by 2024) as the restitution claim faces the reality that Turkey has re-Islamicized the site and the Orthodox reading's demand for restoration or neutrality has zero enforcement capacity. Suppression was high at 1453 (0.92: conquest-era suppression of Christian worship) and has remained low since (0.05–0.18) because the reading has no enforcement machinery. The reading is maintained through rhetoric, diplomacy, and symbolic continuity, not through coercive suppression. This pattern — declining extractiveness over centuries, a spike in theater when the claim was most detached from reality (museum era), and low suppression throughout — marks the constraint as a claim that oscillates between activation and dormancy depending on geopolitical salience and the site's functional status.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats experience this constraint very differently. From the Orthodox diaspora and Greek state seat, the restitution reading is a legitimate claim to historical justice and cultural recovery — the constraint functions as a framework for maintaining identity and diplomatic leverage. From the Turkish sovereignty and Islamic worship seats, the same reading is an external illegitimate claim that perpetually contests their authority and rights. The engine should compute dramatically different types for these seats: the beneficiary seats might assess the constraint as a rope-like coordination mechanism (we are coordinating around a shared claim of foundational legitimacy), while the payer seats should assess it as a snare (we are being extracted from by a claim we cannot escape or refute, backed by international rhetoric we cannot fully suppress). This divergence arises from the structural fact that the reading has no enforcement mechanism — it must be accepted or rejected; it cannot be negotiated down. The Greek state occupies an interesting middle position: it benefits from the claim's diplomatic utility but pays the cost of maintaining it (tension with Turkey, inability to settle the underlying dispute, rhetorical commitment to a claim it cannot enforce). If Greece could costlessly maintain the claim, it would be pure beneficiary; but the diplomatic cost is real and visible in Greece's constrained exit options and the ongoing friction with Turkey.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each stakeholder flows from their role and exit options. Eastern Orthodox diaspora: declared as beneficiary (holds the claim, derives symbolic legitimacy from it) with mobile exit options (they can shift focus to other sites or other forms of Orthodox identity recovery). Their d should be near 0.2–0.3 (beneficiary, but not totally trapped by the constraint — they could exit by reframing their identity). Greek state: declared as beneficiary with secondary_role payer (benefits from diplomatic leverage, pays the cost of maintaining the claim and enduring Turkish resentment) with constrained exit (cannot abandon the claim without domestic political backlash, cannot resolve it without capitulating to Turkey). Their d should be near 0.4–0.5 (symmetric: the leverage benefit matches the diplomatic cost). Turkish sovereignty: declared as victim (faces external claim to its legitimate authority) with trapped exit (cannot exit without ceding sovereign territory or capitulating to the restitution demand). Their d should be near 0.85–0.95 (full target of extraction). Islamic worship continuity: declared as victim with identity_locked exit (Muslims' identity and faith practice are fused to the site; they cannot simply relocate their religious continuity). Their d should be near 0.9 (near-full target, because they cannot escape the claim even psychologically). These directionalities are computed by the engine from the structural data; no overrides are declared here because the beneficiary/victim declarations and exit options already specify the structure clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   The Orthodox restitution reading exhibits classic mandatrophy characteristics at the present moment (2024). The founding mandate was clear: after 1453, the Orthodox diaspora sought restoration of their primary cathedral and the seat of their Patriarchy. That mandate was live and contested through the Ottoman centuries (1453–1923). By 1935, when Turkey converted Hagia Sophia to a museum, the founding mandate was technically rendered moot: the site was neutralized, no longer functioning as Islamic worship space. From 1935–2020, the site's status as a museum was arguably a partial victory for the restitution reading (the site was not functioning as a mosque, honoring its non-Islamic phases). However, the restitution mandate did not actually drive the 1935 decision — the museum status was a choice by the Turkish secular state for reasons of heritage preservation and international diplomacy, not a concession to Orthodox pressure. When Turkey reconverted the site to a mosque in 2020, it explicitly rejected the restitution mandate and the reasoning behind the museum era neutrality. At present, the restitution mandate is dead in any material sense (there is zero enforcement pathway, zero possibility of reversing the 2020 reconversion) but remains active in rhetorical and diplomatic discourse. This is mandatrophy: the constraint persists because of institutional and symbolic inertia (Greek state continues to assert the claim, Orthodox diaspora maintains the narrative) even though the underlying mandate has been structurally obsoleted. The founding problem (Orthodox control of the cathedral) is not solved; it is simply impossible. The constraint extracts (from Turkish sovereignty and Islamic worship) not because any party can enforce the mandate but because the claim itself acts as a persistent delegitimization and a source of geopolitical friction. The theater ratio of 0.72 at present reflects this: the restitution reading is maintained primarily through rhetorical performance and diplomatic assertion rather than through any realistic enforcement activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_legitimacy_vs_conquest,
    'Does the Christian foundational status (330 CE) carry greater legitimacy weight than 500+ years of continuous Islamic endowment and worship (1453–2024)? Or does present custody supersede historical founding as the basis for legitimate authority?',
    'No empirical resolution exists. This is a normative question about how to weight historical claim against continuous possession and practice. Different legal and ethical traditions yield different answers: international law privileges present sovereignty; Orthodox theology privileges foundational sacred function; Islamic jurisprudence privileges continuous waqf endowment. Resolution would require external authority (e.g., international court) to adjudicate between these frameworks, which no such authority has undertaken.',
    'If founding legitimacy is weightier, the Orthodox restitution claim is structurally sound and Turkey bears an obligation to restore or neutralize. If continuous possession and present sovereignty supersede, the restitution claim is a retrospective grievance without enforceability, and the present Islamic status is legitimate. The reading''s entire ε rests on this weighting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_legitimacy_vs_conquest, preference, 'Weighting of foundational historical claim vs. continuous possession and present sovereignty.').

omega_variable(
    kernel_reading_contest_ambiguity,
    'This constraint is one reading of a contested kernel. How much of the measured extractiveness (0.42) arises from the reading itself (the assertion that Turkish control is illegitimate) vs. from the underlying historical fact (1453 conquest and Islamicization)? Would a different reading of the same kernel produce a substantially different ε?',
    'Compare the ε authored in the sibling readings (islamic_sovereignty_reading, universal_heritage_reading). Each reading frames the same physical site differently; if their ε values diverge widely, the divergence signals that ε is reading-indexed, not a property of the site itself. This is structurally correct per OQ-26: ε is a property of a reading (the standing arrangement under contest, assessed by the reading''s own lights), not a reading-independent topic.',
    'If ε values across the three readings are {0.42 (orthodox), 0.15 (universal), 0.78 (islamic)}, the constraint family demonstrates that the same site generates different extraction structures under different readings — confirming that ε is reading-indexed. If the values converge, the site has a reading-independent extraction profile. Divergence refines the corpus''s understanding of how commitment-system readings multiply constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Reading-indexing of ε within a constraint family.').

omega_variable(
    suppression_mechanism_internalization,
    'The measured suppression (0.18) is low because the Orthodox restitution reading has no enforcement capacity — it is a pure legitimacy claim with no coercive machinery. But does the claim suppress alternatives (e.g., the universal heritage reading) through rhetoric, diplomatic pressure, and control of narrative rather than through direct coercion?',
    'Analyze whether suppression is structural (military, legal, economic barriers to exit) or internalized (the reading''s rhetorical force discredits alternatives without material constraint). For this constraint, suppression is primarily rhetorical — the reading''s existence in Greek-Turkish diplomatic discourse shapes what can be said and negotiated without the claim being backed by enforcement. The internalized suppression operates on Turkish actors (if they accept the restitution framing as legitimate, they feel bound to respond) rather than on Orthodox beneficiaries (who embrace the reading).',
    'If suppression is internalized, the effective suppression may be higher than the structural measure suggests — the claim constrains Turkish discourse and diplomatic maneuvering even without enforcement. High internalized suppression would reclassify the constraint as more extractive (higher effective ε) despite low structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in legitimacy claims.').

omega_variable(
    beneficiary_vs_payer_seat_divergence,
    'The Orthodox diaspora and Greek state are listed as beneficiaries, yet the Greek state is also a secondary payer (bearing diplomatic costs of maintaining the claim). Will the engine compute the Greek state''s directionality as beneficiary-dominant (d low) or as genuinely dual? And does this dual positioning make the Greek state a bridge seat that could shift to the other reading?',
    'The engine derives directionality from beneficiary/victim declarations + exit options. Greece is declared beneficiary (diplomatic leverage) + secondary_role payer (tension with Turkey, cost of maintaining the claim). The exit_options are constrained (cannot easily abandon the claim without domestic political cost). The engine should compute a d near 0.4–0.6 (moderate/symmetric) rather than 0.0–0.2 (full beneficiary). If the computed d diverges from this expectation, the beneficiary/victim declarations may be miscalibrated.',
    'If Greece''s d computes symmetric, it suggests the constraint''s benefit to Greek state interests is matched by its cost — the claim is worth maintaining but not heavily subsidized. If Greek d computes beneficiary-dominant (too low), the declaration of secondary_role payer may be inadequate to capture the true cost structure. This would suggest an override is needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_payer_seat_divergence, empirical, 'Directionality computation for dual-positioned institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1453, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1453, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1453, 0.05).
narrative_ontology:measurement(hagi_tr_t1850, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(hagi_tr_t1935, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1935, 0.92).
narrative_ontology:measurement(hagi_tr_t1980, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1980, 0.88).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2010, 0.78).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.68).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.72).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1453, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1453, 0.85).
narrative_ontology:measurement(hagi_be_t1850, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1850, 0.72).
narrative_ontology:measurement(hagi_be_t1935, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(hagi_be_t1980, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1453, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1453, 0.92).
narrative_ontology:measurement(hagi_su_t1850, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(hagi_su_t1935, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1935, 0.05).
narrative_ontology:measurement(hagi_su_t1980, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1980, 0.08).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2020, 0.18).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia substrate kernel decomposes into three constraint readings, each with distinct ε values, beneficiary/victim sets, and classifications. The Orthodox restitution reading (this file) claims legitimacy via Byzantine foundational status and asserts restitution or neutrality. The Islamic sovereignty reading claims legitimacy via the 1453 Ottoman conquest and continuous Islamic endowment (waqf), asserting the present mosque status as legitimate. The universal heritage reading claims legitimacy via transcendence of religious and national claims, asserting the site should remain neutral and accessible to all. These readings are not observers of a single constraint — they are structurally different constraints that share a kernel (the physical site and the contest over its legitimate authority). Each reading is instantiated as a separate constraint story with its own ε, stakeholders, and six-questions answers. They are linked via network.affects_constraints to signal that they are a constraint family and that changes in one reading's salience or acceptability influence the others. The constraint family's composition and the ε divergence across readings is a core data point for the corpus's validation of the ε-invariance principle (OQ-26): the same physical site generates different constraints under different readings, confirming that ε is reading-indexed, not a reading-independent property of the site.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
