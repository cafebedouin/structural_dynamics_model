% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia: Orthodox Restitution Reading
 *   domain: cultural_heritage / religious_authority / sovereignty
 *
 * SUMMARY:
 *   Hagia Sophia is a 1,500-year-old structure built as a Byzantine Christian
 *   cathedral (537 CE), converted to an Ottoman mosque after the 1453
 *   conquest, secularized as a museum under the Turkish Republic (1935–2020),
 *   and returned to Islamic worship in 2020. The Orthodox restitution reading
 *   claims that the site's foundational legitimacy derives from its Christian
 *   cathedral origin and should either be returned to Orthodox ecclesiastical
 *   control or remain neutral as a universal heritage site to honor its
 *   Byzantine roots. This reading embodies a diaspora narrative (the Orthodox
 *   claim symbolic and historical ownership) and a state narrative (Greece
 *   uses the claim as diplomatic leverage). It competes with the Islamic
 *   sovereignty reading (legitimacy derives from 573 years of Ottoman
 *   stewardship and current Turkish control and Islamic endowment) and the
 *   universal heritage reading (legitimacy transcends all religious and
 *   national claims). The Orthodox reading imposes a diffuse delegitimization
 *   cost on Turkish sovereignty and an interrupted-status cost on Islamic
 *   worship, while providing symbolic capital to the Orthodox diaspora and
 *   the Greek state. Measured extractiveness is moderate (0.48) and heavily
 *   theatrical (0.71), indicating a constraint whose material enforcement is
 *   near-zero but whose symbolic and diplomatic power is substantial.
 *
 * KEY AGENTS:
 *   - Eastern Orthodox diaspora (identity-locked beneficiary, global; organized power; civilizational time horizon)
 *   - Greek state (institutional agenda-setter and secondary beneficiary; institutional power; constrained exit; national scope)
 *   - Turkish sovereignty (institutional payer; trapped exit; civilizational scope; the site's de facto controller)
 *   - Islamic worship continuity (organized payer; constrained exit; regional scope; vulnerable to interrupted worship)
 *   - International heritage bodies (analytical observers; global scope; no enforcement power)
 *   - Academic historians (analytical observers; provide grounding for the claim but do not adjudicate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.48).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.22).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia: Orthodox Restitution Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage / religious_authority / sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '9b17aee5-0277-499c-989e-f787db4d1faa').
narrative_ontology:cs_kernel_codification('9b17aee5-0277-499c-989e-f787db4d1faa', fixed_text).
narrative_ontology:cs_authority_grounding('9b17aee5-0277-499c-989e-f787db4d1faa', lineage).
narrative_ontology:cs_interpretation_layer_present('9b17aee5-0277-499c-989e-f787db4d1faa').
narrative_ontology:cs_reading_relation('9b17aee5-0277-499c-989e-f787db4d1faa', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b17aee5-0277-499c-989e-f787db4d1faa', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('9b17aee5-0277-499c-989e-f787db4d1faa', foundational, founding_legitimacy_primacy).
narrative_ontology:cs_axiom_status(founding_legitimacy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9b17aee5-0277-499c-989e-f787db4d1faa', founding_legitimacy_primacy, deontological).
narrative_ontology:cs_axiom('9b17aee5-0277-499c-989e-f787db4d1faa', secondary, orthodox_historical_continuity_binding).
narrative_ontology:cs_axiom_status(orthodox_historical_continuity_binding, holdable).
narrative_ontology:cs_axiom_grounding('9b17aee5-0277-499c-989e-f787db4d1faa', orthodox_historical_continuity_binding, deontological).
narrative_ontology:cs_reference_frame('9b17aee5-0277-499c-989e-f787db4d1faa', byzantine_cathedral_origins).
narrative_ontology:cs_drift_state('9b17aee5-0277-499c-989e-f787db4d1faa', contemporary_multi_regime_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b17aee5-0277-499c-989e-f787db4d1faa', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Orthodox diaspora — concentrated in Greece, Cyprus, Russia, Eastern Europe, and diaspora communities in North America and Western Europe — claims Hagia Sophia as the foundational site of Orthodox Christian civilization. They do not control the site materially, but the restitution claim provides symbolic affirmation of Orthodox historical legitimacy and cultural continuity. Leaving the claim means abandoning a core identity marker. The constraint benefits them by maintaining Hagia Sophia in the Orthodox collective consciousness as 'theirs' — a symbolic possession even if material possession is impossible. Their exit from this claim would feel like loss of self, not merely political defeat.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, civilizational, identity_locked, global).

% Greece articulates and amplifies the Orthodox restitution claim as state policy and diplomatic strategy. The claim provides leverage in negotiations with Turkey on Cyprus, Aegean maritime boundaries, and EU-Turkey relations. Greece does not control Hagia Sophia, but the claim strengthens the Greek position by establishing a normative counter to Turkish regional dominance. Greece is the agenda-setter (it formally asserts the claim in international forums) and the secondary beneficiary (it gains diplomatic advantage). It cannot exit without losing the leverage, but it could exit if the diplomatic costs of maintaining the claim exceeded the benefits — making its exit contingent rather than identity-locked.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary).

% Turkey exercises uncontested material control of Hagia Sophia through state ownership and administrative authority. The Orthodox restitution claim imposes a legitimacy cost: it delegitimizes Turkish sovereignty in international discourse, provides grounds for EU and diaspora actors to question Turkish stewardship, and complicates Turkey's narrative of civilizational succession to the Ottoman Empire and Constantinople. Turkey cannot exit this claim without surrendering the territory itself, which is structurally impossible given Turkey's regional power and the international order's acceptance of Turkish sovereignty. Turkey bears the cost of delegitimization while being unable to escape it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, civilizational, trapped, national).

% Muslim worship at Hagia Sophia was restored in 2020 after 85 years as a museum (1935–2020). The Orthodox restitution claim directly threatens this continuity by advocating for Orthodox control or secular neutrality, either of which would interrupt Islamic worship again. The constraint imposes both existential risk (threat to continued prayer) and legitimacy pressure (the site's status remains contested rather than settled as Islamic property). Islamic communities in Turkey and the Muslim world have a stake in the site's continued availability for worship. They bear the cost of contested status even as they exercise current control.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    organized, civilizational, constrained, regional).

% UNESCO, the International Council of Monuments and Sites (ICOMOS), and other international heritage organizations document Hagia Sophia's status, advocate for preservation and access, and sometimes advance universal heritage framings. They observe the Orthodox restitution claim, acknowledge the site's Christian origins, and note the contested ownership narrative. They lack enforcement power over national sovereignty and cannot adjudicate the claim, but they provide a platform for heritage-universalist arguments that compete with both Orthodox and Islamic claims.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, international_heritage_bodies, observer,
    institutional, generational, analytical, global).

% Historians across traditions — Byzantine specialists, Ottoman historians, comparative religion scholars — research and testify about Hagia Sophia's founding as a Christian cathedral, the Ottoman conquest, successive regimes, and the contemporary status. Their scholarship grounds the Orthodox restitution claim's historical premise (the site was indeed founded as a Byzantine cathedral) but does not resolve the contemporary political question of rightful control. They operate as analytical seats, providing evidence and context without directly benefiting or paying.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, academic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Orthodox restitution reading coordinates a diaspora narrative: it unifies dispersed Orthodox communities around a shared historical claim and uses that unified claim to exert diplomatic pressure. It also coordinates a Greek national interest in contesting Turkish regional dominance. The coordination is not about managing the site itself, but about maintaining a unified normative position against Turkish sovereignty.
% TRANSFER_FUNCTION: Transfers legitimacy: from Turkish state (which would lose international recognition of exclusive stewardship) to the Greek state and Orthodox diaspora (which gain symbolic capital and negotiating leverage). No material goods change hands unless restitution succeeds, which has near-zero probability. The extraction is almost entirely symbolic and diplomatic.
% ABSENT_VOICES: Turkish Muslims currently worshipping at the site are partially heard (they testified about interrupted worship), but their interest in continuity at the specific site is subordinated to the broader Turkish sovereignty claim and is not independently amplified. Secular Turkish nationalists who view the site as Turkish national territory regardless of religious function are present but not as designated seats. Catholics and Protestants, while they have stakes in Christian heritage narratives, are not party to this specific Orthodox restitution claim.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution claim disappeared, Greek-Turkish diplomatic tension would persist but lose one major rhetorical instrument; Orthodox diaspora identity would be diminished but not severed (other identity anchors exist); Turkish sovereignty would be uncontested and unchallenged on this particular front. The world would rearrange in diplomatic relationships and symbolic authority but NOT in material control of the site (Turkey would retain it). The 'contested' verdict reflects that the disappearance question itself is part of the reading's structure: whether the claim should disappear is the very dispute the reading is engaged in.
% FOUNDING_PROBLEM: The founding problem is the question of historical legitimacy and rightful ownership after 573 years of interrupted Orthodox worship and continuous Islamic function: How should a site founded as a Christian cathedral, conquered and repurposed as a mosque, secularized as a museum, and then returned to Islamic worship be legitimately governed? The Orthodox reading's answer: historical origin trumps centuries of occupation; Orthodoxy should be restored or the site should be neutral (secular/universal).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — how to legitimize the site's governance after the Ottoman conquest — was live in the 19th–20th centuries when European powers challenged Ottoman sovereignty and Greek independence movements claimed the site. It is now effectively dead as a practical governance question: Turkey has uncontested administrative control, the international order has recognized Turkish sovereignty, and the 2020 return to Islamic worship resolved the secular museum status. The founding problem persists only as a symbolic/identity question, not as a live institutional governance challenge. Secular Turkish scholars and international heritage bodies attest that the practical governance question is settled; only diaspora actors and the Greek state maintain the restitution claim as live, treating it as an identity and diplomatic matter rather than a governance problem.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as tangled_rope (coordination function: unify diaspora and Greek state around the restitution claim; extraction function: delegitimize Turkish sovereignty and threaten Islamic worship continuity) yet the metrics suggest it is substantially theatrical — theater_ratio of 0.71 and low suppression (0.22) point toward piton dynamics (inertial performance maintaining a defunct function). The measurement series shows extractiveness rising from 0.35 to 0.51 (t=0 to t=50) then declining to 0.48, suggesting a peak around the 2020 return of Islamic worship to Hagia Sophia (when the restitution claim gained renewed publicity) followed by stabilization. Theater_ratio remains high and nearly flat, indicating the constraint's function is increasingly symbolic performance rather than material enforcement. Suppression_requirement stays low because Turkey does not need heavy enforcement to maintain the status quo — possession and international recognition of Turkish sovereignty do the work; the constraint is a diplomatic complaint rather than a threat requiring crushing. The claim is tangled rope because there is genuine coordination (of the diaspora and Greek state) alongside extraction (cost imposed on Turkish sovereignty and Islamic continuity), and the claim does require active enforcement (diplomatic assertion, cultural campaigns, identity maintenance in diaspora communities). But the high theater ratio and low suppression suggest the classification should be monitored for piton drift — if the founding problem (how to legitimize the site after the conquest) is dead, the constraint may be persisting as performance inertia rather than as genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   The Ottoman/Turkish reading and the Orthodox restitution reading compute radically different types from the same physical site because they instantiate incompatible legitimacy frameworks. From Turkey's position, the site is legitimately Turkish Islamic property by conquest, stewardship, and international recognition — a mountain of geopolitical fact (Turkish control is not contingent or enforced; it is the baseline reality). From the Orthodox diaspora's position, the constraint is a real extraction imposed on their collective identity and historical narrative — a snare-like delegitimization mechanism sustained by Turkish state enforcement of territorial control. The Greek state occupies an intermediate position: it uses the restitution claim as a rope (coordinating Greek interests in regional leverage) and as a tool to extract diplomatic concessions from Turkey. These divergent computations are irreducible because they root in different kernel readings — different foundational assumptions about what legitimacy means for the site.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eastern Orthodox diaspora benefits symbolically (the claim vindicates their historical narrative and identity); directionality is low (d ≈ 0.15–0.25), pulling toward the beneficiary end despite the claim's non-material nature. The Greek state benefits diplomatically (leverage in negotiations) and also sets the agenda (articulates the claim publicly); directionality is near 0.5 (symmetric actor with dual benefit and risk). Turkish sovereignty bears the cost of delegitimization and faces pressure to concede or defend; directionality is high (d ≈ 0.75–0.85), pulling toward the target end, though the actual enforcement load is low. Islamic worship continuity bears the existential and legitimacy cost (risk of again-interrupted worship); directionality is high (d ≈ 0.7–0.8). The measurement of directionality assumes the Orthodox restitution reading as the frame — different readings would swap these assignments (the Islamic sovereignty reading would make Orthodoxy the high-d target and Islamic continuity the low-d beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to legitimize the site after conquest — is dead (live only as identity/symbol, not as a live governance question). The constraint persists because the Orthodox diaspora and Greek state have extracted symbolic and diplomatic value from the restitution claim, turning a settled governance question into an ongoing political pressure point. This is the signature of mandatrophy: the founding problem's death (Turkish sovereignty is uncontested; the site is functioning under clear Islamic authority) should have dissolved the constraint, but instead it has metamorphosed into theatrical maintenance — cultural campaigns, diaspora identity claims, diplomatic rhetoric — that keeps the claim alive without proposing any realistic mechanism for change. The high theater_ratio (0.71) and the measurement trend (rising extractiveness followed by stabilization at a moderate level) confirm this pattern: the constraint is inertially maintained through performance, not through genuine institutional need. However, the extraction is not zero: the claim imposes real diplomatic and symbolic costs on Turkey and Islam, and it provides real identity-affirming benefits to the diaspora. Whether this qualifies as extraction-in-service-of-a-dead-cause (piton) or as genuine ongoing coordination (tangled rope) depends on whether one judges the identity/diplomatic benefits to be genuine social functions or mere theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Hagia Sophia substrate kernel is structurally binding: the Orthodox restitution reading, the Islamic sovereignty reading instantiated by Turkish state control, or the universal heritage reading that transcends all religious and national claims?',
    'No mechanism can fully resolve this because the readings embody incompatible value frameworks (historical legitimacy vs. conquest-and-occupation legitimacy vs. transcendence-of-all-claims). Resolution would require one reading to be adopted as the canonical frame by an authority with enforcement power (international court, regional hegemon, or consensus among all parties). None exists.',
    'This reading (Orthodox restitution) assumes that historical founding and religious identity are the binding frame for legitimacy. If the Islamic sovereignty reading is adopted as binding, this reading''s core premise is foreclosed — the site becomes legitimately Islamic by virtue of 573 years of stewardship and current control. If the universal heritage reading is adopted, both religious readings lose their claim-ground — legitimacy derives from human cultural heritage, not religious ownership.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading is canonical; the contest is philosophical and political, not empirical.').

omega_variable(
    symbolic_vs_material_extraction,
    'Is the measured extractiveness (0.48) primarily symbolic/diplomatic (the claim extracts legitimacy cost from Turkey and provides symbolic benefit to the diaspora and Greek state) or does it conceal material extraction mechanisms not yet manifest?',
    'If restitution is pursued as a serious diplomatic lever, material extraction mechanisms would emerge: Turkey would pay diplomatic or economic concessions to defend its sovereignty claim, or Greece would gain leverage to extract territorial or maritime concessions elsewhere. If the claim remains symbolic forever, the material extraction remains zero and the measured ε is purely a diplomatic/symbolic phenomenon.',
    'If material extraction is latent, the classification should account for future extraction potential; if purely symbolic, the constraint is better understood as a piton (mostly theater, no real extraction) than a tangled rope. The measurement series shows theater_ratio > 0.6 at all points — high theater is consistent with piton dynamics, not tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Whether extractiveness is symbolic leverage or latent material extraction.').

omega_variable(
    sibling_reading_mutual_foreclosure,
    'Do the Islamic sovereignty reading and the Orthodox restitution reading logically foreclose each other, or do they coexist as live positions held by different parties without internal logical contradiction within each party''s framework?',
    'Test by asking: Can a single actor (e.g., the Turkish state, or the international community) coherently hold both that the site''s legitimacy derives from Ottoman conquest AND that it derives from Christian origins simultaneously? Answer: No — the readings provide incompatible legitimacy narratives. Within a single framework, both cannot be true. But Turkey holds the sovereignty reading and Greece holds the restitution reading — they are held by different actors without requiring either to admit the other''s premise.',
    'If the readings foreclose each other, the cs_structure.reading_relations should declare ''forecloses''; if they coexist without logical contradiction at the inter-party level, declare ''coexists_with''. The distinction determines whether the kernel is genuinely contested (multiple live readings) or dialectically structured (one reading denies the other).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_mutual_foreclosure, conceptual, 'Logical structure of the kernel''s sibling readings.').

omega_variable(
    identity_lock_mechanism_orthodox,
    'For the Eastern Orthodox diaspora, is their stake in the restitution claim structurally locked by religious/civilizational identity (identity_locked exit), or could they exit the claim if material circumstances changed?',
    'Test by asking: If Hagia Sophia were returned to Orthodox control tomorrow, would the diaspora''s identity be affirmed and the claim''s work be done (identity-lock case)? Or if it remained under Turkish Islamic control permanently, would the diaspora''s religious identity be damaged or invalidated (evidence of lock)? Post-exit trajectory: if the claim disappeared, would Orthodox believers experience it as loss of self (locked) or as a political setback unrelated to faith (not locked)?',
    'If identity_locked is accurate, the Orthodox diaspora''s exit_options are genuinely trapped/identity_locked, not merely constrained. This affects the directionality computation: identity-locked targets experience higher effective extraction because the constraint binds their self-concept, not just their external options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_orthodox, empirical, 'Mechanism binding Orthodox diaspora to the restitution claim.').

omega_variable(
    competing_kernel_reading_sibling_identity,
    'This story instantiates the Orthodox restitution reading of the hagia_sophia_substrate kernel. Are the sibling readings (islamic_sovereignty_reading, universal_heritage_reading) authored as separate constraint stories linked via network.affects_constraints, or are they left external to this generation cohort?',
    'Corpus inspection: check whether the sibling readings have been authored as separate JSON constraint stories in the same corpus. If yes, this story''s cs_structure.reading_relations and network.affects_constraints establish the kernel family. If no (siblings are not yet authored), note the absence and declare the links as ''pending_sibling_stories''.',
    'If siblings are authored, the kernel family can be analyzed for consistency (do the three readings form a coherent contest, or do they contradict each other''s structural premises?). If not, this reading remains a singleton in its kernel family, and the family structure is incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_kernel_reading_sibling_identity, empirical, 'Whether sibling readings have been authored as separate constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement_basis(hagi_tr_t0, observed).
narrative_ontology:measurement(hagi_tr_t16, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 16, 0.66).
narrative_ontology:measurement_basis(hagi_tr_t16, observed).
narrative_ontology:measurement(hagi_tr_t33, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 33, 0.69).
narrative_ontology:measurement_basis(hagi_tr_t33, observed).
narrative_ontology:measurement(hagi_tr_t50, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 50, 0.71).
narrative_ontology:measurement_basis(hagi_tr_t50, observed).
narrative_ontology:measurement(hagi_tr_t67, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 67, 0.72).
narrative_ontology:measurement_basis(hagi_tr_t67, projected).
narrative_ontology:measurement(hagi_tr_t100, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 100, 0.71).
narrative_ontology:measurement_basis(hagi_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hagi_be_t0, observed).
narrative_ontology:measurement(hagi_be_t16, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(hagi_be_t16, observed).
narrative_ontology:measurement(hagi_be_t33, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 33, 0.48).
narrative_ontology:measurement_basis(hagi_be_t33, observed).
narrative_ontology:measurement(hagi_be_t50, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement_basis(hagi_be_t50, observed).
narrative_ontology:measurement(hagi_be_t67, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 67, 0.48).
narrative_ontology:measurement_basis(hagi_be_t67, projected).
narrative_ontology:measurement(hagi_be_t100, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement_basis(hagi_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(hagi_su_t0, observed).
narrative_ontology:measurement(hagi_su_t16, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 16, 0.19).
narrative_ontology:measurement_basis(hagi_su_t16, observed).
narrative_ontology:measurement(hagi_su_t33, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 33, 0.21).
narrative_ontology:measurement_basis(hagi_su_t33, observed).
narrative_ontology:measurement(hagi_su_t50, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 50, 0.23).
narrative_ontology:measurement_basis(hagi_su_t50, observed).
narrative_ontology:measurement(hagi_su_t67, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 67, 0.24).
narrative_ontology:measurement_basis(hagi_su_t67, projected).
narrative_ontology:measurement(hagi_su_t100, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(hagi_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel decomposes into three constraint stories: this Orthodox restitution reading, the Islamic sovereignty reading (Turkish state perspective), and the universal heritage reading (transnational heritage perspective). Each story has a distinct ε, beneficiary/victim structure, and classification. They are linked via network.affects_constraints and share the same kernel_id in cs_structure. The readings are sibling constraints instantiating different normative framings of the same site's legitimacy — not different observables of a single constraint (per the ε-invariance principle), but different readings of an ambiguous kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
