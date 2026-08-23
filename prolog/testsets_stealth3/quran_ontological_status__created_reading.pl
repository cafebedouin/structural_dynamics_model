% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Created-Qur'an Interpretive Regime (Mu'tazilite Reading)
 *   domain: religious/doctrinal/political
 *
 * SUMMARY:
 *   In ninth-century Baghdad and Basra, the teaching that the Qur'an is
 *   created speech (makhlūq) functioned as an interpretive constitution: it
 *   relocated the text from the category of coeternal divine fixture to that
 *   of a produced artifact whose meanings must answer to rational scrutiny,
 *   and it redistributed hermeneutic authority accordingly. The regime had a
 *   genuine coordination function — it preserved divine transcendence against
 *   anthropomorphism and gave the scholarly world a workable protocol for
 *   talking about God's speech — while simultaneously transferring offices,
 *   curriculum control, and public standing from transmission-based jurists
 *   to dialecticians aligned with the court. Historically its community-wide
 *   persistence proved enforcement-dependent: when compulsion ceased in 848
 *   the official position reversed, and the doctrine survived in
 *   self-selecting schools. Per the epsilon-invariance principle, this file
 *   authors ONLY the created_reading as a clean constraint; the inquisition
 *   belongs to state_enforced_creation_reading and the coeternity claim to
 *   uncreated_reading, each with its own epsilon, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   mu_tazilite_rationalist_theologians: Primary beneficiary
 *   (institutional/constrained) — collects hermeneutic authority and helps
 *   administer the regime - abbasid_caliphal_court: Agenda setter
 *   (institutional/arbitrage) — proclaims, appoints, and reverses at low cost
 *   - traditionist_jurists: Primary target (organized/identity_locked) —
 *   authority derives from textual fixity -
 *   literalist_devotional_communities: Secondary target
 *   (powerless/identity_locked) - philosophical_scholars: Secondary
 *   beneficiary (moderate/mobile) - conforming_middle_jurists: Ambivalent
 *   middle (moderate/constrained) - popular_preachers_and_storytellers:
 *   Excluded voice (powerless/constrained) - doctrine_historians: Analytical
 *   observer — sees the full structure
 *
 * KEY AGENTS:
 *   - mu_tazilite_rationalist_theologians: Primary beneficiary (institutional/constrained) — collects hermeneutic authority, staffs the regime's posts, doubly seated as administrator
 *   - abbasid_caliphal_court: Agenda setter (institutional/arbitrage) — proclaims the doctrine, appoints sympathetic judges, reverses cheaply when politics shift
 *   - traditionist_jurists: Primary target (organized/identity_locked) — transmission-based authority displaced by the regime
 *   - literalist_devotional_communities: Secondary target (powerless/identity_locked) — devotional identity unsettled without any seat
 *   - philosophical_scholars: Secondary beneficiary (moderate/mobile) — argumentative space cleared, no existential exposure
 *   - conforming_middle_jurists: Dual-positioned middle (moderate/constrained) — pays integrity cost, keeps livelihood
 *   - popular_preachers_and_storytellers: Excluded voice (powerless/constrained) — their idiom is the regime's named error, formulated without them
 *   - doctrine_historians: Analytical observer (analytical/analytical) — compares all three kernel readings side by side
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.46).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.24).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Created-Qur'an Interpretive Regime (Mu'tazilite Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "religious/doctrinal/political").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '42a37cc6-71da-4b16-a9e4-f2ade4773cba').
narrative_ontology:cs_kernel_codification('42a37cc6-71da-4b16-a9e4-f2ade4773cba', fixed_text).
narrative_ontology:cs_authority_grounding('42a37cc6-71da-4b16-a9e4-f2ade4773cba', expertise).
narrative_ontology:cs_interpretation_layer_present('42a37cc6-71da-4b16-a9e4-f2ade4773cba').
narrative_ontology:cs_reading_relation('42a37cc6-71da-4b16-a9e4-f2ade4773cba', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('42a37cc6-71da-4b16-a9e4-f2ade4773cba', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('42a37cc6-71da-4b16-a9e4-f2ade4773cba', foundational, divine_unity_forbids_coeternal_scripture).
narrative_ontology:cs_axiom_status(divine_unity_forbids_coeternal_scripture, holdable).
narrative_ontology:cs_axiom_grounding('42a37cc6-71da-4b16-a9e4-f2ade4773cba', divine_unity_forbids_coeternal_scripture, theological).
narrative_ontology:cs_axiom('42a37cc6-71da-4b16-a9e4-f2ade4773cba', foundational, reason_adjudicates_revelation_meaning).
narrative_ontology:cs_axiom_status(reason_adjudicates_revelation_meaning, holdable).
narrative_ontology:cs_axiom_grounding('42a37cc6-71da-4b16-a9e4-f2ade4773cba', reason_adjudicates_revelation_meaning, instrumental).
narrative_ontology:cs_reference_frame('42a37cc6-71da-4b16-a9e4-f2ade4773cba', created_text_under_rational_adjudication).
narrative_ontology:cs_drift_state('42a37cc6-71da-4b16-a9e4-f2ade4773cba', post_mihna_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('42a37cc6-71da-4b16-a9e4-f2ade4773cba', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, mu_tazilite_rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_scholars).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, abbasid_caliphal_court).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_devotional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, conforming_middle_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, conforming_middle_jurists).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_unity_excludes_coeternal_partner).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, ethical_rationalism_prior_to_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and elaborate the doctrine that revealed speech originates in time and that sound meanings must survive rational scrutiny. Through court ties they staff judicial and teaching posts, set examination expectations, and collect prestige, students, and institutional placement. When patronage withdraws they continue in Basran and Baghdadi study circles; abandoning their method would mean discarding a lifetime's dialectical training, so most remain within the school even while marginalized.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, mu_tazilite_rationalist_theologians, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, mu_tazilite_rationalist_theologians, agenda_setter).

% Proclaims doctrinal positions by decree, appoints judges sympathetic to the created-speech teaching, and uses doctrinal uniformity to assert final arbitration over religious knowledge. Gains obedience and a lever against independent jurists. When dynastic politics shift, reverses course at negligible cost: the position is an instrument, not a possession.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, abbasid_caliphal_court, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, abbasid_caliphal_court, beneficiary).

% Work adjacent demonstrations concerning divine simplicity and the temporality of creation. The doctrine clears argumentative space by denying the text a status that would override reason, and they benefit through shared methods and protected inquiry. They can relocate, translate, or reframe their work without existential loss.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_scholars, beneficiary,
    moderate, generational, mobile, continental).

% Hold authority through chains of transmission reaching the Prophet's contemporaries and through the text's fixed, verbatim standing. The doctrine recasts the book they transmit as a produced artifact and hands adjudication of its meaning to dialectical specialists, costing them appointments, curriculum standing, and public deference. Accepting the doctrine would dissolve the ground they stand on; refusing costs them office and court access. In the years when the court compelled conformity, open refusal brought imprisonment and flogging — that coercive apparatus is the subject of the sibling story, not this one. They organize through mosque networks and popular followings that outlast the court's enthusiasm.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionist_jurists, payer,
    organized, generational, identity_locked, continental).

% Ordinary worshippers whose piety centers on recitation understood as God's own address to them. The doctrine implies the words they memorize are a produced thing rather than God's unmediated speech, unsettling the intimacy of their devotion. They hold no institutional seat; they carry the doctrine's costs as confusion and as diminished standing for the teachers they trust, and their attachment runs through practice and belonging rather than through any exit they could take.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_devotional_communities, payer,
    powerless, generational, identity_locked, continental).

% Serve as judges and notaries across provincial towns. Under pressure they give the answers that keep their posts while continuing privately to teach transmission-based authority. They pay an integrity cost and simultaneously retain a livelihood the regime administers — burdened and accommodated at once.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, conforming_middle_jurists, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, conforming_middle_jurists, beneficiary).

% Draw authority from vivid narrations of divine speech and action, often in anthropomorphic idiom. The doctrinal program treats their idiom as precisely the error it exists to correct, yet they are never consulted in its formulation and lose pulpit credibility wherever examiners police content.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, popular_preachers_and_storytellers, excluded,
    powerless, biographical, constrained, regional).

% Reconstruct the controversy from chronicles, trial records, and school lineages. They hold no stake in either ontological claim and can set the created, uncreated, and enforcement-centered accounts side by side.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, doctrine_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, mu_tazilite_rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared metaphysical frame in which divine unity survives God's speaking in time: scripture is a produced artifact whose meanings must withstand rational scrutiny, giving scholars a common protocol for adjudicating attribute-language and legal meaning without positing a coeternal text.
% TRANSFER_FUNCTION: Moves interpretive authority, together with the offices, stipends, and curricular control attached to it, away from holders of chain-of-transmission textual authority and toward reason-trained dialectical theologians aligned with court patronage.
% ABSENT_VOICES: Popular preachers and oral storytellers are the regime's standing counterexample yet had no seat in its formulation; non-elite women's devotional circles likewise had none. Dissenting jurists were present in the arena but outmaneuvered institutionally rather than persuaded — unanimity at court reflected appointment leverage, not consensus.
% DISAPPEARANCE_RATIONALE: Without the created-speech regime, judicial appointments and curriculum standards keyed to dialectical competence lapse back toward transmission-based criteria, the rationalist schools lose their institutional rationale, and the settlement between court and jurists renegotiates along the lines the traditionist restoration in fact followed.
% FOUNDING_PROBLEM: Ninth-century monotheism faced a double scandal: vivid scriptural descriptions of God appeared to compromise His transcendence, while treating the Qur'an as coeternal appeared to install a second eternal beside Him — and the caliph needed a doctrinal standard he could enforce uniformly across a fractious religious establishment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: opponent traditionists conceded the anthropomorphism problem was real — their objection targeted the solution, not the problem — and the later Ash'arite and Maturidi syntheses, built by heirs of the opponents, rest on distinguishing eternal interior speech from created recitation, confirming the problem while rejecting the purely-created remedy. No party claims the problem never existed.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.46: the regime's asymmetry is constitutive rather than incidental — locating God above the text necessarily moves adjudication from text-holders to reason-holders — but the extracted good is authority and standing, not material wealth, and the regime's own lights count much of the transfer as due correction. Suppression 0.24: the doctrinal regime itself ran on patronage gating (appointments, examinations, curriculum) rather than coercion; the flogging apparatus of 833-848 belongs to the enforcement sibling, and this story's suppression series traces only the regime's own gating capacity rising with court favor and decaying after. Theater ratio 0.12: the doctrine was functionally load-bearing — attribute language, legal methodology, and school identity all depended on it — with little performative maintenance. Accessibility collapse 0.25: the uncreated rival remained fully articulable throughout and ultimately prevailed; alternatives never closed. Resistance 0.72: sustained, organized, and eventually victorious opposition. The claimed type is tangled_rope from structure — real coordination function plus constitutive asymmetric transfer plus demonstrated enforcement dependence (requires_active_enforcement true, justified by the observed reversal once enforcement lapsed) — authored independently of the metrics; where the engine's computed type diverges, that divergence is the datum. The measurement series run on one shared seven-point grid (21 entries) so no metric borrows another's end-state. One FNL alert on the identity_coordination typing: the regime's orthodox-versus-anthropomorphist boundary language could dress extraction as belonging; the coupling worth watching concentrates costs on powerless agents (literalist communities) at continental scope, which the complexity offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   Three seats should compute differently from identical structural data. From the beneficiary seat (Mu'tazilite theologians) the regime presents as necessary defense of divine unity — coordination they are proud to maintain. From the identity-locked payer seat (traditionist jurists) the same structure presents as usurpation: the ground of their authority reclassified out from under them, with exit priced as self-dissolution. From the caliphal seat the doctrine is an instrument — held while useful, discarded cheaply. The engine derives these divergent per-seat classifications from the declared roles, exits, and directionalities; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the theologian, philosopher, and court seats toward the low-directionality (subsidized) end; victim declarations drive traditionist jurists and literalist communities toward the target end, with identity_lock pushing them further toward full-target than mobile payers would sit. The court is unusual: an agenda_setter that also collects (legitimation, leverage) and pays almost nothing, so its derived directionality sits near the beneficiary end despite running the regime. No directionality_overrides are authored: the derivation from declared beneficiaries, victims, and exit options reproduces the structural relationships faithfully. The one known distortion is conforming_middle_jurists — declared payers, so derived near the target end, when their realized position is mixed; the distortion is accepted rather than overridden because any override keyed to their power atom (moderate) would also distort the genuinely subsidized philosophical scholars who share that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The decomposition prevents symmetric mislabelings. Read naively as a timeless truth-claim, the doctrine masquerades as bedrock — a proposition about God that no one chose; naming its beneficiaries, victims, and enforcement dependence exposes the constructed regime underneath. Read cynically as mere rent-collection, it erases a real coordination achievement: the transcendence problem it addressed was real, conceded even by its opponents, and demanded some settlement. The founding problem's status is authored contested rather than dead — the underlying tension resurfaces whenever communities confront divine predication — so mandatrophy_resolved is deliberately left undeclared, and the R5 mismatch consumer sees status=contested paired with verdict=world_rearranges, which does not trip the zombie flag while still recording that the arrangement outruns any single settled mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_referent_fix,
    'Is this constraint correctly scoped to the created_reading alone — the interpretive regime treating revelation as created speech — excluding the coercive apparatus of the state_enforced_creation_reading and the coeternity claim of the uncreated_reading?',
    'Cross-file referent audit of the kernel family: each sibling story authors its own epsilon over its own standing arrangement (this file: the interpretive regime; the enforcement sibling: the inquisition machinery; the uncreated sibling: revelation-as-ontic-fixture). Mis-scoping is detected when a story''s beneficiary/victim sets or enforcement data duplicate a sibling''s.',
    'If scoping fails, epsilon values merge across readings and per-reading classification comparison becomes invalid; the family decomposition that disambiguates the colloquial label ''status of the Qur''an'' collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_referent_fix, conceptual, 'Committer-structure routing: this story is one reading of kernel quran_ontological_status; sibling readings instantiate different constraints with different epsilon.').

omega_variable(
    enforcement_necessity_counterfactual,
    'Could the created-speech interpretive regime have held community-wide without the state coercion exercised during the 833-848 enforcement window, or was active enforcement a necessary condition of its persistence?',
    'Counterfactual analysis anchored on the observed reversal: enforcement ceased in 848 and the official position reversed within years, while the doctrinal position survived only in self-selecting study circles. Compare with doctrinal regimes that held by persuasion alone.',
    'If coercion was necessary for community-wide persistence, this story''s suppression figure understates the regime''s enforcement dependence and the boundary with the enforcement sibling blurs; if not, the regime approaches a coordination arrangement with incidental asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_necessity_counterfactual, empirical, 'Whether the regime''s persistence structurally required the coercion documented under the sibling story.').

omega_variable(
    founding_problem_liveness,
    'Is the transcendence problem this doctrine answered — how God speaks in time without anthropomorphism or a coeternal partner — still live, or dissolved by competing solutions such as the eternal-interior-speech / created-recitation distinction?',
    'Survey whether contemporary theological and philosophical treatments of divine speech still face the underlying problem or treat it as settled by the competing synthesis.',
    'If the problem is dissolved, the regime''s remaining operation is inertia plus enclave identity maintenance, shifting weight toward degraded-persistence readings; if live, the coordination function retains substance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, conceptual, 'Liveness of the founding problem behind the created-speech doctrine.').

omega_variable(
    authority_transfer_valence,
    'Is the transfer of interpretive authority from transmission-holders to reason-specialists best described as extraction (appropriating a positional good) or as legitimate reallocation toward demonstrated competence?',
    'Evaluate the regime''s own admission criteria: did dialectical training demonstrably produce better adjudication of scriptural meaning, or did office track patronage alignment independent of competence?',
    'If reallocation tracks competence, much of the measured asymmetry is coordination cost rather than overhead; if office tracked patronage, the asymmetry is rent-taking and effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_transfer_valence, preference, 'Valence of the hermeneutic authority transfer at the heart of the regime.').

omega_variable(
    identity_lock_plasticity,
    'Would sustained rationalist engagement dissolve the identity-lock of literalist devotional communities and traditionist jurists, or entrench it?',
    'Longitudinal comparison of communities exposed to sustained dialectical engagement versus isolated ones: measure movement across the doctrinal boundary over generations.',
    'If lock dissolves, the payer seats'' directionality softens over time and the regime''s asymmetry decays; if it entrenches, the asymmetry hardens into durable factional structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_plasticity, empirical, 'Plasticity of identity-lock among the regime''s payer populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qos_created_reading_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qos_created_reading_tr_t10, quran_ontological_status__created_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(qos_created_reading_tr_t20, quran_ontological_status__created_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(qos_created_reading_tr_t35, quran_ontological_status__created_reading, theater_ratio, 35, 0.16).
narrative_ontology:measurement(qos_created_reading_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(qos_created_reading_tr_t75, quran_ontological_status__created_reading, theater_ratio, 75, 0.13).
narrative_ontology:measurement(qos_created_reading_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(qos_created_reading_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(qos_created_reading_be_t10, quran_ontological_status__created_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(qos_created_reading_be_t20, quran_ontological_status__created_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(qos_created_reading_be_t35, quran_ontological_status__created_reading, base_extractiveness, 35, 0.56).
narrative_ontology:measurement(qos_created_reading_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(qos_created_reading_be_t75, quran_ontological_status__created_reading, base_extractiveness, 75, 0.47).
narrative_ontology:measurement(qos_created_reading_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(qos_created_reading_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement(qos_created_reading_su_t10, quran_ontological_status__created_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(qos_created_reading_su_t20, quran_ontological_status__created_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(qos_created_reading_su_t35, quran_ontological_status__created_reading, suppression_requirement, 35, 0.36).
narrative_ontology:measurement(qos_created_reading_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(qos_created_reading_su_t75, quran_ontological_status__created_reading, suppression_requirement, 75, 0.26).
narrative_ontology:measurement(qos_created_reading_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel quran_ontological_status (epsilon-invariance decomposition). The colloquial label 'the status of the Qur'an' covers three structurally distinct claims: (1) THIS story, the created_reading — the interpretive regime in which revelation is a produced artifact under rational adjudication; moderate epsilon over an authority-redistribution arrangement; (2) state_enforced_creation_reading — the inquisition machinery that compelled conformity; substantially higher epsilon, victims defined by coercion suffered; (3) uncreated_reading — revelation as coeternal fixture; a candidate mountain-shaped claim with its own beneficiary/victim structure. The upstream metaphysical thesis (this file) supplied the content the enforcement sibling operationalized, hence the influences edge; the created and uncreated claims are direct contradictions, hence the forecloses edge. Epsilon differs across the family by construction; comparing classifications without referent alignment is invalid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
