% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Created-Qur'an Doctrine: Revelation as Temporal Divine Speech (Mu'tazilite Reading)
 *   domain: religious/political/intellectual
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   quran_ontological_status: the created reading (makhlūq), under which the
 *   Qur'an is temporal divine speech and God's essence transcends every
 *   temporal artifact, revelation included. The standing arrangement under
 *   contest — the ε referent, fixed per the kernel-reading rule — is the
 *   doctrinal-hermeneutic regime this reading establishes: a coordination
 *   structure for rational theology that simultaneously transfers
 *   interpretive authority away from textual-fixity holders. Per the
 *   ε-invariance principle, the colloquial label 'the Qur'an's ontological
 *   status' decomposes into three structurally distinct constraints (this
 *   reading, the uncreated reading, the state-enforced creation reading),
 *   each with its own ε, victim set, and classification; this file authors
 *   only the first. The claim/metric gap is deliberate: the arrangement is
 *   CLAIMED as tangled_rope because one doctrinal claim performs both a
 *   genuine coordination function (securing coherent strict monotheism) and
 *   asymmetric extraction (devaluing traditionalist authority through the
 *   same move), while the authored metrics describe its actual operation
 *   across a rise-and-collapse lifecycle — the engine measures the
 *   divergence; the claim is not tuned to the metrics.
 *
 * KEY AGENTS:
 *   - - rationalist_kalam_theologians: Agenda-setting beneficiary (organized/mobile) — authors and administers the doctrine, collects hermeneutic primacy and institutional posts
 *   - - abbasid_court_patrons: Beneficiary with agenda-setting power (institutional/arbitrage) — adopts the doctrine as an instrument of centralizing reform, exits by redirecting patronage
 *   - - traditionalist_jurists: Primary target (organized/constrained) — bears loss of appointments, standing, and epistemic authority
 *   - - literalist_devotional_communities: Primary target (powerless/identity_locked) — bears the identity cost of the recited word's demotion
 *   - - popular_reciters_qurra: Excluded party (moderate/constrained) — lives from the performed text, holds no seat in the dispute
 *   - - dogmatic_history_scholars: Analytical observer (analytical/analytical) — sees the full structure from outside all commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.38).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.22).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Created-Qur'an Doctrine: Revelation as Temporal Divine Speech (Mu'tazilite Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "religious/political/intellectual").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '0c640176-b11e-417f-8bcd-5dc56f07ae19').
narrative_ontology:cs_kernel_codification('0c640176-b11e-417f-8bcd-5dc56f07ae19', formalized).
narrative_ontology:cs_authority_grounding('0c640176-b11e-417f-8bcd-5dc56f07ae19', expertise).
narrative_ontology:cs_interpretation_layer_present('0c640176-b11e-417f-8bcd-5dc56f07ae19').
narrative_ontology:cs_reading_relation('0c640176-b11e-417f-8bcd-5dc56f07ae19', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('0c640176-b11e-417f-8bcd-5dc56f07ae19', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('0c640176-b11e-417f-8bcd-5dc56f07ae19', foundational, divine_essence_transcends_temporal_artifacts).
narrative_ontology:cs_axiom_status(divine_essence_transcends_temporal_artifacts, holdable).
narrative_ontology:cs_axiom_grounding('0c640176-b11e-417f-8bcd-5dc56f07ae19', divine_essence_transcends_temporal_artifacts, deontological).
narrative_ontology:cs_axiom('0c640176-b11e-417f-8bcd-5dc56f07ae19', secondary, reason_adjudicates_scriptural_meaning).
narrative_ontology:cs_axiom_status(reason_adjudicates_scriptural_meaning, holdable).
narrative_ontology:cs_axiom_grounding('0c640176-b11e-417f-8bcd-5dc56f07ae19', reason_adjudicates_scriptural_meaning, instrumental).
narrative_ontology:cs_reference_frame('0c640176-b11e-417f-8bcd-5dc56f07ae19', strict_tawhid_temporal_revelation_frame).
narrative_ontology:cs_drift_state('0c640176-b11e-417f-8bcd-5dc56f07ae19', post_mihna_sunni_settlement, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0c640176-b11e-417f-8bcd-5dc56f07ae19', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_kalam_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, abbasid_court_patrons).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_devotional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Basran and Baghdadian dialectical theologians who developed the createdness position, argued it in public assemblies and court disputations, and staffed judicial and advisory posts under sympathetic caliphs. Their teaching circles, stipends, and appointment prospects rose and fell with court favor. They could relocate between Baghdad, Basra, and provincial courts, and their school continuity ran through master-disciple chains rather than any single patron.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_kalam_theologians, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, rationalist_kalam_theologians, beneficiary).

% The caliphal circle and secretarial elites who adopted the createdness position as part of a centralizing program: proclaiming it in 827, appointing allied jurists to the judiciary, and testing officials' doctrinal allegiance. The doctrine gave them a standard of orthodoxy independent of the popular hadith party. When the political calculus shifted after 847, they redirected patronage to the traditionalists within a few years, demonstrating that their commitment ran through policy, not conviction.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, abbasid_court_patrons, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, abbasid_court_patrons, agenda_setter).

% Hadith scholars and jurisprudents whose standing rested on transmitting and applying a fixed revealed text. Under the doctrine's ascendancy they lost judicial appointments, faced loyalty examinations, and saw their epistemology publicly demoted from guardian of God's speech to handler of a creaturely artifact. They kept parallel teaching networks alive in mosques and private gatherings, absorbed persecution episodes (most famously the ordeal of Ahmad ibn Hanbal), and could not abandon their method without dissolving the authority their whole careers were built on.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    organized, generational, constrained, continental).

% Pious laypeople and preacher-led congregations whose worship centers on reciting the text as God's own unmediated speech. The doctrine asked them to accept that what they recite in prayer is a created thing. Their devotional life, memorization practice, and self-understanding are constituted through the recited word's divine status; adopting the rationalist frame would unravel that identity rather than add a belief to it, so they carried the dispute as a wound borne collectively rather than a position individually chosen.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_devotional_communities, payer,
    powerless, generational, identity_locked, continental).

% Professional reciters and mosque instructors whose livelihood and prestige rest on the performed text. They had no seat in the dialectical assemblies where the doctrine was argued and settled; they learned outcomes through appointment changes, salary shifts, and congregational politics. Their objection — that the recited word they live by is God's own speech — entered the record only indirectly, through the traditionalist champions who spoke for them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, popular_reciters_qurra, excluded,
    moderate, biographical, constrained, regional).

% Later historians and comparativists who reconstruct the dispute from creeds, polemics, inquisition records, and court chronicles. They hold no position in the arrangement, collect nothing from it, and assess it from outside all parties' commitments, including the reading instantiated in this story.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, dogmatic_history_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_kalam_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a monotheism-coherence problem for the scholarly community: a single doctrine of God under which no second eternal exists alongside Him and divine speech is not confused with divine essence, plus a shared standard for who may interpret scripture and by what method.
% TRANSFER_FUNCTION: Moves interpretive authority — and the appointments, stipends, and public standing attached to it — from holders of textual-fixity authority (hadith-based jurists, reciter guilds, devotional elites) to practitioners of rational theology; moves doctrinal allegiance and courtly resources toward the rationalist schools.
% ABSENT_VOICES: The popular reciters and ordinary devotional communities had no seat in the kalam disputations where the doctrine was settled; their objection was voiced only through traditionalist jurist champions. Had they been present, the unanimity of elite rationalist consensus would have been contested from the start.
% DISAPPEARANCE_RATIONALE: If the createdness doctrine and its institutional support vanished overnight, interpretive authority would flow back to textual-fixity holders, court appointments would shift to the hadith party, and the rationalist schools would lose their patronage base — approximately what actually happened after 847, when the arrangement was withdrawn and the Sunni mainstream reorganized around the uncreated reading.
% FOUNDING_PROBLEM: Built to solve a coherence crisis in strict monotheism: if the Qur'an is coeternal with God, either there are two eternals (compromising divine unity) or God's speech is identical with His essence (collapsing the distinction between God and His word, with anthropomorphist consequences). The created doctrine secured a God whose essence transcends every temporal artifact, including revelation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: rival traditionalist theologians conceded the problem was real — they built elaborate distinctions (eternal inner speech versus articulated recitation) precisely to answer it rather than dismissing it as pseudo; Judeo-Arabic rationalists such as Saadia Gaon treated the parallel created-versus-eternal speech problem in their own tradition; and academic historiography of the mihna and early kalam documents the coherence problem as a live driver of the dispute rather than a retroactive rationalization.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (end-state 0.38, peak 0.66 at T100) tracks the doctrine's institutional grip: low while it was a school position among others, peaking when backed by appointments and loyalty tests, falling as patronage reversed. Suppression (end-state 0.22, peak 0.68) is the enforcement-capacity series — the machinery of appointments, examinations, and exclusion that had to be actively built up and then collapsed; the end-state scalar reflects the shared-grid discipline (scalars equal final grid values), not the arrangement's peak character, which the series carries. Theater rises monotonically (0.10 to 0.46) because functional theological output contracted faster than performative loyalty display after the reversal; the tail approaches the 0.5 Goodhart threshold, a piton-drift symptom noted here without reclassifying — the arrangement's defining structure remains coordination-plus-extraction, not inertia. Resistance (0.75) and accessibility_collapse (0.35) are characteristic-life scalars off the measurement grid: alternatives never collapsed (the uncreated reading stayed fully live and ultimately won), and resistance was persistent, organized, and victorious. The trajectory is a single rise-and-fall arc driven by court politics, not a reinforcing cycle; the oscillation is exogenous (patronage flip), not an extraction mechanism. Time mapping: T0≈750 CE, T50≈800, T75≈825 (proclamation era), T100≈850 (peak enforcement), T150≈900 (Sunni collapse, Zaydi/Shi'i enclave persistence).
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from the same structure. From the kalam seat the arrangement is a necessary safeguard: without it, monotheism collapses into dualism or identity-confusion, and scripture becomes rationally unreadable. From the traditionalist jurist seat the same structure is dispossession: a lifetime's authority, built on transmitting fixed divine speech, demoted to custodianship of a creature. From the devotional seat it is estrangement from the recited word itself. Identity-lock dynamics bind the devotional seat: the fusion is relational-devotional — the community's self-concept is constituted through the recited text as God's unmediated speech — so exit is not a belief revision but a self-dissolution; if that frame broke, the seat's directionality would fall toward constrained and its perceived extraction would drop sharply. Coalition check: the two victim seats differ in power (organized jurists, powerless devotees), and their historical coalition — jurist leadership carrying popular devotional grievance — is exactly what defeated the arrangement; the powerless seat's coalition potential was realized, not hypothetical.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: rationalist_kalam_theologians sit near the beneficiary end (collect hermeneutic primacy; mobile exit across courts), abbasid_court_patrons likewise (gains land demonstrably with them — hence gain_flow names that seat — and arbitrage-grade exit lets them flip patronage at will, keeping them near the subsidy end despite their agenda-setting power). Victims derive high directionality: traditionalist_jurists are organized but constrained (their method cannot be abandoned without self-annulment), and literalist_devotional_communities combine powerlessness with identity-lock, placing them nearest the full-target end. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms produce the correct spread. Suppression is authored as a raw structural property and is deliberately NOT scaled — only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two symmetrical mislabelings. Read as pure snare, the arrangement's genuine coordination function disappears — but the monotheism-coherence problem was real, conceded by rivals, and independently corroborated across traditions; a doctrine that solves it is not cover-only. Read as pure rope, the asymmetric extraction vanishes — but the same claim that secures transcendence simultaneously strips textual-fixity holders of their authority base, and the arrangement held only under active enforcement, collapsing within a decade of enforcement withdrawal: the canonical tangled-rope signature. The analysis also guards the sibling boundary: peak-era coercion (the mihna) belongs structurally to the state_enforced_creation_reading, and folding it into this story would corrupt ε; the attribution ambiguity is carried as an omega rather than resolved by fiat. Founding_problem_status is contested-live, so no dead-mandate zombie flag fires; the rising theater tail is recorded as symptom, not verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does instantiating the created reading as a coordination-extraction arrangement depend on framing choices that a sibling reading would reject — in particular, treating ''the doctrine''s social operation'' rather than ''the metaphysical claim itself'' as the constraint?',
    'Cross-reading comparison across the kernel family: if the uncreated_reading file, framed identically as a social arrangement, produces a congruent beneficiary/victim structure, the framing is robust; if the classifications diverge sharply, the disagreement localizes in the framing layer, not the doctrine.',
    'If the alternative framing (bare metaphysical claim, no social arrangement) were adopted, this story would have no parties, no beneficiaries, and no extraction — collapsing toward a mountain-shaped claim; the current tangled_rope classification is framing-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Committer structure: this file is one reading of the quran_ontological_status kernel; sibling readings instantiate different constraints.').

omega_variable(
    doctrine_vs_inquisition_attribution,
    'How much of the peak-interval suppression and extraction (T75-T100) belongs to the created doctrine as such, versus the state_enforced_creation_reading''s inquisition machinery that shares the doctrinal core?',
    'Compare extraction profiles across non-mihna created-doctrine milieus: pre-833 Basran circles and post-collapse Zaydi and Twelver Shii enclaves where the doctrine persisted without state enforcement. If those milieus show materially lower extraction, the peak values here are partially misattributed to this reading.',
    'If mihna-era coercion is attributed wholly to the state-enforced sibling, this story''s peak extractiveness and suppression drop substantially, softening the tangled_rope profile toward rope; the end-state scalars are less affected since they postdate the enforcement collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_inquisition_attribution, empirical, 'Attribution boundary between this reading and its state-enforced sibling for peak-era coercive load.').

omega_variable(
    devotional_identity_lock_depth,
    'Is the literalist_devotional_communities seat''s exit genuinely identity_locked (identity constituted through the recited word''s divine status), or merely constrained (adoptable at prohibitive social cost)?',
    'Retention and conversion patterns in post-settlement devotional communities: whether individuals who accepted rationalist frameworks retained devotional continuity (constrained) or underwent identity rupture (locked), traceable through biographical and communal records.',
    'If exit is constrained rather than locked, the seat''s directionality falls below the full-target end, lowering effective extraction for that seat and tilting the computed classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_identity_lock_depth, empirical, 'Depth of identity fusion binding the devotional victim seat to the unmediated-speech frame.').

omega_variable(
    interpretive_flexibility_valence,
    'Does the textual flexibility this reading enables register as benefit (reasoned engagement, reform access) or harm (destabilized guidance, contested meaning) for the communities living under it?',
    'Not resolvable by data alone: it depends on whether the evaluator weights epistemic openness or normative stability higher. Survey of how reform movements and literalist communities respectively narrate the same hermeneutic opening would map the distribution of valences.',
    'If flexibility is valued negatively by a majority of affected seats, the beneficiary/victim balance shifts and the arrangement''s classification tilts further from rope toward snare; if positively, the coordination function strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_flexibility_valence, preference, 'Valence of hermeneutic flexibility — the same structural feature reads as benefit or cost depending on evaluative priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t25, quran_ontological_status__created_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(qura_tr_t75, quran_ontological_status__created_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement(qura_tr_t125, quran_ontological_status__created_reading, theater_ratio, 125, 0.42).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.46).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t25, quran_ontological_status__created_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(qura_be_t75, quran_ontological_status__created_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.66).
narrative_ontology:measurement(qura_be_t125, quran_ontological_status__created_reading, base_extractiveness, 125, 0.45).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(qura_su_t25, quran_ontological_status__created_reading, suppression_requirement, 25, 0.2).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(qura_su_t75, quran_ontological_status__created_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(qura_su_t125, quran_ontological_status__created_reading, suppression_requirement, 125, 0.35).
narrative_ontology:measurement(qura_su_t150, quran_ontological_status__created_reading, suppression_requirement, 150, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Qur'an's ontological status' covers three structurally distinct claims with different epsilon values, victim sets, and failure modes. This file authors the created_reading (doctrinal-hermeneutic arrangement; tangled_rope claim; victims are authority-and-identity holders). The uncreated_reading authors revelation as coeternal ontic constraint (mountain-shaped claim; no human beneficiaries; naturality asserted). The state_enforced_creation_reading authors the doctrine plus mihna coercion (highest extraction of the family; enforcement is the object). Upstream/downstream: this reading supplies the doctrinal content the state-enforced sibling operationalizes, so its fortunes set that sibling's legitimacy conditions; the uncreated sibling is the direct contradictory and the historical victor. Each story links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
