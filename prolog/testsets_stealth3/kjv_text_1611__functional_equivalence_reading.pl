% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Complementary-Purposes Translation Ecology (KJV Honored, Modern Versions Authorized)
 *   domain: religious/textual/theological
 *
 * SUMMARY:
 *   This story instantiates the functional_equivalence_reading of the kernel
 *   kjv_text_1611: the settlement in which the 1611 text is honored for
 *   literary, liturgical, and historical reasons while a succession of modern
 *   translations (RV 1881, ASV 1901, RSV 1952, NIV 1978, NRSV 1989, ESV 2001,
 *   CSB 2017) serve clarity, updated philology, and specialized uses. No
 *   single text gates access to scriptural authority; authority is
 *   decentralized across a version ecology. CONSTRAINT FAMILY NOTE
 *   (epsilon-invariance decomposition): colloquial talk of 'the KJV's
 *   authority' conflates three structurally distinct claims — that the KJV is
 *   the exclusively inspired text (sibling story
 *   kjv_text_1611__exclusive_inspiration_reading, high extraction:
 *   gate-keeping rents, suppression of rival texts); that the KJV is
 *   improvable and revision pipelines are legitimate (sibling
 *   kjv_text_1611__revisable_translation_reading, intermediate); and THIS
 *   story, the complementary-purposes settlement, whose epsilon is low
 *   because nothing is gated. The siblings are separate files; this story
 *   links them via network.affects_constraints and does not average over
 *   them. KEY AGENTS (by structural relationship): - translation_committees:
 *   Agenda-setting bodies (organized/constrained) — decide which texts exist
 *   and on what philological basis - commercial_bible_publishers: Principal
 *   economic beneficiary (powerful/arbitrage) — collect licensing streams on
 *   modern versions - kjv_tradition_communities: Voluntary beneficiaries
 *   (moderate/mobile) — retain the 1611 text at zero coercion cost -
 *   scripture_reading_public: Beneficiaries with diffuse cost-sharing
 *   (organized/mobile) — matched texts, mild coordination costs -
 *   theological_seminaries: Cost-bearers with offsetting gains
 *   (institutional/constrained) — carry curricular expense of plurality -
 *   kjv_only_advocates: Organized dissenters outside the governance
 *   conversation (organized/identity_locked) — reject the settlement's
 *   premise - textual_scholarship_community: Analytical observers
 *   (analytical/analytical) — evidence layer beneath every party's claims
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.18).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.12).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Complementary-Purposes Translation Ecology (KJV Honored, Modern Versions Authorized)").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious/textual/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b').
narrative_ontology:cs_kernel_codification('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', fixed_text).
narrative_ontology:cs_authority_grounding('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', lineage).
narrative_ontology:cs_interpretation_layer_present('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b').
narrative_ontology:cs_reading_relation('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', foundational, translation_purposes_complementary).
narrative_ontology:cs_axiom_status(translation_purposes_complementary, holdable).
narrative_ontology:cs_axiom_grounding('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', translation_purposes_complementary, instrumental).
narrative_ontology:cs_axiom('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', foundational, kjv_literary_heritage_standing).
narrative_ontology:cs_axiom_status(kjv_literary_heritage_standing, holdable).
narrative_ontology:cs_axiom_grounding('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', kjv_literary_heritage_standing, conventional).
narrative_ontology:cs_reference_frame('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', complementary_translation_plurality).
narrative_ontology:cs_drift_state('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', contemporary_post_quatercentenary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('41017d7f-10b7-4b54-8ea3-b4c9e7e6e78b', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, kjv_tradition_communities).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, scripture_reading_public).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, textual_scholarship_community).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, commercial_bible_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, theological_seminaries).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, scripture_reading_public).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, theological_seminaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Standing and ad-hoc bodies of biblical scholars, linguists, and denominational representatives that decide manuscript base, translation philosophy, and rendering policy for each version (NIV/Biblica panels, ESV Oversight Committee, NRSV editors, CSB teams). They produce and periodically update the texts the rest of the arrangement runs on. Once a committee accepts sponsorship from a publisher or denomination, steering away from that project's commitments is difficult; disbanding abandons decades of philological work.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, translation_committees, agenda_setter,
    organized, biographical, constrained, global).

% Academic textual critics, palaeographers, and historians of the English Bible (university faculties, the Institut für Septuaginta und biblische Textforschung, IGNTP collaborators) who assess manuscripts, document variant readings, and evaluate how faithfully each version renders its base text. They hold no votes in translation governance but their findings shape which base texts committees adopt and supply the evidentiary record every party cites.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, textual_scholarship_community, observer,
    analytical, civilizational, analytical, global).

% Rights-holding houses (HarperCollins Christian/Zondervan, Crossway, Tyndale House, Thomas Nelson) that commission translations, hold copyrights, license digital and print formats, and collect royalties on each edition sold or streamed. Version proliferation gives them product lines; launching or rebranding an edition lets them re-enter markets on fresh terms. The 1611 text itself carries no copyright, so their economics concentrate in the modern versions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, commercial_bible_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Congregations and traditions that keep the 1611 text in liturgy, choral repertoire, and memorization (parts of the Anglican Communion, Orthodox jurisdictions in English ministry, KJV-devotional households). The pluralist settlement guarantees their text remains printed, recorded, and publicly honored without requiring them to defend it against replacement. They could adopt modern versions at any time; they decline to, and nothing stops them from continuing as they are.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_tradition_communities, beneficiary,
    moderate, generational, mobile, global).

% Lay readers, small groups, and congregants who select among versions for study, devotion, and teaching. They gain texts matched to their literacy and purpose; they bear the mild costs of the same abundance: cross-referencing a passage across versions, pew Bibles that disagree with home Bibles, and the effort of evaluating which edition suits which task. Their aggregate purchasing and app-download behavior visibly steers which versions publishers invest in.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, scripture_reading_public, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, scripture_reading_public, payer).

% Divinity schools and Bible colleges that must maintain faculty competence in Hebrew and Greek textual traditions, teach students to navigate version differences, and stock libraries across editions — a recurring curricular expense created by plurality. In exchange they gain research subjects, a distinctive scholarly niche, and graduates who can serve constituencies anchored to different versions. Curricula are slow to restructure, so the cost side is sticky.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, theological_seminaries, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, theological_seminaries, beneficiary).

% Organized movement networks (Trinitarian Bible Society circles, Dean Burgon Society affiliates, KJV-preferred congregations concentrated in the United States) that hold the 1611 text to be the uniquely preserved English Scripture and every modern version to be corrupted by critical-text and dynamic-equivalence methods. They publish, debate, and plant churches vigorously, but their governing premise has no seat in translation governance: no committee, licensor, or mainline liturgical body adjudicates on exclusivist terms. Leaving the position would mean surrendering a doctrine fused with congregational identity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_only_advocates, excluded,
    organized, generational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__functional_equivalence_reading, commercial_bible_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__functional_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains stable, fit-for-purpose English scriptural texts while language drifts and the manuscript record expands: each translation fixes a consistent text for its user community (liturgy, serious study, devotional reading, evangelism), and the ecology allocates literary continuity to the 1611 text while assigning clarity and up-to-date philology to modern versions.
% TRANSFER_FUNCTION: Moves attention, purchasing, and institutional deference toward whichever versions serve a given purpose: royalty and licensing streams flow from readers and congregations to rights-holding publishers; scholarly authority flows to translation committees whose base-text decisions bind downstream editions; cultural and liturgical prestige remains attached to the 1611 text without carrying any payment obligation.
% ABSENT_VOICES: Lay readers rarely occupy seats in version selection or editorial governance — decisions are made by publishers, committees, and denominational liturgical commissions. Majority-World Anglophone church leadership and non-specialist reader networks have thin representation on editorial boards. KJV-only advocates are loud participants in public argument but structurally outside the arrangement's governance, since no body adjudicates on exclusivist terms.
% DISAPPEARANCE_RATIONALE: If the multi-version settlement vanished overnight — if a single mandated English text replaced the ecology — seminary curricula, lectionaries, study-software ecosystems, parallel-column scholarship, and publishing lines would all require wholesale reorganization, and communities currently served by different versions would lose their matched texts. The arrangement's disappearance forces rearrangement because thousands of institutions have built practices on version plurality.
% FOUNDING_PROBLEM: By the 1870s, two centuries of manuscript discovery (Sinaiticus, Vaticanus, and the critical editions they enabled) plus cumulative English-language drift had made the 1611 translation's underlying Greek text and Jacobean vocabulary inadequate for accurate modern reading — yet the KJV's literary stature and liturgical embeddedness made wholesale replacement unthinkable. The founding problem was how to correct accuracy without discarding heritage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nineteenth-century philologists (Scrivener, the Westcott-Hort school) documented Textus Receptus defects before any modern translation held commercial stakes; literary historians (Norton, Daniell) trace the language-drift problem independently of publishers; and even KJV-only advocates concede the existence of manuscript variation and vocabulary obsolescence — attesting the problem is real while disputing this reading's remedy.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the settlement's defining feature is the absence of a gate-keeper: no text controls access, the 1611 text is public domain, and modern-version royalties are bounded by competitive alternatives. Suppression is low (0.12) because nothing forces a version on anyone; denominational lectionary mandates and bookstore politics create small localized coercions but no structural barrier. Theater is low-to-moderate (0.20) with sharp spikes: the 2011 quatercentenary produced a wave of exhibitions, documentaries, and anniversary editions celebrating the KJV's status far in excess of changes in its use — component-level performance inside a functionally live arrangement. Resistance (0.40) is real and comes almost entirely from the KJV-only flank, which contests pluralism itself rather than any term of it. Accessibility collapse (0.35): the monist alternative (a single authoritative text) has collapsed institutionally across mainstream Anglican, Protestant, and Catholic bodies since 1881, yet persists as a live enclave option, so closure is substantial but incomplete. CYCLICAL DYNAMICS: the measurement series run on one shared seven-point grid (1881–2026) and oscillate on translation-release cycles — each major launch (RV, RSV, NIV, NRSV) triggered a controversy spike in suppression and theater, followed by accommodation and decay until the next release. Part of the oscillation is monetized: publishers time 'new and improved' editions to restart churn, making the release cycle a mild intermittent-reinforcement mechanism layered on a genuinely functional ecology. Suppression is authored as a raw structural property throughout — it is not scaled by power or scope; only effective extraction is scaled, in the engine's computation. The suppression series tracks enforcement-capacity history (mandate waves around 1881 and 1952, decay thereafter), which is why suppression_requirement is traced here rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same settlement. From the publisher seat the ecology is a portfolio of licensable products and the release cycle is revenue strategy; from the committee seat it is a sequence of philological obligations inherited from predecessors; from the KJV-tradition seat it is a protective umbrella that lets an old text survive without defense budgets; from the seminary seat it is an unfunded mandate with a research dividend attached; from the KJV-only seat it is a usurpation that never received legitimate consent. Same nominal domain, radically different experienced arrangements — the engine derives this divergence from the structural data, not from this commentary.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (publishers, KJV-tradition communities, the reading public, scholarship) derive directionality toward the beneficiary end — the settlement subsidizes them, with publishers holding the most concentrated gain (arbitrage-grade exit amplifies their favorable position). Theological seminaries are declared cost-bearers with offsetting benefit and no separate declaration in the victim array; their effective extraction should land near symmetric, reflecting the sticky-cost/offsetting-gain pairing. No group is trapped or identity-locked INTO the settlement's payment structure — the only identity_locked agent (kjv_only_advocates) stands outside it, refusing its premise rather than paying its costs, so identity-lock here signals opposition, not extraction exposure. Spatial scope is global for most seats, which scales effective extraction upward modestly in the engine's arithmetic; with epsilon already low, amplified values stay low for beneficiaries and near-symmetric for the seminarian seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification risk runs both ways and the analysis guards each. Mislabeling risk one: reading the KJV's ceremonial persistence (anniversary galas, 'Authorized Version' branding, coronation and funeral liturgies) as a degraded shell — a piton kept alive by performance. The temporal data refute this at the settlement level: the 2011 theater spike coincides with the LOWEST measured suppression and falling extraction, meaning the performance celebrates a living function rather than masking a dead one; the KJV's theatrical maintenance is component-level, while the ecology's function (matched texts for purposes) is intensively exercised. Mislabeling risk two: reading publisher royalties as proof of a hidden extraction machine behind a coordination front — the tangled-rope error. The structural data show no enforcement arm defending the royalty stream (copyright protects individual editions, not the settlement), no suppressed exit (rival public-domain versions abound), and no identifiable payer class bearing asymmetric cost. Mandatrophy of the founding problem: status is live — manuscript work (CBGM, ECM editions) and language drift continue generating the problem the settlement solves — so no resolved-mandatrophy declaration is authored, and the arrangement shows no sign of outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint instantiates the functional_equivalence_reading of kernel kjv_text_1611; how would the classification shift if the same territory were read through the sibling readings?',
    'Compile and classify the sibling stories (kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading) and compare epsilon, seat structures, and computed types across the kernel family.',
    'Under exclusive_inspiration_reading the same territory computes with sharply higher extraction (gate-keeping rents, suppressed rival texts, likely snare or tangled_rope); under revisable_translation_reading extraction is intermediate. The low-extraction profile authored here is a property of THIS reading, not of the kernel as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame routing: reading-indexed classification over a contested kernel.').

omega_variable(
    publisher_rent_vs_recovery_boundary,
    'Are modern-version royalties extractive rent riding on the settlement, or legitimate recovery of genuine translation investment (decades of committee stipends, linguistic research, testing)?',
    'Cost-accounting studies of completed translation projects comparing cumulative production cost against lifetime royalty streams, with comparable non-scriptural reference-publishing benchmarks.',
    'Wide margin over recovery cost would recast publishers as concentrated gain-collectors and push effective extraction at that seat upward, tilting the settlement toward tangled_rope; thin margins confirm the coordination-economics reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_rent_vs_recovery_boundary, empirical, 'Whether the settlement''s principal economic stream is rent or cost recovery.').

omega_variable(
    monist_alternative_revival_potential,
    'Has the single-authoritative-text alternative collapsed irreversibly, or does it survive in enclaves (KJV-only networks, new traditionalist movements) capable of institutional revival?',
    'Longitudinal tracking of KJV-only institutional footholds, ordination-track requirements, and congregational founding rates; demographic analysis of adherent age structure.',
    'Evidence of revival capacity would mean accessibility_collapse is overstated and the settlement is more contested than authored; continued enclave contraction confirms the monist option has lost institutional viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monist_alternative_revival_potential, empirical, 'Whether the displaced alternative can return.').

omega_variable(
    coordination_cost_harm_attribution,
    'Do the settlement''s coordination costs on lay readers (version-choice burden, cross-version friction, pew/home text mismatch) produce measurable comprehension or cohesion harms, or are they absorbed without effect?',
    'Comparative literacy-and-engagement studies of single-version versus multi-version congregations, controlling for denominational and educational covariates.',
    'Demonstrated harm would justify declaring the reading public a cost-bearing class in the structural arrays and would push the settlement''s effective extraction above its current low band; harmless absorption leaves the low-extraction profile intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_cost_harm_attribution, empirical, 'Whether the increased-coordination-costs delta lands as real harm or background noise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 1881, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_fe_reading_tr_t1881, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1881, 0.35).
narrative_ontology:measurement(kjv_fe_reading_tr_t1901, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1901, 0.25).
narrative_ontology:measurement(kjv_fe_reading_tr_t1952, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1952, 0.38).
narrative_ontology:measurement(kjv_fe_reading_tr_t1978, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement(kjv_fe_reading_tr_t1989, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1989, 0.3).
narrative_ontology:measurement(kjv_fe_reading_tr_t2011, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2011, 0.55).
narrative_ontology:measurement(kjv_fe_reading_tr_t2026, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(kjv_fe_reading_be_t1881, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1881, 0.3).
narrative_ontology:measurement(kjv_fe_reading_be_t1901, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1901, 0.24).
narrative_ontology:measurement(kjv_fe_reading_be_t1952, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1952, 0.26).
narrative_ontology:measurement(kjv_fe_reading_be_t1978, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(kjv_fe_reading_be_t1989, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1989, 0.27).
narrative_ontology:measurement(kjv_fe_reading_be_t2011, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2011, 0.22).
narrative_ontology:measurement(kjv_fe_reading_be_t2026, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(kjv_fe_reading_su_t1881, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1881, 0.45).
narrative_ontology:measurement(kjv_fe_reading_su_t1901, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1901, 0.35).
narrative_ontology:measurement(kjv_fe_reading_su_t1952, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1952, 0.42).
narrative_ontology:measurement(kjv_fe_reading_su_t1978, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(kjv_fe_reading_su_t1989, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1989, 0.33).
narrative_ontology:measurement(kjv_fe_reading_su_t2011, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 2011, 0.18).
narrative_ontology:measurement(kjv_fe_reading_su_t2026, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, resource_allocation).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the authority of the KJV' decomposes, per the epsilon-invariance principle, into three constraint stories with distinct epsilon values, victim structures, and failure modes: exclusive_inspiration_reading (gate-keeping, high extraction), functional_equivalence_reading (this file — decentralized complementarity, low extraction), and revisable_translation_reading (revision legitimacy, intermediate). The upstream claim in the family is the manuscript-evidence base shared by all three; the exclusive reading historically drew its force from denying that evidence, while this reading and the revisable reading both accept it and differ on whether the 1611 text itself remains revisable or is retired to monumental status. Family members cite one another as alternatives; contamination propagates through the shared evidence layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
