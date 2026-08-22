% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Incoherent Institutional Bundle of Kami-Buddha Ontologies
 *   domain: religious_studies/japanese_cultural_history/philosophy_of_religion
 *
 * SUMMARY:
 *   Shinbutsu-shūgō (kami-buddha fusion) is conventionally treated as a
 *   single historical phenomenon — a syncretic system where kami and buddhas
 *   were identified, hierarchized, or merged. This constraint story reads it
 *   differently: not as a coherent system but as an institutionally sustained
 *   bundle of MUTUALLY CONTRADICTORY commitments that were never resolved
 *   because the institutions benefiting from the contradictions had no
 *   incentive to resolve them. The bundle contains simultaneously: (1)
 *   honji-suijaku monism (kami are traces of buddhas), (2) domain partition
 *   (kami govern life/purity, buddhas govern death/impurity), (3)
 *   hierarchical subsumption (kami as protectors of Buddhism), (4) reciprocal
 *   equivalence (kami and buddhas as mutual manifestations), and (5)
 *   unsystematized local fusions with no doctrinal warrant. These are not
 *   'phases' or 'perspectives' — they operated simultaneously in the same
 *   temple-shrine complexes, often in the same ritual, for over a millennium.
 *   The bundle persists because it coordinates ritual interoperability
 *   without requiring doctrinal commitment, and because the institutional
 *   actors who administer it (temple-shrine networks, court ritual office,
 *   syncretic specialists) extract material and positional benefits from the
 *   incoherence itself. The Meiji separation (shinbutsu bunri) attempted to
 *   impose coherence by fiat and failed — the bundle's contradictions survive
 *   in postwar Shinto-Buddhist practice, proving the constraint is maintained
 *   by institutional inertia, not doctrinal conviction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Incoherent Institutional Bundle of Kami-Buddha Ontologies").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_cultural_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '87eacd27-1952-4890-a412-162b8baa379d').
narrative_ontology:cs_kernel_codification('87eacd27-1952-4890-a412-162b8baa379d', distributed).
narrative_ontology:cs_authority_grounding('87eacd27-1952-4890-a412-162b8baa379d', practice).
narrative_ontology:cs_interpretation_layer_present('87eacd27-1952-4890-a412-162b8baa379d').
narrative_ontology:cs_reading_relation('87eacd27-1952-4890-a412-162b8baa379d', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('87eacd27-1952-4890-a412-162b8baa379d', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('87eacd27-1952-4890-a412-162b8baa379d', foundational, no_single_ontology_governs_legitimate_ritual).
narrative_ontology:cs_axiom_status(no_single_ontology_governs_legitimate_ritual, holdable).
narrative_ontology:cs_axiom_grounding('87eacd27-1952-4890-a412-162b8baa379d', no_single_ontology_governs_legitimate_ritual, conventional).
narrative_ontology:cs_axiom('87eacd27-1952-4890-a412-162b8baa379d', foundational, ritual_efficacy_surpasses_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_surpasses_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('87eacd27-1952-4890-a412-162b8baa379d', ritual_efficacy_surpasses_doctrinal_coherence, instrumental).
narrative_ontology:cs_reference_frame('87eacd27-1952-4890-a412-162b8baa379d', heian_syncretic_practice).
narrative_ontology:cs_drift_state('87eacd27-1952-4890-a412-162b8baa379d', medieval_doctrinal_elaboration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87eacd27-1952-4890-a412-162b8baa379d', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, institutional_temple_shrine_networks).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, imperial_court_ritual_office).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, syncretic_ritual_specialists).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, systematic_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, reformist_monastic_orders).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_coherence).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_surpasses_doctrinal_coherence).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, institutional_continuity_as_its_own_justification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the physical and institutional infrastructure where kami and buddhas are enshrined together (jingūji, bettō system). Their material existence — land holdings, tax exemptions, hereditary priestly lineages — depends on maintaining the fused arrangement. They set the ritual calendar, control the interpretation of what happens on the ground, and resist any systematization that would force a choice between fusion and separation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, institutional_temple_shrine_networks, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, institutional_temple_shrine_networks, beneficiary).

% Claims sovereignty over the ritual order through the Daijōkan and Jingi-kan. The court's legitimacy is performatively enacted through the very contradictions it never resolves — it sponsors both the honji-suijaku discourse AND the domain-partition discourse, because each serves different diplomatic and domestic constituencies. The incoherence is the point: it lets the court be all things to all ritual partners.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, imperial_court_ritual_office, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, imperial_court_ritual_office, beneficiary).

% Shugendō practitioners, onmyōji, and local ritual experts who make their living navigating the contradictory terrain. Their expertise IS the ability to operate in both registers simultaneously — invoking kami for this-worldly benefits, buddhas for other-worldly salvation — without ever needing to reconcile the ontologies. Systematization would destroy their professional niche.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, syncretic_ritual_specialists, beneficiary,
    organized, biographical, constrained, regional).

% Tendai, Shingon, and later Pure Land and Nichiren thinkers who try to produce a coherent doctrinal account. They bear the cost of the bundle's incoherence: their systems must either absorb the contradictions (expanding into ever more elaborate hierarchical schemata) or denounce them (risking institutional marginalization). Their intellectual labor is extracted to maintain the appearance of a unified tradition.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, systematic_theologians, payer,
    moderate, biographical, constrained, national).

% Monks and nuns who seek to purify Buddhist practice from kami-worship (or vice versa). They are structurally trapped: the institutional infrastructure they depend on (temples, ordination lineages, lay support networks) is built on the fused arrangement. Exit means losing the material basis of their reform. The bundle extracts their reformist energy by forcing them to either compromise or schism.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, reformist_monastic_orders, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, reformist_monastic_orders, excluded).

% Ordinary people who want a clear answer — is this kami a buddha or not? Do I pray for worldly benefit or other-worldly liberation? They pay with cognitive load and ritual uncertainty. The bundle never resolves their question; it offers situational answers that shift by shrine, by season, by life circumstance. Their exit is constrained by community, geography, and the sheer absence of any coherent alternative.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_coherence, payer,
    powerless, biographical, constrained, local).

% The 1868 shinbutsu bunri policy makers who tried to forcibly separate kami and buddhas. They are excluded from the bundle's internal logic — their project is precisely to destroy it. But their failure (the separation was never complete, the contradictions persist in postwar Shinto and Buddhist institutions) demonstrates the bundle's inertial power. They are the proof that the constraint cannot be removed by fiat.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, meiji_separation_architects, excluded,
    powerful, biographical, mobile, national).

% Scholars who analyze the bundle from outside — historians of religion, philosophers of syncretism, anthropologists of Japanese ritual. They see the structural incoherence clearly but have no stake in its resolution. Their analyses become part of the discourse the bundle absorbs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, modern_religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables ritual interoperability across a fragmented religious landscape: a single shrine-temple complex can serve agrarian petitioners (kami), ancestral rites (buddhas), state ceremonies (imperial kami), and esoteric adepts (honji-suijaku schemata) without requiring any participant to commit to a single ontology. The bundle coordinates by NOT choosing.
% TRANSFER_FUNCTION: Moves interpretive labor and institutional legitimacy from systematic theologians (who must produce ever-more-elaborate reconciliations) and reformist monastics (who must either compromise or schism) to the institutional temple-shrine networks and court ritual office (who collect the material benefits of the fused arrangement — land, taxes, hereditary authority, diplomatic flexibility). Lay practitioners pay with cognitive load and ritual ambiguity.
% ABSENT_VOICES: Would-be systematic theologians who left the tradition rather than produce contradictory schemata; lay practitioners who converted to Christianity or new religions precisely because the bundle offered no coherent answer; the kami and buddhas themselves, whose ontological status is decided by institutional convenience rather than their own nature.
% DISAPPEARANCE_RATIONALE: If the institutional bundle vanished overnight, the physical infrastructure (jingūji complexes, shared ritual calendars, hereditary priesthoods) would face an existential choice: commit to honji-suijaku monism, commit to domain partition, or fracture into competing single-ontology institutions. The material and social order built on the fused arrangement would reorganize — not disappear, but rearrange around a forced choice the bundle currently prevents.
% FOUNDING_PROBLEM: How to integrate imported Buddhist soteriology with indigenous kami worship without requiring the population to abandon either, while giving the imperial court ritual sovereignty over both?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — integrating Buddhism and kami-worship under court ritual sovereignty — was explicitly declared resolved by the Meiji separation edicts (1868), which attempted to assign each domain its proper ontology. The bundle persisted despite the formal resolution, corroborated by the continued existence of syncretic practices (e.g., Daijōsai imperial rites retaining Buddhist elements, Shugendō survival, household kamidana-butsudan dual altars). No beneficiary of the original arrangement (court, temple-shrine networks) claims the founding problem is still live; they claim the arrangement IS the solution, not that the problem persists.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is substantial but not maximal: the bundle extracts interpretive labor from theologians and reformist energy from monastics, while delivering genuine ritual coordination to practitioners. The coordination function is real — the bundle solves a genuine interoperability problem — which prevents classification as pure snare. Suppression (0.72) is high: the bundle actively suppresses systematization attempts (by Tendai, Shingon, Nichiren, Yoshida Shinto, Meiji state) through institutional inertia, hereditary control of ritual sites, and the sheer practical success of the fused rituals. Theater ratio (0.48) is near the piton threshold: by the late medieval period, the doctrinal schemata (honji-suijaku, original enlightenment thought) had become increasingly performative — elaborate theoretical superstructures masking the fact that on the ground, the same shrine served contradictory ontologies situationally. Accessibility collapse (0.35) is moderate: alternatives (pure Buddhism, pure Shinto, Christianity, new religions) always existed but were institutionally marginalized. Resistance (0.62) is high: systematic theologians and reformist monastics continuously produced counter-discourses, but these were absorbed into the bundle rather than displacing it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (temple-shrine networks, court), the bundle is functional coordination — it works, it persists, it delivers ritual efficacy. From the payer seats (theologians, reformists, lay practitioners), it is an extractive incoherence that forces them to do the intellectual work of reconciliation or bear the cost of ambiguity. The engine will compute this divergence from the structural data. The claimed type (tangled_rope) reflects the authoring seat's judgment: genuine coordination function + asymmetric extraction + active enforcement of incoherence against systematization attempts.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional temple-shrine networks and court ritual office are structural beneficiaries (d near 0.0-0.2): they collect land revenue, tax exemptions, hereditary authority, and diplomatic flexibility from the fused arrangement. Their exit is identity_locked — their institutional identity IS the bundle; they cannot leave without ceasing to be what they are. Syncretic ritual specialists are beneficiaries with constrained exit (d ~0.3): they profit from the incoherence but could theoretically retrain. Systematic theologians and reformist monastics are payers (d ~0.7-0.8): they bear the cost of producing reconciliations or suffering marginalization. Their exit is constrained (theologians) or trapped (reformists) because their institutional bases are embedded in the bundle. Lay practitioners are payers with constrained exit (d ~0.6): they pay cognitive load but lack alternatives. Meiji separation architects are excluded (d not applicable): they sought to destroy the bundle, not operate within it. Modern observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating Buddhism and kami-worship under court sovereignty) was formally resolved by Meiji separation — yet the bundle persisted. This is textbook mandatrophy: the arrangement outlived its founding justification by ~150 years (from early Heian systematization attempts to Meiji) and continues in transformed form today. The bundle's persistence is not explained by the founding problem but by the institutional interests that crystallized around the contradictions. The mandated type (tangled_rope) captures this: it coordinates (ritual interoperability) AND extracts (interpretive labor, reformist energy, cognitive load), and it requires active enforcement (suppression of systematization). The mandate has atrophied; the constraint remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_vs_historical_sequence,
    'Is the incoherent bundle a single persistent structure, or a historical sequence of distinct coherent systems (early fusion → honji-suijaku systematization → medieval elaboration → early modern dissolution) that only appears bundled in retrospect?',
    'Diachronic analysis of whether the same institutional actors simultaneously maintain contradictory commitments at single sites, versus successive actors adopting successive systems at different sites. Temple-shrine complex records (engi, garan-narabi, ritual calendars) are the primary evidence.',
    'If historical sequence, each phase could be a separate constraint (rope → tangled_rope → piton). If single persistent bundle, the tangled_rope classification holds across the millennium with rising extractiveness as systematization attempts accumulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_vs_historical_sequence, empirical, 'Whether the bundle is synchronic structure or diachronic sequence.').

omega_variable(
    coordination_necessity_of_incoherence,
    'Is the bundle''s incoherence STRUCTURALLY NECESSARY for its coordination function (ritual interoperability requires ontological ambiguity), or is incoherence merely the contingent byproduct of institutional interests blocking systematization?',
    'Counterfactual: if a coherent honji-suijaku system had been universally imposed in 900 CE, would ritual interoperability have collapsed? Compare with domains where coherent ontology was imposed (e.g., Yoshida Shinto, Meiji State Shinto) — did they lose interoperability?',
    'If incoherence is necessary for coordination, the bundle is a genuine rope with unavoidable ambiguity. If incoherence is contingent on institutional blocking, the extraction is avoidable and the constraint is a tangled_rope where the coordination function could survive systematization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_incoherence, conceptual, 'Whether incoherence is functional or extractive.').

omega_variable(
    meiji_separation_as_bundle_continuation,
    'Does the Meiji shinbutsu bunri policy represent the bundle''s destruction, or its transformation into a new incoherent bundle (State Shinto''s ''non-religious'' kami vs. ''religious'' Buddhism distinction)?',
    'Analyze whether the post-1868 arrangement sustains the same structural contradictions: simultaneous fusion/separation (imperial rites retain Buddhist elements), hierarchical/reciprocal (Emperor as kami descendant vs. Buddhist devotee), systematized/unsystematized (State Shinto doctrine vs. folk practice).',
    'If Meiji separation continues the bundle, the interval should extend to present and the constraint''s extraction/suppression metrics should show continuity across 1868. If it genuinely breaks the bundle, 1868 is a type transition (tangled_rope → scaffold → snare/rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_bundle_continuation, empirical, 'Whether 1868 is rupture or transformation of the bundle.').

omega_variable(
    theologian_extraction_vs_vocation,
    'Is the interpretive labor extracted from systematic theologians genuinely exploitative, or is producing reconciliations the constitutive vocation of Buddhist philosophy in Japan (making the ''extraction'' a category error)?',
    'Compare the institutional rewards/penalties for theologians who produce reconciliations vs. those who refuse. If refusal leads to marginalization while production leads to patronage, the labor is extracted. If both paths are equally viable within the tradition, it is vocational.',
    'If vocational, the ''payer'' role of systematic_theologians is misassigned — they are beneficiaries of a tradition that values doctrinal elaboration. This would reduce the constraint''s measured extractiveness and potentially shift classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theologian_extraction_vs_vocation, conceptual, 'Whether theologian labor is extraction or vocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 750, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbo_incoh_tr_t750, kami_buddha_ontology__incoherent_bundle, theater_ratio, 750, 0.15).
narrative_ontology:measurement(kbo_incoh_tr_t850, kami_buddha_ontology__incoherent_bundle, theater_ratio, 850, 0.25).
narrative_ontology:measurement(kbo_incoh_tr_t950, kami_buddha_ontology__incoherent_bundle, theater_ratio, 950, 0.35).
narrative_ontology:measurement(kbo_incoh_tr_t1050, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1050, 0.42).
narrative_ontology:measurement(kbo_incoh_tr_t1150, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1150, 0.45).
narrative_ontology:measurement(kbo_incoh_tr_t1250, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1250, 0.48).
narrative_ontology:measurement(kbo_incoh_tr_t1350, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1350, 0.5).
narrative_ontology:measurement(kbo_incoh_tr_t1450, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1450, 0.52).
narrative_ontology:measurement(kbo_incoh_tr_t1550, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1550, 0.5).
narrative_ontology:measurement(kbo_incoh_tr_t1650, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1650, 0.48).
narrative_ontology:measurement(kbo_incoh_tr_t1750, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1750, 0.47).
narrative_ontology:measurement(kbo_incoh_tr_t1870, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1870, 0.48).

% Extraction over time
narrative_ontology:measurement(kbo_incoh_be_t750, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 750, 0.22).
narrative_ontology:measurement(kbo_incoh_be_t850, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 850, 0.35).
narrative_ontology:measurement(kbo_incoh_be_t950, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 950, 0.45).
narrative_ontology:measurement(kbo_incoh_be_t1050, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1050, 0.52).
narrative_ontology:measurement(kbo_incoh_be_t1150, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1150, 0.55).
narrative_ontology:measurement(kbo_incoh_be_t1250, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1250, 0.53).
narrative_ontology:measurement(kbo_incoh_be_t1350, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1350, 0.57).
narrative_ontology:measurement(kbo_incoh_be_t1450, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1450, 0.6).
narrative_ontology:measurement(kbo_incoh_be_t1550, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1550, 0.58).
narrative_ontology:measurement(kbo_incoh_be_t1650, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1650, 0.56).
narrative_ontology:measurement(kbo_incoh_be_t1750, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(kbo_incoh_be_t1870, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1870, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kbo_incoh_su_t750, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 750, 0.3).
narrative_ontology:measurement(kbo_incoh_su_t850, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 850, 0.45).
narrative_ontology:measurement(kbo_incoh_su_t950, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 950, 0.55).
narrative_ontology:measurement(kbo_incoh_su_t1050, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1050, 0.62).
narrative_ontology:measurement(kbo_incoh_su_t1150, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1150, 0.68).
narrative_ontology:measurement(kbo_incoh_su_t1250, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1250, 0.7).
narrative_ontology:measurement(kbo_incoh_su_t1350, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1350, 0.72).
narrative_ontology:measurement(kbo_incoh_su_t1450, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1450, 0.73).
narrative_ontology:measurement(kbo_incoh_su_t1550, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement(kbo_incoh_su_t1650, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement(kbo_incoh_su_t1750, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(kbo_incoh_su_t1870, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1870, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, meiji_shinbutsu_bunri_policy).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, postwar_shinto_buddhist_practice).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three constraint stories: honji_suijaku_monism (ontological identity, claimed Mountain, computed Mountain), domain_partition (functional separation, claimed Rope, computed Rope), and incoherent_bundle (this story, claimed Tangled Rope, computed Tangled Rope). The upstream monism and partition readings are the contradictions this bundle sustains; they affect this constraint by providing the raw contradictory material. This constraint affects the Meiji separation policy (which tried to impose partition and triggered the bundle's transformation) and postwar practice (where the bundle persists).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, institutional, 0.1).
constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, organized, 0.35).
constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, moderate, 0.7).
constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
