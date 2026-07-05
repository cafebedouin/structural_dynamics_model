% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shūgō as Incoherent Bundle Enforced by State Ritual Administration
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   During the Tokugawa period, Japanese religious life was organized around
 *   jingūji shrine-temple complexes and honji suijaku doctrine (kami as
 *   'traces' of buddhas), enforced through compulsory temple registration
 *   (terauke/danka seido) that doubled as a surveillance and anti-Christian
 *   mechanism. This reading treats the resulting arrangement as an incoherent
 *   bundle: no single kernel commitment unifies kami cosmology and Buddhist
 *   doctrine at any point in the record. The apparent coherence visible in
 *   honji suijaku texts is the output of doctrinal brokers producing bespoke
 *   reconciliations shrine-by-shrine, not evidence of a settled metaphysics.
 *   The 1868 shinbutsu bunri separation, which proceeded with startling speed
 *   and often violent iconoclasm, is read here as diagnostic: a genuinely
 *   unified or even genuinely partitioned system would not fracture that fast
 *   or that destructively.
 *
 * KEY AGENTS:
 *   - bakufu_ritual_administration: institutional agenda-setter — needs population control, not theological coherence
 *   - shrine_temple_complex_administrators: organized beneficiaries — deploy fusion language opportunistically for revenue and status
 *   - honji_suijaku_doctrinal_brokers: organized beneficiaries — profit from the reconciliation problem staying open, not solved
 *   - lay_practitioners: powerless payers — bear unresolved contradictory obligations with no institutional mechanism to question them
 *   - meiji_era_shinbutsu_bunri_reformers: excluded — retrospective actors whose separation decision is read as evidence, but who had no voice inside the bundle's operating period
 *   - comparative_religion_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu-shūgō as Incoherent Bundle Enforced by State Ritual Administration").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '7ea69623-1b66-4606-8282-1183f38a524d').
narrative_ontology:cs_kernel_codification('7ea69623-1b66-4606-8282-1183f38a524d', distributed).
narrative_ontology:cs_authority_grounding('7ea69623-1b66-4606-8282-1183f38a524d', extraction).
narrative_ontology:cs_interpretation_layer_present('7ea69623-1b66-4606-8282-1183f38a524d').
narrative_ontology:cs_reading_relation('7ea69623-1b66-4606-8282-1183f38a524d', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('7ea69623-1b66-4606-8282-1183f38a524d', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('7ea69623-1b66-4606-8282-1183f38a524d', foundational, no_settled_kernel_commitment_exists).
narrative_ontology:cs_axiom_status(no_settled_kernel_commitment_exists, holdable).
narrative_ontology:cs_axiom_grounding('7ea69623-1b66-4606-8282-1183f38a524d', no_settled_kernel_commitment_exists, empirically_contingent).
narrative_ontology:cs_axiom('7ea69623-1b66-4606-8282-1183f38a524d', secondary, apparent_doctrinal_coherence_is_administrative_artifact).
narrative_ontology:cs_axiom_status(apparent_doctrinal_coherence_is_administrative_artifact, holdable).
narrative_ontology:cs_axiom_grounding('7ea69623-1b66-4606-8282-1183f38a524d', apparent_doctrinal_coherence_is_administrative_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('7ea69623-1b66-4606-8282-1183f38a524d', pre_bundle_independent_kami_cosmology).
narrative_ontology:cs_drift_state('7ea69623-1b66-4606-8282-1183f38a524d', peak_terauke_enforcement_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('7ea69623-1b66-4606-8282-1183f38a524d', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, bakufu_ritual_administration).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_complex_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, honji_suijaku_doctrinal_brokers).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, itinerant_ascetics_outside_registered_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, peripheral_shrine_priests_without_temple_backing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the shrine-temple registration system (jinja/jiin) and requires households to affiliate with both a temple (for death ritual and census/surveillance via danka registration) and local kami cults (for agricultural and communal rites). Does not need the fusion to be metaphysically coherent — only administratively stable. Benefits from a single population-control apparatus that would fracture into competing jurisdictions if kami and buddha cults were forced to resolve their doctrinal relationship one way or the other.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, bakufu_ritual_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Run combined shrine-temple complexes (jingūji) that collect both temple tithes and shrine offerings under one institutional roof. Deploy honji suijaku language flexibly — sometimes as ontological claim, sometimes as convenient administrative fiction — depending on which framing secures more land grants, corvée exemptions, or ritual monopoly at a given moment. Have no incentive to settle the ontological question because ambiguity lets them claim whichever revenue stream is under threat.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_complex_administrators, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_complex_administrators, agenda_setter).

% Scholar-monks and ritual specialists who produce and sell the interpretive labor of reconciling kami with specific buddhas/bodhisattvas on a case-by-case, shrine-by-shrine basis. Their livelihood depends on the reconciliation never becoming settled doctrine — a fixed, closed kernel would make their interpretive services unnecessary. They profit from the bundle staying open.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, honji_suijaku_doctrinal_brokers, beneficiary,
    organized, generational, mobile, national).

% Required by danka registration to affiliate with a temple for funerary/ancestral ritual while also participating in kami-based communal and agricultural rites whose cosmological relationship to the temple's teachings is never explained to them in a way that resolves the contradiction. Bear the cost of maintaining two ritual obligations, two tithe structures, and an unresolved metaphysical picture, with no institutional mechanism available to question whether the arrangement coheres. Cannot opt out of either without loss of communal standing or legal jeopardy (danka registration was compulsory under the terauke system).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, trapped, local).

% Mountain ascetics, mediums, and unaffiliated ritualists whose practice often predates or sits outside the bundled shrine-temple framework. Face suppression, forced affiliation, or exclusion from ritual legitimacy precisely because their independent cosmologies expose that the 'unified' system is a jurisdictional bundle rather than a coherent metaphysics. Absorbed into registered lineages or driven underground.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, itinerant_ascetics_outside_registered_lineages, payer,
    powerless, biographical, trapped, regional).

% Kami priests at shrines that lack a powerful jingūji partnership. Subordinated within the administrative hierarchy to temple institutions that control the doctrinal narrative (kami as manifestations of buddhas, honji suijaku) and, with it, access to state protection and land. Bear reduced status and revenue relative to bundled complexes without any coherent theological reason — the subordination tracks institutional leverage, not settled doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, peripheral_shrine_priests_without_temple_backing, payer,
    moderate, biographical, constrained, regional).

% Later actors (not present during the bundle's centuries of operation) who eventually forced separation (shinbutsu bunri) in 1868, treating the bundle's incoherence as proof it had never been a real fusion. Their retrospective judgment is excluded from the bundle's own operating period — no one inside the system during its centuries of function was permitted to raise the incoherence question as a live administrative option.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, meiji_era_shinbutsu_bunri_reformers, excluded,
    organized, biographical, analytical, national).

% Analyze the historical record of jingūji administration, danka registration, and honji suijaku textual production to assess whether shinbutsu-shūgō constituted a genuine unified cosmology or an accreted administrative arrangement retroactively narrated as coherent. Draw on institutional records, land grant documents, and the abruptness/violence of the Meiji separation as evidence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundled shrine-temple system did solve a real administrative problem for the state: a single registration and surveillance apparatus (danka + local shrine affiliation) that tracked population, suppressed banned religions (notably Christianity), and extracted ritual-linked revenue, without requiring the state to adjudicate a genuine theological question it had no interest in resolving.
% TRANSFER_FUNCTION: Moves tithes, land-use rights, corvée exemptions, and ritual-monopoly revenue from lay practitioners and marginal ritualists to bundled shrine-temple administrators and the doctrinal brokers who service the bundle's ambiguity, while moving surveillance and social-control capacity to the state.
% ABSENT_VOICES: Lay practitioners' own accounts of whether the kami/buddha relationship made sense to them were never institutionally solicited — no seat existed for 'this arrangement is incoherent to me' within the terauke/danka system. Itinerant ascetics who might have named the incoherence directly were suppressed or absorbed rather than heard.
% DISAPPEARANCE_RATIONALE: If the enforced bundle vanished, jingūji complexes would fracture into separately-administered shrines and temples (as in fact happened rapidly and often violently at shinbutsu bunri in 1868), danka registration would lose its dual-ritual leverage over lay households, and doctrinal brokers' interpretive labor would lose its institutional market. The speed and violence of the historical separation is itself evidence that concrete arrangements, not settled metaphysics, had been holding the bundle together.
% FOUNDING_PROBLEM: The state needed a single, low-friction administrative mechanism to register, surveil, and extract from the entire population (partly to suppress Christianity and other disfavored sects) without having to settle or even engage the actual cosmological relationship between indigenous kami cults and imported Buddhist institutions.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era administrators and modern historians of religion (outside the beneficiary set of bundled shrine-temple administrators) attest that the founding administrative problem — Tokugawa population control and sectarian suppression — no longer exists, and that the 1868 shinbutsu bunri separation proceeded rapidly with state backing precisely because the bundle's coordination function had become obsolete while its extractive and status-allocating functions persisted. No corroboration from outside the beneficiary set supports a continuing coherent theological necessity for the bundle; the doctrinal brokers themselves are the primary source for continued-coherence claims.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71 by interval end) reflects tithe/land-rent/ritual-monopoly revenue captured by bundled complexes and doctrinal brokers without a stable underlying service being rendered — the 'service' (coherent religious meaning) is precisely what this reading denies exists. Suppression (0.78) is high because the terauke/danka registration system was legally compulsory and backed by state anti-Christian enforcement; households could not opt out. Theater ratio (0.58) is elevated because a substantial share of visible doctrinal production (honji suijaku commentary, ritual syncretism texts) functioned to perform coherence for administrative and status purposes rather than to resolve any actual cosmological question — and that theatrical share grew over the period as the bundle's original administrative rationale (population control, anti-Christian surveillance) receded in urgency while the revenue-extraction function persisted. Accessibility collapse is moderate (0.42), not extreme, because unlike a mountain, alternative framings (partition, fusion) remained visible and contested throughout — the bundle never fully closed off alternative readings, which is part of why three distinct readings of this kernel are authorable at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bakufu administration, bundled complex administrators, doctrinal brokers) sit near the full-beneficiary end: they collect tithes, land grants, or interpretive fees and control the terms on which fusion/partition/incoherence questions may even be raised. Victims (lay practitioners, itinerant ascetics, peripheral shrine priests) sit near the full-target end: trapped or constrained exit under compulsory registration, bearing the cost of an arrangement whose coherence question was never put to them. The Meiji reformers are excluded rather than beneficiary or victim — their power to act arrives only after the bundle's operative period, which is why they are marked excluded rather than assigned a directionality inside the constraint's active interval.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state population control and anti-Christian suppression via dual-ritual registration) is dead — Japan has not needed this specific mechanism since the Meiji Restoration abolished the shogunate's registration apparatus. Yet during the Tokugawa period itself, the bundle persisted and intensified (rising extraction and suppression trajectories) well past the point where a purely coordination-based account would predict decay, because bundled administrators and doctrinal brokers had captured ongoing rents from the arrangement's continuation. This reading's contribution to mandatrophy analysis is specifically that it denies the coordination function was ever more than incidental: what looks like 'coordination successfully solving a problem, later outliving it' under domain_partition_reading is, under this reading, better described as 'extraction dressed in shifting coordination language from the outset,' which is why claimed_type here is snare rather than tangled_rope — there is no era in the record, under this reading, where a genuine unified coordination function was primary rather than a post-hoc administrative convenience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_coherence_underdetermination,
    'Is shinbutsu-shūgō better modeled as (a) a genuine unified ontological commitment (syncretic_fusion_reading), (b) a stable functional partition of jurisdictions (domain_partition_reading), or (c) an incoherent accreted bundle with no unifying commitment at all (this reading)? The historical record of jingūji administration and honji suijaku textual production is consistent with more than one of these framings depending on which documents and which period are weighted most heavily.',
    'Systematic textual-historical analysis of whether honji suijaku commentary shows doctrinal convergence over time (supporting fusion), stable jurisdictional boundary-marking (supporting partition), or persistent ad hoc, shrine-specific, non-convergent reconciliation with no accumulating settlement (supporting incoherent bundle) — cross-checked against whether the 1868 separation required active dismantling of settled doctrine or merely administrative unbundling of an already-loose arrangement.',
    'If fusion or partition is vindicated, the coordination function is primary and the correct classification shifts toward tangled_rope or rope; if incoherence is vindicated (this reading), extraction is primary and snare is the correct classification, since no genuine settled commitment exists for the state or brokers to be coordinating on behalf of.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_coherence_underdetermination, conceptual, 'Whether the kernel itself is coherent (fusion/partition) or genuinely absent (incoherent bundle) — the central committer-axis question this reading answers one way.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does asserting ''no coherent kernel exists'' logically foreclose the syncretic_fusion_reading''s claim of genuine ontological unity, or can both be held by different parties without contradiction (e.g., a doctrinal broker genuinely believing in fusion while an outside historian reads the same record as incoherent accretion)?',
    'Examine whether the incoherent_bundle_reading and syncretic_fusion_reading could both be true from different observer positions (participant belief vs. institutional-historical analysis) or whether they make a single factual claim about the same object (whether a kernel commitment existed) that cannot be split by observer position.',
    'If the two readings answer the same factual question about kernel existence, they cannot coexist in one framework and the relation should be forecloses rather than coexists_with; if they answer different questions (participant phenomenology vs. institutional record), coexists_with is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether this reading forecloses or merely coexists with the fusion reading, given both make claims about the same historical kernel.').

omega_variable(
    meiji_separation_as_evidence_or_artifact,
    'Is the speed and violence of the 1868 shinbutsu bunri separation genuine evidence that the pre-1868 bundle was incoherent, or is it better explained as a Meiji-specific nationalist project (state Shinto construction) that would have forcibly separated even a genuinely coherent fused system for unrelated political reasons?',
    'Comparative analysis of Meiji religious policy motives (state Shinto construction, anti-Buddhist sentiment among reformers) independent of the bundle''s internal coherence, to isolate whether the separation''s manner reveals pre-existing bundle structure or reflects purely exogenous political motivation.',
    'If the separation was primarily politically motivated regardless of prior coherence, the key evidentiary basis for this reading (rapid/violent unbundling as proof of incoherence) weakens substantially, and the fusion or partition readings become more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence_or_artifact, empirical, 'Whether the Meiji separation''s character is diagnostic of prior incoherence or an artifact of unrelated political motive.').

omega_variable(
    lay_practitioner_phenomenology_unrecoverable,
    'Did ordinary lay practitioners across the Tokugawa period actually experience the kami/buddha relationship as contradictory or incoherent, or is ''bearing contradictory beliefs without resolution'' a category imposed retrospectively by scholars applying a logical-consistency standard that lived religious practice never required?',
    'This is likely irrecoverable at scale — no systematic first-person lay testimony on perceived doctrinal coherence survives from the period. Partial evidence might come from confraternity records, pilgrimage diaries, or folk-religious practice manuals, but these are sparse and non-representative.',
    'If lay practitioners experienced no contradiction (living comfortably with multiple ritual frames the way many people hold multiple non-competing practical commitments), the ''victim bears contradiction'' framing central to this reading''s extraction claim weakens, and the suppression/extraction metrics may overstate lived cost relative to institutional cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_practitioner_phenomenology_unrecoverable, empirical, 'Whether the felt contradiction this reading attributes to lay practitioners is empirically supported or a scholarly projection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 150, 0.51).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 200, 0.55).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 250, 0.58).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 200, 0.69).
narrative_ontology:measurement(shin_be_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 250, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 100, 0.66).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 200, 0.76).
narrative_ontology:measurement(shin_su_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 250, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, terauke_danka_registration_system).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the shinbutsu_ontological_substrate kernel. syncretic_fusion_reading claims genuine ontological unity (kami as buddha-manifestations, ε low, functioning as rope/tangled_rope with a real metaphysical coordination function); domain_partition_reading claims a stable functional jurisdictional split (this-world/afterlife) with moderate ε; this reading (incoherent_bundle_reading) claims no coherent kernel exists at all and reads the entire arrangement as accreted institutional extraction under state enforcement, with correspondingly high ε and snare classification. The three do not share an ε value because they are not measuring the same claim with different instruments — they are three structurally distinct claims about what, if anything, unifies the historical record. Each should be evaluated independently; the terauke/danka registration system is the shared enforcement infrastructure across all three readings and is linked as a downstream-shared dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
