% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Priority of the Hypothetical Original Text
 *   domain: religious/academic/textual
 *
 * SUMMARY:
 *   This constraint instantiates the critical-reconstructive reading of the
 *   biblical source-text kernel: the claim that historical recovery of a
 *   hypothetical original text must be established before any question of
 *   structure (formal correspondence) or meaning (dynamic/functional
 *   correspondence) can be legitimately addressed. This is not a claim about
 *   how to translate once the text is settled — it is a claim about
 *   sequencing and priority: textual basis first, everything else deferred.
 *   For academic readers operating inside the discipline's own institutions,
 *   this priority rule is low-cost: it is simply the field's method, and
 *   practitioners are also its beneficiaries (publication, credentialing,
 *   institutional standing). For confessional communities who treat a
 *   received text as load-bearing for doctrine and liturgy, the same priority
 *   rule is substantially extractive: it declares their inherited textual
 *   basis provisional pending scholarly resolution that may never fully
 *   arrive, since new manuscript discoveries and refined stemmatic methods
 *   continually reopen settled-seeming questions. The formal-equivalence and
 *   dynamic-equivalence readings of this same kernel do not defer to textual
 *   reconstruction in this way — they each pick a different primary
 *   commitment (structural fidelity, or communicative effect) and treat
 *   textual basis as a solvable precondition rather than an open, perpetually
 *   prior gate.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholarship: agenda-setter and primary beneficiary — administers the critical apparatus and the gate itself
 *   - critical_text_publishers: beneficiary — commercial and institutional stake in perpetual apparatus revision
 *   - confessional_reading_communities: primary target — received textual basis destabilized
 *   - vernacular_translation_committees: secondary target — bear procedural cost and delay
 *   - lay_congregants: diffuse, powerless target — bear downstream confusion with no voice in process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.34).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Priority of the Hypothetical Original Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/textual").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '68c726c1-c234-48da-9b56-bac61f9dc89b').
narrative_ontology:cs_kernel_codification('68c726c1-c234-48da-9b56-bac61f9dc89b', distributed).
narrative_ontology:cs_authority_grounding('68c726c1-c234-48da-9b56-bac61f9dc89b', expertise).
narrative_ontology:cs_interpretation_layer_present('68c726c1-c234-48da-9b56-bac61f9dc89b').
narrative_ontology:cs_reading_relation('68c726c1-c234-48da-9b56-bac61f9dc89b', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('68c726c1-c234-48da-9b56-bac61f9dc89b', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('68c726c1-c234-48da-9b56-bac61f9dc89b', foundational, textual_basis_has_strict_sequential_priority).
narrative_ontology:cs_axiom_status(textual_basis_has_strict_sequential_priority, holdable).
narrative_ontology:cs_axiom_grounding('68c726c1-c234-48da-9b56-bac61f9dc89b', textual_basis_has_strict_sequential_priority, conventional).
narrative_ontology:cs_axiom('68c726c1-c234-48da-9b56-bac61f9dc89b', secondary, meaning_claims_are_illegitimate_pending_textual_resolution).
narrative_ontology:cs_axiom_status(meaning_claims_are_illegitimate_pending_textual_resolution, holdable).
narrative_ontology:cs_axiom_grounding('68c726c1-c234-48da-9b56-bac61f9dc89b', meaning_claims_are_illegitimate_pending_textual_resolution, instrumental).
narrative_ontology:cs_reference_frame('68c726c1-c234-48da-9b56-bac61f9dc89b', critical_edition_apparatus_as_provisional_consensus).
narrative_ontology:cs_drift_state('68c726c1-c234-48da-9b56-bac61f9dc89b', post_digital_manuscript_discovery_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68c726c1-c234-48da-9b56-bac61f9dc89b', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_text_publishers).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, doctoral_training_pipelines).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_reading_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, vernacular_translation_committees).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_congregants).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, text_critical_methodology_as_prerequisite_discipline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the disciplinary standard that no claim about a passage's structure or meaning is admissible in scholarly discourse until the textual basis (which manuscript readings, which stemma, which reconstructed Urtext) has been established through critical apparatus. Controls journal gatekeeping, doctoral training, and critical edition production. Collects prestige, publication output, and institutional funding from maintaining textual criticism as the mandatory first gate.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary).

% Produce and continually revise critical editions (apparatus-heavy Greek/Hebrew texts) that become the required reference point for any serious translation or commentary work. Revenue and institutional standing depend on the perpetual unsettledness of the 'original text' question generating demand for new editions.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_text_publishers, beneficiary,
    organized, generational, arbitrage, global).

% Seminaries and university departments structure years of required coursework around textual criticism as prerequisite competence. Faculty positions, dissertation topics, and credentialing authority are built on the premise that meaning cannot be safely discussed until textual basis is certified by specialists trained in this pipeline.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, doctoral_training_pipelines, beneficiary,
    organized, generational, arbitrage, global).

% Rely on a received text (whether Masoretic, Textus Receptus, or a denomination's traditional canon) as the stable basis for doctrine, liturgy, and pastoral teaching. The critical-reconstructive priority destabilizes that received basis by treating it as merely one witness among many competing manuscript traditions, none of which can be presumed original. This routinely surfaces as apologetic crisis literature, seminary disputes, and lay confusion when reconstructed readings diverge from familiar texts. Exit is constrained: communities can reject academic textual criticism outright (fundamentalist retreat) but then lose standing in interfaith and scholarly dialogue, or engage it and absorb repeated doctrinal destabilization.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_reading_communities, payer,
    moderate, biographical, constrained, national).

% Must adjudicate translation choices against a constantly shifting critical apparatus rather than a fixed textual basis, delaying translation projects, forcing footnote proliferation ('some manuscripts add...'), and requiring committees to justify every choice against reconstructive scholarship whose conclusions change between editions. Cannot simply commit to structure or meaning without first litigating textual basis, which the critical-reconstructive reading declares must be settled first even though it is rarely fully settled.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, vernacular_translation_committees, payer,
    moderate, biographical, constrained, national).

% Receive translations and sermons downstream of the textual-critical gate without visibility into the manuscript disputes driving footnotes, omitted verses, or bracketed passages in their pew Bibles. Bear the confusion and occasional crisis of faith when a familiar verse is flagged as a later addition, with no direct voice in the scholarly process that produced the change.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_congregants, payer,
    powerless, biographical, trapped, local).

% The corpus of extant manuscripts, papyri, and versions itself — the raw material the reconstructive method works from. Not an agent; included for completeness since the constraint's legitimacy claim rests on fidelity to this evidence.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, manuscript_evidence, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_source_text__critical_reconstructive_reading, manuscript_evidence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, falsifiable procedure (stemmatics, eclecticism, manuscript weighting) for adjudicating disputed readings among thousands of variant witnesses, so that translation and interpretation do not proceed from arbitrarily chosen or theologically motivated text bases.
% TRANSFER_FUNCTION: Moves interpretive authority from confessional tradition and received-text communities to credentialed textual critics; moves resources (seminary curriculum hours, publishing revenue, doctoral placement) toward institutions that control critical apparatus production and away from communities whose received texts are treated as provisional.
% ABSENT_VOICES: Confessional communities committed to a fixed received text (e.g., Textus Receptus adherents, communities for whom liturgical texts are load-bearing) are structurally outside the peer-review process that sets textual-critical standards; their objection — that meaning and structure are not actually suspended pending textual resolution in lived practice, only in scholarly publication — rarely appears in the journals that gatekeep the field.
% DISAPPEARANCE_RATIONALE: Academic scholarship would say the field reorganizes catastrophically without the priority rule — translation and theology would proceed on unexamined textual assumptions, reviving pre-critical naivety. Confessional communities would say very little changes for lived practice, since most congregations already function on a received text regardless of ongoing critical debate; the rule mainly governs what counts as publishable scholarship, not what is preached or prayed. The disagreement over what would rearrange is itself part of the contested kernel.
% FOUNDING_PROBLEM: Multiple divergent manuscript traditions exist for every biblical book, with no single manuscript universally recognized as the autograph; early modern and Enlightenment scholarship needed a disciplined, non-sectarian method to adjudicate variants rather than defaulting to whichever text a given church tradition inherited.
% FOUNDING_PROBLEM_CORROBORATION: Independent papyrologists and paleographers outside biblical studies proper (classical philology, Dead Sea Scrolls specialists working across religious traditions) corroborate that manuscript variation is real and substantial and that some adjudication method is needed. However, corroboration that the specific priority ordering — textual basis strictly before structure or meaning — is the only defensible method, rather than a disciplinary convention that could run iteratively or in parallel, comes primarily from within the field's own methodological literature; few outside voices attest to the priority ordering itself as opposed to the underlying variant problem.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not total extraction: the reconstructive method does produce real, checkable knowledge (a coordination function is genuinely present — comparative manuscript work is not invented), but the priority ordering itself (textual basis strictly before structure/meaning, rather than iteratively alongside them) primarily serves the discipline's own institutional reproduction rather than being strictly necessary for translation or pastoral use. Suppression (0.34) is moderate: confessional communities are not coercively prevented from using received texts, but engagement with mainstream scholarship, interfaith credibility, and seminary accreditation increasingly requires accepting the priority framework. Theater ratio (0.22) is modest — most of the activity is genuine textual-critical labor, not performance, though a growing share of apparatus revision serves publishing-cycle and credentialing demand rather than resolving live variants. Resistance (0.62) is comparatively high because confessional communities actively contest the priority claim (textual maximalism, single-manuscript traditions, canonical-approach theology) rather than passively accepting it.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat, the priority rule is simply methodological discipline — obviously correct, minimally costly, effectively invisible as an imposition because practitioners are also the ones who benefit from and control it. From the confessional-community seat, the same rule structurally requires indefinite deferral of settled meaning pending expert resolution that is, by design, never fully final — every new manuscript discovery reopens the question. The engine's per-seat computation should register this as a genuine structural divergence, not a difference of opinion about the same experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholarship and critical text publishers sit near the full-beneficiary end: they set the rule, administer its apparatus, and collect institutional and commercial returns from its perpetuation, with arbitrage-grade exit (they can move between institutions or projects without losing standing). Confessional communities and vernacular translation committees sit nearer the target end: they bear the destabilization of their working textual basis and have only constrained exit (rejecting the framework costs them scholarly and interfaith standing). Lay congregants are the most target-like: powerless, trapped in the sense that they receive the downstream product of decisions made far upstream, with no participation in the process and full exposure to its doctrinal effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine manuscript variation requiring disciplined adjudication) remains live — this is not a dead mandate maintained by inertia alone. What is contested is the STRICT PRIORITY ordering: whether textual basis must be exhaustively settled before structure or meaning can be addressed at all, versus resolved iteratively alongside them. Treating this as tangled_rope rather than snare or pure rope captures both halves honestly: there is a real coordination function (shared, falsifiable method for variant adjudication) and there is asymmetric extraction (the priority rule entrenches a credentialing gate whose institutional benefits concentrate in academic scholarship while its destabilizing costs concentrate in confessional communities who did not choose the priority ordering and cannot easily exit it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priority_ordering_vs_underlying_problem,
    'Is the strict sequencing claim (textual basis must be resolved BEFORE structure or meaning can be addressed) a necessary feature of responsible textual scholarship, or a disciplinary convention that could be replaced by iterative, parallel treatment of textual basis, structure, and meaning without loss of rigor?',
    'Comparative study of translation projects and commentaries that proceed iteratively (revising textual judgments alongside structural/semantic analysis rather than gating on prior textual settlement) against projects following strict sequential priority: do the iterative projects produce demonstrably worse textual outcomes, or comparable ones with less institutional overhead?',
    'If iterative and sequential methods produce comparable textual rigor, the strict priority ordering is revealed as primarily serving disciplinary gatekeeping rather than being epistemically required — strengthening the tangled_rope over rope classification. If sequential priority demonstrably produces superior outcomes, the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_ordering_vs_underlying_problem, conceptual, 'Whether the strict priority ordering is epistemically necessary or a gatekeeping convention.').

omega_variable(
    reading_disagreement_location,
    'Where exactly do the three kernel readings (critical-reconstructive, formal-equivalence, dynamic-equivalence) locate their disagreement — is it about what the correct FINAL priority should be, or only about SEQUENCING (what must be settled first, procedurally) while all three could in principle converge on a shared final text/meaning?',
    'Textual analysis of translation committee working notes and scholarly commentary practice: do practitioners who nominally hold different readings actually reach different final translation choices, or only differ in the order and apparatus by which they justify the same choices?',
    'If the disagreement is purely procedural/sequencing rather than substantive, the extraction attributed to the critical-reconstructive reading may be overstated — the destabilization of confessional communities may stem more from public visibility of scholarly disagreement (footnotes, textual apparatus) than from actual divergent outcomes. If the disagreement is substantive (different readings genuinely produce different final texts), the extraction claim is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_disagreement_location, empirical, 'Whether the kernel''s three readings disagree about final outcomes or only about procedural sequencing.').

omega_variable(
    confessional_capture_of_critical_scholarship,
    'To what extent has academic textual criticism itself been shaped by the confessional commitments of its practitioners (many biblical scholars remain religiously affiliated), such that the ''purely historical'' framing of textual basis recovery is itself not neutral but carries residual confessional assumptions?',
    'Comparative analysis of textual-critical judgments made by scholars from different confessional backgrounds (or none) on contested variants with doctrinal stakes: do judgments cluster by confessional affiliation in ways inconsistent with purely manuscript-evidentiary reasoning?',
    'If textual judgments cluster by confessional background on doctrinally loaded variants, the claimed neutrality of the critical-reconstructive priority is undermined, and the coordination-function claim (impartial adjudication) weakens relative to the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_capture_of_critical_scholarship, empirical, 'Whether claimed scholarly neutrality in textual criticism is itself confessionally inflected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__critical_reconstructive_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__critical_reconstructive_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__critical_reconstructive_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(bibl_tr_t32, biblical_source_text__critical_reconstructive_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(bibl_be_t32, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(bibl_su_t24, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(bibl_su_t32, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 32, 0.32).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_source_text kernel. formal_equivalence_reading and dynamic_equivalence_reading share the same kernel (the contested question of what textual/interpretive commitment is primary) but instantiate structurally different constraints with different beneficiary sets and different extraction profiles. The critical_reconstructive_reading uniquely destabilizes confessional received-text authority by treating textual basis as perpetually prior and unsettled; the sibling readings treat textual basis as a precondition to be handled rather than an ongoing gate, and so do not generate the same extraction against confessional communities. Each story authors its own epsilon; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
