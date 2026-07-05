% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical/Ecclesiastical Latin Correctness Standard
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the
 *   classical_latin_standard kernel: correct Latin requires BOTH Classical
 *   textual fidelity AND recognition of legitimate post-Classical
 *   technical/ecclesiastical development. This is one of three sibling
 *   readings of the same contested kernel — continuity_reading (living
 *   transmission legitimizes all drift) and reconstruction_reading (only
 *   philological archaeology recovers correctness, medieval drift rejected
 *   wholesale) are separate constraint stories, not alternative measurements
 *   of this one. The hybrid reading's structural signature is moderate on
 *   every axis: it delegitimizes some post-Classical forms (the 'barbarisms')
 *   while legitimizing others (sanctioned technical/liturgical vocabulary),
 *   producing a reduced victim set relative to reconstruction_reading and a
 *   narrower beneficiary set relative to continuity_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.42).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical/Ecclesiastical Latin Correctness Standard").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '888e3757-8cb4-4910-b985-94b8a43d1a0f').
narrative_ontology:cs_kernel_codification('888e3757-8cb4-4910-b985-94b8a43d1a0f', distributed).
narrative_ontology:cs_authority_grounding('888e3757-8cb4-4910-b985-94b8a43d1a0f', lineage).
narrative_ontology:cs_interpretation_layer_present('888e3757-8cb4-4910-b985-94b8a43d1a0f').
narrative_ontology:cs_reading_relation('888e3757-8cb4-4910-b985-94b8a43d1a0f', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('888e3757-8cb4-4910-b985-94b8a43d1a0f', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('888e3757-8cb4-4910-b985-94b8a43d1a0f', foundational, domain_bounded_development_is_legitimate).
narrative_ontology:cs_axiom_status(domain_bounded_development_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('888e3757-8cb4-4910-b985-94b8a43d1a0f', domain_bounded_development_is_legitimate, conventional).
narrative_ontology:cs_axiom('888e3757-8cb4-4910-b985-94b8a43d1a0f', foundational, undomained_drift_constitutes_barbarism).
narrative_ontology:cs_axiom_status(undomained_drift_constitutes_barbarism, holdable).
narrative_ontology:cs_axiom_grounding('888e3757-8cb4-4910-b985-94b8a43d1a0f', undomained_drift_constitutes_barbarism, conventional).
narrative_ontology:cs_reference_frame('888e3757-8cb4-4910-b985-94b8a43d1a0f', ciceronian_augustan_normative_core).
narrative_ontology:cs_drift_state('888e3757-8cb4-4910-b985-94b8a43d1a0f', post_tridentine_curial_standardization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('888e3757-8cb4-4910-b985-94b8a43d1a0f', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_latinists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, curial_and_seminary_bodies).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philology_faculties).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_influenced_writers).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, regional_medieval_latin_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the operative correctness standard for academic and ecclesiastical Latin: Classical syntax and morphology as baseline, with an approved lexicon of post-Classical technical and liturgical terms admitted as legitimate. Adjudicates which medieval forms count as sanctioned development versus disqualified 'barbarism.' Collects prestige and gatekeeping authority from being the body that draws this line.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, institutional_latinists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, institutional_latinists, beneficiary).

% Uses the hybrid standard to keep centuries of liturgical, canonical, and theological vocabulary intact and authoritative while still claiming continuity with Classical eloquence. Benefits from a standard that neither strands them with archaic-only diction nor abandons them to unregulated vernacular drift.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, curial_and_seminary_bodies, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Trains students and certifies competence against the hybrid norm; their disciplinary authority depends on there being a defensible, teachable standard that is neither pure reconstruction (too narrow to serve applied domains) nor pure continuity (too permissive to certify anything).
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philology_faculties, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, classical_philology_faculties, agenda_setter).

% Writers, notaries, and local clergy whose Latin absorbed regional vernacular syntax and vocabulary are told their usage counts as 'barbarism' rather than legitimate development, unlike sanctioned ecclesiastical terms. They can conform to the hybrid norm at the cost of unlearning inherited local practice, or continue writing in a register that institutional readers mark as incorrect.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_influenced_writers, payer,
    moderate, biographical, constrained, regional).

% Centuries-deep local scribal and administrative Latin traditions are selectively raided: some features are absorbed into the sanctioned technical lexicon, most are declared corrupt and excluded from the correctness standard. The tradition as a whole has no seat that decides which of its features survive the sorting; it is trapped as an object of classification, not a party to it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, regional_medieval_latin_traditions, payer,
    powerless, generational, trapped, regional).

% Argue the hybrid standard is an unprincipled compromise that smuggles in medieval accretion under the cover of 'legitimate technical development.' They would prefer strict return to Classical textual sources but are structurally outvoted by institutions whose functioning depends on retaining the ecclesiastical and technical vocabulary the hybrid standard protects.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, reconstruction_oriented_philologists, excluded,
    moderate, generational, constrained, continental).

% Argue that drawing any hard line between 'legitimate technical development' and 'barbarism' is arbitrary — living usage should be its own warrant. They lose standing whenever institutional certification bodies invoke the hybrid standard's approved lexicon rather than treating regional variation as equally valid Latin.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, continuity_oriented_practitioners, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, institutional_latinists).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single teachable, certifiable standard that lets Classical grammar be taught rigorously while still permitting technical and liturgical vocabulary that accumulated over a millennium of continuous ecclesiastical and scholarly use — solving the problem that pure Classical restriction would strand institutions without words for their own core concepts (sacramentum in its developed sense, technical philosophical termini, etc.).
% TRANSFER_FUNCTION: Moves legitimacy and certification authority toward institutions that can draw and enforce the boundary between 'sanctioned post-Classical development' and 'barbarism,' and moves it away from regional and vernacular-influenced Latin traditions whose features are sorted into the excluded category by bodies they do not sit on.
% ABSENT_VOICES: Reconstruction-oriented philologists object that the standard's compromises are principle-free line-drawing; continuity-oriented practitioners object that any barbarism/development distinction at all misdescribes how living Latin actually worked. Both factions are structurally excluded from the adjudicating bodies, which are staffed by institutions with a stake in the specific line drawn.
% DISAPPEARANCE_RATIONALE: Curial, seminary, and academic bodies would say Latin correctness collapses into either sterile archaism or unregulated drift without the hybrid standard's adjudication. Continuity-oriented practitioners and regional traditions would say the world barely changes for actual usage, which was always heterogeneous — only the certification apparatus and its prestige economy would disappear.
% FOUNDING_PROBLEM: Post-Classical institutions (church, universities, chanceries) needed Latin competence standards that could certify writers and texts, but pure Classical restriction excluded the technical and theological vocabulary those institutions actually needed, while pure permissiveness offered no teachable, testable line at all.
% FOUNDING_PROBLEM_CORROBORATION: Institutional Latinists and seminary bodies attest the problem remains live — certification and liturgical precision still require a defensible standard. Independent historical linguists outside these institutions attest that the specific barbarism/development boundary has shifted repeatedly across centuries in ways that track institutional convenience more than any stable linguistic principle, suggesting the founding problem was real but its current resolution is at least partly self-serving.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, contested).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the standard's delegitimization is partial and selective, not total: it captures a smaller victim population (regional vernacular-influenced usage) than reconstruction_reading would, but still extracts prestige and certification authority from populations whose usage falls on the wrong side of an institutionally-drawn line. Suppression is moderate (0.42) — the standard actively excludes 'barbarism' but does not require wholesale rejection of medieval forms, so its coercive reach is narrower than a pure reconstruction standard. Theater ratio (0.30) reflects that a real coordination function (a teachable, certifiable standard institutions actually need) coexists with a genuine performative layer (the barbarism/development line itself shifts across centuries in ways that track institutional convenience).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (institutional Latinists), the hybrid standard looks like principled coordination — a reasonable middle path between sterile archaism and unregulated drift. From the payer seat (regional traditions, vernacular-influenced writers), the same structure looks like selective extraction: some post-Classical drift is laundered into legitimacy because institutions need it, while structurally similar drift elsewhere is punished because no institution has a stake in preserving it. The engine should register this asymmetry as tangled-rope-shaped: real coordination function, real asymmetric cost-bearing, both riding the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Latinists and the bodies that depend on retained technical/liturgical vocabulary (curial and seminary bodies, philology faculties) sit at the beneficiary end: they get a defensible standard AND keep the vocabulary their institutions actually need. Vernacular-influenced writers and regional medieval Latin traditions sit at the target end: their usage is sorted into 'barbarism' by bodies they have no seat on, and their exit options are constrained or trapped because Latin competence is certified by the very institutions drawing the line against them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions needed certifiable Latin competence that didn't strand them without their own technical vocabulary) remains genuinely live for curial and academic bodies — this blocks a simple mandatrophy verdict. But the specific line between 'sanctioned development' and 'barbarism' has drifted across centuries in ways that correlate with institutional convenience rather than any stable linguistic principle, per the founding_problem_corroboration from independent historians. This is exactly the case the tangled_rope classification exists to catch: a real coordination function does not certify that every cost imposed in its name is coordination cost rather than extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barbarism_boundary_principled_or_convenient,
    'Is the line the hybrid standard draws between ''legitimate post-Classical development'' and ''barbarism'' a principled linguistic distinction, or does it track which vocabulary institutions with adjudicating power happen to need?',
    'Historical trace of which specific medieval forms were reclassified from barbarism to legitimate (or vice versa) across centuries, cross-referenced against which institutions needed those forms at the time of reclassification.',
    'If the boundary tracks institutional convenience rather than stable linguistic principle, the hybrid reading''s coordination story is substantially cover for extraction, pushing the classification toward snare; if the boundary has independent philological grounding, the tangled_rope reading (real coordination + real but bounded extraction) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barbarism_boundary_principled_or_convenient, conceptual, 'Whether the sanctioned-development/barbarism line is principled or institutionally convenient.').

omega_variable(
    kernel_reading_adjudication_authority,
    'Given that continuity_reading, hybrid_reading, and reconstruction_reading are all live positions with different institutional homes, which reading a given text or speaker is judged against depends on which institution is doing the judging — is there any authority above the three readings that adjudicates between them, or is the kernel itself permanently distributed across competing readings?',
    'Survey of which institutional contexts (Vatican curial offices, university classics departments, epigraphic/papyrological philology, regional historical societies) invoke which reading, and whether any cross-institutional body has ever successfully imposed one reading over the others.',
    'If no supra-institutional authority exists, the kernel is genuinely distributed and all three readings coexist permanently as rival frameworks rather than converging; this bears on whether the hybrid reading''s claimed compromise status is stable or merely one more contested position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_adjudication_authority, conceptual, 'Whether any authority adjudicates between the three kernel readings or the kernel remains permanently distributed.').

omega_variable(
    victim_set_boundary_stability,
    'Is the reduced victim set (only ''barbarisms,'' not all medieval drift) that distinguishes hybrid_reading from reconstruction_reading stable over time, or does the sanctioned lexicon expand and contract as institutional needs change, meaning the actual population of victims shifts generationally?',
    'Compare canonical lists of sanctioned ecclesiastical/technical Latin terms across major reference works (e.g., successive editions of standard Church Latin dictionaries) to see whether the sanctioned set has grown, shrunk, or reshuffled.',
    'A stable victim set supports treating hybrid_reading as a fixed structural compromise; an unstable, expanding/contracting set suggests the ''reduced victim set'' framing understates ongoing extraction, since the boundary itself is a live site of institutional negotiation rather than a settled fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_stability, empirical, 'Whether the sanctioned-vocabulary boundary that defines the reduced victim set is stable or continually renegotiated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__hybrid_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__hybrid_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__hybrid_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__hybrid_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the classical_latin_standard kernel. continuity_reading treats all continuous transmission as legitimate (minimal victim set, low extraction). reconstruction_reading treats all medieval drift as illegitimate (maximal victim set, high extraction — the entire post-Classical tradition is delegitimized). hybrid_reading occupies the structural middle: it partially delegitimizes (excludes 'barbarism') while partially legitimizing (sanctions technical/ecclesiastical development), producing moderate ε and moderate suppression that are genuinely distinct from both siblings' values, not an average of them. Each story's ε is fixed and stable within its own reading; no observable-switching occurs within a single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
