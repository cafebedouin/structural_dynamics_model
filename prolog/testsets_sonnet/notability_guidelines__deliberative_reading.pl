% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N as Ongoing Deliberative Boundary-Negotiation Process (AfD)
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   This story instantiates the deliberative reading of the WP:N kernel:
 *   notability guidelines are not a fixed epistemic filter (deletionist
 *   reading) nor a systematically exclusionary gatekeeping apparatus
 *   (inclusionist reading), but a perpetual negotiation whose boundary is the
 *   OUTPUT of accumulated AfD deliberation rather than an INPUT applied to
 *   it. Structurally this makes the constraint a governance Scaffold: the
 *   coordination function (letting a huge, changing volunteer body decide
 *   inclusion without a complete a priori definition) is real, and the
 *   process is explicitly transitional in the sense that no single AfD
 *   outcome is meant to be final — precedent is always revisable by future
 *   discussion. The theater_ratio rises over the measured interval as the
 *   volume of cited-precedent boilerplate in AfD arguments has grown relative
 *   to substantive discussion of sourcing, a documented drift in AfD practice
 *   as the corpus of precedent accumulated.
 *
 * KEY AGENTS:
 *   - afd_participants: agenda_setter (organized/mobile) — perform the ongoing boundary negotiation
 *   - wikipedia_editor_community: beneficiary (organized/mobile) — gains adaptive legitimacy
 *   - encyclopedia_readers: beneficiary (powerless/mobile) — passive beneficiary of adaptive coverage
 *   - article_subjects_in_contested_categories: payer (powerless/trapped) — bears repeated adjudication cost
 *   - novice_editors_unfamiliar_with_precedent: payer (powerless/constrained) — bears unwritten-precedent cost
 *   - deletion_review_administrators: agenda_setter/observer (institutional/mobile) — records and shapes consensus
 *   - wikimedia_foundation: observer (institutional/analytical) — hosts but does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.32).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N as Ongoing Deliberative Boundary-Negotiation Process (AfD)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '4076166c-a65e-4400-8ad5-e9e1a74cdf84').
narrative_ontology:cs_kernel_codification('4076166c-a65e-4400-8ad5-e9e1a74cdf84', distributed).
narrative_ontology:cs_authority_grounding('4076166c-a65e-4400-8ad5-e9e1a74cdf84', practice).
narrative_ontology:cs_interpretation_layer_present('4076166c-a65e-4400-8ad5-e9e1a74cdf84').
narrative_ontology:cs_reading_relation('4076166c-a65e-4400-8ad5-e9e1a74cdf84', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4076166c-a65e-4400-8ad5-e9e1a74cdf84', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('4076166c-a65e-4400-8ad5-e9e1a74cdf84', foundational, notability_is_process_output_not_fixed_input).
narrative_ontology:cs_axiom_status(notability_is_process_output_not_fixed_input, holdable).
narrative_ontology:cs_axiom_grounding('4076166c-a65e-4400-8ad5-e9e1a74cdf84', notability_is_process_output_not_fixed_input, conventional).
narrative_ontology:cs_axiom('4076166c-a65e-4400-8ad5-e9e1a74cdf84', foundational, boundary_revisability_is_the_source_of_legitimacy).
narrative_ontology:cs_axiom_status(boundary_revisability_is_the_source_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4076166c-a65e-4400-8ad5-e9e1a74cdf84', boundary_revisability_is_the_source_of_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('4076166c-a65e-4400-8ad5-e9e1a74cdf84', consensus_through_repeated_deliberation).
narrative_ontology:cs_drift_state('4076166c-a65e-4400-8ad5-e9e1a74cdf84', contemporary_precedent_accumulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4076166c-a65e-4400-8ad5-e9e1a74cdf84', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_editor_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, afd_participants).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, encyclopedia_readers).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, article_subjects_in_contested_categories).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, novice_editors_unfamiliar_with_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regular editors who nominate, argue, and !vote in Articles for Deletion discussions. They collectively set and re-set the practical meaning of notability discussion by discussion, citing precedent from prior AfDs (which are not binding but are persuasive). They can leave any single discussion or the project entirely without much cost, and their aggregate deliberation is the mechanism that keeps the boundary moving rather than fixed.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, afd_participants, agenda_setter,
    organized, generational, mobile, global).

% The broader volunteer base benefits from a boundary that is never permanently settled: it can absorb new domains of knowledge (e.g. internet culture, non-Western subjects, emerging science) as consensus shifts, without requiring a rewrite of the underlying policy text. The perpetual-negotiation structure is what lets the guideline stay legitimate as the encyclopedia's scope and contributor base change over decades.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editor_community, beneficiary,
    organized, generational, mobile, global).

% Readers receive an encyclopedia whose coverage boundary is continuously recalibrated by an active deliberative process rather than frozen by a single historical committee. They have no seat in AfD but benefit passively from the process's adaptiveness; if the site's coverage decisions became worse, they would simply consult other sources.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, encyclopedia_readers, beneficiary,
    powerless, civilizational, mobile, global).

% People, organizations, and works whose notability is borderline under current, still-moving precedent bear the direct cost of the negotiation: repeated AfD nominations, deletion and recreation cycles, and outcomes that depend on which editors happen to show up to a given discussion. They cannot appeal to a fixed rule because the rule is explicitly non-fixed; their only recourse is participating in or appealing individual discussions, which they often lack standing or expertise to do effectively.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, article_subjects_in_contested_categories, payer,
    powerless, biographical, trapped, global).

% New contributors who write articles believing they meet the written notability text discover that the operative standard is really the accumulated, unwritten precedent of thousands of past AfDs, which is not centrally documented and shifts over time. Their good-faith work is deleted under a standard they had no practical way to learn in advance; their exit option is to stop contributing, which many do.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, novice_editors_unfamiliar_with_precedent, payer,
    powerless, immediate, constrained, global).

% Admins who close AfD discussions and adjudicate Deletion Review appeals interpret and record the community's evolving consensus, giving the negotiation process procedural continuity. They are volunteers who can step back from admin duties at will, and their closes both reflect and further shape the precedent that later discussions cite.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletion_review_administrators, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, deletion_review_administrators, observer).

% The Foundation hosts the infrastructure and generally does not intervene in content-notability decisions, treating them as community self-governance. It benefits from the encyclopedia's continued legitimacy but bears none of the direct cost of individual AfD outcomes.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikimedia_foundation, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a living mechanism for deciding, article by article and era by era, what belongs in a general encyclopedia without requiring unanimous prior agreement on a complete, fixed definition of notability — letting thousands of editors coordinate inclusion decisions across an enormous and changing subject space.
% TRANSFER_FUNCTION: Moves the burden of defining the coverage boundary from a one-time drafting committee onto an ongoing stream of AfD participants and administrators; moves uncertainty and repeated adjudication cost onto marginal article subjects and new editors, in exchange for adaptive legitimacy for the community as a whole.
% ABSENT_VOICES: Article subjects themselves (especially non-notable-by-current-precedent living people, small organizations, and topics from underrepresented regions or languages) are not parties to AfD in any formal sense and rarely participate; new editors who do not know AfD exists until their work is nominated are also structurally absent from the negotiation that governs them.
% DISAPPEARANCE_RATIONALE: If AfD and the deliberative negotiation of notability vanished overnight, either a fixed inclusion rule would have to be imposed top-down (freezing the boundary and provoking immediate disputes over whose fixed line to use) or inclusion would become unconstrained, rapidly changing the character and maintainability of the encyclopedia; the deliberative process is load-bearing for how the project currently manages growth and disagreement.
% FOUNDING_PROBLEM: Early Wikipedia had no principled way to decide which of an unbounded set of possible articles belonged in a general encyclopedia, and a single fixed definition of 'notable' could not anticipate every subject domain the project would eventually cover.
% FOUNDING_PROBLEM_CORROBORATION: Academic studies of Wikipedia governance (e.g. work on Wikipedia's bureaucratic and adhocratic decision cultures by researchers outside the editor community) corroborate that the scope-definition problem remains unresolved by design and that AfD functions as the project's primary ongoing adjudication mechanism; this is not solely asserted by AfD participants themselves.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end): the process genuinely redistributes decision cost onto specific article subjects and new editors, but the primary function is real coordination, not rent extraction — no party collects a toll from the negotiation itself. Suppression is moderate (0.32): dissenting deletionist or inclusionist positions are not silenced, but the deliberative frame itself is not optional — one cannot opt out of having one's article's fate decided by evolving precedent rather than a fixed, citable rule. Theater ratio rising to 0.4 reflects the accretion of ritualized precedent-citation in AfD discourse, a Goodhart-style drift where citing prior AfDs substitutes for fresh sourcing analysis in a growing share of arguments. Accessibility collapse is moderate (0.35): editors CAN in principle propose fixed criteria, and some subject-area guidelines have hardened into quasi-fixed rules, so the deliberative frame has not totally foreclosed alternatives. Resistance is moderate-high (0.55): every AfD is itself a site of resistance/contestation, which is a structural feature of the deliberative reading rather than a threat to it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (afd_participants, administrators), the process reads as a functioning, self-correcting Scaffold: it exists precisely because a permanent, complete definition of notability was never achievable, and the negotiation is meant to continue indefinitely as new subject domains arise — this is transitional-by-design, not merely transitional-in-name. From the payer seats (contested article subjects, novices), the same structure reads closer to arbitrary and extractive: the 'process' looks less like principled deliberation and more like unpredictable exposure to whichever precedent-fluent editors happen to attend a given discussion. The engine computes these as different per-seat classifications from the same structural data; this divergence is the substance of the deliberative-reading claim, not a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   AfD participants and the broader editor community sit near the beneficiary end: they run the process, and it primarily serves their collective coordination need to keep pace with an expanding subject space. Encyclopedia readers are diffuse beneficiaries with mobile exit (they can simply use another source) and thus contribute little to the constraint's measured extraction. Article subjects in contested categories and novice editors sit toward the target end: they bear concentrated, repeated cost from a boundary that is deliberately never finalized, and their exit options are trapped or constrained respectively (a deleted subject cannot simply relocate to another encyclopedia's notability regime and expect the same coverage; a discouraged novice editor's realistic exit is simply leaving the project).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no fixed definition can anticipate an unbounded, growing subject space) remains live by the corroboration of researchers outside the editor community, which supports the Scaffold reading over a Piton reading: this is not a vestigial process defended by inertia, but an ongoing mechanism still solving the problem it was built for. Classifying this as Scaffold rather than Snare prevents mislabeling a genuine, still-functioning coordination process as pure extraction merely because it has identifiable, powerless payers — the payers exist, but the process is not cover for a beneficiary capturing rents from their situation; no stakeholder here profits from a payer's specific deletion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberation_vs_gatekeeping_framing,
    'Is the perpetual non-finality of AfD outcomes genuine adaptive deliberation (this reading), or is ''ongoing negotiation'' itself a legitimating story that obscures a gatekeeping function whose effective boundary is set by which demographic of editors shows up (the inclusionist reading)?',
    'Longitudinal analysis of AfD outcome patterns by article-subject demographic (gender, region, language, institutional affiliation) compared against changes in participant composition over the same period; if outcome patterns track participant demographics more than sourcing quality, the gatekeeping reading gains support.',
    'If deliberation tracks participant composition rather than evolving epistemic standards, this story''s Scaffold classification (transitional, functionally justified) would be undermined in favor of the inclusionist reading''s Tangled-Rope-or-Snare classification for the same underlying process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_vs_gatekeeping_framing, conceptual, 'Whether perpetual negotiation is genuine adaptive process or a legitimating frame for demographic gatekeeping.').

omega_variable(
    afd_precedent_as_de_facto_fixed_rule,
    'Has the accumulated body of AfD precedent effectively hardened into a de facto fixed rule (undermining the ''perpetual negotiation'' premise) even though no single canonical text states it?',
    'Text-mining a large corpus of AfD closes over time to measure whether cited precedent converges toward a small, stable set of criteria versus continuing to shift with each subject-domain wave.',
    'Convergence toward a stable, unwritten rule set would suggest this constraint is drifting toward the deletionist reading''s fixed-filter structure despite its deliberative self-description, which the rising theater_ratio measurement partially anticipates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(afd_precedent_as_de_facto_fixed_rule, empirical, 'Whether unwritten AfD precedent has effectively fixed the boundary despite the deliberative frame.').

omega_variable(
    sunset_condition_definability,
    'Can this Scaffold ever meet a genuine sunset condition, or is ''perpetual negotiation'' definitionally sunset-less, making the Scaffold classification a courtesy rather than a structural fact?',
    'Examine whether any subject-domain notability question has ever been considered fully and permanently settled (no further AfDs needed) versus perpetually reopened; a domain with zero reopenings over a long interval would support genuine transitional closure.',
    'If no domain ever reaches durable settlement, the Scaffold''s declared sunset clause is nominal, and the constraint''s practical behavior may be closer to a Piton (process maintained by institutional habit after its transitional justification has quietly become permanent).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_condition_definability, conceptual, 'Whether the Scaffold''s transitional premise is ever actually satisfied or is structurally permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deliberative_reading, 0.1).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the notability_guidelines kernel. deletionist_reading claims a fixed epistemic-quality-filter structure (low extraction, high accessibility_collapse); inclusionist_reading claims a structural-gatekeeping structure (high extraction, identifiable excluded classes as victims); this deliberative_reading claims a Scaffold structure (moderate extraction, process legitimacy is the coordination function, negotiation itself is the mechanism rather than an input criterion). Each has its own epsilon and its own stakeholder set per the eps-invariance principle; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
