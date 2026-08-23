% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous-Climb Doctrine of Commitment Displacement (Imposition-Pathway Kernel)
 *   domain: historical sociology / state formation / commitment systems
 *
 * SUMMARY:
 *   This story instantiates the endogenous-climb reading of the
 *   imposition-pathway kernel: the claim that all commitment displacement
 *   proceeds through fringe adoption and gradual climb, so that apparent
 *   top-down impositions are compressed climbs whose early stages left no
 *   visible trace. Its flagship exemplar is Meiji Japan: Western calendar and
 *   dress practices were already established among treaty-port merchants,
 *   naval and military modernizers, and parts of the merchant class before
 *   the 1872 calendar decree and the early-1870s hair and clothing edicts, so
 *   the decrees accelerated and ratified an existing climb rather than
 *   initiating displacement. The constraint modeled here is the doctrine's
 *   operation inside historical sociology and state-formation studies: it
 *   supplies the field's shared protocol for reading institutional-change
 *   episodes (seek the pre-decree adoption chain; a traced climb is an
 *   explanation) while taxing scholars whose cases resist the frame. Sibling
 *   readings — the exogenous-override reading and the hybrid-cascade reading
 *   — are separate constraints with their own files; this story does not
 *   average over them. Claim and metrics are authored independently: the type
 *   is claimed from structural truth, the metrics describe actual operation.
 *
 * KEY AGENTS:
 *   - diffusion_theory_incumbents: agenda-setting beneficiary (institutional power, identity-locked exit) — edits flagship journals, directs graduate training, collects citations and interpretive authority from the doctrine
 *   - override_narrative_scholars: primary target (moderate power, constrained exit) — produces decree-first or override-friendly accounts and pays review friction, reframing demands, and citation exclusion
 *   - doctoral_students_in_historical_sociology: target-in-training (powerless, identity-locked) — professional identity forms through the climb frame; dissent registers as incompetence
 *   - area_specialists_with_fringe_evidence: secondary beneficiary (moderate power, mobile exit) — treaty-port, merchant-diary, and procurement archives become load-bearing evidence under the doctrine
 *   - quantitative_path_dependence_modelers: excluded voice (organized, mobile) — would formalize multi-pathway competition but sit outside the qualitative gatekeeping venues
 *   - philosophy_of_history_methodologists: analytical observer — sees the full structure of the doctrine's coordination and enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.6).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.58).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous-Climb Doctrine of Commitment Displacement (Imposition-Pathway Kernel)").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical sociology / state formation / commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'b000dfd9-7ee5-43bf-897e-adfb1d51db5f').
narrative_ontology:cs_kernel_codification('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', formalized).
narrative_ontology:cs_authority_grounding('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', expertise).
narrative_ontology:cs_interpretation_layer_present('b000dfd9-7ee5-43bf-897e-adfb1d51db5f').
narrative_ontology:cs_reading_relation('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', imposition_pathway_kernel__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', foundational, displacement_requires_prior_fringe_adoption).
narrative_ontology:cs_axiom_status(displacement_requires_prior_fringe_adoption, holdable).
narrative_ontology:cs_axiom_grounding('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', displacement_requires_prior_fringe_adoption, empirically_contingent).
narrative_ontology:cs_axiom('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', secondary, visible_decrees_ratify_existing_climbs).
narrative_ontology:cs_axiom_status(visible_decrees_ratify_existing_climbs, holdable).
narrative_ontology:cs_axiom_grounding('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', visible_decrees_ratify_existing_climbs, empirically_contingent).
narrative_ontology:cs_reference_frame('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', universal_fringe_climb_regularity).
narrative_ontology:cs_drift_state('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', post_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b000dfd9-7ee5-43bf-897e-adfb1d51db5f', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, diffusion_theory_incumbents).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, area_specialists_with_fringe_evidence).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, override_narrative_scholars).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, doctoral_students_in_historical_sociology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, doctoral_students_in_historical_sociology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit the flagship comparative-history journals, chair the graduate committees, and write the canonical textbooks in which the climb protocol is taught. Their accumulated capital — citations, editorial control, doctoral lineages — is denominated in the doctrine; abandoning it would devalue a career's worth of framework-specific work. They collect interpretive authority from every case successfully coded as a climb and set the referee standards that decide which accounts circulate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, diffusion_theory_incumbents, agenda_setter,
    institutional, generational, identity_locked, global).

% Study episodes — late-Ottoman legal reform, colonial administrative calendrics, post-colonial language policy — where the documentary record suggests decree-first sequences with no recoverable adoption chain. To publish in core venues they must either locate a fringe stage from thin traces or accept reframing as compressed climbs; otherwise their work migrates to area-studies outlets with thinner citation flow. Exit to adjacent disciplines is possible but costs accumulated specialization.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, override_narrative_scholars, payer,
    moderate, biographical, constrained, global).

% Are trained into the climb protocol as the definition of doing historical sociology correctly; their qualifying exams, dissertation prospectuses, and job-market talks are evaluated against it. The frame supplies a coherent toolkit (a real benefit) while making dissent feel like personal incompetence rather than theoretical disagreement (a real cost). Career risk from deviation falls entirely on them before tenure.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, doctoral_students_in_historical_sociology, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, doctoral_students_in_historical_sociology, beneficiary).

% Hold the treaty-port ledgers, merchant diaries, and military procurement archives in which pre-decree Western adoption is documented. The doctrine makes their material load-bearing: a dated adoption entry in a Yokohama commercial record becomes the pivot of a general theory. They can move between area studies and comparative theory with little loss.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, area_specialists_with_fringe_evidence, beneficiary,
    moderate, biographical, mobile, regional).

% Build formal models in which multiple displacement pathways compete and pathway frequency is an empirical parameter. They would test whether decree-first sequences occur at above-chance rates, but the qualitative gatekeeping venues that set coding standards do not admit their methods, so they publish in adjacent fields and their results rarely enter the doctrine's evidence base.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, quantitative_path_dependence_modelers, excluded,
    organized, generational, mobile, global).

% Analyze the structure of interpretive commitments in the historical sciences — what a coding protocol does, when a universal mechanism claim becomes unfalsifiable, how enforcement migrates from argument to gatekeeping. They collect nothing from the doctrine and pay nothing to it; they see the whole board.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, philosophy_of_history_methodologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, diffusion_theory_incumbents).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a uniform interpretive protocol for comparative institutional analysis: analysts know what evidence to seek (pre-decree adoption traces), what counts as an explanation (a traced climb), and how to code episodes across societies, making cases commensurable and deflating heroic founder myths.
% TRANSFER_FUNCTION: Moves interpretive authority, citations, and career resources toward scholars able to produce fringe-stage evidence; moves decree-first and override-friendly accounts toward marginal venues; transfers credibility from state-centered to society-centered explanations.
% ABSENT_VOICES: Scholars holding the override reading appear mainly as foils in textbook refutations rather than as participants setting coding standards. Archivists and non-Western historians whose sources record decree-first sequences without recoverable adoption chains are rarely in the rooms where review criteria are written. Formal modelers of multi-pathway competition sit outside the qualitative gatekeeping venues entirely.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, comparative historical sociology would lose its dominant coding protocol: dissertation topics, case-selection rules, journal review criteria, and the treaty-port evidence industry would all reorganize around whatever replaced the climb account; the Meiji exemplar would be re-read under different questions.
% FOUNDING_PROBLEM: Mid-century comparative history faced incommensurable national case narratives: each society's institutional change was told as a heroic act of state founders, and no cross-case comparison was possible. The climb doctrine was built to supply a uniform mechanism — diffusion through adoption chains — that made episodes comparable and deflated founder mythology.
% FOUNDING_PROBLEM_CORROBORATION: Methodologists outside the diffusion tradition (comparative political science, historical demography) corroborate that the original comparability problem was real and that uniform coding had value. But attestation that the problem is still best solved by the endogenous-climb mechanism comes almost entirely from within the benefiting tradition; outside that set, corroborators of the doctrine's continuing adequacy are largely absent — which is itself signal.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The doctrine's base extractiveness sits at 0.60 at interval end: it taxes contrary evidence directly (a decree-first case must either manufacture a fringe stage from thin traces or accept dismissal as anomalous), redirects interpretive credit from state-centered to society-centered accounts, and converts counter-evidence into confirmatory labor. Suppression (0.58) is a mix of structural gatekeeping — referee standards, grant-panel expectations, syllabus canon — and internalized fusion produced by graduate training; the split is carried by the suppression_mechanism_ambiguity omega. Theater ratio (0.50) reflects growing ritual compliance: obligatory mechanism sections and canonical citations that perform adherence without testing the universality claim. Accessibility collapse stays moderate (0.45) because workable alternatives persist in adjacent disciplines — political science, anthropology, quantitative history — so exit is costly but real. Resistance (0.55) is sustained: revisionist historians and formal modelers keep publishing pathway-plural accounts. Identity-lock binds the incumbent seat institutionally (the framework is the career's accumulated capital) and the student seat professionally (competence itself is defined by the frame); breaking either frame would convert the corresponding seat's classification quickly. Junior scholars retain latent coalition leverage through collective action over review burdens and job-market standards, though it is rarely exercised. All three tracked metrics share one six-point time grid (interval years roughly 1950-2025); the mild reversal at t45 reflects the cultural-turn relaxation of gatekeeping, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat the doctrine computes as coordination-dominant: it is the field's shared grammar, and demanding pre-decree adoption evidence is simply rigor. From the override-scholar seat the identical demand computes as extraction: an unfalsifiable tax paid in reframing labor and venue exclusion. From the student seat the frame is not experienced as a constraint at all but as competence itself — the deepest identity-lock in the story. The engine derives these divergent per-seat classifications from the structural data (role, power, exit options); the divergence, not any single verdict, is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents sit near the beneficiary pole (low d): the doctrine subsidizes them with citations, editorial control, and training pipelines, and their identity-lock removes exit pressure. Area specialists with fringe evidence also sit low — the doctrine elevates their archival material and they can move. Override scholars sit near the target pole (high d): they bear the framing tax under constrained exit. Doctoral students sit nearest full-target: powerless, identity-locked, paying in career risk. Note the same-level contrast: override scholars and area specialists hold comparable nominal standing (moderate power, comparable seniority), yet occupy opposite poles because the fit between their evidence type and the doctrine's demand determines their position — evidence-type fit, not rank, is the constraint-specific differentiator. The discipline's global scope raises verification difficulty, which the engine folds into effective extraction at the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — incommensurable nationalist case narratives blocking comparison — is partially live: comparability remains necessary, but whether a single uniform mechanism can supply it is now disputed. Reading the doctrine as pure rope would hide the real extraction from override-account scholars; reading it as pure snare would erase the genuine comparability function that still organizes thousands of case studies. The tangled-rope classification keeps both faces visible. Mandatrophy is not resolved: the doctrine has not outlived its function, but its function and its rent-collection have grown apart, and the theater series tracks the widening gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the endogenous-climb account the correct instantiation of the imposition-pathway kernel, or do the exogenous-override and hybrid-cascade readings capture displacement episodes this reading must explain away?',
    'Case-level adjudication: systematically search decree-driven episodes under archival saturation for any adoption trace preceding the decree; episodes with decree-first sequences despite saturated archives support the override reading, documented artificial-fringe cascades support the hybrid reading, and systematic pre-decree traces support this reading.',
    'If override-type episodes exist at scale, this reading''s universality axiom fails, its extraction profile shifts from protective-of-truth to enforcement-of-error, and the sibling reading becomes the live mechanism cell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of the imposition-pathway kernel the case evidence actually supports.').

omega_variable(
    discovered_law_vs_constructed_doctrine,
    'Is the fringe-then-climb pathway a discovered regularity of social process, or a constructed interpretive convention whose universality benefits the tradition that maintains it?',
    'Pre-registered case searches run by researchers with no stake in the doctrine, with detection rates of pre-decree adoption traces compared against preservation-adjusted chance baselines.',
    'If constructed, the constraint is scholarly enforcement wearing a natural-law costume and payer-seat classifications harden toward snare; if discovered, much of the measured extraction is the price of a true and useful standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_law_vs_constructed_doctrine, empirical, 'Natural regularity versus constructed doctrine — the false-summit ambiguity for this constraint.').

omega_variable(
    invisible_stage_unfalsifiability,
    'Can appeals to ''invisible fringe stages'' be distinguished from unfalsifiable retrofitting — is every missing pre-decree trace attributable to preservation failure rather than a genuine decree-first sequence?',
    'Differential-preservation tests: compare documentary-density-adjusted detection rates of pre-decree adoption traces across archive-rich and archive-poor cases; if traces appear only where archives permit finding them, the stage may be an artifact of the search protocol.',
    'High retrofit risk raises the doctrine''s effective extraction (it taxes contrary evidence) and pushes payer seats toward snare classifications; robust detection in archive-poor cases vindicates the reading''s exemplar logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_stage_unfalsifiability, empirical, 'Whether the invisible-stage device is evidence-sensitive or retrofit-proof.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of override accounts structural (referee gatekeeping, funding panels, canon control) or internalized (graduate training fusing professional identity with the climb frame so dissent registers as incompetence)?',
    'Post-exit trajectory: track scholars who leave the tradition; immediate unimpeded override-friendly publication elsewhere indicates structural suppression, continued self-censorship indicates internalization.',
    'Internalized suppression raises effective suppression above the structural measure and deepens identity-lock at the student and junior seats; purely structural suppression would fall quickly if gatekeeping venues lost authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the doctrine''s enforcement.').

omega_variable(
    meiji_precedence_evidence_weight,
    'How strong is the documentary case that Western calendar and dress adoption preceded the Meiji decrees among treaty-port merchants, military modernizers, and the merchant class — strong enough to carry the reading''s flagship claim?',
    'Systematic dating of adoption events in treaty-port commercial records, naval and army procurement logs, and urban custom surveys relative to the 1872 solar-calendar decree and the early-1870s hair and clothing edicts.',
    'Strong precedence stabilizes the reading''s universality claim and its coordination function; weak or post-hoc precedence collapses the exemplar, shifting evidential weight to the hybrid reading and raising this constraint''s extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_precedence_evidence_weight, empirical, 'Evidential strength of the Meiji pre-decree adoption record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(impo_tr_t15, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t45, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 45, 0.44).
narrative_ontology:measurement_basis(impo_tr_t45, observed).
narrative_ontology:measurement(impo_tr_t60, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(impo_tr_t60, observed).
narrative_ontology:measurement(impo_tr_t75, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 75, 0.5).
narrative_ontology:measurement_basis(impo_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(impo_be_t15, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t45, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement_basis(impo_be_t45, observed).
narrative_ontology:measurement(impo_be_t60, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(impo_be_t60, observed).
narrative_ontology:measurement(impo_be_t75, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement_basis(impo_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(impo_su_t15, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t45, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement_basis(impo_su_t45, observed).
narrative_ontology:measurement(impo_su_t60, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(impo_su_t60, observed).
narrative_ontology:measurement(impo_su_t75, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement_basis(impo_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'top-down imposition' conflates three structurally distinct claims about how commitment displacement proceeds — endogenous climb only (this file), exogenous override as a distinct mechanism, and hybrid cascade (override initiates, climb completes). Each member carries its own epsilon, beneficiary/victim structure, and enforcement profile. This reading is the upstream member: it historically supplied the interpretive standard against which the siblings define themselves, so edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
