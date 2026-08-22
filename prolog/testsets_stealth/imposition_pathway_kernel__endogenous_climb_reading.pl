% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Endogenous Climb Doctrine of Commitment Displacement
 *   domain: historical sociology / state formation / commitment systems
 *
 * SUMMARY:
 *   On the endogenous_climb_reading, commitment displacement — the
 *   replacement of one collective commitment (calendars, dress codes,
 *   scripts, ritual and institutional forms) by another — always begins at
 *   the social periphery: fringe actors adopt the incoming practice, the
 *   climb proceeds through adjacent strata, and the center ratifies what is
 *   already underway. Apparent top-down impositions are compressed climbs
 *   whose fringe stages escaped the administrative record. The flagship case
 *   is Meiji Japan: Western calendrical and sartorial practice had already
 *   taken hold in treaty-port commerce, merchant households, and modernizing
 *   military circles before the 1872-73 decrees, so the state's role was
 *   acceleration and generalization of an existing climb, not initiation.
 *   Within the discipline this reading operates as an interpretive
 *   settlement: explanations of displacement route through the climb pathway,
 *   and cases that appear to violate it are re-described (an invisible fringe
 *   stage is posited) rather than allowed to count against it. The
 *   enforcement surface is peer review, textbook canonization, and hiring
 *   norms; the burden of proof falls on anyone claiming a center-initiated
 *   sequence. Family relations and sibling readings are recorded in
 *   commentary.kernel_context and network.dual_formulation_note. KEY AGENTS
 *   (by structural relationship): - diffusion_paradigm_scholars: Primary
 *   beneficiary (organized/identity_locked) — collects citation flows and
 *   interpretive authority - disciplinary_journal_editors: Agenda setter
 *   (institutional/arbitrage) — administers the review gates that screen
 *   pathway framings - climb_template_textbook_authors: Secondary beneficiary
 *   (moderate/constrained) — canonizes the template in curricula -
 *   decentred_state_formation_historians: Primary target
 *   (moderate/constrained) — bears the re-reading of decree-first evidence -
 *   pathway_pluralist_researchers: Secondary target (moderate/constrained) —
 *   presses for distinct mechanism cells, meets gatekeeping -
 *   graduate_students_in_historical_sociology: Tertiary target
 *   (powerless/trapped) — absorbs training costs and career risk -
 *   decree_executing_officials: Excluded witness (powerless/trapped) — holds
 *   ground-level knowledge of initiation sequences, absent from venues -
 *   philosophy_of_history_analysts: Analytical observer
 *   (analytical/analytical) — sees the whole enforcement structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.62).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.58).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Doctrine of Commitment Displacement").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical sociology / state formation / commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, '3a8d59cf-2c53-458c-abf4-a932d8467798').
narrative_ontology:cs_kernel_codification('3a8d59cf-2c53-458c-abf4-a932d8467798', distributed).
narrative_ontology:cs_authority_grounding('3a8d59cf-2c53-458c-abf4-a932d8467798', expertise).
narrative_ontology:cs_interpretation_layer_present('3a8d59cf-2c53-458c-abf4-a932d8467798').
narrative_ontology:cs_reading_relation('3a8d59cf-2c53-458c-abf4-a932d8467798', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('3a8d59cf-2c53-458c-abf4-a932d8467798', imposition_pathway_kernel__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('3a8d59cf-2c53-458c-abf4-a932d8467798', foundational, no_center_initiated_displacement).
narrative_ontology:cs_axiom_status(no_center_initiated_displacement, holdable).
narrative_ontology:cs_axiom_grounding('3a8d59cf-2c53-458c-abf4-a932d8467798', no_center_initiated_displacement, empirically_contingent).
narrative_ontology:cs_axiom('3a8d59cf-2c53-458c-abf4-a932d8467798', secondary, apparent_impositions_are_compressed_climbs).
narrative_ontology:cs_axiom_status(apparent_impositions_are_compressed_climbs, holdable).
narrative_ontology:cs_axiom_grounding('3a8d59cf-2c53-458c-abf4-a932d8467798', apparent_impositions_are_compressed_climbs, empirically_contingent).
narrative_ontology:cs_reference_frame('3a8d59cf-2c53-458c-abf4-a932d8467798', endogenous_climb_default_pathway).
narrative_ontology:cs_drift_state('3a8d59cf-2c53-458c-abf4-a932d8467798', contemporary_pluralist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a8d59cf-2c53-458c-abf4-a932d8467798', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, diffusion_paradigm_scholars).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, climb_template_textbook_authors).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, decentred_state_formation_historians).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, pathway_pluralist_researchers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, graduate_students_in_historical_sociology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, disciplinary_journal_editors).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, rogers_adopter_category_model).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, s_curve_displacement_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers in the Tarde-Rogers diffusion lineage studying how practices, technologies, and commitments spread through populations. Their methods courses, doctoral lineages, and published corpora are framed in adopter-category terms; citation flows, invited lectures, and editorial invitations accrue to fluency in the template. Leaving the framework would mean reframing a lifetime of publications and severing the intellectual lineage that anchors their standing.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, diffusion_paradigm_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Authors of the canonical syntheses and graduate textbooks that organize norm-change material into fringe-to-center sequences. Royalties, course adoptions, and revision cycles depend on the unified template remaining the organizing scheme; a plural-mechanism reorganization would strand their backlists and force full rewrites against their own prior work.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, climb_template_textbook_authors, beneficiary,
    moderate, biographical, constrained, national).

% Historians working on decree-led transformations — calendar reforms, dress laws, script and language policy — whose archival sequences show the official act preceding broad popular adoption. Under the prevailing template their cases are re-read as missed fringe stages, a re-reading they must rebut case by case at their own expense; their realistic fallback venues are area-studies and national journals with thinner international readership.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, decentred_state_formation_historians, payer,
    moderate, biographical, constrained, continental).

% Scholars arguing that displacement has more than one pathway and pressing for a distinct treatment of center-initiated change. They organize workshops and special issues at the field's margins, spend disproportionate effort on admissibility arguments rather than substantive findings, and watch their proposals recast as variants of the climb template rather than rivals to it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, pathway_pluralist_researchers, payer,
    moderate, biographical, constrained, global).

% Doctoral students whose committees, reference lists, and job-market letters are saturated with the template. Deviating from it risks advisor support and placement; conforming locks their earliest publications into climb framings they will spend their careers defending or abandoning at personal cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, graduate_students_in_historical_sociology, payer,
    powerless, immediate, trapped, national).

% Editors of the leading comparative-historical and sociological journals. They set review queues, commission special issues, and decide which pathway framings count as mainstream; the template supplies their screening heuristics and their authority rests on the paradigm's prestige, so administering it reproduces their own position. They could alter screening standards unilaterally but have little incentive to.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, disciplinary_journal_editors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, disciplinary_journal_editors, beneficiary).

% Historical administrators, ministers, and officers who drafted and enforced displacement edicts. They hold ground-level knowledge of initiation sequences — what existed before the decree, what the decree actually changed — but are dead, retired, or institutionally distant from the seminars and journals where the pathway question is adjudicated; their testimony sits in archives that few template-driven projects consult.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, decree_executing_officials, excluded,
    powerless, biographical, trapped, regional).

% Philosophers and social-studies-of-science scholars who study how disciplines canonize templates, absorb counterexamples, and police mechanism boundaries. They take no side in the pathway dispute; their vantage point sees the whole enforcement structure, including the argumentative role the rescue clause plays.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, philosophy_of_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, diffusion_paradigm_scholars).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives dispersed researchers of norm change a shared descriptive scheme — adopter categories, S-curve expectations, fringe-to-center sequencing — that makes case studies comparable and cumulative; it solved the fragmentation of adoption observations across societies, sectors, and epochs.
% TRANSFER_FUNCTION: Moves interpretive authority, citation flow, and curriculum space toward scholarship framed in climb terms; moves the burden of proof onto researchers claiming center-initiated displacement, who must fund and defend a search for a fringe stage the template presumes exists; moves dissent toward marginal venues.
% ABSENT_VOICES: Decree-executing officials and the archivists of center-led campaigns — the people who drafted, signed, and enforced displacement edicts — are absent from the venues where the pathway question is adjudicated, as are historians of regimes whose records begin with the decree. Their testimony bears directly on initiation sequences; the template's universality was ratified without them in the room.
% DISAPPEARANCE_RATIONALE: If the universal climb doctrine vanished overnight, comparative-historical syllabi, in-progress dissertations, and funded research programs framed in adopter-category terms would require reorganization around plural mechanism cells; the discipline's case-comparison vocabulary would fragment pending a successor scheme. Textbook backlists, doctoral lineages, and journal screening heuristics would all need rebuilding — a real rearrangement, not a return to some natural default.
% FOUNDING_PROBLEM: Mid-twentieth-century students of adoption faced an unordered mass of observations — hybrid seed, medical practice, agricultural technique spreading at different rates through different strata — with no common model; the Tarde-Rogers lineage built the fringe-adoption/climb template to impose order and enable prediction, and the template was later extended to state-led displacements it was not built from.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: social-studies-of-science histories document the template's origin in mid-century American rural sociology rather than in state-formation evidence, and coercion-centered state-formation historiography attests that decree-led displacements remain poorly ordered by the climb template. Both non-beneficiary sources agree the founding problem is live for adoption-style cases and contested for center-led ones; no party outside the dispute attests that the universal extension was ever settled.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.62 because the universal quantifier plus the invisible-fringe rescue clause shifts the burden of proof wholesale onto dissenters: every decree-first case must purchase admissibility by searching for a fringe stage the template presumes exists, an unfalsifiable demand paid in research time and venue access. Suppression is 0.58 — gatekeeping is real but soft-power; heterodox venues and interdisciplinary outlets exist, so alternatives are taxed rather than eliminated. Theater is 0.28: most maintenance is genuine analytical labor (documenting pre-decree adoption is real archival work), but a growing share is ritual citation of the S-curve without testing rival mechanisms. Accessibility collapse is 0.55: accepting the universal claim collapses rival mechanistic accounts almost completely, yet the rescue clause is itself visible to critics, keeping partial alternatives alive. Resistance is 0.60: the sibling readings and coercion-centered state-formation historiography constitute organized, persistent pushback. The temporal series run on one shared grid (t=0..60, mapped to roughly 1962-2022): extractiveness climbed monotonically through the paradigm's canonization decades and plateaus near its ceiling; theater crept upward as routine citation replaced engagement; suppression_requirement traces an enforcement arc — a ratchet through the canonization peak (t=40) followed by partial decay as open-access and interdisciplinary publishing pluralized venues. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change; base_properties.suppression (0.58) states the current enforcement picture as a raw, unscaled structural property, whereas extractiveness is the quantity the engine scales by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From inside the paradigm community the template is accumulated science: decades of cumulative adoption findings, a shared vocabulary, teachable regularities. From the state-formation historian's desk the same structure is a gate: their archival sequences are inadmissible until rebutting an unfalsifiable presumption. Graduate students experience the constraint as career topology before they ever evaluate its truth content — deviation is priced in placement risk, not argument. Journal editors occupy the hinge: they administer the screening that produces the asymmetry while depending on the paradigm's prestige for their own authority. The engine computes these per-seat divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Paradigm scholars and textbook authors derive directionality near the beneficiary end: the template subsidizes their interpretive capital, and their identity-locked exit (leaving means reframing a life's work) dampens any cost they bear from maintaining it. Decentred state-formation historians and pluralist researchers derive high directionality: they pay the burden-shifting directly, with constrained exit (area-studies venues, workshop circuits) that traps part of the cost. Graduate students sit nearest the full-target end: powerless, trapped exit, paying training and career costs with no offsetting collection. Journal editors, as agenda setters who also collect prestige, derive low-to-moderate directionality — their enforcement amplifies what targets bear without raising their own exposure. The discipline's global spatial scope raises verification difficulty (whose archives count as evidence of a fringe stage?), which scales effective extraction modestly upward for targets. Suppression, by contrast, enters the computation unscaled: it is a raw structural property of the arrangement, not a per-seat quantity.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both faces of the arrangement visible. The coordination half is genuine: mid-century adoption studies were a fragmented mass of observations, and the fringe-to-center template imposed order, comparability, and predictive tools — that problem was real and the template solved it. The extraction half is equally real: the universal quantifier converted a useful descriptive scheme into a gate that taxes rival mechanisms, with the invisible-fringe clause as the load-bearing immunization. Calling the arrangement pure coordination would launder the burden-shifting; calling it pure extraction would erase the documented pre-decree adoption findings and the template's real cumulative yield. The R5 interview finds no zombie: founding_problem_status is contested, not dead — the ordering problem the template was built for remains live for adoption-style displacements, so the mismatch flag (dead status x rearranging world) does not fire. Mandatrophy is not resolved; the doctrine has not outlived its function so much as entered renegotiation. The trajectory to watch is theatrical: if the pluralist challenge prevails and the template survives mainly as ritual citation, theater_ratio will climb past 0.5 and the arrangement will drift toward inertial maintenance — the piton path — unless the community pays the cost of narrowing the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint instantiates the endogenous_climb_reading of the imposition_pathway_kernel; what structural differences would instantiating the sibling readings (exogenous_override_reading, hybrid_cascade_reading) produce?',
    'Compile the sibling stories and compare computed classifications, victim sets, and epsilon values across the kernel family; divergent verdicts locate the structural carrier of the disagreement.',
    'Under the exogenous reading, a distinct center-initiated mechanism cell opens, this reading''s universal quantifier fails, and its burden-shifting profile collapses toward a plain shared template; under the hybrid reading, official acts become initiators rather than ratifiers, relocating the contested surface from archives to decision processes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Kernel membership: this story is one of three readings of the imposition-pathway kernel; siblings are separate files.').

omega_variable(
    invisible_fringe_falsifiability,
    'Is the invisible-fringe-stage clause a disciplined empirical discovery procedure (directing archive search for pre-decree adoption) or an immunizing stratagem (absorbing any counterexample after the fact)?',
    'Pre-registered archival protocols specifying in advance what evidence would count as absence of a fringe stage, applied to a sample of decree-led displacements; track whether practitioners accept any null result.',
    'If immunizing, the doctrine converts falsifiers into confirmers and its burden-shifting exceeds the authored measure, trending the classification toward pure gatekeeping; if disciplined, the clause is ordinary abductive inference and the hybrid coordination-plus-burden reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_fringe_falsifiability, empirical, 'Falsifiability status of the invisible-fringe-stage rescue clause.').

omega_variable(
    meiji_flagship_representativeness,
    'Is the Meiji calendar-and-dress sequence representative of apparent top-down impositions generally, or a selected flagship whose treaty-port fringe was unusually well documented?',
    'Systematic cross-regime sampling of decree-led displacements (Peterine Russia, Republican Turkey, Soviet cultural campaigns) with uniform archival search for pre-decree adoption traces.',
    'If Meiji is unrepresentative, the universal claim fails on base rates, the reading''s evidentiary core erodes, and the arrangement migrates toward gatekeeping sustained by inertia; if representative, the reading''s descriptive claim strengthens and its coordination value rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_flagship_representativeness, empirical, 'Representativeness of the Meiji flagship case for the universal claim.').

omega_variable(
    coordination_extraction_separability,
    'Is the climb template''s coordination value (shared case vocabulary, cumulative comparison) separable from its universal quantifier, such that a pluralist-compatible weakening would retain the coordination while shedding the burden-shifting?',
    'Observe whether pluralist-friendly reformulations retain citation cumulativity and teaching utility in venues that admit multiple mechanism cells.',
    'If separable, the burden rides on the universality claim specifically and a transitional, sunsettable status becomes available for the template; if inseparable, the burden is intrinsic to the template''s boundary-marking function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Separability of the template''s coordination function from its universal claim.').

omega_variable(
    paradigm_identity_fusion,
    'Is the pressure bearing on dissenters structural (gatekept venues, hiring norms) or partly internalized (professional identity fused with the paradigm such that deviation feels like self-betrayal)?',
    'Post-exit trajectory of scholars who publicly switched frameworks: if reputational and self-concept penalties persist after leaving gatekept venues, an internalized component is present.',
    'Internalized pressure raises effective suppression above the structural measure and deepens identity-locked exit for paradigm scholars; purely structural pressure would ease as venue pluralization continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_identity_fusion, empirical, 'Structural versus internalized suppression in paradigm enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(impo_tr_t40, observed).
narrative_ontology:measurement(impo_tr_t50, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(impo_tr_t50, observed).
narrative_ontology:measurement(impo_tr_t60, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(impo_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(impo_be_t40, observed).
narrative_ontology:measurement(impo_be_t50, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement_basis(impo_be_t50, observed).
narrative_ontology:measurement(impo_be_t60, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(impo_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(impo_su_t40, observed).
narrative_ontology:measurement(impo_su_t50, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement_basis(impo_su_t50, observed).
narrative_ontology:measurement(impo_su_t60, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(impo_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'top-down imposition' covers three structurally distinct claims about displacement pathways; per the epsilon-invariance principle they are authored as three stories sharing the imposition_pathway_kernel: this file (endogenous_climb_reading — all displacement is periphery-initiated climb, impositions are compressed climbs), imposition_pathway_kernel__exogenous_override_reading (center-initiated displacement as a distinct mechanism requiring its own cell), and imposition_pathway_kernel__hybrid_cascade_reading (official acts create an artificial fringe that then climbs organically). Each carries its own epsilon, victim set, and classification. Edges run from this story to both siblings because the endogenous reading is the established settlement: its universality defines the operating environment the siblings must argue against, so changes in its standing propagate to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
