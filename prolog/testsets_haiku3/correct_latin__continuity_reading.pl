% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Evolved Form (Continuity Reading)
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading establishes that correct Latin is the form
 *   transmitted through unbroken living practice, not a reconstructed ancient
 *   standard. Medieval Latin—with its phonetic shifts, metric innovations,
 *   and vocabulary additions—is legitimate evolved Classical Latin, not
 *   corruption. The constraint operates through institutional practice
 *   (ecclesiastical authority, manuscript production, pedagogy) rather than
 *   through explicit rules. It redistributes authority from textual-based
 *   correctionists to practicing communities. The reading claimed here
 *   (continuity) frames medieval Latin as valid; its sibling readings
 *   (discontinuity, hybrid) would frame it as corrupt or partially corrupted.
 *   This story generates the continuity reading's own constraint structure:
 *   its extraction, enforcement, beneficiary set, and the resistance it faces
 *   from classical purists and reconstructionists.
 *
 * KEY AGENTS:
 *   - medieval clergy and cathedral school masters: the primary practicing community whose usage defines correct Latin under this reading
 *   - ecclesiastical institutional authority: enforces the reading by endorsing medieval clergy's Latin as legitimate and training new practitioners
 *   - humanist philologists: bear the burden of adjudicating correctness claims; have exit options to switch readings
 *   - classical purists: constrained targets whose corrective agenda is delegitimized by the continuity reading
 *   - textual reconstructionists: excluded, marginalized as antiquarian rather than authoritative
 *   - romance language etymologists: benefit from the continuity reading because medieval forms become legitimate evolutionary steps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.38).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.42).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Medieval Latin as Legitimate Evolved Form (Continuity Reading)").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '4a2b46f1-53b4-4d47-8741-ef51e73a329a').
narrative_ontology:cs_kernel_codification('4a2b46f1-53b4-4d47-8741-ef51e73a329a', distributed).
narrative_ontology:cs_authority_grounding('4a2b46f1-53b4-4d47-8741-ef51e73a329a', practice).
narrative_ontology:cs_interpretation_layer_present('4a2b46f1-53b4-4d47-8741-ef51e73a329a').
narrative_ontology:cs_reading_relation('4a2b46f1-53b4-4d47-8741-ef51e73a329a', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a2b46f1-53b4-4d47-8741-ef51e73a329a', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('4a2b46f1-53b4-4d47-8741-ef51e73a329a', foundational, unbroken_transmission_legitimates_evolution).
narrative_ontology:cs_axiom_status(unbroken_transmission_legitimates_evolution, holdable).
narrative_ontology:cs_axiom_grounding('4a2b46f1-53b4-4d47-8741-ef51e73a329a', unbroken_transmission_legitimates_evolution, deontological).
narrative_ontology:cs_axiom('4a2b46f1-53b4-4d47-8741-ef51e73a329a', foundational, practice_based_authority_trumps_textual_reconstruction).
narrative_ontology:cs_axiom_status(practice_based_authority_trumps_textual_reconstruction, overridden).
narrative_ontology:cs_axiom_grounding('4a2b46f1-53b4-4d47-8741-ef51e73a329a', practice_based_authority_trumps_textual_reconstruction, conventional).
narrative_ontology:cs_reference_frame('4a2b46f1-53b4-4d47-8741-ef51e73a329a', unbroken_transmission_legitimacy).
narrative_ontology:cs_drift_state('4a2b46f1-53b4-4d47-8741-ef51e73a329a', renaissance_manuscript_recovery, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a2b46f1-53b4-4d47-8741-ef51e73a329a', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_clergy_and_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, living_latin_communities).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, romance_language_etymologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, humanist_philologists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classical_purists).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, organic_language_evolution_principle).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, practice_based_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use medieval Latin forms (metric contractions, phonetic shifts, vocabulary innovations) as their everyday written language for theology, law, and governance. Their legitimacy to author in these forms rests on the continuity reading: what they write is valid evolved Latin, not corrupted Latin. They are the primary community that *practices* continuity; their writing constitutes the evidence for the claim.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_clergy_and_scholars, beneficiary,
    organized, generational, constrained, continental).

% Bear the burden of adjudicating correctness claims across two distinct linguistic systems. Under the continuity reading, they must accept medieval forms as legitimate evolved usage; under a discontinuity reading they could dismiss medieval usage as error. The continuity reading constrains their authority to mark medieval Latin as corrupt and forces them to engage with organic evolution rather than textual-canonical authority. Their arbitrage is high: they can switch readings, publish in discontinuity framing, or seek patronage from courts that back a particular reading.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, humanist_philologists, payer,
    powerful, biographical, arbitrage, national).

% Scholars and ecclesiastical authorities committed to Classical purity as the standard of correct Latin. The continuity reading delegitimizes their corrective work: under continuity, medieval forms that they view as deviation are reframed as natural evolution. They cannot easily exit because institutional prestige and textual authority are tied to the Classical standard; yet the continuity reading reduces their leverage to enforce it. They face suppression of their corrective agenda.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_purists, payer,
    moderate, biographical, constrained, national).

% Institutions (scriptoria, cathedral schools, monasteries) that embody and transmit the living practice of medieval Latin writing. They set the standard through what they actually produce: their manuscripts ARE the evidence for continuity. They enforce the reading by continuing medieval practice and by training new scribes in medieval conventions. Their enforcement is low-coercion because it is embedded in institutional routine rather than explicit correction.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, manuscript_authorities, agenda_setter,
    powerful, generational, mobile, continental).

% Scholars and textual editors who reconstruct Classical texts from medieval manuscripts. They would argue that medieval copyists corrupted the Classical originals and that reconstructive philology must correct back to the 'true' forms. The continuity reading marginalizes their authority by treating medieval forms as legitimate evolution rather than error to be undone. They are excluded from co-authoring the legitimacy standard; their corrective claims are reframed as antiquarian, not authoritative.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_reconstructionists, excluded,
    institutional, generational, trapped, continental).

% The Church (through popes, councils, and cathedral schools) endorses the continuity reading because it legitimates the medieval clergy's own Latin usage and their authority to author sacred and legal texts. Endorsement of continuity serves institutional stability: the Church can claim its clergy speak correct Latin, not corrupt or reformed Latin. Ecclesiastical authority both enforces and benefits from the reading.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, ecclesiastical_authority, beneficiary).

% Scholars of Occitan, Old French, Old Spanish, and Italian who trace those languages' origins in medieval Latin. The continuity reading permits them to treat medieval Latin forms as legitimate evolutionary steps between Classical and Romance languages, not as corrupt departures. Their academic authority depends on showing legitimate linguistic descent; continuity reading validates this genealogy. They have exit options: they can shift to treating medieval Latin as error and Romance languages as spontaneous reformation.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, romance_language_etymologists, beneficiary,
    powerful, biographical, mobile, national).

% Renaissance and early-modern scholars (humanists of the discontinuity camp) who advocate reconstructing Classical Latin by correcting medieval 'barbarisms'. They observe the constraint from outside but mount real pressure against it, especially as printing and classical scholarship increase access to ancient texts. They measure the gap between medieval practice and Classical evidence.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, antiquarian_reformers, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for what counts as 'correct Latin' by anchoring it to continuous living practice rather than to a fixed canonical text. This solves the coordination problem: medieval clergy can confidently author in their own linguistic forms and know those forms are legitimate, not because they match some ancient standard but because they continue an unbroken transmission chain. Scholars can align their judgments by asking 'what do the practitioners do?' rather than 'what does the Classical text say?'
% TRANSFER_FUNCTION: Transfers authority from textual reconstructionists (who would correct medieval forms against Classical evidence) to practitioners of medieval Latin (clergy, monks, cathedral school masters). Under this reading, the living community's practice is the source of legitimacy; under the discontinuity reading, the ancient text is. The continuity reading moves the seat of authority from manuscript editors to working linguists.
% ABSENT_VOICES: Textual reconstructionists and classical purists who advocate for Classical purity are structurally excluded: the reading reframes their corrective work as antiquarian or pedantic, not as authoritative adjudication. They would argue that medieval forms ARE corrupted and should be corrected by reference to Classical sources. Writers and speakers in Romance languages (descendants of medieval Latin) have no voice in the legitimacy standard, though their existence is cited as evidence that medieval forms were valid evolutionary steps.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared (replaced by discontinuity), medieval texts would be relabeled as corrupt rather than legitimately evolved, teaching curricula would shift from acceptance to correction, and the authority of textual reconstructionists would rise dramatically. The Church would face pressure to 'correct' its Latin back to Classical purity. Educational and ecclesiastical practices would reorganize around the corrective agenda.
% FOUNDING_PROBLEM: Early medieval church needed a standard for correct Latin when Classical education had fragmented and Latin was evolving in living communities. The continuity reading solved this by anchoring correctness to what practitioners actually do, not to reconstructed ancient forms that were no longer accessible or directly observable. It allowed medieval clergy to confidently author in their own language without waiting for textual reconstruction or Classical purism.
% FOUNDING_PROBLEM_CORROBORATION: Medieval manuscripts and ecclesiastical practice attest to the continuity reading (the clergy used medieval forms confidently and trained successors in those forms). Renaissance humanists and textual reconstructionists attest the founding problem is no longer live: Classical texts become increasingly accessible through manuscript recovery (by t~20, the interval endpoint), and the 'correct' form should align with demonstrable ancient usage. Modern historical linguistics supports continuity as a description of what actually happened (Romance languages evolved from medieval Latin), but debates whether medieval forms should be *normatively* deemed 'correct' (a different question the reading seeks to settle). The founding problem is dead by interval end; ecclesiastical investment in the continuity reading persists beyond its functional necessity.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).
:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.22 at t=0) because the reading does extract authority from textual correctionists and consolidate it around practitioners, but the extraction is not pure: the reading genuinely solves the medieval coordination problem (how to confidently author in evolving forms). Theater ratio is low-moderate (0.29) because institutional practice of medieval Latin is functionally real—clergy do use these forms for actual governance and theology—but the performance element rises over time as Renaissance humanists mount increasing pressure for reconstructed Classical purity, forcing defenders of continuity to more actively justify their reading. Suppression requirement tracks theater ratio closely (0.42 at end) because maintaining the continuity reading requires suppressing the reconstructionist corrective agenda—marginalizing their voice as pedantic rather than authoritative. The temporal arc shows rising pressure from the Renaissance onward: extractiveness, theater, and suppression all increase from t=0 to t=20, then stabilize slightly as the reading reaches a defensible equilibrium. The reading is CLAIMED as rope (genuine coordination + transmission function) and the metrics are authored honestly: there is real coordination work (transmitting a living standard), real extraction (redirected authority), and real enforcement (institutional suppression of the discontinuity reading). The claim/metric gap is deliberate and reflects the reading's own internal coherence claim: 'we coordinate the community; the extractive element is merely the cost of excluding false alternatives.'
 *
 * PERSPECTIVAL GAP:
 *   The practicing community (medieval clergy, manuscript authorities) experiences this as pure coordination: they are aligned around a shared standard (medieval practice), and the constraint enables their collaborative work in theology and governance. The humanist philologists and classical purists experience it as extraction: the same constraint that enables clergy's authority also disables their corrective work. The ecclesiastical institutional authority experiences the constraint as both coordination (internally) and enforcement (externally, against the purist challenge). The engine computes per-seat directionality from power + exit + beneficiary/victim data; this three-way divergence is structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval clergy and ecclesiastical authority are beneficiaries under this reading (d near 0.0–0.3): they benefit from a standard that legitimates their practice without requiring reconstructive work. Humanist philologists and classical purists are targets (d near 0.7–1.0): the reading suppresses their authority to correct medieval forms against Classical evidence. Their exit is constrained because institutional prestige is still tied to textual authority, but the continuity reading reduces their leverage. Romance etymologists are beneficiaries (d near 0.2): the reading validates the evolutionary chain from medieval Latin to Romance languages. Textual reconstructionists are excluded rather than harmed; they experience the constraint as delegitimizing but are not formally targeted. No directionality override is necessary: the structural data (beneficiaries, victims, exit constraints) derive the divergence automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval clergy need a standard for correct Latin when classical forms are inaccessible) is LIVE as the reading is authored: medieval clergy genuinely lack direct access to Classical sources and genuinely need a practice-based standard. However, the founding problem DIES by t=20 as manuscript recovery accelerates and printing brings Classical texts into wider circulation. At t=25 and t=30, the mandatrophy is pronounced: Classical sources are accessible, so the founding coordination problem is solved, yet the continuity reading persists through institutional inertia and ecclesiastical investment in medieval forms. The measurement series show suppression rising as the founding problem dies (t=15–t=20), which is the marker of mandatrophy: the constraint persists not because it solves the founding problem but because its beneficiaries maintain it. A rope whose founding problem is dead is a candidate for reclassification as piton. The reading itself remains internally coherent (medieval forms ARE legitimately evolved), but its functional justification erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_chain_breakage,
    'Is the transmission chain of medieval Latin truly unbroken, or are there ruptures where practice was interrupted and relearned from texts?',
    'Manuscript codicology and linguistic paleography tracing pedagogical methods and the introduction of textual reference in scriptoria; oral history of transmission lines where records survive.',
    'If transmission is genuinely unbroken, the continuity reading is empirically validated and medieval forms are legitimately evolved. If transmission was periodically interrupted and relearned from texts, the claim is undermined: medieval forms may be corrupted imitations of texts, not evolved continuations. This would strengthen the discontinuity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_chain_breakage, empirical, 'Whether medieval Latin transmission is continuous or episodically interrupted and textually reconstructed.').

omega_variable(
    evolution_vs_corruption_criterion,
    'What structural criterion distinguishes ''legitimate evolution'' from ''corruption''? Is the criterion observable change in phonology/morphology (evolution) vs. loss of intelligibility or functional capacity (corruption)? Or is the criterion political/institutional endorsement?',
    'Comparative study of other language evolution cases (Romance languages, Germanic languages post-Latin) where the evolution/corruption distinction is clearer; linguistic analysis of whether medieval Latin forms remained functionally communicative and generationally transmissible.',
    'If evolution is defined by observability and functional continuity, medieval Latin qualifies under almost any analysis, and the reading is robust. If evolution is defined by institutional approval or textual fidelity, the reading is more vulnerable to competing readings that invoke different institutional frames.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evolution_vs_corruption_criterion, conceptual, 'Whether legitimate linguistic evolution is a descriptive fact or a normative judgment dependent on institutional framing.').

omega_variable(
    reading_foreclosure_possibility,
    'Does the continuity reading logically foreclose the discontinuity reading, or do they coexist as rival institutional framings?',
    'Logical analysis: can one institution hold both framings simultaneously (medieval forms are evolving AND corrupted), or must it choose? How do actual medieval and Renaissance institutions navigate the choice?',
    'If readings foreclose each other, the constraint is a genuine zero-sum institutional competition. If they coexist (different institutions choose different readings), the constraint is a coordination equilibrium with rivals. The characterization affects how the dualism is modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the continuity and discontinuity readings are logically incompatible within a single framework or merely institutionally divergent.').

omega_variable(
    ecclesiastical_investment_duration,
    'How long does ecclesiastical institutional investment in the continuity reading persist after the founding problem (inaccessibility of Classical texts) is solved?',
    'Historical record: when do universities and clerical authorities shift to emphasizing Classical standards? When does the curriculum move from medieval Latin practice to Classical grammar and reconstruction?',
    'Measuring the lag between founding problem solution (t~20, Renaissance manuscript recovery) and behavioral shift (t~?) reveals whether the constraint is transitioning to piton status (maintained by inertia rather than function). A long lag is evidence of institutional mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_investment_duration, empirical, 'Duration of the continuity reading''s maintenance after its founding coordination function is no longer needed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t5, correct_latin__continuity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(corr_tr_t5, observed).
narrative_ontology:measurement(corr_tr_t10, correct_latin__continuity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(corr_tr_t10, observed).
narrative_ontology:measurement(corr_tr_t15, correct_latin__continuity_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(corr_tr_t15, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin__continuity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t25, correct_latin__continuity_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(corr_tr_t25, observed).
narrative_ontology:measurement(corr_tr_t30, correct_latin__continuity_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(corr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t5, correct_latin__continuity_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(corr_be_t5, observed).
narrative_ontology:measurement(corr_be_t10, correct_latin__continuity_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(corr_be_t10, observed).
narrative_ontology:measurement(corr_be_t15, correct_latin__continuity_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(corr_be_t15, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin__continuity_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t25, correct_latin__continuity_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement_basis(corr_be_t25, observed).
narrative_ontology:measurement(corr_be_t30, correct_latin__continuity_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(corr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t5, correct_latin__continuity_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(corr_su_t5, observed).
narrative_ontology:measurement(corr_su_t10, correct_latin__continuity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(corr_su_t10, observed).
narrative_ontology:measurement(corr_su_t15, correct_latin__continuity_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(corr_su_t15, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin__continuity_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t25, correct_latin__continuity_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(corr_su_t25, observed).
narrative_ontology:measurement(corr_su_t30, correct_latin__continuity_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(corr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__continuity_reading, 0.05).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The 'correct_latin' kernel decomposes into three distinct constraint stories, one per reading: continuity (this file, unbroken transmission legitimates medieval forms), discontinuity (Classical texts are the standard; medieval forms are corrupted), and hybrid (Classical + textual evidence, with corrective authority). Each reading defines legitimacy differently, creates different beneficiary/victim structures, and assigns authority differently. They are not three perspectives on one constraint—they are three competing institutional frameworks for adjudication, instantiated as three separate constraints linked by kernel family relations. The epsilon values differ substantially across readings because what counts as extraction depends on which authority structure is endorsed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
