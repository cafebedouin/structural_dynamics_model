% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Classical-Medieval Latin Continuity Thesis
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading of the Classical-Medieval Latin kernel treats
 *   Medieval innovations in spelling, morphology, syntax, and vocabulary as
 *   the natural, organic result of linguistic evolution. Under this reading,
 *   a Medieval scribe's use of ae→e substitution, or a shift in case
 *   government, or neologism is not an error requiring correction but a
 *   legitimate development within the living Latin tradition. This reading
 *   benefits Medieval Latinists and church scholars by validating their
 *   subject matter as intrinsically continuous with Classical authority; it
 *   disadvantages humanist grammarians and textual critics whose authority
 *   rested on recovering prescriptive Classical standards and emending
 *   Medieval manuscripts toward them. The constraint's persistence depends on
 *   actively enforcing the narrative frame—teaching Medieval Latin via
 *   continuity historiography, resisting alternative frames that treat
 *   Medieval Latin as discontinuous—despite ongoing resistance from scholars
 *   who find the frame obscures real textual variance and complicates
 *   emendation.
 *
 * KEY AGENTS:
 *   - continuity_philologists: agenda-setter (institutional); frame the problem; identity-locked (career constituted through continuity thesis)
 *   - ecclesiastical_latinity_defenders: beneficiary (institutional); validated as legitimate rather than corrupted
 *   - humanist_grammarians: payer (institutional); must defend prescriptive rules against 'natural evolution' objections
 *   - textual_critics: payer (moderate); lose grounding for emendation when scribal variance is reframed as linguistic development
 *   - manuscript_custodians: observer (organized); record empirical variance without adjudicating frames
 *   - linguistic_conservationists: excluded (moderate); would object but are outside the canonical debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Classical-Medieval Latin Continuity Thesis").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, 'dc7031ee-e1b4-47c3-8ae9-39225c0edcf2').
narrative_ontology:cs_kernel_codification('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', fixed_text).
narrative_ontology:cs_authority_grounding('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', lineage).
narrative_ontology:cs_interpretation_layer_present('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2').
narrative_ontology:cs_reading_relation('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', foundational, medieval_latin_as_organic_development).
narrative_ontology:cs_axiom_status(medieval_latin_as_organic_development, holdable).
narrative_ontology:cs_axiom_grounding('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', medieval_latin_as_organic_development, empirically_contingent).
narrative_ontology:cs_axiom('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', foundational, reconstruction_as_internal_correction).
narrative_ontology:cs_axiom_status(reconstruction_as_internal_correction, holdable).
narrative_ontology:cs_axiom_grounding('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', reconstruction_as_internal_correction, instrumental).
narrative_ontology:cs_reference_frame('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', unified_latin_tradition).
narrative_ontology:cs_drift_state('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', contemporary_philological_consensus, gap(stable, minor, false)).
narrative_ontology:cs_created_at('dc7031ee-e1b4-47c3-8ae9-39225c0edcf2', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, continuity_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, ecclesiastical_latinity_defenders).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_grammarians).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, textual_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval Latinists and church historians who frame Medieval Latin as the natural, continuous evolution of Classical Latin. They set the interpretive frame—what counts as 'correct' Latin—by publishing editions, curricula, and historical accounts that normalize Medieval developments as organic linguistic drift. Their intellectual identity is constituted through the continuity thesis; rejecting it means abandoning a career frame and reframing one's entire scholarly output as built on a false premise. They benefit from this frame because it validates their subject matter (Medieval Latin) as intrinsically worthy of study, not as 'corruption' requiring external correction.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, continuity_philologists, agenda_setter,
    institutional, generational, identity_locked, regional).

% Church scholars and monastic communities whose scriptural commentaries, liturgical texts, and theological writings are composed in Medieval Latin. The continuity frame legitimates their linguistic practices as acceptable Latin, not as failed imitation of a golden standard. This frame protects their texts from being read as illiterate or defective—a reading that would diminish their authority and require wholesale retranslation and reinterpretation.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, ecclesiastical_latinity_defenders, beneficiary,
    institutional, generational, constrained, regional).

% Renaissance and post-Renaissance scholars who undertook systematic reconstruction of Classical Latin grammar, phonology, and usage from texts. They developed prescriptive rules (Ciceronian Latin, Augustan standards) by reading surviving Classical texts as a unified standard. They encounter the continuity thesis as resistance to their grammars: medieval scribes' spellings, forms, and constructions are labeled 'natural evolution' rather than 'errors' by continuity-frame practitioners. Their intellectual investment is in precision and differentiation; they pay the cost of defending why Medieval forms diverged from recoverable Classical patterns.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_grammarians, payer,
    institutional, biographical, mobile, regional).

% Scholars engaged in manuscript emendation and textual reconstruction of Classical texts. When a Medieval manuscript preserves a Classical text with spellings or inflections that differ from humanist reconstructions, the continuity frame suggests the manuscript is a witness to natural linguistic drift, not an error to be 'corrected.' This ambiguity suppresses their ability to distinguish scribal error from authentic witness, and weakens their authority to propose emendations grounded in Classical rules.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, textual_critics, payer,
    moderate, biographical, constrained, regional).

% Librarians and archivists who preserve medieval manuscripts and manage access to them. They are not parties to the debate but observe it; they record what the manuscripts actually contain without adjudicating whether it is 'correct.' They can testify to manuscript variation and consistency but do not benefit from or bear costs of either frame.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, manuscript_custodians, observer,
    organized, generational, analytical, regional).

% Scholars who argue for reconstructing Medieval Latin on its own terms, as a distinct living language with its own internal coherence—neither subordinate to Classical standards nor derivative from them. They would object that the continuity frame still treats Medieval Latin as a footnote to the Classical original, just a footnote framed as 'evolution' rather than 'corruption.' But they are largely excluded from the canonical conversation, which pivots on the Classical/Medieval binary.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, linguistic_conservationists, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, continuity_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework for reading Medieval and Classical texts as manifestations of a single continuous linguistic entity: Latin. This allows Classical scholars, Medieval scholars, theologians, and church historians to refer to 'Latin' as a coherent tradition without negotiating whether Medieval innovations are valid or errors.
% TRANSFER_FUNCTION: Transfers interpretive authority from humanist grammarians and textual critics (who claim exclusive right to define 'correct' Latin via reconstruction of Classical standards) to continuity-frame medievalists and church scholars (whose Medieval texts and practices are reframed as legitimate linguistic developments). The transfer mechanism is a historical narrative: instead of 'Medieval writers failed to write proper Latin,' the frame says 'Medieval writers wrote Latin as their generation inherited it.'
% ABSENT_VOICES: Scholars advocating for Medieval Latin as an autonomous system (not continuous with, not derivative from, Classical Latin) are largely excluded from the primary debate. They would argue the continuity frame itself is a form of prescriptivism—one that privileges Classical standards even while claiming neutrality. Alternative framings of what counts as 'Latin' are not present in the conversation.
% DISAPPEARANCE_RATIONALE: If the continuity thesis disappeared and scholars instead treated Medieval Latin as a discontinuous system requiring symbolic reoccupation, Medieval scholars would need to justify their subject via new legitimacy claims (linguistic interest independent of Classical authority, historical importance, or intrinsic cultural value). Textual critics would gain clearer grounds for emendation (deviation from Classical rules would signal error rather than development). But the fundamental philological and historical practices would not vanish—scholars would still edit manuscripts, translate texts, and reconstruct usage; they would do so with a different frame. The 'world' does not rearrange; the interpretive authority grid shifts.
% FOUNDING_PROBLEM: Classical Latin is attested only in texts from a finite period (~150 BCE–200 CE). Medieval Latin is attested in a continuous record from ~300 CE onward through church and administrative use. The founding problem: what is the relationship between these two bodies of text, separated by centuries, with visible changes in spelling, grammar, vocabulary, and syntax? How should a scholar read a Medieval manuscript that preserves a Classical text with variant forms?
% FOUNDING_PROBLEM_CORROBORATION: Continuity-frame scholars attest the problem is live and solved by their frame: it is natural for language to change; the variants in Medieval copies reflect linguistic evolution. Humanist scholars and textual critics attest the problem is live but unsolved: the variants cannot be addressed without distinguishing scribal error from legitimate witness, and the continuity frame obscures that distinction. Manuscript studies scholars (custodians) and linguistic historians outside the debate attest the problem is empirically real: variants ARE present, and they correlate with period, geography, and text-type.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the continuity frame concentrates interpretive authority in medievalists and church scholars while suppressing grammarians' and critics' claims to define correctness via Classical reconstruction. The frame does solve a real coordination problem (unifying Classical and Medieval under one language category) but uses that coordination to transfer authority. Suppression is moderate-high (0.62) because the frame's persistence requires actively resisting alternative framings and marginalizing scholars who would argue Medieval Latin is autonomous. Theater is moderate (0.41)—the historical narrative is partly genuine (languages do evolve naturally) but partly performative (the continuity frame is deployed strategically to defend Medieval texts and medieval scholars against humanist critique). The measurements plateau after t=20, indicating equilibrium: the frame has stabilized; extractiveness and suppression are no longer rising because the contest has settled into an institutionalized status quo where continuity reading is the default in medievalist scholarship.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading instantiates different experienced positions depending on the interpreter's position in the authority structure. For continuity-frame medievalists, it is descriptive history: they observe Medieval texts as they are and note continuity with Classical Latin—no constraint, just reality. For humanist grammarians and textual critics, it is an active constraint: their attempts to apply Classical standards are continuously resisted by the reframing of Medieval deviations as 'natural evolution.' This asymmetry is structural, not optional—it is the feature that makes it extraction rather than coordination. The reading itself does not construct this asymmetry; it only enacts it.
 *
 * DIRECTIONALITY LOGIC:
 *   Continuity_philologists and ecclesiastical_latinity_defenders benefit because the frame protects Medieval Latin from being read as defective and validates Medieval scholars' intellectual autonomy. Humanist grammarians and textual critics bear costs because their authority to define Latin (via Classical reconstruction) is suppressed—they must now justify why Medieval variants should be treated as errors rather than developments. The exit options differentiate precisely: medievalists are identity_locked (rejecting continuity means rejecting one's entire scholarly identity and output); grammarians are mobile (they can shift to other languages, to pre-Medieval texts, or to modern linguistics). Textual critics are constrained (they cannot easily reframe their editorial practice without wholesale methodological revision). This explains directionality: the medievalists extract value by remaining in the frame, while the grammarians and critics extract value by leaving it (but cannot easily do so).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is tangled_rope, not snare, because a genuine coordination function exists: unifying Classical and Medieval under one language category does solve a real scholarly problem (how to talk about Latin as a continuous tradition without negotiating frames constantly). But the coordination is asymmetric—it benefits those inside the frame (medievalists) more than those inside it but disadvantaged (grammarians/critics). Mandatrophy is not yet present because the founding problem (the relationship between Classical and Medieval texts) remains live: scholars still need to read Medieval copies of Classical texts and decide what to do with variants. If the founding problem became dead (e.g., if digital edition techniques made it irrelevant whether a Medieval manuscript was 'correct'), then mandatrophy would apply: the frame persists for honor and tradition but no longer solves the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_ideology,
    'Is the observed continuity between Classical and Medieval Latin a description of natural linguistic evolution, or a historiographical frame chosen to protect Medieval texts and Medieval scholars from humanist critique?',
    'Comparative analysis: does Medieval Latin show the same types of systematic change (sound shifts, grammatical reanalysis, lexical innovation) as attested in other language-transmission scenarios? Do languages with clearer attested transmission show the same patterns? Or is Medieval Latin uniquely marked by ideological defense against external critique?',
    'If natural, the continuity frame is descriptive and the classification remains tangled_rope (coordination with asymmetric benefit). If ideological, the frame is extractive and may shift toward snare (pure defense of authority with suppressed alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_ideology, empirical, 'Whether continuity is observed linguistic phenomenon or historiographical frame.').

omega_variable(
    identity_lock_mechanism,
    'Why do continuity-frame medievalists experience the thesis as identity-constituting? Is the lock structural (career paths, publishing venues, institutional location) or ideological (belief that Medieval studies requires the continuity thesis for legitimacy)?',
    'Ethnographic study of medieval Latinists'' career decisions when continuity thesis is contested; analysis of whether scholars can adopt discontinuity frame without career penalty or identity rupture.',
    'If lock is structural, it cannot be easily broken (scholars have material incentive to stay in the frame). If ideological, it could shift if the frame''s legitimacy is challenged. Either way, the lock is extractive: it binds the beneficiary to the frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Source and rigidity of identity fusion between continuity-frame medievalists and the thesis.').

omega_variable(
    manuscript_variance_ambiguity,
    'When a Medieval manuscript preserves a Classical text with variant forms, does the continuity frame correctly classify this as linguistic evidence, or does it obscure the distinction between scribal error and authentic witness?',
    'Computational analysis of error rates in Medieval manuscript copying of other texts (known-source comparisons); linguistic analysis of whether Medieval variants follow systematic rules (suggesting development) or are random (suggesting error).',
    'If variants follow rules, the continuity frame is supported (they represent linguistic change). If random, the frame obscures error and weakens textual criticism. This determines whether the suppression is justified (protecting legitimate development) or unjustified (defending corruption).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manuscript_variance_ambiguity, empirical, 'Whether Medieval textual variance is systematic (supporting continuity) or noise (obscured by the frame).').

omega_variable(
    sibling_reading_foreclosure,
    'Does the continuity reading logically foreclose the discontinuity reading, or can both coexist as live readings held by different scholarly communities?',
    'Logical analysis: if Medieval Latin is continuous with Classical, is it logically possible that it is also discontinuous? Or does one premise rule out the other? Can a scholar hold both simultaneously (in different contexts) without contradiction?',
    'If forecloses, the reading_relations entry should be ''forecloses.'' If both can coexist, it should be ''coexists_with.'' This determines the strength claim being made by the continuity frame—whether it is a descriptive finding or a prescriptive imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between continuity and discontinuity readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__continuity_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(corr_tr_t5, observed).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__continuity_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(corr_tr_t10, observed).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__continuity_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(corr_tr_t15, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__continuity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__continuity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(corr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__continuity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(corr_be_t5, observed).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__continuity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(corr_be_t10, observed).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__continuity_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(corr_be_t15, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__continuity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__continuity_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(corr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__continuity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(corr_su_t5, observed).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__continuity_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(corr_su_t10, observed).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__continuity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(corr_su_t15, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__continuity_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__continuity_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(corr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel kernel decomposes into three constraint stories, one per reading. Each reading is a distinct constraint with its own ε, beneficiary/victim structure, and classification. The continuity_reading treats Medieval innovations as organic development; the discontinuity_reading treats them as requiring symbolic reoccupation from texts; the hybrid_reading treats some components as continuous and others as requiring recovery. Each story names one kernel but instantiates one reading. The three stories are linked via network.affects_constraints because the academic contest over which reading prevails affects the epistemic standing of the others—adoption of one reading shifts the authority grid for the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
