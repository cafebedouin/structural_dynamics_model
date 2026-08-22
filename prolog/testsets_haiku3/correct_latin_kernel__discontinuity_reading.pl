% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical-Medieval Latin Discontinuity: Reconstruction as Symbolic Reoccupation
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   This constraint instantiates the discontinuity reading of the contested
 *   Latin kernel: Classical Latin (1st century BCE – 2nd century CE) and
 *   Medieval Latin (roughly 6th–15th centuries) are treated as distinct
 *   linguistic systems, separated by a break in transmission. Under this
 *   reading, Medieval forms are corruptions or deviations from Classical
 *   norms; 'correct' Latin is defined by Classical authors; reconstruction
 *   means recovering the lost Classical structure from fragmentary texts and
 *   emending Medieval copies to Classical standards. This reading generates a
 *   tangled-rope structure: the Classical philology tradition coordinates a
 *   unified interpretive standard (genuine coordination function) while
 *   simultaneously extracting authority and prestige from Medieval texts and
 *   their interpreters by treating them as degraded (asymmetric extraction).
 *   The constraint requires active enforcement: emendation practices, textual
 *   apparatus conventions, and journal publication standards all reinforce
 *   the discontinuity framing and suppress alternative readings of Medieval
 *   Latin on its own terms.
 *
 * KEY AGENTS:
 *   - Classical philology tradition: institutional agenda-setter, defines correctness, controls emendation apparatus and canon
 *   - Humanist scholars: powerful beneficiaries, gain career authority from mastering Classical standard and producing authoritative emendations
 *   - Medieval Latin practitioners: moderate-power payers, their texts are systematically judged as corrupted, treated as objects of correction rather than agents
 *   - Medieval text interpreters: moderate-power payers, professionally identity-locked to Classical framework, cannot advance by reading Medieval texts on their own terms
 *   - Medieval clerical communities: analytically powerless excluded voices, dead and unable to testify to their own linguistic intentions
 *   - Textual manuscript tradition: analytical observer, the actual body of texts that both readings must account for
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical-Medieval Latin Discontinuity: Reconstruction as Symbolic Reoccupation").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '36d643db-3363-445b-8433-e68240866201').
narrative_ontology:cs_kernel_codification('36d643db-3363-445b-8433-e68240866201', fixed_text).
narrative_ontology:cs_authority_grounding('36d643db-3363-445b-8433-e68240866201', lineage).
narrative_ontology:cs_interpretation_layer_present('36d643db-3363-445b-8433-e68240866201').
narrative_ontology:cs_reading_relation('36d643db-3363-445b-8433-e68240866201', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('36d643db-3363-445b-8433-e68240866201', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('36d643db-3363-445b-8433-e68240866201', foundational, medieval_latin_is_corrupted_classical).
narrative_ontology:cs_axiom_status(medieval_latin_is_corrupted_classical, holdable).
narrative_ontology:cs_axiom_grounding('36d643db-3363-445b-8433-e68240866201', medieval_latin_is_corrupted_classical, empirically_contingent).
narrative_ontology:cs_axiom('36d643db-3363-445b-8433-e68240866201', foundational, classical_norms_are_reconstruction_standard).
narrative_ontology:cs_axiom_status(classical_norms_are_reconstruction_standard, holdable).
narrative_ontology:cs_axiom_grounding('36d643db-3363-445b-8433-e68240866201', classical_norms_are_reconstruction_standard, conventional).
narrative_ontology:cs_reference_frame('36d643db-3363-445b-8433-e68240866201', classical_textual_authenticity_standard).
narrative_ontology:cs_drift_state('36d643db-3363-445b-8433-e68240866201', contemporary_medieval_studies_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36d643db-3363-445b-8433-e68240866201', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philology_tradition).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, humanist_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_text_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standard for 'correct Latin' by reference to Classical authors (Cicero, Virgil, Livy). Frames Medieval Latin forms as deviations from this standard. Controls the apparatus of textual reconstruction, emendation practices, and the definition of what counts as an error versus an acceptable variant. Derives institutional authority from centuries of accumulated commentary tradition and the prestige of the Classical canon.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philology_tradition, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain interpretive authority and methodological clarity by treating Classical Latin as the coherent reference system. Their scholarly credibility and career advancement depend on mastering the Classical apparatus and producing emendations that restore texts to Classical norms. The discontinuity reading legitimizes their role as arbiters of textual authenticity.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, humanist_scholars, beneficiary,
    powerful, generational, mobile, global).

% Medieval scribes, clerics, and authors whose Latin is evaluated by Classical standards and systematically judged as corrupt or inferior. Their texts are 'fixed' through emendation, their linguistic choices are treated as errors to be corrected rather than as features of a coherent system. They cannot defend their system on its own terms because the institutional frame already treats it as degraded.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_practitioners, payer,
    moderate, biographical, constrained, regional).

% Scholars and translators trying to understand Medieval Latin texts as they were written encounter the framework that treats those texts as corrupted versions of a lost Classical ideal. Reading the Medieval text in its own terms requires swimming against the interpretive current; career advancement and publication venues reward Classical-standard approaches. Professional identity is tied to mastery of the Classical apparatus, making exit costly.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_text_interpreters, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, medieval_text_interpreters, excluded).

% The communities that produced Medieval Latin texts are long dead and cannot testify to their own linguistic intentions. Their textual legacy is interpreted through the discontinuity lens, which frames their language as failed imitation rather than as a coherent system adapted to new communicative needs. They are voiceless in the modern reconstruction apparatus.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_clerical_communities, excluded,
    powerless, civilizational, trapped, regional).

% The actual body of surviving texts, which contain both Classical citations and Medieval linguistic innovations. The texts themselves are the ground of investigation but are read through the discontinuity frame that treats them as evidence of corruption and loss rather than of living adaptation.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, textual_manuscript_tradition, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__discontinuity_reading, textual_manuscript_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_philology_tradition).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative standard for 'correctness' in Latin across all texts and periods, enabling scholars to communicate about textual authenticity using consistent criteria. Creates a shared framework for deciding which textual variants are genuine and which are scribal errors or linguistic degradation.
% TRANSFER_FUNCTION: Transfers interpretive authority from Medieval texts and their communities to Classical texts and the scholars who master the Classical apparatus. Moves scholarly prestige and career advancement opportunity toward those who can demonstrate mastery of Classical norms and produce authoritative emendations. Moves the obligation to accept 'corrections' toward Medieval texts and their interpreters.
% ABSENT_VOICES: Medieval scribes and authors cannot testify to whether their forms were intentional or corrupt; Medieval Latin speakers are dead. Modern scholars of Medieval Latin who would argue their texts should be interpreted on their own terms are structurally excluded from the emendation and canonization apparatus. Voices that would argue for Medieval Latin as a coherent system rather than a degenerate variant are marginalized.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading and its enforcement machinery vanished, textual scholarship would reorganize: Medieval texts would be read on their own grammatical and syntactic terms rather than as corrupted Classical texts awaiting emendation; the apparatus of 'correction' would dissolve; scholarly authority would distribute across multiple linguistic systems rather than centralizing on the Classical standard. The humanist scholar's role as arbiter of authenticity would fundamentally change.
% FOUNDING_PROBLEM: In the Renaissance, Classical Latin texts were being recovered from manuscripts in fragmentary and corrupted form. Scholars needed a method to reconstruct the original Classical text from surviving imperfect copies. The discontinuity reading posits that Medieval scribes and copyists had lost access to the genuine Classical system and could only produce corrupted versions.
% FOUNDING_PROBLEM_CORROBORATION: The discontinuity reading itself was born from Renaissance scholars' discovery that surviving Medieval manuscripts differed substantially from Classical texts they were copying. However, modern scholarship on Medieval Latin as a coherent linguistic system (Dag Norberg, Jan Ziolkowski, others) contests whether the 'corruption' narrative accurately describes what Medieval scribes were doing. The founding problem's empirical basis — whether Medieval degradation of Classical forms was accidental corruption or systematic evolution — is under active scholarly dispute, with substantial corroboration outside the Classical philology establishment.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.48 → 0.62) reflects the constraint's institutional hardening over the interval. Early in the adoption period, the discontinuity reading competed with other framings and carried lower suppressive cost. As philological institutions consolidated around the Classical standard, emendation became standard practice, and Medieval Latin scholarship was marginalized, extractiveness rose and then plateaued — not because new extraction was added, but because the enforcement machinery matured and the constraint became institutionalized. The theater_ratio (0.28 → 0.41) shows rising performative activity: the discovery and publication of 'corrections' to Medieval texts, the rhetorical justification of the discontinuity thesis, the theatrical display of Classical knowledge become increasingly central to the apparatus's legitimacy. At t=20–25, both metrics plateau, suggesting institutional equilibrium — the constraint's extraction and theater have stabilized at a point where Medieval Latin studies exist as a marginal subdiscipline whose primary value is to the Classical tradition (providing manuscripts to emend, evidence of corruption to cite). Suppression_requirement rises (0.42 → 0.58) because the constraint must actively exclude alternatives: Medieval Latin as a living system must be delegitimized; Medieval scholars must be kept out of the emendation apparatus; the possibility that Medieval forms are intentional innovations rather than errors must be suppressed. The gridded time-points share one axis: every metric is authored at every examined moment.
 *
 * PERSPECTIVAL GAP:
 *   The Classical philology tradition and humanist scholars perceive this as genuine coordination — a shared standard enabling scholarly communication and textual authenticity. They experience it as beneficial (gaining prestige and method from the Classical apparatus). Medieval text interpreters and Medieval Latin scholars perceive it as extraction and suppression: their interpretive authority is stripped, their texts are treated as corrupted, their career paths are constrained by the requirement to work within the Classical framework. The payers (Medieval interpreters) and the excluded (Medieval communities) experience a constraint whose enforcement denies the legitimacy of their own textual and linguistic tradition. The engine will compute different directionalities from these stakeholder seats, reflecting that the same constraint feels like coordination from one position and extraction-plus-suppression from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philology tradition: d ≈ 0.15–0.25 (beneficiary: controls the standard, derives prestige and authority). Humanist scholars: d ≈ 0.10–0.20 (beneficiary: advance careers through mastery of Classical apparatus). Medieval text interpreters: d ≈ 0.75–0.85 (target: their interpretive authority is stripped, their professional paths are constrained by identity-lock to the Classical framework, they must accept emendations of texts they study). Medieval Latin practitioners (as historical agents, now dead): d ≈ 0.90 (full target: their texts are systematically reinterpreted and emended against their own norms). Directionality reflects the asymmetry: the constraint coordinates a standard for the beneficiary seats while extracting from the payer seats through the same mechanism — unified standards + emendation authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity reading posits a founding problem (Renaissance scribal corruption of Classical texts) that was live in the 15th–16th centuries. By the 19th–20th centuries, when philological methods matured and Medieval Latin was recognized as a coherent system by scholars like Dag Norberg, the founding problem shifted into 'contested' status. However, the constraint persists because it has become institutionalized: it is embedded in curriculum design, textual apparatus conventions, publication standards, and career advancement criteria. A mandatrophy resolution would require acknowledging that Medieval Latin is a coherent system deserving interpretation on its own terms, which would dissolve the emendation apparatus's authority. The constraint persists not because the founding problem is live but because institutional structures now benefit from treating Medieval forms as corruptions. The theater_ratio rise (0.28 → 0.41) suggests growing performative maintenance relative to functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_continuity_vs_discontinuity,
    'Was Medieval Latin a natural linguistic evolution from Classical Latin, or a genuinely distinct system created by break in transmission and loss of direct Classical training?',
    'Comparative reconstruction of Medieval grammatical and phonological rules: if Medieval forms follow coherent internal rules distinct from Classical rules, the system is coherent; if Medieval forms appear as random corruptions of Classical rules, discontinuity is supported. Sociolinguistic analysis of Medieval Latin''s functional contexts (liturgy, legal documents, scholarly texts) and audience.',
    'If Medieval Latin is shown to be coherent on its own terms, the discontinuity reading''s core premise collapses — Medieval forms are features of an adapted system, not corruptions. This would reclassify the constraint from tangled_rope toward snare (extraction without genuine coordination), or dissolve it entirely as Medieval texts become interpretable without the emendation apparatus. If discontinuity is supported, the constraint''s tangled_rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_continuity_vs_discontinuity, empirical, 'Whether Medieval Latin is coherent internally or corrupted externally.').

omega_variable(
    reconstruction_necessity_ambiguity,
    'Does Medieval Latin require reconstruction toward Classical norms to be intelligible, or is it directly readable on its own terms?',
    'Modern scholarly editions of Medieval texts that present them unemended and analyze their linguistic features as intentional rather than errors. Reader comprehension studies comparing Classical-standard editions with Medieval-original editions among scholars trained in both systems.',
    'If Medieval texts are directly readable without emendation, the reconstruction apparatus loses its functional justification, revealing the constraint as primarily extractive (enforcing Classical authority) rather than coordinative. The theater_ratio would be reinterpreted as the entire apparatus being performative rather than functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_necessity_ambiguity, empirical, 'Whether Medieval Latin is intelligible without emendation to Classical norms.').

omega_variable(
    classical_supremacy_is_constructed,
    'Is the discontinuity reading''s core axiom — that Classical Latin is the standard of correctness — grounded in linguistic structure or in institutional/ideological choice?',
    'Historical analysis of why Classical authors became canonical (Renaissance humanist preferences, printing-press accidents of which texts survived, prestige associations with Rome''s imperial peak); comparative study of other linguistic traditions that treat different historical periods as standard (Old English vs. Middle English, Classical Chinese vs. Medieval Chinese).',
    'If Classical supremacy is revealed as a constructed choice rather than a linguistic necessity, the constraint''s legitimacy fractures. The beneficiary reading (humanist scholars) would become visible as a reading that serves humanist institutional interests rather than reflecting linguistic structure. The constraint would shift from appearing as coordination around a natural standard toward appearing as organized extraction backed by institutional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classical_supremacy_is_constructed, conceptual, 'Whether the discontinuity reading''s preference for Classical Latin as the standard is linguistically grounded or institutionally constructed.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression of Medieval Latin interpretation purely structural (institutional barriers, publication gatekeeping) or partially internalized (scholars internalize the belief that Medieval Latin is inferior and unworthy of study on its own terms)?',
    'Post-exit trajectory: scholars who leave Classical philology and study Medieval Latin in autonomous medieval studies contexts; observation of whether they retain suppressive self-concepts or develop confidence in Medieval Latin as a legitimate system. Interviews with Medieval Latin scholars about their experience of institutional pressure vs. internalized inferiority beliefs.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the authored 0.58 suggests — targets carry the suppression with them even after institutional exit. This complicates the tangled_rope classification by showing the coordination function (shared standard) itself contains a suppressive subcomponent (delegitimization). If suppression is primarily structural, institutional reforms (opening publication venues, changing hiring criteria) could more readily dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism in the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__discontinuity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__discontinuity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__discontinuity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__discontinuity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__discontinuity_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__discontinuity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__discontinuity_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__discontinuity_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__discontinuity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__discontinuity_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__discontinuity_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__discontinuity_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__discontinuity_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__discontinuity_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__discontinuity_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel decomposes into three structurally distinct constraints, each a reading of the kernel. The discontinuity_reading (this story) treats Medieval Latin as a separate system requiring symbolic reoccupation of Classical authority. The continuity_reading treats Medieval Latin as natural evolution. The hybrid_reading posits layered recovery (core continuous, syntax/lexicon recovered). Each reading instantiates different ε (extracted authority), different beneficiary structures, and different reconstructionist approaches. The readings coexist as competing scholarly positions; see cs_structure.reading_relations for the logical structure. All three stories link via network.affects_constraints to show constraint family decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
