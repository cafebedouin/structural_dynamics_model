% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Binding Precedent as Backward Constraint
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   Strict stare decisis is one institutionalized reading of how precedent
 *   should bind in common-law systems. Under this reading, prior holdings
 *   bind future courts absent extraordinary justification for overruling. The
 *   constraint operates through judicial commitment to backward consistency:
 *   litigants seeking norm reform must demonstrate fundamental doctrinal
 *   error, changed circumstances severe enough to make precedent
 *   inapplicable, or reliance breakdown. The reading instantiates high
 *   rigidity: departure is rare, contestation is channeled into narrow
 *   pathways (distinguishing cases, limiting holdings), and the burden of
 *   proof sits on the reformer, not on the defender of established doctrine.
 *   This constraint story describes the strict stare decisis reading
 *   specifically — the reading that treats precedent as a backward-binding
 *   rule — not the alternative readings (evolutionary reinterpretation,
 *   pluralist balancing) that other constraint stories will instantiate.
 *
 * KEY AGENTS:
 *   - senior_judiciary: Interprets and enforces the strict stare decisis rule; maintains the precedent corpus; controls what counts as extraordinary justification.
 *   - established_doctrine_holders: Institutional and individual parties whose interests are stable under existing precedent; benefit from high barriers to norm change.
 *   - norm_reform_advocates: Civil rights movements, legal reformers, and doctrinal dissenters bearing the cost of the extraordinary-justification barrier; identity often fused with precedent-breaking the constraint suppresses.
 *   - litigants_challenging_precedent: Individual and organizational parties seeking reconsideration of settled law; face substantial gatekeeping barriers.
 *   - appellate_courts: Lower and intermediate courts that apply the constraint; both enforcers and targets of the precedent binding rule.
 *   - alternative_reading_advocates: Jurists and scholars advocating evolutionary or pluralist frameworks; excluded from setting the constraint's operation because strict stare decisis defines the permissible terms of contestation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.62).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.71).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Binding Precedent as Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'a9414249-531f-4ffd-ad0e-08822b6e3c95').
narrative_ontology:cs_kernel_codification('a9414249-531f-4ffd-ad0e-08822b6e3c95', fixed_text).
narrative_ontology:cs_authority_grounding('a9414249-531f-4ffd-ad0e-08822b6e3c95', lineage).
narrative_ontology:cs_interpretation_layer_present('a9414249-531f-4ffd-ad0e-08822b6e3c95').
narrative_ontology:cs_reading_relation('a9414249-531f-4ffd-ad0e-08822b6e3c95', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('a9414249-531f-4ffd-ad0e-08822b6e3c95', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('a9414249-531f-4ffd-ad0e-08822b6e3c95', foundational, precedent_categorical_binding).
narrative_ontology:cs_axiom_status(precedent_categorical_binding, holdable).
narrative_ontology:cs_axiom_grounding('a9414249-531f-4ffd-ad0e-08822b6e3c95', precedent_categorical_binding, conventional).
narrative_ontology:cs_axiom('a9414249-531f-4ffd-ad0e-08822b6e3c95', foundational, extraordinary_justification_burden_on_reformer).
narrative_ontology:cs_axiom_status(extraordinary_justification_burden_on_reformer, holdable).
narrative_ontology:cs_axiom_grounding('a9414249-531f-4ffd-ad0e-08822b6e3c95', extraordinary_justification_burden_on_reformer, conventional).
narrative_ontology:cs_reference_frame('a9414249-531f-4ffd-ad0e-08822b6e3c95', backward_binding_precedent).
narrative_ontology:cs_drift_state('a9414249-531f-4ffd-ad0e-08822b6e3c95', contemporary_reform_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9414249-531f-4ffd-ad0e-08822b6e3c95', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_continuity_constituency).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, norm_reform_advocates).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_profession).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, appellate_courts).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, legal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the rule that prior holdings bind future decisions absent extraordinary justification. Maintains the precedent corpus, authors the narrow exceptions (overruling doctrine, narrow distinguishing). Controls which cases merit reconsideration and what 'extraordinary' means. Collects institutional authority and legitimacy from adherence to precedent fidelity.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, senior_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Parties whose interests and legal positions are anchored in existing precedent. Their stakes are stable and predictable under the constraint. They benefit from the high burden placed on norm revision and from the judiciary's institutional commitment to backward consistency. They include entrenched doctrinal constituencies, institutional actors with settled legal footing, and doctrine-defending professional networks.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders, beneficiary,
    powerful, civilizational, arbitrage, national).

% Legal reformers, civil rights movements, and doctrinal dissenters who believe existing precedent is unjust, outdated, or incoherent. They bear the cost of the extraordinary-justification barrier: their path to norm change is constrained to the narrow channels the court recognizes as legitimate overruling grounds (fundamental error, reliance breakdown, changed circumstances). Their identity as reform advocates is often fused with commitments to precedent-breaking that the constraint systematically suppresses.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, norm_reform_advocates, payer,
    organized, generational, identity_locked, national).

% Individual and organizational parties seeking judicial reconsideration of settled law. They face substantial barriers: precedent fidelity as doctrine, burden of proof on the exceptioner, narrow gatekeeping. Their exit options are non-juridical (legislative override, constitutional amendment) but those are far costlier and less directly available. They cannot exit the legal system itself if their core interest is adjudication.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent, payer,
    moderate, biographical, trapped, national).

% Institutional interests in legal and social stability: government regulators, settled business arrangements, legislative reliance on judicial holdings. They benefit from high predictability and from the constraint's operation as a break on norm volatility. Their interest is in the legal order's structural integrity, not in any particular doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, institutional_continuity_constituency, beneficiary,
    institutional, civilizational, analytical, national).

% Jurists, scholars, and reform movements advocating evolutionary or pluralist readings of the precedent corpus. They argue for reinterpretation, flexible balancing, or contextualized weight. They are excluded from setting the constraint's operation because the strict stare decisis framework defines the terms of permissible doctrinal contestation; their alternative frames are treated as outside-the-question rather than live options.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, alternative_reading_advocates, excluded,
    organized, generational, identity_locked, national).

% Lower and intermediate appellate courts apply the constraint: they are bound by higher precedent and have limited authority to overrule. They experience the constraint as both enforcer and target — they must apply it and are also constrained by it. Their institutional position depends on precedent fidelity, but their capacity for doctrinal development is substantially curtailed.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_courts, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, appellate_courts, payer).

% Attorneys benefit from the predictability and closure the constraint provides — stable precedent enables advice-giving and negotiation. They also bear cost: counseling reform litigants requires navigating the narrow overruling pathways, and doctrinal uncertainty is confined to predictable channels. Their professional authority partly rests on mastery of the precedent corpus as constituted.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_profession, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, legal_profession, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, senior_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables judicial decision-making to accumulate into a coherent, backward-consistent body of law that lawyers and citizens can rely on. Prevents the legal system from oscillating randomly or being restructured wholesale on each new court composition. Establishes the precedent corpus as a shared reference that coordinates behavior across jurisdictions and time.
% TRANSFER_FUNCTION: Transfers doctrinal stability and predictability FROM norm-reform advocates and precedent challengers TO established-doctrine holders and institutional continuity constituencies. The price is narrow access to norm revision; the gain to beneficiaries is constraint on the judiciary's capacity to unwind settled law.
% ABSENT_VOICES: Alternative reading advocates and marginalized communities whose interests conflict with established doctrine are structurally excluded: their voices are not absent from doctrine formation, but they operate under a constraint regime that treats norm reform as exceptional rather than permissible. Jurisdictions where precedent authority is contested (other national legal systems, critical legal scholarship traditions) offer alternative readings but are treated as external to the question under strict stare decisis framing.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished — if precedent ceased to bind and each court could reinterpret freely — the legal order would experience rapid doctrinal volatility, settlement patterns would shift as parties no longer trusted precedent, and the judiciary's capital as an authoritative norm-setting body would fragment. Existing arrangements dependent on settled law would be exposed to rapid reconstitution.
% FOUNDING_PROBLEM: Early common-law systems lacked coherence: judicial decisions were inconsistent, predictability was low, and the law appeared as an instrument of arbitrary authority rather than binding principle. Stare decisis was established to create doctrinal continuity, reduce arbitrariness, and enable subjects and judges to rely on accumulated holdings.
% FOUNDING_PROBLEM_CORROBORATION: Legal traditionalists and institutional interests attest the founding problem persists: without precedent binding, predictability and continuity degrade, and the rule of law weakens. Reform advocates and comparative legal scholars from outside the common-law establishment attest the problem was substantially solved by the 20th century and that strict stare decisis now persists as a mechanism for entrenching established doctrine rather than solving instability. Independent legal history and comparative analysis support the contested diagnosis.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.48 (early common-law period, precedent as custom but not fully binding) and rises to 0.62 (modern era, stare decisis doctrine solidly entrenched). The curve flattens after t=24 because the institutional commitment to the constraint reaches saturation — further increase requires changes to the constraint itself, not merely stricter application. Suppression follows a similar trajectory, rising from 0.58 to 0.71 as doctrinal gatekeeping machinery hardens and the judiciary's commitment to precedent fidelity deepens. Theater ratio rises from 0.12 to 0.28 as the rhetorical covering of precedent binding grows relative to its functional cost — the doctrine becomes more theatrical partly because it must justify itself against increasing reform pressure. The shared time grid ensures all three metrics are authored at every time point and the measurements are temporally coherent.
 *
 * PERSPECTIVAL GAP:
 *   The senior judiciary and established-doctrine holders experience the constraint as legitimate coordination (stability, predictability, rule of law). The norm-reform advocates and litigants challenging precedent experience it as enforced extraction (they bear the burden of extraordinary justification; their paths to norm change are systematically narrowed). The constraint generates seat divergence because power asymmetry maps directly to directionality: the judiciary and doctrine defenders have structural interest in maintaining high precedent rigidity (beneficiary end of d); reform advocates have structural interest in reducing the justification burden (target end of d). The engine should compute this divergence from the stakeholder power/exit data: powerful institutions with arbitrage options compute as beneficiaries with low d; organized reform advocates with identity-locked exit compute as targets with high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior judiciary (powerful, constrained exit, sets the rules) derives low d → benefits from the constraint. Established-doctrine holders (powerful, arbitrage options, stable positions) derive low d → benefit. Norm-reform advocates (organized, identity-locked, challenger status) derive high d → pay the cost via suppressed reform pathways. Litigants challenging precedent (moderate power, trapped exit, dependent on judicial mercy) derive high d → pay via gatekeeping. The constraint's beneficiary structure is institutional: those whose interests lie in doctrinal stability and predictability (regardless of whether the current doctrine is just) benefit; those whose interests lie in norm change (reform advocates, oppressed groups seeking doctrinal reconsideration) pay. Directionality does not require overrides under the strict stare decisis reading: the structural data (who sets the rule, who has exit, who benefits from stability) directly supports the power-asymmetric beneficiary/victim split.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrariness, inconsistency in early common law) was genuinely solved by the establishment of precedent binding. The constraint should therefore compute as coordination (rope) if the problem remained live and the restriction on norm change remained proportionate to the problem. However, the constraint's persistence under modern conditions shows clear signs of mandatrophy: the original problem (doctrinal chaos, arbitrariness) is substantially solved; the constraint persists not because the problem persists but because established interests benefit from high doctrinal stability and because the judiciary's institutional authority is partially constituted by commitment to precedent. The theater ratio rise (from 0.12 to 0.28) indicates growing rhetorical defense of the constraint even as its functional justification weakens. The suppression rise (from 0.58 to 0.71) indicates growing active enforcement burden — the constraint requires more intensive defense as reform pressure mounts. This pattern is characteristic of a constraint whose original function is largely solved but whose extraction function (protecting established doctrine) remains active. The constraint is claimed as tangled_rope because it coordinates backward consistency while extracting from reform advocates; the measurement trajectory supports mandatrophy resolution: the original coordination problem is largely dead (the legal system is predictable and coherent), but the extraction mechanism persists under the cover of the solved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_binding_vs_judicial_authority,
    'Does strict stare decisis bind precedent because it solves an ongoing problem (doctrinal coherence and predictability) or because it constitutes the judiciary''s claim to authority (precedent fidelity as the source of legitimacy)?',
    'Comparative study of legal systems with weaker precedent binding (civil law systems, jurisdictions with judicial review powers but reduced stare decisis) examining whether predictability and coherence degrade or whether alternative authority sources sustain them.',
    'If binding solves an ongoing problem, the constraint is coordination and mandatrophy is absent. If binding constitutes judicial authority (the problem is solved but the constraint persists because the judiciary''s legitimacy depends on precedent commitment), mandatrophy is present and the constraint converts from rope to snare as reform pressure increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_binding_vs_judicial_authority, empirical, 'Whether precedent binding persists to solve a live problem or to sustain judicial institutional authority.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (legal barriers, gatekeeping machinery, high litigation burden) or internalized (litigants and advocates believe norm change is illegitimate, not just costly)?',
    'Post-reform trajectory analysis: if legal barriers to precedent challenge are removed (explicit overruling made easier by statute or constitutional amendment), do reform advocates and challenger litigants maintain suppressive self-constraints or do they immediately escalate norm-change demands?',
    'If structural, removing the formal barrier would substantially increase norm-change challenges. If internalized, suppression persists even after barrier removal because reformers have internalized the constraint as legitimate. This affects exit_options classification: identity-locked advocates might become mobile (constrained) if the structural barrier weakens enough to make internalization unnecessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'The locus of suppression in identity-locked norm-reform advocates.').

omega_variable(
    reading_foreclosure_evolutionary_vs_strict,
    'Is there a logical contradiction between the strict stare decisis reading (precedent binds as backward constraint, departure exceptional) and the evolutionary reading (precedent provides framework for reinterpretation, evolution permissible)?',
    'Jurisprudential analysis: can a court hold both that precedent categorically binds AND that contemporary normative evolution permits reinterpretation without internal contradiction? Or does adoption of one reading logically commit the interpreter to rejecting the other?',
    'If the readings are logically contradictory (forecloses relation), adoption of strict stare decisis in a given jurisdiction or judgment should be incompatible with evolutionary reasoning. If they are not contradictory (coexists or influences relation), they can coexist as different parties'' positions or even within a single judgment as applied to different doctrinal domains. This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_evolutionary_vs_strict, conceptual, 'Whether strict stare decisis and evolutionary interpretation are logically foreclosing or coexisting readings.').

omega_variable(
    identity_lock_mechanism_reform_advocates,
    'What specific dimension of reform advocate identity makes exit from the legal system impossible or unthinkable? Is it professional identity (career invested in precedent-changing litigation), relational identity (self-constituted through the norm-reform project), or ideological identity (worldview incompatible with abandoning legal change pathways)?',
    'Biographical and institutional analysis of reform movements: examine whether advocates pivot to non-legal change pathways when precedent barriers are especially high, or whether they remain identity-committed to judicial change even when success is unlikely.',
    'If professional: advocates might shift careers or jurisdictions if legal pathways close. If relational: advocates remain committed to precedent-challenging litigation as expression of collective identity. If ideological: abandoning legal pathways would constitute betrayal of movement principles. This affects how exit_options and d-values should be interpreted and whether any intervention can reduce the suppression these advocates experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_reform_advocates, empirical, 'The specific mechanism binding reform advocates'' identity to norm-change litigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(comm_tr_t8, observed).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(comm_tr_t16, observed).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(comm_tr_t32, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(comm_be_t8, observed).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(comm_be_t16, observed).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(comm_be_t32, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(comm_su_t8, observed).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(comm_su_t16, observed).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(comm_su_t32, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, appellate_jurisdiction_gatekeeping).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_amendment_burden).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel has three distinct constraint readings: strict_stare_decisis (this story), evolutionary_framework, and pluralist_balancing. Each reading instantiates a different constraint because each has different ε (extraction), different victim sets (who bears the cost of the reading), and different beneficiary structures (who benefits from that particular approach to precedent binding). The three are linked via network.affects_constraints because they compete for doctrinal authority within the same kernel — adoption of one reading influences the structural conditions for the others. Strict stare decisis is the upstream reading in this family because it defines the permissible terms of doctrinal contestation; the other readings challenge or reframe those terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
