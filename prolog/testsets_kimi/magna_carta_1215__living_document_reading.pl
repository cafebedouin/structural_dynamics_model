% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate
 *   domain: constitutional/legal/historical
 *
 * SUMMARY:
 *   This constraint instantiates the living_document_reading of the
 *   magna_carta_1215 kernel. It treats the 1215 charter not as a fixed feudal
 *   contract or a direct source of universal rights, but as an adaptive
 *   constitutional substrate whose original meaning has been legitimately
 *   superseded by centuries of interpretive tradition and precedential
 *   accumulation. The constraint operates as a meta-constraint on
 *   interpretive authority: it scaffolds constitutional development by
 *   allowing judicial precedent to continuously reconfigure the charter's
 *   legal significance. This generates genuine coordination (constitutional
 *   adaptation without revolution) alongside asymmetric extraction
 *   (concentration of authority in the judiciary and legal profession). The
 *   constraint is structurally linked to sibling readings:
 *   baronial_privilege_reading (historical feudal contract reading) and
 *   universal_rights_reading (transhistorical rights precedent reading).
 *
 * KEY AGENTS:
 *   - common_law_judiciary (agenda_setter/beneficiary, institutional/identity_locked): administers interpretive tradition and accumulates precedential authority
 *   - legal_profession (beneficiary, organized/identity_locked): derives status and income from precedential complexity
 *   - democratic_legislatures (payer, institutional/constrained): statutes interpreted and constrained by accumulated precedent
 *   - textual_originalists (payer, moderate/constrained): bear doctrinal marginalization costs
 *   - constitutional_scholars (beneficiary, organized/mobile): legitimate the interpretive framework through scholarship
 *   - lay_citizens (payer, powerless/constrained): bear diffuse costs of legal unpredictability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.5).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional/legal/historical").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, 'd899a419-a398-4cc1-8f0a-5632fbf2c41f').
narrative_ontology:cs_kernel_codification('d899a419-a398-4cc1-8f0a-5632fbf2c41f', fixed_text).
narrative_ontology:cs_authority_grounding('d899a419-a398-4cc1-8f0a-5632fbf2c41f', lineage).
narrative_ontology:cs_interpretation_layer_present('d899a419-a398-4cc1-8f0a-5632fbf2c41f').
narrative_ontology:cs_reading_relation('d899a419-a398-4cc1-8f0a-5632fbf2c41f', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('d899a419-a398-4cc1-8f0a-5632fbf2c41f', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('d899a419-a398-4cc1-8f0a-5632fbf2c41f', foundational, original_meaning_supersession_legitimate).
narrative_ontology:cs_axiom_status(original_meaning_supersession_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d899a419-a398-4cc1-8f0a-5632fbf2c41f', original_meaning_supersession_legitimate, conventional).
narrative_ontology:cs_axiom('d899a419-a398-4cc1-8f0a-5632fbf2c41f', foundational, precedent_constitutes_development).
narrative_ontology:cs_axiom_status(precedent_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('d899a419-a398-4cc1-8f0a-5632fbf2c41f', precedent_constitutes_development, conventional).
narrative_ontology:cs_reference_frame('d899a419-a398-4cc1-8f0a-5632fbf2c41f', common_law_adaptive_tradition).
narrative_ontology:cs_drift_state('d899a419-a398-4cc1-8f0a-5632fbf2c41f', contemporary_constitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d899a419-a398-4cc1-8f0a-5632fbf2c41f', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_scholars).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, democratic_legislatures).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, textual_originalists).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, lay_citizens).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, common_law_constitutionalism).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises binding interpretive authority over constitutional and statutory texts through precedent. Magna Carta's clauses are treated as living principles subject to judicial refinement. Judges cannot easily exit this role without abandoning professional identity and institutional position.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, common_law_judiciary, beneficiary).

% Derives professional status, income, and epistemic monopoly from the complexity of accumulated precedent. Arguments grounded in original meaning are systematically secondary to arguments from precedent. Career trajectory depends on mastery of interpretive tradition rather than textual fixity.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Enacts statutes that may be struck down or interpreted in light of judicial precedent traceable to Magna Carta. Legislative supremacy is constrained by the interpretive tradition that treats ancient charter clauses as continually developable constraints on parliamentary or congressional power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, democratic_legislatures, payer,
    institutional, biographical, constrained, national).

% Argue for interpretation fixed to historical original meaning. Their interpretive methodology is structurally disadvantaged within common-law courts where precedent accumulates and original meaning is treated as legitimately superseded. They bear the cost of persistent doctrinal marginalization.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, textual_originalists, payer,
    moderate, generational, constrained, national).

% Produce scholarship legitimating the interpretive tradition and tracing precedential lineages back to Magna Carta. Academic reputation and citation networks reward contributions to the living-document framework. They are free to dissent but career incentives align with the dominant paradigm.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars, beneficiary,
    organized, generational, mobile, national).

% Subject to legal obligations derived from precedential chains originating in medieval charter clauses, the contemporary meaning of which they cannot predict from the text. They bear the diffuse cost of legal complexity and judicial supremacy without direct access to the interpretive process.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, lay_citizens, payer,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables constitutional and legal norms to adapt to changing social, political, and economic conditions without requiring formal textual amendment, by treating founding charter clauses as principles subject to continuous judicial refinement through precedent.
% TRANSFER_FUNCTION: Moves interpretive authority from historical enactors and democratic legislatures to the judiciary and legal profession, transferring constraint-making power from textually fixed rules to accumulated precedent.
% ABSENT_VOICES: Civil law comparatists who reject precedential supremacy, radical democrats who advocate legislative override of judicial interpretation, and strict textualists who deny the legitimacy of superseding original meaning are structurally marginalized in common-law constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the living-document constraint vanished and Magna Carta's original meaning became permanently fixed, judicial review would lose its primary developmental mechanism, legislative supremacy would expand, the legal profession's epistemic monopoly would weaken, and constitutional rights and limitations would require explicit democratic enactment rather than judicial inference.
% FOUNDING_PROBLEM: How to maintain a constitutional order's continuity and legitimacy across centuries of social change when formal amendment is infrequent, politically costly, or institutionally blocked.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians corroborate the difficulty of formal amendment and the need for adaptation. Democratic theorists outside the legal profession corroborate that legislative processes and popular constitutionalism provide alternative adaptation mechanisms, suggesting the founding problem is partially solved by other means and the interpretive tradition now persists beyond strict necessity.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate-high extraction: the interpretive class captures substantial authority and role-complexity from the precedential accumulation process. Suppression (0.50) is moderate because originalism and legislative supremacy remain live dissenting traditions that are not fully extinguished, though they are structurally disadvantaged in court. Theater_ratio (0.38) captures the performative dimension of originalist argumentation that courts routinely entertain before subordinating it to precedent. Accessibility_collapse (0.52) indicates that once inside the common-law system, alternatives like strict textual fixity or legislative override appear increasingly institutionally remote. Resistance (0.45) reflects persistent originalist movements and occasional legislative pushback against judicial supremacy. The trajectory shows extractiveness and theater rising as precedential accumulation thickens over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary and legal profession, the constraint appears as necessary coordination preventing constitutional obsolescence. From democratic legislatures and textual originalists, the same structure appears as extraction of democratic and textual authority into an unelected interpretive class. The engine computes this divergence from the structural data: beneficiaries with identity-locked exit experience low effective extraction (subsidized by the constraint), while payers with constrained exit experience amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Common_law_judiciary and legal_profession are declared beneficiaries with identity-locked or constrained exit, yielding low directionality (subsidized by the interpretive tradition). Democratic_legislatures, textual_originalists, and lay_citizens are declared victims with constrained or powerless positioning, yielding high directionality (extracted from). Constitutional_scholars as beneficiaries with mobile exit sit near the beneficiary end but retain more optionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâconstitutional adaptation across centuriesâis arguably partially solved by modern democratic processes and formal amendment procedures, yet the interpretive tradition has thickened into a self-sustaining authority structure. The Tangled Rope classification prevents mislabeling this as pure coordination (Rope) because identifiable victims bear concentrated costs (democratic constraint, doctrinal marginalization), and prevents mislabeling it as pure extraction (Snare) because the coordination function (peaceful constitutional adaptation) is structurally genuine and historically demonstrated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_document_kernel_position,
    'Is this constraint a genuine coordination mechanism for constitutional adaptation, or an authority structure that extracts legitimacy from the kernel while displacing democratic and textual constraints?',
    'Comparative constitutional analysis measuring legislative override rates, judicial independence metrics, and constitutional stability across systems with and without living-document interpretive traditions.',
    'If primarily extractive, classification shifts toward snare; if coordinative with incidental extraction, remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_document_kernel_position, conceptual, 'Whether the living document reading is coordination or extraction-dominant').

omega_variable(
    precedential_accumulation_as_development,
    'Does precedential accumulation constitute genuine constitutional development or merely institutional drift captured by the interpretive class?',
    'Historical counterfactual analysis comparing constitutional outcomes under strict textual fixity versus common-law development paths.',
    'Would reclassify the coordination-extraction balance if development is shown to be largely epiphenomenal to judicial power expansion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precedential_accumulation_as_development, empirical, 'Whether precedent tracks development or drift').

omega_variable(
    sibling_reading_coexistence,
    'Can the living document reading structurally coexist with baronial privilege and universal rights readings without collapsing into one or the other?',
    'Analysis of whether the authority structure genuinely sustains all three readings simultaneously or progressively privileges the living document reading.',
    'If coexistence is performative rather than structural, the constraint''s coordination function degrades toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Structural stability of kernel reading pluralism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcld_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mcld_tr_t20, magna_carta_1215__living_document_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(mcld_tr_t40, magna_carta_1215__living_document_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(mcld_tr_t60, magna_carta_1215__living_document_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(mcld_tr_t80, magna_carta_1215__living_document_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(mcld_tr_t100, magna_carta_1215__living_document_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(mcld_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mcld_be_t20, magna_carta_1215__living_document_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(mcld_be_t40, magna_carta_1215__living_document_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(mcld_be_t60, magna_carta_1215__living_document_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(mcld_be_t80, magna_carta_1215__living_document_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(mcld_be_t100, magna_carta_1215__living_document_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mcld_su_t0, magna_carta_1215__living_document_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mcld_su_t20, magna_carta_1215__living_document_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(mcld_su_t40, magna_carta_1215__living_document_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(mcld_su_t60, magna_carta_1215__living_document_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(mcld_su_t80, magna_carta_1215__living_document_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(mcld_su_t100, magna_carta_1215__living_document_reading, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the magna_carta_1215 kernel, decomposed per the epsilon-invariance principle. Each reading instantiates a structurally distinct constraint with different beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
