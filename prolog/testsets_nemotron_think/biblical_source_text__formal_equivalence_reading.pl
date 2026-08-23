% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Reading of Biblical Source Text
 *   domain: religious/translation/authority
 *
 * SUMMARY:
 *   The formal equivalence reading of the biblical source text kernel
 *   mandates that translation preserve the morphosyntactic structure, word
 *   order, and lexical correspondence of the Hebrew and Greek originals,
 *   treating intelligibility as the responsibility of the reading community
 *   rather than the translation itself. This reading instantiates a
 *   constraint that extracts high comprehension costs from non-specialist
 *   readers while concentrating interpretive authority in communities and
 *   institutions that maintain the requisite linguistic expertise. The
 *   constraint presents itself as fidelity to divine revelation but operates
 *   as a gatekeeping mechanism: the same communities that define the standard
 *   also control the training, credentialing, and authorization pipelines
 *   that make compliance possible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.72).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.68).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/translation/authority").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'a2c338bf-1c56-4664-993d-0d67d4f4636d').
narrative_ontology:cs_kernel_codification('a2c338bf-1c56-4664-993d-0d67d4f4636d', fixed_text).
narrative_ontology:cs_authority_grounding('a2c338bf-1c56-4664-993d-0d67d4f4636d', lineage).
narrative_ontology:cs_interpretation_layer_present('a2c338bf-1c56-4664-993d-0d67d4f4636d').
narrative_ontology:cs_reading_relation('a2c338bf-1c56-4664-993d-0d67d4f4636d', biblical_source_text__dynamic_equivalence_reading, forecloses).
narrative_ontology:cs_reading_relation('a2c338bf-1c56-4664-993d-0d67d4f4636d', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('a2c338bf-1c56-4664-993d-0d67d4f4636d', foundational, formal_correspondence_preserves_divine_intent).
narrative_ontology:cs_axiom_status(formal_correspondence_preserves_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('a2c338bf-1c56-4664-993d-0d67d4f4636d', formal_correspondence_preserves_divine_intent, deontological).
narrative_ontology:cs_axiom('a2c338bf-1c56-4664-993d-0d67d4f4636d', foundational, intelligibility_responsibility_rests_with_community).
narrative_ontology:cs_axiom_status(intelligibility_responsibility_rests_with_community, holdable).
narrative_ontology:cs_axiom_grounding('a2c338bf-1c56-4664-993d-0d67d4f4636d', intelligibility_responsibility_rests_with_community, conventional).
narrative_ontology:cs_reference_frame('a2c338bf-1c56-4664-993d-0d67d4f4636d', reformation_sola_scriptura_stability).
narrative_ontology:cs_drift_state('a2c338bf-1c56-4664-993d-0d67d4f4636d', contemporary_textual_criticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a2c338bf-1c56-4664-993d-0d67d4f4636d', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, conservative_interpretive_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, theological_institutions).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, formal_equivalence_translation_committees).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_readers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_believers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, resource_poor_translation_communities).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, textual_stability_preserves_doctrinal_integrity).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, source_language_structure_mediates_divine_intent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set translation standards and doctrinal boundaries through confessional statements and institutional authority. Maintain authority by controlling which translations are authorized for liturgical and catechetical use. Benefit from the gatekeeping function that makes formal equivalence the only legitimate approach. Exit would require abandoning confessional identity and institutional position.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, conservative_interpretive_communities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, conservative_interpretive_communities, beneficiary).

% Operate seminaries, publishing houses, and denominational structures that train translators, approve curricula, and credential ministers. Formal equivalence is embedded in degree requirements, ordination exams, and institutional publishing pipelines. Benefit from the specialized training infrastructure they control. Exit would dismantle their educational and credentialing monopoly.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, theological_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, theological_institutions, beneficiary).

% Receive funding, prestige, and institutional backing for producing formal equivalence translations. Their expertise is validated only within this paradigm. Constrained exit because their specialized linguistic training (biblical Hebrew/Greek, discourse analysis) has limited transferability outside the formal equivalence ecosystem.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, formal_equivalence_translation_committees, beneficiary,
    organized, biographical, constrained, global).

% Encounter translations that preserve source-language syntax, idiom, and word order at the expense of target-language intelligibility. Must depend on teaching ministries, study aids, or clergy mediation to access meaning. Constrained exit because switching translations risks community alienation and the available alternatives are framed as theologically suspect.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_specialist_readers, payer,
    powerless, biographical, constrained, global).

% Their spiritual formation is mediated through a text they cannot fully comprehend without institutional mediation. The constraint fuses their religious identity with the formal equivalence translation — questioning the translation feels like questioning the faith. Identity-locked exit: leaving the translation paradigm threatens communal belonging and self-understanding.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, lay_believers, payer,
    powerless, biographical, identity_locked, global).

% Minority language communities lacking trained biblical scholars must either adopt formal equivalence translations produced by outside agencies (often in a language of wider communication) or attempt translation without adequate linguistic support. Trapped: no local capacity to produce alternatives, no resources to acquire the required expertise, and the constraint declares their vernacular efforts illegitimate.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, resource_poor_translation_communities, payer,
    powerless, generational, trapped, global).

% Analyze the textual history, translation theory, and power dynamics from academic positions. Their critique of formal equivalence as ideologically motivated rather than linguistically necessary is structurally excluded from confessional decision-making but shapes the broader discourse.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, critical_scholars, observer,
    analytical, generational, analytical, global).

% Argue that communicative effectiveness should govern translation. Their translations are produced but barred from liturgical and catechetical use in conservative communities. Constrained exit: they operate parallel structures (Bible societies, missionary agencies) but cannot access the institutional authority that legitimates translations for the beneficiary communities.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a stable textual anchor across generations, languages, and communities — a fixed reference point for doctrine, liturgy, and communal identity that does not drift with translator judgment or cultural change.
% TRANSFER_FUNCTION: Moves interpretive authority and epistemic access from non-specialist readers to trained specialists and the institutions that credential them. The cost is paid in comprehension effort, dependence on mediation, and exclusion of vernacular agency; the gain accrues as institutional control over the text's meaning and the training pipeline that produces authorized interpreters.
% ABSENT_VOICES: Resource-poor translation communities (trapped by lack of specialist capacity), lay believers whose identity is fused to the translation paradigm (identity-locked), and dynamic equivalence advocates (excluded from liturgical/catechetical authorization). These voices would challenge the necessity of structural fidelity over communicative clarity but are not seated at the tables where translation standards are set.
% DISAPPEARANCE_RATIONALE: If formal equivalence were abandoned as the normative standard, confessional institutions would lose their primary textual anchor, translation committees would lose their mandate, seminaries would restructure curricula, and millions of believers would encounter a different mediating text — the entire ecosystem of authority, formation, and communal boundary-maintenance would reorganize around a new translation paradigm.
% FOUNDING_PROBLEM: The Protestant Reformation's recovery of the source texts created a crisis: without a stable Hebrew/Greek text and a translation philosophy that resisted theological drift, vernacular Bibles could become vehicles for whatever theology the translator preferred. Formal equivalence was built to solve the problem of doctrinal instability in translation.
% FOUNDING_PROBLEM_CORROBORATION: Conservative institutions attest the problem remains live — textual stability is still the bulwark against theological revisionism. Critical scholars (Metzger, Ehrman, Tov) and dynamic equivalence theorists (Nida, Bratcher) attest the founding problem is substantially solved by modern textual criticism and translation science — the Hebrew/Greek text is now more stable than ever, and meaning-based translation has proven doctrinally reliable. The corroboration is split along the kernel's fault lines.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers the entire burden of bridging the linguistic-cultural gap onto the reader, requiring either years of language study or dependence on institutional mediation. Suppression (0.68) is substantial because alternative translation philosophies are not merely disfavored but excluded from liturgical and catechetical use in conservative communities — the enforcement machinery includes denominational publishing controls, ordination requirements, and confessional subscription. Theater ratio (0.38) reflects that the coordination function (textual stability across generations) is real but increasingly performed by modern textual criticism rather than translation philosophy, while the extraction function (authority concentration) grows. Accessibility collapse (0.79) is high because once the constraint is understood as a choice rather than a necessity, alternatives (dynamic equivalence, critical reconstruction) become visible but remain structurally inaccessible to identity-locked and trapped agents.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (conservative communities, institutions), the constraint is genuine coordination: it preserves the text that grounds their identity and doctrine. From the payer seats (non-specialist readers, lay believers, resource-poor communities), the same structure operates as extraction: they pay the comprehension cost and surrender interpretive agency while the beneficiaries control the mediation pipeline. The engine computes this divergence from the structural data — identity-locked exit for lay believers means they experience near-full target directionality despite sharing the beneficiaries' confessional commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conservative_interpretive_communities, theological_institutions, formal_equivalence_translation_committees) are declared because they collect rents from the constraint: institutional authority, training monopolies, publishing control, and the credentialing pipeline. Victims (non_specialist_readers, lay_believers, resource_poor_translation_communities) are declared because they bear the comprehension costs, dependence on mediation, and exclusion from translation agency. The identity_locked exit for lay_believers reflects fusion of religious self-concept with the translation paradigm — exit is not merely costly but identity-threatening. The trapped exit for resource_poor_translation_communities reflects absolute absence of local specialist capacity combined with the constraint's delegitimization of vernacular efforts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal instability in translation) was real in 1517-1881 when textual bases were unstable and translation was unregulated. Modern textual criticism (NA28/UBS5, BHS) has stabilized the source text beyond what the Reformers imagined. Dynamic equivalence translations (NIV, NLT, CEB) have demonstrated doctrinal reliability across decades of global use. Yet the constraint persists and intensifies because the institutions that benefited from the solution now depend on the problem's persistence for their authority. The mandate has atrophied into a maintenance structure for the benefiting institutions — classic mandatrophy where the solution outlives the problem and becomes the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the formal_equivalence_reading a distinct constraint from the biblical_source_text kernel, or does it claim to be the kernel itself?',
    'Analyze whether confessional documents treat formal equivalence as a hermeneutical choice among readings or as the only legitimate instantiation of the kernel. Track language: ''the translation philosophy required by Scripture'' vs. ''our translation philosophy.''',
    'If the reading claims to BE the kernel, it forecloses all sibling readings by definitional fiat (forecloses relation). If it acknowledges itself as one reading among others, it coexists_with them structurally. The classification shifts from extraction-maintained monopoly to contested coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading acknowledges the kernel''s contested nature or naturalizes itself as the kernel').

omega_variable(
    textual_stability_source,
    'Does the textual stability that formal equivalence coordinates around derive from the translation philosophy itself, or from modern textual criticism (which operates independently of translation philosophy)?',
    'Compare textual stability metrics across translation philosophies using the same critical editions. If dynamic equivalence translations based on NA28/UBS5 show equal cross-generational stability, the coordination function belongs to textual criticism, not formal equivalence.',
    'If stability is sourced in textual criticism, formal equivalence''s coordination claim is partly performative — it claims credit for a function it does not uniquely provide. This would increase the extraction-to-coordination ratio, reinforcing tangled_rope or pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stability_source, empirical, 'Attribution of the coordination function (textual stability) to translation philosophy vs. textual criticism').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative translations structural (institutional publishing controls, ordination requirements) or internalized (believers believe formal equivalence is the only faithful approach)?',
    'Post-reform trajectory study: in communities that officially adopted dynamic equivalence (e.g., some mainline Protestant denominations), measure whether lay resistance persists after institutional barriers are removed. Persistent resistance indicates internalized suppression.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after institutional exit. This would amplify extraction for identity-locked payers beyond what the structural d-value captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative translation exclusion').

omega_variable(
    resource_poor_community_agency,
    'Do resource-poor translation communities have latent capacity for meaning-based translation that the constraint suppresses, or is formal equivalence genuinely the only viable approach given their constraints?',
    'Case studies of minority language translations produced outside formal equivalence paradigms (e.g., SIL International''s meaning-based approach). Compare comprehension outcomes, community ownership, and sustainability.',
    'If meaning-based approaches work better for resource-poor communities, the constraint''s claim to be the only viable path is falsified — the trapped exit is manufactured by the constraint''s delegitimization of alternatives, not by objective necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_poor_community_agency, empirical, 'Whether the trapped exit of resource-poor communities is structurally necessary or constraint-manufactured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 1517, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_source_text__formal_equivalence_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(bibl_tr_t1611, biblical_source_text__formal_equivalence_reading, theater_ratio, 1611, 0.2).
narrative_ontology:measurement(bibl_tr_t1881, biblical_source_text__formal_equivalence_reading, theater_ratio, 1881, 0.25).
narrative_ontology:measurement(bibl_tr_t1952, biblical_source_text__formal_equivalence_reading, theater_ratio, 1952, 0.3).
narrative_ontology:measurement(bibl_tr_t1978, biblical_source_text__formal_equivalence_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(bibl_tr_t2025, biblical_source_text__formal_equivalence_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(bibl_be_t1611, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1611, 0.45).
narrative_ontology:measurement(bibl_be_t1881, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1881, 0.55).
narrative_ontology:measurement(bibl_be_t1952, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1952, 0.62).
narrative_ontology:measurement(bibl_be_t1978, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1978, 0.68).
narrative_ontology:measurement(bibl_be_t2025, biblical_source_text__formal_equivalence_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(bibl_su_t1611, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1611, 0.45).
narrative_ontology:measurement(bibl_su_t1881, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1881, 0.55).
narrative_ontology:measurement(bibl_su_t1952, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1952, 0.6).
narrative_ontology:measurement(bibl_su_t1978, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(bibl_su_t2025, biblical_source_text__formal_equivalence_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.08).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, confessional_boundary_maintenance).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, seminary_curriculum_authority).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, vernacular_translation_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_source_text kernel. The kernel's ε-invariance requires decomposition: formal_equivalence_reading (this story, tangled_rope, high extraction on non-specialists), dynamic_equivalence_reading (lower extraction, different beneficiary set — mission agencies, Bible societies), critical_reconstructive_reading (extraction on confessional communities via textual instability claims). Each reading has distinct ε, stakeholders, and classification. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, organized, 0.25).
constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
