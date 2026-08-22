% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Translation Reading (Biblical Source Text)
 *   domain: religious_authority/translation_theory/biblical_studies
 *
 * SUMMARY:
 *   The dynamic equivalence reading of the biblical source text kernel
 *   elevates communicative effectiveness in the target language above
 *   structural fidelity to the source. Originating in Eugene Nida's
 *   functional equivalence theory (1960s), it became the dominant paradigm
 *   for Bible translation in missionary and mainline Protestant contexts by
 *   the 1980s. The constraint operates through translation guidelines,
 *   denominational publishing mandates, and seminary curricula that treat
 *   dynamic equivalence as the responsible default for mission. Beneficiaries
 *   are lay readers gaining intelligibility and missionary teams gaining
 *   deployable texts; victims are scholars and formal-equivalence translators
 *   who lose access to morphological and syntactic precision that word-study
 *   and textual criticism require. The same kernel generates two sibling
 *   readings: formal equivalence (structure primary) and critical
 *   reconstructive (historical recovery primary). This story instantiates
 *   only the dynamic equivalence reading.
 *
 * KEY AGENTS:
 *   - lay_readers: Primary beneficiary (moderate/constrained) — gains intelligibility, loses structural access
 *   - missionary_contexts: Primary beneficiary (organized/constrained) — gains deployable texts, loses source-language accountability
 *   - pastoral_ministry_teams: Secondary beneficiary (moderate/constrained) — gains preaching-ready texts, loses exegetical depth
 *   - scholars_requiring_word_study_precision: Primary victim (moderate/trapped) — loses morphological/syntactic data for word-study
 *   - formal_equivalence_translators: Primary victim (organized/constrained) — marginalized by institutional dominance of dynamic equivalence
 *   - textual_critics_dependent_on_morphological_fidelity: Secondary victim (powerful/constrained) — loses translation-level evidence for textual decisions
 *   - translation_agencies: Agenda setter (institutional/arbitrage) — sets guidelines, controls publishing, collects institutional legitimacy
 *   - denominational_publishing_boards: Agenda setter (institutional/constrained) — mandates translation philosophy for curriculum and liturgy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.45).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Translation Reading (Biblical Source Text)").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious_authority/translation_theory/biblical_studies").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, 'ca93ae02-46bd-459e-8121-51d3fd5f6007').
narrative_ontology:cs_kernel_codification('ca93ae02-46bd-459e-8121-51d3fd5f6007', fixed_text).
narrative_ontology:cs_authority_grounding('ca93ae02-46bd-459e-8121-51d3fd5f6007', lineage).
narrative_ontology:cs_interpretation_layer_present('ca93ae02-46bd-459e-8121-51d3fd5f6007').
narrative_ontology:cs_reading_relation('ca93ae02-46bd-459e-8121-51d3fd5f6007', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca93ae02-46bd-459e-8121-51d3fd5f6007', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('ca93ae02-46bd-459e-8121-51d3fd5f6007', foundational, communicative_effectiveness_primacy).
narrative_ontology:cs_axiom_status(communicative_effectiveness_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ca93ae02-46bd-459e-8121-51d3fd5f6007', communicative_effectiveness_primacy, instrumental).
narrative_ontology:cs_axiom('ca93ae02-46bd-459e-8121-51d3fd5f6007', foundational, pastoral_mission_supremacy_over_structural_fidelity).
narrative_ontology:cs_axiom_status(pastoral_mission_supremacy_over_structural_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('ca93ae02-46bd-459e-8121-51d3fd5f6007', pastoral_mission_supremacy_over_structural_fidelity, deontological).
narrative_ontology:cs_reference_frame('ca93ae02-46bd-459e-8121-51d3fd5f6007', nida_functional_equivalence_paradigm).
narrative_ontology:cs_drift_state('ca93ae02-46bd-459e-8121-51d3fd5f6007', contemporary_mission_praxis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ca93ae02-46bd-459e-8121-51d3fd5f6007', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_contexts).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, pastoral_ministry_teams).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, scholars_requiring_word_study_precision).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translators).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, textual_critics_dependent_on_morphological_fidelity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, pastoral_ministry_teams).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, communicative_effectiveness_primacy).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, pastoral_mission_supremacy_over_structural_fidelity).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, intelligibility_as_translation_telos).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive Bible translations in their heart language that prioritize immediate comprehensibility over linguistic precision. Gain access to Scripture's meaning without needing original-language training. Lose the ability to trace translation choices to source morphology — dependent on translators' decisions. Exit is constrained: alternative translations exist but may not be available in their language or promoted by their church.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    moderate, biographical, constrained, global).

% Deploy dynamic equivalence translations as primary evangelism and discipleship tools. Gain rapidly producible, immediately usable texts for oral cultures and low-literacy contexts. Lose accountability to source-language structure — translation decisions cannot be checked by sending churches or partner scholars. Exit is constrained: translation agencies and funding bodies mandate dynamic equivalence as best practice; switching paradigms risks institutional support.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_contexts, beneficiary,
    organized, generational, constrained, global).

% Preach and teach from dynamic equivalence translations. Gain sermon-ready texts that communicate clearly to congregations. Lose exegetical depth — cannot reliably trace English terms to Greek/Hebrew morphology for word studies. Exit is constrained: denominational curricula and approved translation lists favor dynamic equivalence; using formal equivalence translations creates friction with leadership and congregants accustomed to dynamic equivalence style.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, pastoral_ministry_teams, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, pastoral_ministry_teams, payer).

% Depend on translation-level morphological and syntactic data for lexical semantics, textual criticism, and theological argument. Dynamic equivalence translations systematically obscure this data through restructuring and domestication. Lose the ability to use translations as evidence for source-language claims. Exit is trapped: professional identity and career are built on textual precision; the field's dominant paradigm treats their need as secondary. Cannot switch to a paradigm that serves their need without marginalization.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, scholars_requiring_word_study_precision, payer,
    moderate, biographical, trapped, global).

% Produce translations prioritizing source-language structure. Marginalized by institutional dominance of dynamic equivalence: denied missionary funding, excluded from major translation partnerships, underrepresented in seminary curricula. Gain scholarly respect and doctrinal precision. Lose institutional resources and deployment channels. Exit is constrained: can continue their work but with reduced support; paradigm shift in agencies would require generational change.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translators, payer,
    organized, generational, constrained, global).

% Use translation evidence (especially ancient versions) for textual decisions. Dynamic equivalence translations provide no usable morphological evidence — their restructuring erases the formal correspondences textual criticism requires. Lose a potential data stream for reconstructing the textual tradition. Exit is constrained: can work with formal equivalence translations and ancient versions, but the growing dominance of dynamic equivalence in modern vernaculars shrinks the pool of translation evidence.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, textual_critics_dependent_on_morphological_fidelity, payer,
    powerful, generational, constrained, global).

% Set translation guidelines, approve projects, control publishing and distribution. Justify dynamic equivalence as mission-best-practice. Collect institutional legitimacy and donor funding tied to 'effective communication.' Could switch paradigms but face massive sunk costs in training, partnerships, and published catalog. Exit is arbitrage-grade: they hold the power to change the constraint but benefit from its persistence.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Mandate translation philosophy for denominational curricula, liturgy, and approved Bible lists. Dynamic equivalence is the default for mission and education materials. Gain coherence across denominational resources. Lose flexibility — formal equivalence alternatives create curricular fragmentation. Exit is constrained: congregational expectation and ecumenical partnerships lock in the paradigm; switching would require synodical action.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, denominational_publishing_boards, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes ancient biblical texts communicably intelligible to lay audiences across radical linguistic, cultural, and literacy distance — solves the genuine coordination problem of 'how does this text mean to them?' without requiring every reader to learn Greek/Hebrew.
% TRANSFER_FUNCTION: Moves morphological and syntactic precision from the translation layer (where scholars and formal-equivalence translators need it) to the intelligibility layer (where lay readers and missionaries need immediate comprehension). The transfer is mediated by translation agencies and publishing boards who set the paradigm.
% ABSENT_VOICES: Oral-tradition communities who might prefer formal correspondence for memorization fidelity; minority-language groups whose first translation is dynamic equivalence and who never get a formal equivalence option; scholars in Global South institutions who are trained in dynamic equivalence paradigms and lack access to formal equivalence tools.
% DISAPPEARANCE_RATIONALE: If dynamic equivalence as a mandated paradigm vanished overnight, translation agencies would face immediate paradigm crisis: missionaries would demand formal equivalence alternatives, seminaries would rewrite curricula, publishing boards would need new approval standards. The global Bible translation ecosystem would reorganize around a new default — likely a pluralistic model where multiple translation philosophies coexist per language.
% FOUNDING_PROBLEM: Mid-20th century missionary contexts faced a genuine crisis: existing formal equivalence translations (KJV-style) were unintelligible to target audiences with no Christian vocabulary, and the linguistic distance between biblical languages and receptor languages made word-for-word translation communicate nonsense. Dynamic equivalence was built to solve 'how do we make this text mean what it means to people who don't share our linguistic world?'
% FOUNDING_PROBLEM_CORROBORATION: Translation agencies (Wycliffe, SIL, UBS) attest the problem remains live: linguistic distance and cultural unfamiliarity persist. Formal equivalence scholars (ESV, NASB translation committees) and critical reconstructive scholars (textual critics) attest the problem is substantially solved by improved formal equivalence methods and that dynamic equivalence now persists as institutional inertia. Independent linguistic anthropology (e.g., work on translation and cognition) corroborates that both communicative effectiveness AND structural access are achievable simultaneously in many receptor languages — the trade-off is not structurally necessary.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).
:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint extracts morphological and syntactic precision from scholars and formal-equivalence translators to subsidize intelligibility for lay readers and missionaries. The loss is not zero (linguistic distance requires some restructuring) but exceeds the coordination floor — theological domestication of key terms (e.g., 'Christ' → 'Anointed One', 'justification' → 'made right with God') extracts precision for doctrinal accessibility. Suppression (0.38) is moderate: formal equivalence translations remain legally publishable but face institutional marginalization (curriculum exclusion, missionary funding denial, seminary hiring bias). Theater ratio (0.22) is low-moderate: the coordination function (making Scripture intelligible) is genuine, but a growing share of enforcement activity defends the paradigm's institutional dominance rather than its communicative effectiveness. Accessibility collapse (0.42) is moderate: alternatives (formal equivalence translations) exist but are discouraged in mission contexts; resistance (0.55) is moderate: formal equivalence and critical reconstructive readings persist as live scholarly and ecclesiastical positions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is genuine coordination: it solves the real problem of making ancient texts communicable across radical linguistic and cultural distance. From the scholar/payer seat, the same structure operates as extraction: morphological data essential for textual criticism and word-study is systematically erased by translation philosophy, and the paradigm's institutional dominance suppresses alternatives. The engine computes this divergence from structural data — the claimed type (tangled_rope) reflects the author's assessment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda setters (translation agencies, publishing boards) sit near beneficiary end (d ~ 0.2): they control the constraint, collect institutional legitimacy, and face arbitrage-grade exit (can switch paradigms). Lay readers and missionary contexts are beneficiaries (d ~ 0.3): genuine intelligibility gain, constrained exit (depend on available translations). Pastoral ministry teams are dual-positioned (beneficiary + secondary payer, d ~ 0.4): gain preaching ease, lose exegetical depth. Scholars and formal-equivalence translators are payers/victims (d ~ 0.8): bear precision loss, trapped by professional identity and institutional gatekeeping. Textual critics are payers (d ~ 0.7): powerful but constrained by translation-layer data loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making Scripture communicable to non-specialist audiences across cultures) remains live, so mandatrophy is not resolved. However, the constraint's persistence now depends more on institutional inertia and professional socialization than on the founding problem's urgency — many mission contexts now have formal equivalence alternatives that were unavailable in 1960. The mandate has not atrophied, but its justification has narrowed: dynamic equivalence is no longer the only solution to the coordination problem, yet it retains institutional primacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a single reading of the contested ''biblical_source_text'' kernel, or does the label ''dynamic equivalence'' conflate structurally distinct translation philosophies (e.g., Nida''s functional equivalence vs. later meaning-based approaches)?',
    'Disaggregate translation projects by their explicit theoretical commitments: if Nida-era functional equivalence and later meaning-based theories produce different beneficiary/victim structures and extractiveness profiles, they are separate constraints.',
    'If conflated, the single story masks divergent structural relationships; if decomposed, each reading gets its own ε and classification, linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether ''dynamic equivalence'' labels one constraint or a family of related constraints').

omega_variable(
    extraction_boundary_precision_vs_intelligibility,
    'Where does the moderate extractiveness (ε=0.45) land: is it the inherent cost of cross-linguistic communication (coordination floor), or does it encode theological/ecclesial preferences that extract from scholars and formal-equivalence communities?',
    'Compare dynamic equivalence translations against a control set of formal equivalence translations for the same source passages: measure morphological precision loss, theological term domestication, and syntactic restructuring. If precision loss correlates with doctrinal distinctives rather than linguistic necessity, the extraction is preferential, not structural.',
    'If extraction is preferential, the constraint is a snare or tangled_rope with identifiable theological beneficiaries; if structural, it is a rope with a genuine coordination floor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_precision_vs_intelligibility, empirical, 'Whether morphological precision loss is linguistically necessary or theologically motivated').

omega_variable(
    suppression_mechanism_canonical_pressure,
    'Is the measured suppression (0.38) structural (denominational mandates, publishing gatekeeping, curriculum requirements) or internalized (scholars and pastors self-censor because dynamic equivalence is the ''responsible'' choice for mission)?',
    'Post-reform suppression trajectory: in denominations that officially adopted dynamic equivalence, did suppression of formal equivalence decrease after the mandate, or did it persist as professional norm?',
    'If internalized, effective suppression is higher than structural measure; the constraint carries its enforcement into the agent''s cognitive frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_canonical_pressure, empirical, 'Structural vs. internalized suppression in translation mandate enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1960, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(bibl_tr_t1975, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(bibl_tr_t1990, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bibl_tr_t2005, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(bibl_tr_t2020, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1960, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(bibl_be_t1975, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(bibl_be_t1990, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(bibl_be_t2005, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(bibl_be_t2020, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1960, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(bibl_su_t1975, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(bibl_su_t1990, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(bibl_su_t2005, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(bibl_su_t2020, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2020, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.03).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% Biblical source text kernel family (3 readings): dynamic equivalence (this story) extracts morphological precision for intelligibility; formal equivalence preserves structure at intelligibility cost; critical reconstructive suspends both until textual basis is established. The three constraints are linked by shared kernel and mutually exclusive teloi.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, institutional, 0.25).
constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
