% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Reading of the Biblical Source Text Kernel
 *   domain: religious/textual/institutional
 *
 * SUMMARY:
 *   This story instantiates the formal-equivalence reading of the biblical
 *   source text kernel: the position that fidelity to source-language
 *   grammar, word order, and idiom is the primary translation obligation, and
 *   that any resulting difficulty for readers is a subordinate problem to be
 *   solved through teaching and institutional mediation rather than by
 *   adjusting the translation itself. This reading coexists with, but is
 *   structurally distinct from, the dynamic-equivalence reading (which
 *   relocates the burden onto the translation for communicative
 *   effectiveness) and the critical-reconstructive reading (which contests
 *   whether a stable source text can be privileged at all prior to
 *   establishing its textual basis). Each reading is authored as its own
 *   constraint with its own epsilon; this file addresses only the
 *   formal-equivalence reading, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - hermeneutically_conservative_denominations: agenda-setting institutional beneficiary — sets and enforces the structural-fidelity policy
 *   - credentialed_biblical_language_scholars: professional beneficiary — expertise scarcity is the coordination and extraction mechanism
 *   - seminary_publishing_institutions: commercial beneficiary — monetizes the teaching apparatus the policy requires
 *   - non_specialist_lay_readers and second_language_congregants: primary payers — bear the comprehension cost the reading assigns to 'the reader/community'
 *   - under_resourced_congregations: structurally disadvantaged payer — inherits the teaching burden without the resources to discharge it
 *   - dynamic_equivalence_translation_communities: excluded rival reading — argues the burden allocation itself is the defect
 *   - textual_critics: analytical observer — notes the base-text stability both formal and dynamic readings presuppose
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.62).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/textual/institutional").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, '8e88b3db-ea70-4342-a013-7f0ed70a38f7').
narrative_ontology:cs_kernel_codification('8e88b3db-ea70-4342-a013-7f0ed70a38f7', fixed_text).
narrative_ontology:cs_authority_grounding('8e88b3db-ea70-4342-a013-7f0ed70a38f7', lineage).
narrative_ontology:cs_interpretation_layer_present('8e88b3db-ea70-4342-a013-7f0ed70a38f7').
narrative_ontology:cs_reading_relation('8e88b3db-ea70-4342-a013-7f0ed70a38f7', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e88b3db-ea70-4342-a013-7f0ed70a38f7', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('8e88b3db-ea70-4342-a013-7f0ed70a38f7', foundational, structural_form_carries_theological_content).
narrative_ontology:cs_axiom_status(structural_form_carries_theological_content, holdable).
narrative_ontology:cs_axiom_grounding('8e88b3db-ea70-4342-a013-7f0ed70a38f7', structural_form_carries_theological_content, deontological).
narrative_ontology:cs_axiom('8e88b3db-ea70-4342-a013-7f0ed70a38f7', foundational, comprehension_burden_properly_rests_on_reader_via_teaching).
narrative_ontology:cs_axiom_status(comprehension_burden_properly_rests_on_reader_via_teaching, holdable).
narrative_ontology:cs_axiom_grounding('8e88b3db-ea70-4342-a013-7f0ed70a38f7', comprehension_burden_properly_rests_on_reader_via_teaching, conventional).
narrative_ontology:cs_reference_frame('8e88b3db-ea70-4342-a013-7f0ed70a38f7', source_language_structural_primacy).
narrative_ontology:cs_drift_state('8e88b3db-ea70-4342-a013-7f0ed70a38f7', contemporary_lay_literacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e88b3db-ea70-4342-a013-7f0ed70a38f7', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, credentialed_biblical_language_scholars).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, seminary_publishing_institutions).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_lay_readers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, second_language_congregants).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, under_resourced_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrinal preference for word-for-word translation and enforces it through approved translation lists, seminary curricula, and pulpit authority. Maintains that interpretive authority properly resides with trained teachers, not with lay readers encountering the text unaided. Its institutional continuity depends on the text remaining structurally opaque enough to require its mediating role.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations, beneficiary).

% Trains in Hebrew, Greek, and Aramaic; produces and defends formal-equivalence translations; teaches the courses required to access the source structure directly. Career, income, and status accrue from the text's structural difficulty remaining a scarce, teachable skill.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, credentialed_biblical_language_scholars, beneficiary,
    organized, biographical, mobile, global).

% Publishes formal-equivalence study editions, commentaries, and pedagogical materials; sells the teaching apparatus that the reading declares intelligibility depends on. Revenue and institutional relevance scale with how much mediation the text is held to require.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, seminary_publishing_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Encounters a translation retaining source-language syntax, idiom, and word order that often reads as opaque or archaic in the target language. Is told that confusion is a personal or educational deficiency to be remedied by joining a study program or deferring to a trained teacher, not a translation defect. Has no practical means to independently verify whether the difficulty is inherent to the text or an artifact of the translation policy.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_specialist_lay_readers, payer,
    powerless, biographical, trapped, local).

% Reads the formal-equivalence text in a language that is not their first, compounding the structural opacity with linguistic distance. Bears the heaviest accessibility cost of any reader group while having the least access to the institution's compensating education pipeline.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, second_language_congregants, payer,
    powerless, biographical, trapped, regional).

% Lacks funded seminary-trained clergy or systematic teaching infrastructure. Inherits the intelligibility burden the reading assigns to 'the reader/community through teaching' without inheriting the resources the assignment presupposes; the promised remedy (teaching) is unevenly available by wealth and geography.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, under_resourced_congregations, payer,
    powerless, generational, constrained, regional).

% Argues that communicative effectiveness, not structural mirroring, should be primary, and that the teaching burden this reading imposes is an avoidable cost rather than a theological necessity. Largely excluded from formal-equivalence institutional structures, publishing channels, and seminary curricula that treat the structural-fidelity premise as settled rather than contested.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_translation_communities, excluded,
    organized, biographical, mobile, global).

% Studies manuscript variation and the history of translation philosophy without institutional stake in either formal or dynamic equivalence prevailing. Notes that both readings presuppose a stable base text that the critical-reconstructive reading itself contests.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, textual_critics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__formal_equivalence_reading, seminary_publishing_institutions).
narrative_ontology:fixing_cost_class(biblical_source_text__formal_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a stable, structurally faithful rendering of the source text across generations and communities, enabling cross-community verification, cross-referencing, and a shared textual object that different teachers and denominations can jointly interpret rather than each producing incompatible paraphrases.
% TRANSFER_FUNCTION: Moves interpretive authority and the labor of comprehension from the text-production side (translators, institutions) to the reader side, while moving the resources required to discharge that labor (tuition, seminary access, teacher time) from readers to the institutions that supply formal training — readers pay in comprehension difficulty and, where available, in fees for the education needed to resolve it.
% ABSENT_VOICES: Non-specialist and second-language readers who find the structural-fidelity policy itself the source of their difficulty, rather than a personal deficiency, are rarely consulted in translation-committee decisions; dynamic-equivalence advocates who would relocate the burden back onto the translation are institutionally excluded from formal-equivalence publishing and curricular structures.
% DISAPPEARANCE_RATIONALE: If the formal-equivalence policy disappeared overnight, the specialized teaching apparatus built to compensate for structural opacity would lose much of its rationale, seminary curricula emphasizing source-language mastery would need to justify themselves on other grounds, and lay readers would gain direct access to more idiomatic renderings without needing institutional mediation — a substantial redistribution of interpretive authority away from credentialed intermediaries.
% FOUNDING_PROBLEM: Early modern and pre-modern translation efforts sought to prevent doctrinal drift and mistranslation by anchoring the vernacular text as closely as possible to the attested source-language forms, so that meaning could be checked against the original rather than trusting a translator's paraphrase.
% FOUNDING_PROBLEM_CORROBORATION: Denominational bodies and seminary scholars attest the founding problem (doctrinal fidelity, prevention of interpretive drift) remains fully live. Independent linguists and dynamic-equivalence practitioners, external to the beneficiary institutions, attest that structural fidelity and doctrinal fidelity are separable — a text can preserve meaning without preserving source syntax — and that the persistence of the structural-primacy rule now serves institutional authority maintenance more than the original anti-drift concern; no fully disinterested third party outside translation-adjacent institutions has adjudicated the dispute.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is set at 0.62 — substantial but not extreme — because the reading does perform a genuine coordination function (a stable, cross-community reference text enabling shared interpretation and doctrinal accountability) while also imposing a real, unevenly distributed comprehension cost on readers who lack access to the compensating teaching infrastructure. Suppression is moderate (0.48): there is no coercive prohibition on alternative translations existing, but denominational endorsement lists, seminary accreditation, and pulpit gatekeeping meaningfully narrow which translations reach congregations. Theater ratio is modest (0.28) and rises slowly — the teaching function is largely real, not merely performative, though an increasing share of institutional activity over time defends the policy's authority rather than serving comprehension directly. Accessibility collapse (0.58) and resistance (0.55) reflect that alternatives (dynamic-equivalence translations) are readily available in the marketplace, so collapse is partial, not total, and resistance from lay readers and rival translation communities is real and organized, not suppressed to silence.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational authorities, credentialed scholars, and seminary publishers sit near the beneficiary end: they set the rule, derive professional and institutional standing from the structural-fidelity requirement, and monetize or credential the remediation it necessitates. Non-specialist lay readers, second-language congregants, and under-resourced congregations sit near the target end: they bear the comprehension cost directly and have the least capacity to access the compensating education the reading assigns as their responsibility. Trapped exit options for these payer groups (limited practical ability to select a different translation tradition within their existing faith community, given social and doctrinal ties) push their effective extraction upward; the institutional beneficiaries' arbitrage-grade exit (able to reposition doctrinally or commercially without losing standing) pushes their effective extraction toward subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — anchoring translation to source-language forms to prevent doctrinal drift — is not obviously dead: doctrinal fidelity remains a live concern for the beneficiary institutions. But the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals a possible drift from anti-drift safeguard toward authority-maintenance function: the teaching apparatus built to compensate for structural opacity has become a durable revenue and status structure independent of whether it remains the most effective anti-drift mechanism available. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (shared reference text, doctrinal accountability) while still registering the asymmetric extraction imposed on payer seats through active enforcement (curricular gatekeeping, endorsed-translation lists) — collapsing it to snare would erase the real coordination value; collapsing it to rope would erase the documented asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_equivalence_reading_identity,
    'Is the structural-fidelity requirement a theologically necessary anti-drift safeguard, or a historically contingent institutional choice that has since become self-sustaining through the teaching economy it necessitates?',
    'Comparative doctrinal-outcome study across communities using formal-equivalence versus dynamic-equivalence translations over multiple generations, controlling for institutional oversight quality, to assess whether doctrinal drift is actually lower under formal equivalence.',
    'If doctrinal outcomes are equivalent across translation philosophies, the structural-fidelity requirement''s coordination justification weakens substantially and the constraint reads closer to snare; if outcomes diverge meaningfully, the tangled_rope classification (genuine coordination plus asymmetric cost) is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formal_equivalence_reading_identity, conceptual, 'Whether structural fidelity is doctrinally necessary or institutionally self-perpetuating.').

omega_variable(
    sibling_reading_delta,
    'How would the beneficiary and victim sets change under the dynamic-equivalence or critical-reconstructive readings of the same kernel?',
    'Author and compare the sibling constraint files (dynamic_equivalence_reading, critical_reconstructive_reading) with their own independently-derived epsilon and stakeholder data; compare beneficiary/victim overlap across the three.',
    'A finding that beneficiary sets substantially overlap across readings (e.g., publishing institutions benefit regardless of translation philosophy) would suggest the extraction is institutional rather than reading-specific; a finding of distinct beneficiary sets per reading confirms each reading is a genuinely distinct constraint, not a relabeling of the same one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta, empirical, 'Cross-reading comparison of beneficiary/victim structure within the shared kernel.').

omega_variable(
    teaching_burden_allocation_fairness,
    'Is assigning intelligibility as ''the subordinate responsibility of reader/community through teaching'' a fair allocation given the documented unevenness in teaching-resource access across congregations?',
    'Survey of teaching-resource availability (funded clergy, study materials, language instruction) across denominational and geographic lines, weighted against comprehension outcomes for lay readers under formal-equivalence translations.',
    'If teaching resources are shown to correlate strongly with wealth and geography, the reading''s burden allocation functions as a regressive tax on comprehension even under a sincere anti-drift rationale, strengthening the tangled_rope reading and its victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teaching_burden_allocation_fairness, empirical, 'Whether the reader/community teaching burden is equitably dischargeable across contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__formal_equivalence_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__formal_equivalence_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__formal_equivalence_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(bibl_tr_t32, biblical_source_text__formal_equivalence_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__formal_equivalence_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__formal_equivalence_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__formal_equivalence_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__formal_equivalence_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(bibl_be_t32, biblical_source_text__formal_equivalence_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__formal_equivalence_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__formal_equivalence_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__formal_equivalence_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(bibl_su_t24, biblical_source_text__formal_equivalence_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(bibl_su_t32, biblical_source_text__formal_equivalence_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__formal_equivalence_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the biblical_source_text kernel. The dynamic_equivalence_reading relocates the intelligibility burden onto the translation itself and is expected to show lower extraction on lay readers but potential extraction on communities valuing structural traceability. The critical_reconstructive_reading contests the stability of the base text both other readings presuppose, and is expected to show extraction concentrated on communities invested in textual certainty rather than on lay comprehension. All three share the kernel but are authored as independent constraints with independent epsilon values per the ε-invariance principle; do not merge or average their metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
