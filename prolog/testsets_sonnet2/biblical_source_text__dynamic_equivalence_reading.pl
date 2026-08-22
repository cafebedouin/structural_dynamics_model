% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Reading of the Biblical Source Text Kernel
 *   domain: religious/linguistic/institutional
 *
 * SUMMARY:
 *   This story instantiates the dynamic-equivalence reading of the biblical
 *   source text kernel: the working commitment, held by most missionary
 *   translation agencies and mass-market Bible publishers since Eugene Nida's
 *   functional-equivalence theory took hold in the mid-20th century, that a
 *   translation's primary obligation is to reproduce the source's
 *   communicative effect on its original audience in the receptor audience,
 *   even where this requires departing from source syntax, morphology, or
 *   lexical one-to-one correspondence. This reading treats structural
 *   fidelity as instrumentally subordinate to intelligibility and
 *   evangelistic reach. It coexists with, but is structurally distinct from,
 *   sibling readings that treat structural fidelity as primary
 *   (formal_equivalence_reading) or that treat the textual basis itself as
 *   unsettled and prior to any translation-philosophy choice
 *   (critical_reconstructive_reading). The three readings are not the same
 *   constraint measured differently — they instantiate genuinely different
 *   arrangements with different beneficiaries, different victims, and
 *   different ε. This file authors only the dynamic-equivalence reading.
 *
 * KEY AGENTS:
 *   - missionary_translation_agencies: institutional agenda-setter, arbitrage exit — controls translation methodology and funding
 *   - lay_readers: powerless beneficiary, constrained exit — receives comprehensible text without access to what was smoothed over
 *   - text_critical_scholars: moderate-power payer, mobile exit — loses primary data source for philological work
 *   - liturgical_traditionalist_communities: organized payer, constrained exit — loses inherited liturgical phrasing and cadence
 *   - evangelistic_publishers: organized beneficiary and co-agenda-setter — commercial and missionary interest in the methodology's dominance
 *   - receptor_language_communities: powerless beneficiary, trapped exit — first access to Scripture, no comparison basis
 *   - translation_committees: institutional observer/agenda-setter — adjudicates between pastoral and scholarly claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.42).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic/institutional").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '461a3cfb-b8a5-42b2-8051-7f3c97785cba').
narrative_ontology:cs_kernel_codification('461a3cfb-b8a5-42b2-8051-7f3c97785cba', distributed).
narrative_ontology:cs_authority_grounding('461a3cfb-b8a5-42b2-8051-7f3c97785cba', practice).
narrative_ontology:cs_interpretation_layer_present('461a3cfb-b8a5-42b2-8051-7f3c97785cba').
narrative_ontology:cs_reading_relation('461a3cfb-b8a5-42b2-8051-7f3c97785cba', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('461a3cfb-b8a5-42b2-8051-7f3c97785cba', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('461a3cfb-b8a5-42b2-8051-7f3c97785cba', foundational, communicative_effect_equivalence_is_the_translation_norm).
narrative_ontology:cs_axiom_status(communicative_effect_equivalence_is_the_translation_norm, holdable).
narrative_ontology:cs_axiom_grounding('461a3cfb-b8a5-42b2-8051-7f3c97785cba', communicative_effect_equivalence_is_the_translation_norm, instrumental).
narrative_ontology:cs_axiom('461a3cfb-b8a5-42b2-8051-7f3c97785cba', secondary, pastoral_accessibility_outranks_lexical_traceability).
narrative_ontology:cs_axiom_status(pastoral_accessibility_outranks_lexical_traceability, holdable).
narrative_ontology:cs_axiom_grounding('461a3cfb-b8a5-42b2-8051-7f3c97785cba', pastoral_accessibility_outranks_lexical_traceability, conventional).
narrative_ontology:cs_reference_frame('461a3cfb-b8a5-42b2-8051-7f3c97785cba', nida_functional_equivalence_framework).
narrative_ontology:cs_drift_state('461a3cfb-b8a5-42b2-8051-7f3c97785cba', contemporary_mature_market_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('461a3cfb-b8a5-42b2-8051-7f3c97785cba', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishers).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, text_critical_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, morphology_dependent_exegetes).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, liturgical_traditionalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, receptor_language_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set translation methodology (Nida-derived functional equivalence) for Bible societies operating in thousands of language communities. Train translators, publish style guides, and fund distribution. Justify the method by pastoral reach: a text that communicates in the receptor language's natural idiom serves the mission of comprehension over the mission of philological preservation. Control which translation philosophy gets institutional funding and imprimatur across most working translation projects worldwide.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Read Scripture in an idiomatic rendering that reads fluently and requires no specialized training to follow. Gain immediate comprehension of sense-for-sense meaning but have no independent way to know where lexical ambiguity, wordplay, or grammatical structure in the source was smoothed over or interpretively resolved for them. Their exit option — learning source languages — is realistically unavailable to almost all of them.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    powerless, biographical, constrained, global).

% Rely on morphological and syntactic precision for word studies, textual criticism, and argument from grammatical structure (verb tense, word order, syntactic ambiguity). A dynamic-equivalence rendering erases the very data their discipline depends on, forcing them back to source texts or formal-equivalence translations for any serious work — meaning the popular translation is functionally unusable for their purposes despite being the one most readers hold. They can exit to source-language study, but this exit is unavailable to the populations they aim to serve.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, text_critical_scholars, payer,
    moderate, biographical, mobile, global).

% Maintain liturgical practice built on the cadence, structural repetition, and specific phrasing of formal-equivalence or traditional-language texts. When dynamic-equivalence translations displace these in common use, the phrases embedded in shared memory, hymnody, and catechesis are altered or lost. They can resist adoption within their own institutions but face pressure from broader publishing markets and mission funding that favor the more accessible text.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, liturgical_traditionalist_communities, payer,
    organized, generational, constrained, national).

% Produce and sell dynamic-equivalence translations at scale; readability drives adoption, adoption drives revenue and market share, and market share funds further translation and distribution work. Benefit directly from the methodology's commercial and missionary success, and have institutional incentive to defend it against formal-equivalence competitors.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishers, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishers, agenda_setter).

% Seminary-trained clergy and teachers who build sermons and instruction on close grammatical reading. Find their preferred pedagogical text increasingly marginal in congregations that have adopted dynamic-equivalence versions, requiring them to either teach against the pew Bible or abandon close-reading pedagogy for the audience they actually have.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, morphology_dependent_exegetes, payer,
    moderate, biographical, mobile, global).

% Minority-language populations who receive Scripture in their own language for the first time under this methodology, where no prior translation existed. Gain access to religious text in comprehensible form; have no basis for comparison to source structure and no alternative translation to consult if the dynamic-equivalence choices misrepresent ambiguous passages.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, receptor_language_communities, beneficiary,
    powerless, generational, trapped, regional).

% Adjudicate translation philosophy disputes within denominational and interdenominational bodies, weighing pastoral reach against scholarly fidelity. Sit between the mission agencies and the scholarly critics, with authority to certify or withhold denominational endorsement of a given translation approach.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_committees, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, translation_committees, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates comprehension across a vast number of receptor-language communities by prioritizing natural idiom over source-structure mimicry, solving the real problem that word-for-word rendering across unrelated language families frequently produces unintelligible or misleading text.
% TRANSFER_FUNCTION: Moves interpretive authority from the reader (who would otherwise wrestle with ambiguous source structure) to the translation committee, which resolves ambiguity on the reader's behalf during translation; moves the philological data needed for close study away from lay and even much clerical use toward specialist-only source consultation.
% ABSENT_VOICES: Speakers of receptor languages who received a dynamic-equivalence translation as their first and only Scripture were never in the methodological conversation that decided how ambiguity would be resolved on their behalf; morphology-dependent exegetes in low-resource regions frequently have no access to formal-equivalence alternatives or original-language training to compensate.
% DISAPPEARANCE_RATIONALE: If dynamic-equivalence methodology vanished as an institutionally endorsed practice, missionary translation output would slow sharply in low-literacy and typologically distant receptor languages, existing dynamic-equivalence Bibles already in circulation would face pressure for revision or replacement, and evangelistic publishers built around readability-driven sales would lose their primary product line — the arrangement is load-bearing for how most living Christians outside historic literary languages actually encounter the text.
% FOUNDING_PROBLEM: Word-for-word translation into typologically distant languages (documented extensively by Eugene Nida's field linguistics work across Latin America, Africa, and Asia) frequently produced grammatically foreign, semantically opaque, or actively misleading renderings that failed at the basic task of conveying meaning to the receptor audience.
% FOUNDING_PROBLEM_CORROBORATION: Field linguists and missiologists outside the publishing arms of Bible societies (e.g. SIL-affiliated but academically independent linguistic researchers) corroborate that the intelligibility problem was real and remains live in newly-translated minority languages. Text-critical scholars and formal-equivalence advocates, from outside the beneficiary set, attest that for already-literate populations with existing formal translations, the founding problem is substantially solved and the continued preference for dynamic equivalence in those markets functions more as commercial and pastoral-convenience preference than as a live intelligibility crisis.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42 at interval end) because the reading trades a real cost — loss of morphological and structural precision — for a real gain — comprehensibility across typologically distant languages. This is not zero-sum theft; it is a genuine coordination function (intelligibility) purchased at a genuine cost (loss of source-structure data), which is why tangled_rope rather than snare is the structurally accurate claim. Suppression (0.38) reflects that formal-equivalence and interlinear alternatives remain available and are not banned, but institutional endorsement, funding, and market dominance of dynamic-equivalence output does constrain which text a given reader is likely to actually encounter. Theater ratio is modest and rising slowly (0.15 to 0.28) — some of the methodology's continued institutional defense in already-literate, already-served language markets increasingly serves publisher market share and denominational branding rather than the original intelligibility problem, which is itself resolved in those markets.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (missionary agencies, publishers) this is coordination: a real translation-theoretic solution to a real cross-linguistic communication problem, defended by field linguistics going back to Nida. From the payer seat (text-critical scholars, traditionalist liturgical communities) the same methodological dominance looks like an institutionally enforced narrowing of available textual options that degrades the source data available to serious study and severs inherited liturgical continuity. The engine should compute these as different seat-level types from the same structural facts — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Missionary translation agencies and evangelistic publishers sit near the beneficiary end: they set the methodology and capture funding, market share, and mission-fulfillment credit from its dominance. Lay readers and receptor-language communities are structural beneficiaries of the coordination function itself (comprehension) even though they bear an invisible cost (interpretive choices made on their behalf that they cannot audit). Text-critical scholars and morphology-dependent exegetes are the clearest targets: the same methodological choice that produces intelligibility for the mass audience actively destroys the data layer their discipline requires, and their mobility (they can consult source texts) does not help the populations they serve, who cannot. Liturgical traditionalist communities are targets in a different register — they lose inherited textual memory, not analytical capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unintelligibility of literal translation across typologically distant languages) remains genuinely live in newly-translated minority-language contexts, which is why founding_problem_status is authored as contested rather than dead: it is dead in some markets (fully literate populations with existing formal-equivalence Bibles, where dynamic equivalence now competes on convenience and market share rather than necessity) and very much alive in others (first-translation contexts). Treating this as uniformly resolved would falsely delegitimize genuine ongoing missionary linguistics; treating it as uniformly still-necessary would launder continued institutional dominance in markets where the original problem no longer applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_equivalence_reading_of_source_text_kernel,
    'Is ''the biblical source text'' one stable object that translation merely renders, or does the choice between dynamic-equivalence, formal-equivalence, and critical-reconstructive commitments constitute three structurally different objects of translation practice?',
    'This omega is not resolvable by further textual evidence; it names the committer structure itself. A sibling reading (formal_equivalence_reading) would hold source-structure fidelity as primary and intelligibility as the reader/community''s responsibility through teaching — this would relocate the extraction from scholars (who lose data) to lay readers (who lose comprehension without instruction), inverting the beneficiary/victim sets entirely. A second sibling (critical_reconstructive_reading) would treat the textual basis as unsettled prior to any translation-philosophy choice, bracketing this entire dispute as premature.',
    'Adopting the formal_equivalence_reading instead would make current beneficiaries (lay readers, missionary agencies) into victims of comprehension loss, and current victims (scholars) into beneficiaries of preserved structural data. The classification of ''who is extracted from'' is reading-relative, not a fact about translation in general.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dynamic_equivalence_reading_of_source_text_kernel, conceptual, 'This constraint is one reading of the biblical_source_text kernel; sibling readings redistribute beneficiaries and victims rather than merely re-scoring the same ones.').

omega_variable(
    intelligibility_versus_market_capture,
    'Where dynamic-equivalence translations now dominate markets with existing literate populations and prior formal-equivalence texts, is continued institutional preference for the methodology still solving a live intelligibility problem, or has it become a self-sustaining commercial and denominational-branding preference riding on the original theory''s legitimacy?',
    'Comparative readership studies measuring actual comprehension gaps between demographics using formal-equivalence versus dynamic-equivalence texts in already-literate markets, plus publisher revenue and licensing data disaggregated by market maturity.',
    'If the intelligibility gain in mature literate markets is small or absent, the extraction in those specific markets is closer to pure rent (commercial capture of a legitimate original innovation) than to genuine ongoing coordination, which would argue for a market-segmented classification rather than one uniform ε across all deployment contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligibility_versus_market_capture, empirical, 'Whether continued dominance in mature markets tracks a live coordination need or has decoupled into rent on the methodology''s original legitimacy.').

omega_variable(
    structural_data_loss_severity,
    'How much of the morphological and syntactic information lost in dynamic-equivalence rendering is recoverable by a motivated lay reader through supplementary tools (interlinear texts, study Bibles, commentaries) versus genuinely inaccessible without source-language competence?',
    'Systematic comparison of specific disputed passages across translation types, cross-referenced against what supplementary study material actually restores versus what remains lost even to a well-resourced lay reader.',
    'If most lost structural information is recoverable through widely available supplementary tools, the victim-side extraction for scholars is overstated relative to what a well-resourced reader can access, and the tangled_rope classification''s asymmetric-extraction leg would weaken; if largely unrecoverable outside source-language training, it strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_data_loss_severity, empirical, 'Whether structural fidelity loss is a genuinely closed door for lay and even most clerical readers or a recoverable gap given supplementary resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_source_text__dynamic_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the biblical_source_text kernel. formal_equivalence_reading inverts the beneficiary/victim structure (scholars and structural traditionalists become beneficiaries; lay comprehension becomes the bearer of cost). critical_reconstructive_reading brackets the entire fidelity-vs-intelligibility question as premature pending settlement of the underlying textual basis, and so has a different coordination function entirely (historical-critical reconstruction rather than either fidelity or intelligibility). Each reading carries its own ε, its own claimed_type, and its own stakeholder set per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
