% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   domain: religious/linguistic/institutional
 *
 * SUMMARY:
 *   This story instantiates the formal equivalence reading of the biblical
 *   source text kernel: the position that fidelity to source-language
 *   grammatical structure, word order, and lexical choice is the primary
 *   translation virtue, and that any resulting loss of immediate
 *   intelligibility is a burden properly discharged through teaching,
 *   catechesis, and clerical mediation rather than through altering the
 *   translation itself. This is a distinct constraint from the dynamic
 *   equivalence reading (which inverts the priority) and from the critical
 *   reconstructive reading (which defers both structure and meaning claims
 *   pending textual-critical resolution). Each reading has its own extraction
 *   profile: this reading's ε is elevated specifically by the comprehension
 *   burden it displaces onto lay readers and non-source-language communities,
 *   which the dynamic equivalence reading would report as near-zero and the
 *   critical reconstructive reading would treat as not-yet-answerable pending
 *   manuscript work.
 *
 * KEY AGENTS:
 *   - confessional_seminary_faculties: institutional agenda-setter — trains the interpretive mediators the reading requires
 *   - denominational_translation_committees: institutional agenda-setter/beneficiary — authorizes which structure-preserving editions carry denominational standing
 *   - credentialed_clergy: organized beneficiary — occupies the mediating role the reading's own logic makes necessary
 *   - lay_congregants_without_biblical_languages: powerless payer — bears the comprehension cost directly
 *   - new_converts: powerless, trapped payer — receives the heaviest immediate burden with least institutional support
 *   - non_english_dominant_language_communities: powerless, trapped, global-scope payer — bears amplified extraction because scope enlarges verification difficulty
 *   - dynamic_equivalence_advocates: excluded organized actor — structurally locked out of authorizing committees
 *   - biblical_scholars_comparative_linguistics: analytical observer — reports comprehension outcomes without institutional stake
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
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/linguistic/institutional").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'f37d831a-3214-4206-8f66-bbde7535e59a').
narrative_ontology:cs_kernel_codification('f37d831a-3214-4206-8f66-bbde7535e59a', fixed_text).
narrative_ontology:cs_authority_grounding('f37d831a-3214-4206-8f66-bbde7535e59a', lineage).
narrative_ontology:cs_interpretation_layer_present('f37d831a-3214-4206-8f66-bbde7535e59a').
narrative_ontology:cs_reading_relation('f37d831a-3214-4206-8f66-bbde7535e59a', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37d831a-3214-4206-8f66-bbde7535e59a', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('f37d831a-3214-4206-8f66-bbde7535e59a', foundational, source_structure_preservation_guards_against_translator_bias).
narrative_ontology:cs_axiom_status(source_structure_preservation_guards_against_translator_bias, holdable).
narrative_ontology:cs_axiom_grounding('f37d831a-3214-4206-8f66-bbde7535e59a', source_structure_preservation_guards_against_translator_bias, instrumental).
narrative_ontology:cs_axiom('f37d831a-3214-4206-8f66-bbde7535e59a', foundational, intelligibility_is_delegable_to_teaching_institutions).
narrative_ontology:cs_axiom_status(intelligibility_is_delegable_to_teaching_institutions, holdable).
narrative_ontology:cs_axiom_grounding('f37d831a-3214-4206-8f66-bbde7535e59a', intelligibility_is_delegable_to_teaching_institutions, conventional).
narrative_ontology:cs_reference_frame('f37d831a-3214-4206-8f66-bbde7535e59a', source_language_structural_priority).
narrative_ontology:cs_drift_state('f37d831a-3214-4206-8f66-bbde7535e59a', contemporary_multilingual_missions_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f37d831a-3214-4206-8f66-bbde7535e59a', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, confessional_seminary_faculties).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, denominational_translation_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, credentialed_clergy).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_congregants_without_biblical_languages).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, new_converts).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_english_dominant_language_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and certifies who is qualified to read the source languages and adjudicate translation disputes. Sets curriculum standards that treat formal equivalence as the responsible scholarly default, and reviews or endorses translation projects accordingly. Its institutional relevance depends on the ongoing need for trained mediators between source text and congregation.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, confessional_seminary_faculties, agenda_setter,
    institutional, generational, arbitrage, national).

% Produces and authorizes translations (e.g. word-for-word or structure-preserving editions) for denominational use, requiring extensive footnoting and teaching materials to bridge the resulting opacity. Controls which translation editions carry denominational imprimatur, which determines what congregations may use in worship.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, denominational_translation_committees, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, denominational_translation_committees, beneficiary).

% Occupies the necessary interpretive role between the structurally faithful but often syntactically foreign text and the congregation's need for meaning. Their professional standing and pulpit authority depend partly on the text remaining difficult enough that trained mediation is expected and valued.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, credentialed_clergy, beneficiary,
    organized, biographical, constrained, regional).

% Reads translations that preserve source-language word order, idiom, and syntax at the cost of immediate comprehensibility. Must rely on sermons, study notes, or clergy explanation to access meaning that a more idiomatic rendering would have delivered directly. Can switch to a different translation edition but risks being told by their own community that the alternative is 'less faithful' or theologically suspect.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, lay_congregants_without_biblical_languages, payer,
    powerless, biographical, constrained, local).

% Arrives with no prior training and is handed a text whose primary virtue, by this reading's own account, requires years of catechesis to unlock. Their comprehension gap is treated as a pedagogical opportunity for the institution rather than a defect of the text, which means the burden of the gap falls entirely on them.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, new_converts, payer,
    powerless, immediate, trapped, local).

% Receives formal-equivalence translations into languages whose grammar and idiom differ sharply from the source languages, producing renderings that can be substantially less intelligible than in the original target-language context this reading was formulated for. Has little influence over which translation philosophy governing bodies mandate for their language.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_english_dominant_language_communities, payer,
    powerless, generational, trapped, global).

% Argues that pastoral mission requires prioritizing intelligibility over structural mirroring, and that this reading's teaching-burden solution is a rationalization for maintaining clerical gatekeeping. Produces competing translations but is often excluded from denominational-imprimatur processes controlled by formal-equivalence-aligned committees.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_advocates, excluded,
    organized, generational, mobile, global).

% Studies translation outcomes across language families and reports on comprehension effects, without direct stake in denominational authority structures. Can document where formal equivalence produces genuine ambiguity versus where it produces recoverable-with-teaching difficulty.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, biblical_scholars_comparative_linguistics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared standard for what counts as a textually responsible translation, preserving source-language grammatical and lexical structure across editions so that scholarly cross-reference, doctrinal argument, and textual continuity with historical tradition remain stable and auditable.
% TRANSFER_FUNCTION: Moves interpretive labor and comprehension cost from the translation process (where it could be resolved once, professionally) onto individual readers and local teaching structures, who must repeatedly expend effort or depend on clergy to recover meaning the source-preserving rendering does not deliver directly. Correspondingly moves interpretive authority and institutional relevance toward those credentialed to perform that mediation.
% ABSENT_VOICES: Lay readers and new converts who find the resulting text opaque rarely sit on translation committees or seminary faculties; their comprehension difficulties are documented anecdotally (complaints, dropped Bible-study attendance) rather than through formal representation in translation-policy decisions. Dynamic equivalence advocates are present in the broader field but frequently excluded from committees that hold denominational authority to authorize liturgical use.
% DISAPPEARANCE_RATIONALE: Confessional seminaries and translation committees would argue the world rearranges catastrophically — textual fidelity, doctrinal precision, and continuity with two millennia of textual tradition would erode. Lay-comprehension advocates and dynamic-equivalence communities would argue the world barely changes for ordinary readers, who would simply read a more intelligible text with no loss of substantive doctrine, because most doctrinal precision arguments rest on structures recoverable in idiomatic translation. The dispute over which claim is true is itself the kernel contest.
% FOUNDING_PROBLEM: Early vernacular and academic translators sought a defensible method for rendering ancient Hebrew, Aramaic, and Greek texts without inserting translator interpretation where the source itself is ambiguous — preserving word order, syntax, and lexical choices was meant to give readers and scholars direct access to the source's own structure rather than a translator's paraphrase of it.
% FOUNDING_PROBLEM_CORROBORATION: Confessional seminary faculties and translation committees attest the problem remains fully live: translator-inserted interpretation is an ongoing risk in every dynamic-equivalence rendering, they argue, and only structural fidelity guards against it. Comparative-linguistics scholars, writing from outside the beneficiary institutions, corroborate that structural preservation does reduce one class of translator bias but note it introduces a different, comprehension-based cost that the tradition's own teaching-burden framing does not measure or report to congregations choosing an edition.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects a genuine and substantial comprehension cost transferred onto readers who have no biblical-language training, amplified for non-English-dominant communities by spatial scope (global) where cross-language structural mismatches routinely worsen rather than replicate the intelligibility gap the reading was framed to manage. Suppression (0.48) is moderate rather than high: exit to another translation exists, but is often socially costly within a given denominational community, and institutional imprimatur processes actively narrow which alternatives are treated as legitimate. Theater ratio is comparatively low (0.28) because the coordination function — genuine scholarly value in structural stability across editions — is real and substantial, not merely performative; the divergence between claim and outcome here is not that the coordination is fake but that its cost is asymmetrically distributed and its remedy (teaching) is underfunded relative to its stated sufficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Seminary faculties and translation committees sit closest to the beneficiary end: they set the standard, administer its transmission, and their institutional relevance is partly constituted by the ongoing need for trained mediation the standard produces. Credentialed clergy benefit similarly, occupying the necessary interpretive gap. Lay congregants, new converts, and non-English-dominant communities sit toward the target end: they bear the comprehension cost the reading declares to be their (or their community's) responsibility, with limited institutional voice in whether that responsibility is adequately resourced. Exit options differentiate the payer seats sharply — lay congregants in resourced denominations have constrained but real exit to alternative editions; new converts are effectively trapped by unfamiliarity; non-English-dominant communities are trapped by the scarcity of any alternative-philosophy translation into their language at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guarding against translator-inserted interpretive drift — remains partly live: unconstrained paraphrase does introduce translator judgment where the source is genuinely ambiguous, and that risk is not imaginary. What is contested is whether structural preservation is still the least-costly guard against that risk, or whether it has become a maintained institutional posture that also, not incidentally, sustains the professional and doctrinal-authority position of those who administer it. The disappearance_verdict is authored as contested rather than resolved for exactly this reason: this is not a case of an obsolete mandate propping up empty institutions (that would be a piton), nor a case of pure extraction with no genuine coordination problem (that would be a snare) — it is a live coordination function (textual stability for scholarship and cross-tradition doctrinal argument) bundled with a real, asymmetrically borne cost (lay comprehension), which is the structural signature of tangled rope rather than either pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    teaching_sufficiency_claim,
    'Is the claim that comprehension burden is adequately discharged through community teaching and catechesis empirically true across denominational contexts, or does it function primarily as a legitimating narrative for maintaining clerical mediation?',
    'Comparative studies of lay biblical literacy and comprehension outcomes across congregations with strong versus weak catechetical infrastructure, controlling for translation edition used.',
    'If teaching infrastructure is generally adequate, the tangled_rope''s extraction component is substantially mitigated in well-resourced communities, narrowing the victim set to under-resourced congregations specifically. If teaching infrastructure is generally inadequate, the ''subordinate responsibility of the reader/community'' framing functions as an unfunded mandate that shifts blame for comprehension failure onto readers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teaching_sufficiency_claim, empirical, 'Whether the reading''s teaching-remedy claim holds up empirically or functions as cover for an unaddressed comprehension gap.').

omega_variable(
    structural_fidelity_vs_institutional_interest,
    'Is source-structure preservation maintained because it is genuinely the best available guard against translator-inserted bias, or because it sustains the institutional and professional position of those credentialed to interpret it — and can these two motivations even be disentangled in practice?',
    'Track whether seminary faculties and translation committees have historically revised formal-equivalence commitments in response to comprehension research, or only in response to internal doctrinal disputes — a body responsive to comprehension evidence would suggest the coordination function is primary; a body unresponsive to it would suggest institutional interest dominates.',
    'If institutional interest dominates, the constraint drifts from tangled_rope toward snare (coordination becomes cover). If the scholarly guard function dominates and institutions do respond to comprehension evidence, tangled_rope is the durable classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_fidelity_vs_institutional_interest, conceptual, 'Whether the coordination rationale and the institutional beneficiary interest can be structurally separated.').

omega_variable(
    cross_language_amplification_scope,
    'Does formal equivalence produce comparably manageable difficulty across all target languages, or does it produce severe, qualitatively different opacity in languages structurally distant from the biblical source languages?',
    'Linguistic analysis comparing comprehension outcomes for formal-equivalence translations into languages closely related to Greek/Hebrew syntax versus typologically distant languages (e.g. isolating or polysynthetic languages).',
    'If opacity is qualitatively worse in distant languages, the extraction borne by non_english_dominant_language_communities is not merely quantitatively higher but represents a different kind of failure the reading''s own teaching-remedy framework was never designed to address, strengthening the case for decomposing this into a separate constraint story specific to cross-linguistic-family translation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_language_amplification_scope, empirical, 'Whether spatial/linguistic scope amplification is uniform or produces a structurally distinct failure mode requiring its own constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__formal_equivalence_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__formal_equivalence_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__formal_equivalence_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__formal_equivalence_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__formal_equivalence_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__formal_equivalence_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__formal_equivalence_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__formal_equivalence_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__formal_equivalence_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__formal_equivalence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__formal_equivalence_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__formal_equivalence_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__formal_equivalence_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__formal_equivalence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__formal_equivalence_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__formal_equivalence_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__formal_equivalence_reading, suppression_requirement, 50, 0.46).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__formal_equivalence_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial 'Bible translation philosophy' claim, per the ε-invariance principle: formal_equivalence_reading (this file, tangled_rope, elevated extraction on non-specialist readers), dynamic_equivalence_reading (a separate file, expected lower reader-facing extraction but its own extraction profile around interpretive discretion concentrated in translators), and critical_reconstructive_reading (a separate file, defers both structure and meaning claims pending textual-critical resolution, with its own distinct beneficiary set among textual critics). Each carries its own ε and classification; they are linked here as a constraint family, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
