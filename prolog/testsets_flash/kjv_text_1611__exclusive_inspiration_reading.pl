% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine
 *   domain: religious_studies/theology/textual_criticism
 *
 * SUMMARY:
 *   This constraint represents the 'exclusive inspiration' reading of the KJV
 *   text, where the 1611 King James Version is held to be the only truly
 *   inspired and inerrant English Bible. All other translations are deemed
 *   corrupted or inferior. This doctrine functions as a snare, actively
 *   suppressing alternative translations and their proponents, while
 *   concentrating textual authority and associated benefits within a specific
 *   leadership and publishing ecosystem. The high extractiveness and
 *   suppression reflect the active enforcement required to maintain this
 *   position against linguistic and textual evidence.
 *
 * KEY AGENTS:
 *   - kjv_only_leadership: Agenda setter (institutional/identity_locked) — enforces the doctrine, gains authority.
 *   - kjv_publishers: Beneficiary (organized/mobile) — profits from exclusive market.
 *   - modern_bible_readers: Payer (powerless/identity_locked) — bears cost of archaic language, intellectual isolation.
 *   - modern_bible_scholars: Payer (moderate/constrained) — dismissed, opposed, marginalized.
 *   - other_translation_publishers: Payer (organized/constrained) — excluded from market, reputational damage.
 *   - critical_theologians: Observer (analytical/analytical) — analyzes the phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.85).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.9).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious_studies/theology/textual_criticism").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'ab52efc7-5af1-4320-95fa-f5774b3f90d0').
narrative_ontology:cs_kernel_codification('ab52efc7-5af1-4320-95fa-f5774b3f90d0', fixed_text).
narrative_ontology:cs_authority_grounding('ab52efc7-5af1-4320-95fa-f5774b3f90d0', lineage).
narrative_ontology:cs_interpretation_layer_present('ab52efc7-5af1-4320-95fa-f5774b3f90d0').
narrative_ontology:cs_reading_relation('ab52efc7-5af1-4320-95fa-f5774b3f90d0', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('ab52efc7-5af1-4320-95fa-f5774b3f90d0', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('ab52efc7-5af1-4320-95fa-f5774b3f90d0', foundational, kjv_exclusively_inspired).
narrative_ontology:cs_axiom_status(kjv_exclusively_inspired, holdable).
narrative_ontology:cs_axiom_grounding('ab52efc7-5af1-4320-95fa-f5774b3f90d0', kjv_exclusively_inspired, theological).
narrative_ontology:cs_axiom('ab52efc7-5af1-4320-95fa-f5774b3f90d0', foundational, modern_translations_corrupted).
narrative_ontology:cs_axiom_status(modern_translations_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('ab52efc7-5af1-4320-95fa-f5774b3f90d0', modern_translations_corrupted, theological).
narrative_ontology:cs_reference_frame('ab52efc7-5af1-4320-95fa-f5774b3f90d0', kjv_divine_preservation_framework).
narrative_ontology:cs_drift_state('ab52efc7-5af1-4320-95fa-f5774b3f90d0', contemporary_textual_criticism_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ab52efc7-5af1-4320-95fa-f5774b3f90d0', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_bible_readers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_bible_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, other_translation_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes and enforces the doctrine of KJV's exclusive inspiration, positioning themselves as the sole authoritative interpreters of 'true' scripture. They gain authority, influence, and often financial support from adherents who rely on their guidance.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the exclusive market for KJV Bibles and related study materials within KJV-Only communities. Their profits are directly tied to the doctrine's persistence.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_publishers, beneficiary,
    organized, biographical, mobile, global).

% Are pressured to abandon more accessible modern translations for the KJV, often struggling with its archaic language. They bear the cost of reduced comprehension and intellectual isolation from broader Christian scholarship.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_bible_readers, payer,
    powerless, biographical, identity_locked, local).

% Are dismissed or actively opposed by KJV-Only adherents, their work on textual criticism and translation deemed 'corrupt'. They face professional marginalization within these communities and must defend their academic integrity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_bible_scholars, payer,
    moderate, biographical, constrained, global).

% Are excluded from KJV-Only markets and their products are actively denounced as inferior or heretical. They lose market share and face reputational damage within these specific religious segments.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, other_translation_publishers, payer,
    organized, biographical, constrained, global).

% Analyze the KJV-Only movement as a sociological and theological phenomenon, examining its historical roots, textual claims, and impact on religious communities. They are outside the direct influence of the constraint but observe its effects.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, critical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate a unified, authoritative understanding of scripture within a specific religious community, preventing perceived doctrinal drift or confusion from multiple translations.
% TRANSFER_FUNCTION: Transfers textual authority and interpretive power from a broad scholarly consensus and individual discernment to a specific leadership group, along with financial gains to KJV publishers.
% ABSENT_VOICES: Linguistic experts, textual critics, and scholars of ancient languages who would point out the KJV's known translation errors and reliance on a less complete manuscript base are actively suppressed and excluded from the conversation within KJV-Only circles.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, KJV-Only communities would face a profound crisis of authority and identity. Many adherents would likely adopt modern translations, leading to a significant shift in religious practice, publishing markets, and the authority structures of these groups.
% FOUNDING_PROBLEM: The perceived problem was the proliferation of 'corrupt' or 'inferior' modern Bible translations, leading to doctrinal confusion and a weakening of traditional faith, necessitating a single, unassailable English standard.
% FOUNDING_PROBLEM_CORROBORATION: The KJV-Only leadership consistently attests that the problem of 'corrupt' modern translations is live and growing. However, independent textual scholars and mainstream theological institutions outside the benefiting parties universally corroborate that the 'problem' is a mischaracterization of legitimate textual scholarship and translation methodology, and that the KJV-Only position is based on historical and linguistic inaccuracies.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because adherents are compelled to use a specific, often less comprehensible, text, and to reject alternatives, incurring costs in understanding and intellectual freedom. Suppression is very high (0.90) due to active denunciation, social pressure, and exclusion of those who use or promote other translations. The theater ratio is low (0.10) because the enforcement is genuinely aimed at maintaining the doctrine's claims, not merely performing a function that has atrophied. The increasing extractiveness and suppression over time reflect the hardening of this position in response to the proliferation of modern translations and textual scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of KJV-Only leadership, this is a necessary defense of divine truth (claimed as a mountain or rope). From the perspective of modern Bible readers and scholars, it is a highly extractive and suppressive snare that limits access to understanding and intellectual inquiry. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership and publishers are clear beneficiaries, gaining authority and market share. Modern Bible readers, scholars, and other translation publishers are targets, bearing the costs of linguistic difficulty, intellectual marginalization, and market exclusion. The 'identity_locked' exit option for adherents reflects the deep social and spiritual ties that make leaving the KJV-Only framework extremely difficult, amplifying their effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving a single, pure English Bible) is presented as live by its beneficiaries. However, external corroboration indicates the 'problem' it solves is largely a manufactured one, or at least one whose solution (the KJV) has become a source of new problems (linguistic barriers, suppression of scholarship). The persistence of the constraint, despite its high extraction and suppression, suggests it functions as a snare, where the coordination story (preserving truth) is cover for the extraction of authority and resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_grounding,
    'Is the KJV''s authority grounded in its historical transmission and linguistic accuracy, or in a unique, post-original-language inspiration event?',
    'Comparative textual analysis of the KJV against original language manuscripts and other historical translations; theological examination of the doctrine of ''double inspiration'' (inspiration of the translation itself).',
    'If grounded in historical transmission, the KJV''s known textual limitations would undermine its exclusive claim, reclassifying the constraint as a snare. If a unique inspiration event is credibly established, it would support the mountain claim, but this is highly contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_grounding, empirical, 'The epistemic basis of the KJV''s claimed authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (exclusion from communities, market bans) or internalized (adherents'' belief in corruption of other texts)?',
    'Post-exit survey of former KJV-Only adherents: if suppression of other translations persists in their personal practice after leaving the community, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''kjv_text_1611'' kernel, what would be the classification of the ''revisable_translation_reading'' and ''functional_equivalence_reading'' siblings?',
    'Generate full constraint stories for each sibling reading, applying the same metric and stakeholder analysis.',
    'The ''revisable_translation_reading'' would likely classify as a Rope (coordination around ongoing scholarship), and the ''functional_equivalence_reading'' as a Rope or Mountain (coordination around diverse utility or inherent textual properties). This would highlight the extreme divergence in classification stemming from different interpretations of the same textual kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Classification divergence across sibling readings of the KJV text kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1900, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(kjv__tr_t1930, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1930, 0.07).
narrative_ontology:measurement(kjv__tr_t1960, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement(kjv__tr_t1990, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1900, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(kjv__be_t1930, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1930, 0.7).
narrative_ontology:measurement(kjv__be_t1960, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1960, 0.78).
narrative_ontology:measurement(kjv__be_t1990, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1900, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(kjv__su_t1930, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(kjv__su_t1960, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(kjv__su_t1990, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(kjv__su_t2024, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kjv_text_1611' kernel. Sibling readings include 'revisable_translation_reading' and 'functional_equivalence_reading', which would likely yield different classifications due to their differing claims about textual authority and translation validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
