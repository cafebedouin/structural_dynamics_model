% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Discontinuity Reading of the Correct-Latin Kernel: Classical/Medieval as Distinct Systems Requiring Reconstruction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the discontinuity reading of the correct-Latin
 *   kernel: the claim that Classical Latin and Medieval Latin constitute
 *   genuinely distinct linguistic systems, such that the Renaissance humanist
 *   project of textual reconstruction amounted to symbolic reoccupation of
 *   lost classical structure from surviving symbols, not mere internal
 *   correction of a continuously evolving language. On this reading, Medieval
 *   Latin forms are corruptions relative to a system that had, in an
 *   important sense, ceased and had to be recovered rather than merely
 *   tidied. This is a distinct constraint from the continuity reading (which
 *   treats Medieval Latin as Classical Latin after ordinary diachronic
 *   change, making 'reconstruction' internal correction with much lower
 *   extraction) and from the hybrid reading (which splits morphology,
 *   continuous, from syntax/lexicon, requiring recovery). The three readings
 *   are not measurement variants of one constraint — they license
 *   structurally different beneficiary/victim maps and different degrees of
 *   institutional violence toward medieval usage, so they are authored as
 *   three separate, ε-invariant stories linked through the network field.
 *
 * KEY AGENTS:
 *   - renaissance_humanist_philologists: Primary agenda-setter (institutional/arbitrage) — defines and enforces the discontinuity standard
 *   - classical_pedagogy_establishment: Primary beneficiary (institutional/arbitrage) — durable institutional base built on the standard
 *   - medieval_latin_scribal_tradition: Primary victim (powerless/trapped) — retrospectively judged with no voice
 *   - vernacular_continuity_scholars: Secondary victim (moderate/constrained) — career cost for dissenting from the reading
 *   - comparative_historical_linguists: Analytical observer (analytical/analytical) — evaluates the discontinuity claim against general linguistic-change theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Discontinuity Reading of the Correct-Latin Kernel: Classical/Medieval as Distinct Systems Requiring Reconstruction").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '7bc9579b-0657-4e45-b69c-6626baa9939b').
narrative_ontology:cs_kernel_codification('7bc9579b-0657-4e45-b69c-6626baa9939b', fixed_text).
narrative_ontology:cs_authority_grounding('7bc9579b-0657-4e45-b69c-6626baa9939b', lineage).
narrative_ontology:cs_interpretation_layer_present('7bc9579b-0657-4e45-b69c-6626baa9939b').
narrative_ontology:cs_reading_relation('7bc9579b-0657-4e45-b69c-6626baa9939b', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7bc9579b-0657-4e45-b69c-6626baa9939b', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('7bc9579b-0657-4e45-b69c-6626baa9939b', foundational, medieval_latin_constitutes_a_ruptured_system).
narrative_ontology:cs_axiom_status(medieval_latin_constitutes_a_ruptured_system, holdable).
narrative_ontology:cs_axiom_grounding('7bc9579b-0657-4e45-b69c-6626baa9939b', medieval_latin_constitutes_a_ruptured_system, empirically_contingent).
narrative_ontology:cs_axiom('7bc9579b-0657-4e45-b69c-6626baa9939b', secondary, classical_norm_is_the_sole_legitimate_recovery_target).
narrative_ontology:cs_axiom_status(classical_norm_is_the_sole_legitimate_recovery_target, holdable).
narrative_ontology:cs_axiom_grounding('7bc9579b-0657-4e45-b69c-6626baa9939b', classical_norm_is_the_sole_legitimate_recovery_target, conventional).
narrative_ontology:cs_reference_frame('7bc9579b-0657-4e45-b69c-6626baa9939b', ciceronian_classical_norm).
narrative_ontology:cs_drift_state('7bc9579b-0657-4e45-b69c-6626baa9939b', post_comparative_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7bc9579b-0657-4e45-b69c-6626baa9939b', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_pedagogy_establishment).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, critical_edition_publishers).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_scribal_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, vernacular_continuity_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_literature_readership).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_purity_standard).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, ad_fontes_recovery_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish the editorial and pedagogical apparatus that treats Ciceronian usage as the recoverable original and Medieval forms as accretions to be stripped away. They author critical editions, set university curricula, and adjudicate which manuscript readings count as corruption versus authentic transmission. Their scholarly authority and institutional standing are built directly on the discontinuity claim being true.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists, beneficiary).

% Teaches Latin as a fixed classical target restored from symbolic reconstruction, treating Medieval usage as a deviation to be corrected out of students. Textbook markets, examination standards, and academic prestige all track fidelity to the reconstructed classical norm, so the discontinuity frame is a durable revenue and status base for the field.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_pedagogy_establishment, beneficiary,
    institutional, generational, arbitrage, continental).

% Produce and sell editions whose entire value proposition is emending Medieval manuscript readings back toward a reconstructed classical original. Their commercial model depends on there being a 'correct' classical text to recover and a 'corrupted' medieval layer to strip away; a continuity reading would collapse much of the editorial apparatus they sell.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, critical_edition_publishers, beneficiary,
    organized, generational, mobile, continental).

% The scribes, notaries, and clerics who actually used Latin as a living working language for a millennium have no seat in the reconstruction debate — they are dead, and their usage is treated retrospectively as error against a standard they never claimed to follow. Their linguistic labor is recast as decline rather than evolution, with no possibility of objection or revision from their side.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_scribal_tradition, payer,
    powerless, civilizational, trapped, continental).

% Scholars arguing Medieval Latin represents continuous internal development (rather than rupture and symbolic reoccupation) face structural disadvantage in a field whose editorial standards, hiring committees, and journal gatekeeping were built around the discontinuity premise. Publishing against the dominant frame costs career capital; full exit from the field abandons their expertise entirely.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_continuity_scholars, payer,
    moderate, biographical, constrained, continental).

% Students and general readers encountering Medieval Latin texts inherit an apparatus that frames those texts' own grammar as deficient rather than as evidence of a functioning, distinct linguistic system. This shapes what gets taught, translated, and anthologized, narrowing access to Medieval Latin on its own terms.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_literature_readership, payer,
    powerless, generational, constrained, continental).

% Apply general theories of language change and text transmission to evaluate whether the Classical/Medieval split reflects genuine system discontinuity or a philological construction imposed after the fact. They can adjudicate the kernel dispute empirically but sit outside the institutional incentives on either side of it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, comparative_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, teachable target for restoring and transmitting a coherent body of ancient texts: without SOME reconstructive standard, manuscript variation across thirteen centuries of copying would leave no basis for a shared edited corpus at all.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional prestige from the diffuse, undocumented practice of medieval Latin users toward the humanist scholarly apparatus that certifies which forms count as authentic classical Latin and which count as corruption to be edited out.
% ABSENT_VOICES: The medieval scribes, notaries, and clerics whose actual usage is being retrospectively judged have no voice in the matter — they produced the texts under contest but left no metalinguistic defense of their grammar as a legitimate system rather than a decayed one.
% DISAPPEARANCE_RATIONALE: If the discontinuity premise were abandoned, critical editions built on 'restoring' classical readings against medieval 'corruption' would need substantial re-editing on continuity grounds, pedagogical curricula treating medieval forms as errors would need revision, and Medieval Latin would likely be taught and read as a first-class system in its own right rather than a degraded classical remnant.
% FOUNDING_PROBLEM: Renaissance humanists faced a genuinely fragmented and heterogeneous manuscript record spanning centuries of copying, and needed a method to recover something resembling the language of Cicero and Virgil rather than accept every scribal variant as equally authoritative.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists working outside classical philology (comparative Romance linguistics, sociolinguistics of diglossia) attest that the sharp discontinuity framing overstates the case relative to what is known about gradual language change generally; the humanist tradition's own successors within classical philology continue to attest the founding problem as live and the reconstruction as recovery rather than construction.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.58 — substantial but not maximal — because the discontinuity reading does perform genuine coordination work (it gave humanism a workable, teachable standard against a genuinely heterogeneous manuscript record) while also asymmetrically devaluing an entire millennium of actual Latin usage as mere corruption, which is where the extraction lands. Suppression starts high (0.70) reflecting the forceful early humanist campaign against 'barbarous' medieval Latin in favor of Ciceronian purity, and eases modestly over the interval (to 0.62) as the discontinuity frame becomes institutionalized common sense requiring less active polemic to sustain — enforcement shifts from combat to curriculum. Theater ratio rises from 0.20 to 0.42 as the original textual-recovery function becomes increasingly overlaid with performative classicism (prize Latin composition, purism contests) whose relationship to actual manuscript problems weakens over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and the pedagogy establishment sit near the beneficiary end: they built and administer the standard, and their institutional and commercial position depends on the discontinuity claim holding. Medieval scribes are maximally targeted — trapped by death and by having no metalinguistic voice, their usage judged by a standard invented after the fact for purposes they never shared. Vernacular continuity scholars sit closer to the target end than the beneficiaries despite comparable formal power, because the field's gatekeeping apparatus was built around the discontinuity premise; dissent is costly within the same institutions that would need to reward it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, variant-riddled manuscripts needing a workable editorial standard — was real and, in a narrow sense, remains live (editors still must choose among variant readings). But treating that narrow problem as license for a sweeping discontinuity claim about the language's actual historical status is where mandatrophy risk enters: the editorial-practicality problem is dead or much reduced by modern stemmatics, while the discontinuity doctrine persists as an inherited framing that still shapes how medieval texts are taught and valued. Classifying this as tangled_rope rather than snare preserves the genuine coordination kernel (a recovery standard was needed) while still registering the asymmetric cost imposed on medieval usage and its readers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_vs_continuity_empirical_status,
    'Does the historical-linguistic evidence (attested manuscript corpora, comparative Romance evidence, sociolinguistic models of diglossia) actually support a genuine system discontinuity between Classical and Medieval Latin, or does it support gradual continuous change that the humanist reconstruction project reframed as rupture for its own institutional purposes?',
    'Systematic corpus-based comparison of morphosyntactic change rates across the Classical-to-Medieval transition against established typologies of gradual diachronic change in other well-documented language histories; assessment of whether the rate and character of change exceeds what unbroken transmission typically produces.',
    'If continuity is empirically supported, the discontinuity reading''s central premise is a constructed frame serving humanist institutional interests rather than a linguistic fact, sharpening this story''s tangled_rope classification toward snare. If genuine discontinuity is supported, the coordination function is more clearly a real response to real linguistic rupture rather than sheer institutional advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_vs_continuity_empirical_status, empirical, 'Whether the discontinuity premise itself is empirically warranted or an institutionally convenient construction.').

omega_variable(
    committer_framing_location,
    'Where exactly does the discontinuity/continuity/hybrid dispute live — in the empirical facts of language change, in the definitional threshold for what counts as ''a distinct system,'' or in the institutional interests of the parties adjudicating the question?',
    'Explicit separation of (a) the empirical rate-of-change data, (b) the theoretical threshold different linguists use to call a change ''systemic'' versus ''gradual,'' and (c) the institutional stakes each reading carries for its proponents; track whether experts converge on (a) while diverging on (b) and (c).',
    'If disagreement is located mainly in (b) the definitional threshold rather than in (a) the underlying facts, this is a conceptual dispute about system-individuation criteria rather than a resolvable empirical question — which would mean no future evidence dissolves the kernel contest, only clarifies what each reading is claiming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_location, conceptual, 'Committer-frame note: locating the discontinuity/continuity/hybrid disagreement among fact, definition, and institutional interest.').

omega_variable(
    reconstruction_value_neutrality,
    'Is symbolic reoccupation of lost classical structure from surviving texts a value-neutral scholarly recovery method, or does the very framing of ''reoccupation'' (versus ''internal correction'') already presuppose the discontinuity thesis it purports to investigate?',
    'Philosophy-of-science style analysis of whether editorial methodology can be practiced independently of a prior commitment to continuity or discontinuity, or whether the choice of method (stemmatic emendation toward a classical target vs. descriptive cataloguing of medieval variants) already encodes the answer.',
    'If methodology is not neutral, then editions produced under the discontinuity reading cannot serve as independent evidence for the discontinuity reading — much of the apparent empirical support for this reading would be circular, weakening confidence in the claimed_type relative to the continuity reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_value_neutrality, conceptual, 'Whether editorial reconstruction methodology presupposes the very discontinuity thesis it is used to support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__discontinuity_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__discontinuity_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__discontinuity_reading, theater_ratio, 300, 0.34).
narrative_ontology:measurement(corr_tr_t400, correct_latin_kernel__discontinuity_reading, theater_ratio, 400, 0.37).
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__discontinuity_reading, theater_ratio, 500, 0.4).
narrative_ontology:measurement(corr_tr_t600, correct_latin_kernel__discontinuity_reading, theater_ratio, 600, 0.42).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__discontinuity_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__discontinuity_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__discontinuity_reading, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(corr_be_t400, correct_latin_kernel__discontinuity_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__discontinuity_reading, base_extractiveness, 500, 0.57).
narrative_ontology:measurement(corr_be_t600, correct_latin_kernel__discontinuity_reading, base_extractiveness, 600, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__discontinuity_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__discontinuity_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__discontinuity_reading, suppression_requirement, 300, 0.63).
narrative_ontology:measurement(corr_su_t400, correct_latin_kernel__discontinuity_reading, suppression_requirement, 400, 0.62).
narrative_ontology:measurement(corr_su_t500, correct_latin_kernel__discontinuity_reading, suppression_requirement, 500, 0.62).
narrative_ontology:measurement(corr_su_t600, correct_latin_kernel__discontinuity_reading, suppression_requirement, 600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'correct/authentic Latin reconstruction' per the ε-invariance principle. continuity_reading authors Medieval Latin as Classical Latin after ordinary evolution (low ε, reconstruction as internal correction). discontinuity_reading (this story) authors a genuine system break requiring symbolic reoccupation (moderate-high ε, medieval usage recast as corruption). hybrid_reading authors a layered split: morphology continuous, syntax/lexicon requiring recovery (intermediate ε). Each carries its own beneficiary/victim structure and classification; they are linked here rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
