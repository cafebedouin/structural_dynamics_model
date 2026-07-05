% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Classical Text as Sole Authority
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the DISCONTINUITY reading of the 'correct Latin'
 *   kernel: the claim that authentic Latin is the Classical form preserved in
 *   ancient texts, that medieval Latin constitutes corrupt deviation, and
 *   that correction requires external reconstruction from textual sources
 *   rather than deference to any continuous living practice. This is a
 *   distinct constraint from the continuity_reading (which holds medieval
 *   Latin as legitimately evolved Classical Latin transmitted through
 *   unbroken practice) and the hybrid_reading (which allows partial
 *   continuity with targeted textual correction). Each reading has its own
 *   beneficiary/victim structure and its own epsilon; they are linked, not
 *   merged, in the network graph. Rising extraction and theater over the
 *   interval track the entrenchment of humanist philological authority from
 *   the 14th through 19th centuries as critical editing and prestige pedagogy
 *   institutionalized the Classical/medieval rupture as settled fact rather
 *   than a contestable framing choice.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda-setters who define correctness by Classical manuscript reconstruction
 *   - humanist_textual_editors: beneficiaries whose editorial output depends on the corruption narrative
 *   - prestige_latin_pedagogy_institutions: beneficiaries whose gatekeeping function requires an exacting external standard
 *   - medieval_latin_scribal_tradition: primary victims, relabeled as corrupt rather than recognized as evolving practice
 *   - vernacular_derived_latin_speakers: victims whose natural competence is defined as deficient
 *   - monastic_and_ecclesiastical_scholars: victims whose accumulated training is devalued
 *   - modern_classicists: analytical observers who can examine the construction of the boundary itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Classical Text as Sole Authority").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, 'd2119cdd-7ca5-450b-8713-0f00d1101a39').
narrative_ontology:cs_kernel_codification('d2119cdd-7ca5-450b-8713-0f00d1101a39', fixed_text).
narrative_ontology:cs_authority_grounding('d2119cdd-7ca5-450b-8713-0f00d1101a39', expertise).
narrative_ontology:cs_interpretation_layer_present('d2119cdd-7ca5-450b-8713-0f00d1101a39').
narrative_ontology:cs_reading_relation('d2119cdd-7ca5-450b-8713-0f00d1101a39', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d2119cdd-7ca5-450b-8713-0f00d1101a39', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('d2119cdd-7ca5-450b-8713-0f00d1101a39', foundational, classical_text_as_sole_linguistic_authority).
narrative_ontology:cs_axiom_status(classical_text_as_sole_linguistic_authority, holdable).
narrative_ontology:cs_axiom_grounding('d2119cdd-7ca5-450b-8713-0f00d1101a39', classical_text_as_sole_linguistic_authority, conventional).
narrative_ontology:cs_axiom('d2119cdd-7ca5-450b-8713-0f00d1101a39', foundational, medieval_usage_constitutes_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_usage_constitutes_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('d2119cdd-7ca5-450b-8713-0f00d1101a39', medieval_usage_constitutes_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('d2119cdd-7ca5-450b-8713-0f00d1101a39', classical_manuscript_authority).
narrative_ontology:cs_drift_state('d2119cdd-7ca5-450b-8713-0f00d1101a39', post_comparative_philology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2119cdd-7ca5-450b-8713-0f00d1101a39', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_textual_editors).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, prestige_latin_pedagogy_institutions).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scribal_tradition).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, vernacular_derived_latin_speakers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, monastic_and_ecclesiastical_scholars).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_purity_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_reconstruction_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define correctness by collating and editing surviving Classical-era manuscripts, treating deviations from reconstructed Classical norms as errors to be corrected rather than data about a living language. Their professional standing and editorial authority derive directly from the claim that Classical forms are the sole legitimate standard and that everything after constitutes decline requiring their expert intervention to reverse.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Produce critical editions purging medieval orthography, vocabulary, and syntax in favor of reconstructed Classical forms. Their scholarly output and market for humanist pedagogy depend on the discontinuity frame; if medieval Latin were recognized as legitimate evolved Latin, the market for 'purified' editions and Ciceronian style manuals would shrink substantially.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_textual_editors, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, humanist_textual_editors, agenda_setter).

% Teach Latin composition and reading using Classical grammars and lexicons as the exclusive standard, certifying students' command of 'correct' Latin against that yardstick. Institutional prestige (and tuition/patronage revenue) is built on gatekeeping access to a difficult, textually-reconstructed register that most working medieval Latin users never needed to meet.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, prestige_latin_pedagogy_institutions, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Centuries of monastic and chancery scribes who used Latin as a functioning administrative, liturgical, and scholarly language, developing it continuously. Under the discontinuity reading their entire corpus is relabeled 'corrupt' or 'barbarous' rather than legitimate linguistic evolution, erasing the authority of an unbroken practice tradition they cannot defend because they are historically absent from the debate that judges them.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_scribal_tradition, payer,
    powerless, civilizational, trapped, continental).

% Communities whose spoken and written Latin absorbed local vernacular influence over generations. The discontinuity reading treats their usage not as dialectal variation within a living language but as deviation from an external textual standard they never had direct access to, making their natural linguistic competence structurally inadequate by definition.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, vernacular_derived_latin_speakers, payer,
    powerless, civilizational, trapped, regional).

% Maintained Latin learning through copying, commentary, and liturgy across the medieval centuries using inherited medieval conventions. Once humanist discontinuity philology becomes prestigious, their training and textual conventions are recast as needing correction against rediscovered Classical sources, devaluing generations of accumulated scholarly practice and requiring them to relearn a reconstructed register to remain credible.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, monastic_and_ecclesiastical_scholars, payer,
    moderate, generational, constrained, continental).

% Study the historical dispute itself, tracing how humanist scholars constructed the Classical/medieval boundary and what interests that construction served. They can examine manuscript evidence, patronage records, and pedagogical materials from outside the commitments of either faction.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, modern_classicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, diffuse).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, textually-anchored standard against which Latin usage can be evaluated, enabling humanist scholars and pedagogues to coordinate around a single reconstructed register for teaching, editing, and scholarly communication rather than tolerating unbounded regional and period variation.
% TRANSFER_FUNCTION: Moves interpretive and pedagogical authority from the accumulated practice-based competence of medieval scribal, monastic, and ecclesiastical Latin users to humanist philologists and textual editors who control access to and interpretation of ancient manuscript sources; also moves prestige and patronage from medieval institutions to Renaissance humanist ones.
% ABSENT_VOICES: The medieval scribes, monastic copyists, and vernacular-influenced Latin speakers whose usage is being judged are dead or otherwise absent from the humanist debate that reclassifies their language as corruption; they left no comparable school of advocates to contest the discontinuity framing in the terms it was argued.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading were abandoned, humanist critical editions purged of 'medievalisms' would lose their claim to represent the only legitimate Latin; pedagogy built on Ciceronian purity would need to accommodate medieval Latin as a legitimate historical register in its own right; the market and prestige structure built around textual reconstruction and correction would need to reorganize around continuity-based or hybrid philological methods.
% FOUNDING_PROBLEM: Renaissance humanists faced a corpus of medieval Latin texts that differed markedly from surviving Classical-era sources and sought a principled way to recover what they took to be a purer, more authoritative ancient Latin obscured by centuries of scribal and vernacular influence.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists and their pedagogical descendants attest the founding problem remains live: that genuine Classical Latin exists as a recoverable object distinct from and superior to medieval accretion. Historical linguists and medievalists working outside the humanist tradition, including modern comparative philologists studying language change, corroborate instead that no such rupture is empirically detectable in the continuous documentary record — medieval Latin shows regular, rule-governed evolution rather than corruption, undermining the discontinuity premise from outside the beneficiary group.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62) because the discontinuity reading does real coordination work (a single textual standard is genuinely useful for editing and cross-regional communication) while also transferring prestige and interpretive authority away from an entire tradition of practitioners who have no voice in the judgment against them. Suppression (0.58) reflects the active work required to maintain the rupture claim against the continuous documentary evidence of gradual, rule-governed language change — the discontinuity reading must actively police its boundary rather than let it stand as self-evident. Theater ratio (0.44) is substantial because much of the corrective apparatus (systematic 'purification' of medieval orthography and syntax in editions) is now more performative signaling of scholarly rigor than functionally necessary for comprehension. Accessibility collapse (0.68) is high because once the discontinuity framing is accepted, alternative readings of the same textual evidence become difficult to see as legitimate. Resistance (0.5) reflects ongoing but incomplete pushback from historical linguists who document continuous change rather than rupture.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and humanist editors sit near the full-beneficiary end: they set the interpretive terms, control access to the reconstructive method, and derive professional standing directly from the corruption narrative. Prestige pedagogy institutions benefit similarly by gatekeeping a demanding, textually-defined register. Medieval scribal tradition, vernacular-derived speakers, and monastic scholars sit near the full-target end: they are trapped (the tradition is historical and cannot exit or self-represent) or constrained (living scholars must retrain against a standard imposed after the fact), and the constraint's operation directly devalues their accumulated linguistic and scholarly capital without their participation in the judgment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — humanists confronting a large corpus of Latin that seemed to diverge from ancient sources — was a live scholarly question in the 14th-16th centuries. Whether it remains live today is exactly what is contested: humanist-descended pedagogy treats the Classical/medieval rupture as settled fact requiring ongoing correction, while modern historical linguistics outside that tradition treats it as a constructed boundary that does not correspond to any detectable discontinuity in the documentary record. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a live capture-flag candidate: an arrangement whose original justification is empirically undermined but whose institutional structure (editions, pedagogy, prestige) still depends on maintaining the original framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuum_empirical_status,
    'Does the documentary record actually show a discontinuity between Classical and medieval Latin usage, or does it show continuous, rule-governed language change with no principled break point?',
    'Corpus-linguistic analysis of dated Latin texts across the transition period (3rd-9th centuries CE) tracing morphological, syntactic, and orthographic change rates against the change rates documented in undisputed cases of continuous language evolution in other traditions.',
    'If the record shows continuous gradual change with no discontinuity, the discontinuity reading''s founding premise is empirically unsupported and the constraint would need to be understood primarily as a constructed prestige mechanism rather than a description of linguistic fact. If a genuine sharp break is found, the discontinuity reading gains empirical grounding independent of the humanist interest in it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuum_empirical_status, empirical, 'Whether Classical/medieval Latin shows a genuine linguistic rupture or continuous evolution.').

omega_variable(
    committer_framing_choice,
    'Given that the kernel (what counts as correct Latin) supports at least three coherent readings (discontinuity, continuity, hybrid), what determines which reading a given scholarly community adopts, and is that determination itself interest-driven?',
    'Historical analysis of humanist patronage networks, printing economics, and pedagogical market structure during the reading''s period of ascendance (14th-19th centuries), compared against the same analysis for communities that instead adopted continuity or hybrid framings.',
    'If reading choice tracks patronage and market incentives for the choosing community rather than independent evidentiary weight, that corroborates the tangled_rope classification (real coordination function riding alongside interest-driven asymmetric extraction) over a pure rope reading in which the discontinuity claim was adopted purely on evidentiary merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Whether adoption of the discontinuity reading over its siblings tracks scholarly interest rather than evidence.').

omega_variable(
    natural_law_vs_constructed_standard,
    'Is ''correct Latin'' in the discontinuity sense a description of an objective linguistic fact (the ancient texts simply ARE the language, full stop) or a constructed evaluative standard serving identifiable modern beneficiaries?',
    'Distinguish between the mountain-like claim (Classical texts preserve what ancient speakers actually wrote — largely uncontested as a textual-historical fact) and the tangled-rope-like claim (therefore all non-Classical usage is illegitimate and requires correction — a normative leap with identifiable beneficiaries). Track whether removing the beneficiary structure changes the plausibility of the correctness claim.',
    'If the correctness claim is separable from the beneficiary structure, the underlying textual-preservation fact is closer to a mountain (uncontested, no rents) while only the normative overlay (illegitimacy of medieval forms) is the tangled-rope extraction. This story addresses the combined claim as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_standard, conceptual, 'Whether the underlying textual fact and the normative correctness claim built on it should be treated as separable constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(corr_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(corr_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(corr_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.44).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.44).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corr_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(corr_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(corr_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(corr_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.62).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(corr_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(corr_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(corr_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(corr_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the correct_latin kernel. correct_latin__continuity_reading holds medieval Latin as legitimately evolved Classical Latin with no rupture; correct_latin__hybrid_reading holds partial continuity with targeted textual correction. Each reading has a distinct epsilon and distinct beneficiary/victim structure reflecting a genuinely different structural claim about the same underlying kernel (what counts as correct Latin), not a measurement-basis variant of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
