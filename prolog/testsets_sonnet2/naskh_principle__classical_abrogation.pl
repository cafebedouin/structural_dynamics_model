% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh: Chronological Abrogation of Earlier Quranic Verses
 *   domain: religious/legal/theological
 *
 * SUMMARY:
 *   This story authors the classical abrogation (naskh) reading of the
 *   Quranic supersession kernel: where two verses address the same legal or
 *   theological topic and appear to conflict, the verse revealed
 *   chronologically later cancels the legal force of the earlier one. This
 *   reading produces a formal hierarchy — a corpus of 'abrogating' and
 *   'abrogated' verses — administered through occasion-of-revelation
 *   chronology and codified in classical fiqh manuals. It is one of three
 *   competing readings of the same underlying kernel (the status of
 *   apparently conflicting Quranic legal verses); the sibling readings
 *   (contextual harmonization, progressive restriction) are separate
 *   constraint stories with their own ε and structural data, per the
 *   ε-invariance principle. This story's ε describes the classical-abrogation
 *   arrangement as it actually operates — the settled hierarchy of rulings,
 *   its institutional custodians, and who bears the cost of contested
 *   classification — not the harmonized or pedagogical alternative it
 *   displaces.
 *
 * KEY AGENTS:
 *   - classical_fiqh_schools: agenda_setter (institutional/arbitrage) — administers the chronology apparatus and abrogation lists
 *   - state_aligned_jurists: beneficiary (powerful/constrained) — relies on the hierarchy for binding, final rulings
 *   - hadith_chronology_scholars: beneficiary (organized/constrained) — professional specialization depends on chronology mattering
 *   - harmonization_oriented_exegetes: payer (moderate/constrained) — marginalized interpretive alternative
 *   - lay_readers_of_apparent_contradictions: payer (powerless/trapped) — receives the ruling without access to the underlying debate
 *   - reform_minded_jurists: payer (moderate/constrained) — bears the burden of proof to overturn settled supersession claims
 *   - comparative_religious_scholars: observer (analytical) — studies the device across traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.58).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.62).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh: Chronological Abrogation of Earlier Quranic Verses").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/theological").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f').
narrative_ontology:cs_kernel_codification('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', formalized).
narrative_ontology:cs_authority_grounding('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', lineage).
narrative_ontology:cs_interpretation_layer_present('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f').
narrative_ontology:cs_reading_relation('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', foundational, chronologically_later_revelation_legally_supersedes_earlier).
narrative_ontology:cs_axiom_status(chronologically_later_revelation_legally_supersedes_earlier, holdable).
narrative_ontology:cs_axiom_grounding('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', chronologically_later_revelation_legally_supersedes_earlier, conventional).
narrative_ontology:cs_axiom('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', foundational, single_determinate_ruling_required_per_legal_topic).
narrative_ontology:cs_axiom_status(single_determinate_ruling_required_per_legal_topic, holdable).
narrative_ontology:cs_axiom_grounding('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', single_determinate_ruling_required_per_legal_topic, instrumental).
narrative_ontology:cs_reference_frame('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', classical_naskh_hierarchy_consolidation).
narrative_ontology:cs_drift_state('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', contemporary_reformist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('87af3bb8-d98e-4b6b-ad2d-d8ed5a6c3b2f', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_fiqh_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_aligned_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, hadith_chronology_scholars).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, harmonization_oriented_exegetes).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_readers_of_apparent_contradictions).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reform_minded_jurists).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_priority_of_later_revelation).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, legal_certainty_requires_single_ruling_per_topic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the abrogation-chronology apparatus (asbab al-nuzul chains, naskh manuals, tafsir hierarchies) that determines which verse rules and which is superseded. Sets the criteria for what counts as valid abrogation evidence and trains the jurists who apply it. Its authority as final arbiter of contested rulings depends on the abrogation framework existing and being treated as settled method.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_fiqh_schools, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_fiqh_schools, beneficiary).

% Issues binding legal rulings (fatwas, court judgments) that rely on a clean supersession hierarchy to produce a single applicable rule rather than a contested plurality. Benefits from the predictability and finality naskh provides for governance, inheritance, and criminal law. Exit from the framework would destabilize rulings already issued under it.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, state_aligned_jurists, beneficiary,
    powerful, generational, constrained, national).

% Their scholarly specialization — establishing revelation order via occasion-of-revelation reports — only has authority and demand because the abrogation model requires chronological sequencing to adjudicate conflicts. Their institutional standing and career investment ride on chronology mattering.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, hadith_chronology_scholars, beneficiary,
    organized, generational, constrained, global).

% Argues that apparently conflicting verses can be read together as context-specific rather than one nullifying the other. Their interpretive method is treated as marginal or heterodox wherever classical abrogation lists are institutionally entrenched; publishing or teaching a harmonization reading against a settled naskh ruling risks accusations of unorthodoxy.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, harmonization_oriented_exegetes, payer,
    moderate, biographical, constrained, national).

% Encounters verses that appear to contradict and is told which one is 'abrogated' and no longer legally operative, without access to the underlying chronological evidence debates. Must accept the ruling as delivered by local religious authority; has no independent means to verify revelation-order claims or contest the resulting practical rule (e.g., on inheritance shares, alcohol, warfare conduct).
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_readers_of_apparent_contradictions, payer,
    powerless, biographical, trapped, local).

% Seeks to revive earlier, more permissive or more contextually bounded rulings that classical abrogation lists have declared legally void. Bears the burden of overturning a centuries-old supersession consensus rather than simply proposing a reading; the abrogation apparatus places the evidentiary and social cost of change on them.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reform_minded_jurists, payer,
    moderate, generational, constrained, regional).

% Studies naskh as a hermeneutical device across traditions, comparing it to rabbinic and patristic supersession doctrines, without a stake in which ruling is currently binding.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, comparative_religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_fiqh_schools).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable procedure for resolving apparent textual conflicts in a legal corpus that governs marriage, inheritance, criminal punishment, and ritual practice — without a fixed hierarchy, courts and muftis would need to adjudicate contradictions case by case with no settled method.
% TRANSFER_FUNCTION: Moves interpretive authority and legal finality from the plain multiplicity of the revealed text toward the institutions that administer chronology and supersession — jurists, chronology scholars, and the schools that certify their rulings — at the cost of readings that would keep both verses operative.
% ABSENT_VOICES: Harmonization-oriented exegetes and reform-minded jurists routinely raise the alternative that apparent conflicts reflect context rather than nullification, but where classical naskh lists are institutionally settled, their arguments are treated as reopening closed questions rather than as live scholarship; lay readers who experience the tension directly are almost never consulted on which resolution is chosen.
% DISAPPEARANCE_RATIONALE: If the chronological-abrogation apparatus vanished, existing fatwas and school rulings built on 'verse X abrogates verse Y' would lose their stated warrant; jurists would need to re-derive rulings from harmonization or restriction-based readings, and areas of law with long-contested abrogation claims (e.g., sword verse vs. tolerance verses, wine verses, inheritance verses) would reopen for relitigation.
% FOUNDING_PROBLEM: Early Muslim jurists faced apparently contradictory verses revealed at different times addressing the same legal topic (e.g., changing rules on alcohol, warfare, qibla direction) and needed a method to produce one operative ruling for courts and daily practice rather than leaving multiple conflicting commands simultaneously binding.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh schools and state-aligned jurists attest the problem remains live: legal systems still require single determinate rulings. Independent scholars outside the beneficiary set — including modern reformist theologians and some comparative-religion academics — argue the practical need for a single ruling could be met by harmonization or contextual specification instead, and that the chronological-supersession solution was one contingent method among others adopted for reasons of juristic convenience and early state consolidation, not textual necessity.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high: the classical abrogation apparatus does solve a genuine coordination problem (courts need one operative rule, not two competing commands), but it also concentrates interpretive authority in jurists and chronology specialists whose institutional standing depends on the framework's continued operation, and it forecloses live alternative readings that would keep both verses valid. Suppression (0.62) reflects that the hierarchy is maintained less by persuasive force than by institutional consensus-enforcement: teaching a rejected harmonization reading in settled contexts carries real professional and social cost. Theater ratio (0.28) is present but not dominant — a meaningful share of the scholarly apparatus (chronology verification, occasion-of-revelation study) is genuine textual-historical work, though a growing share over time defends settled rulings rather than investigating them, which the rising trajectory captures. Accessibility collapse (0.68) is high because once a ruling is classified as abrogating/abrogated within a school's canon, the alternative reading becomes practically unavailable to ordinary adherents and even to jurists trained within that school. Resistance (0.55) reflects active, organized pushback from harmonization exegetes and reformists, distinguishing this from a mountain (which would meet almost none).
 *
 * DIRECTIONALITY LOGIC:
 *   Classical fiqh schools and hadith chronology scholars sit near the beneficiary end: the framework is the source of their institutional authority and professional specialization, and their exit options are effectively arbitrage-grade (they can move between schools of thought while remaining within the naskh paradigm). State-aligned jurists benefit from legal finality but are constrained by prior rulings issued under the framework. Harmonization exegetes, reform-minded jurists, and lay readers sit toward the target end: they bear the cost of a settled hierarchy that forecloses readings they find more textually coherent, with lay readers the most trapped (no independent means of verification, complete dependence on local religious authority for which ruling applies).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — courts and daily practice needing one determinate rule where two verses seem to conflict — was genuinely live in the early centuries of Islamic legal consolidation. Whether it remains live today (founding_problem_status: contested) turns on whether contextual harmonization or progressive-restriction readings could serve the same coordination function without the supersession hierarchy's exclusionary cost. Classifying this as tangled_rope rather than pure snare or pure rope reflects that both a real coordination function AND asymmetric extraction (in favor of the institutions that administer the chronology apparatus) are present simultaneously — collapsing either component would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_kernel_reading_selection,
    'Is classical chronological abrogation the structurally correct reading of the Quranic-conflict kernel, or do contextual_harmonization / progressive_restriction better describe what the text and its early reception actually license?',
    'Comparative textual-historical analysis of early tafsir traditions before classical naskh lists were codified; examination of which reading the earliest generations of exegetes actually held versus which was consolidated later for juristic administrability.',
    'If harmonization or progressive-restriction readings are historically prior and only displaced by classical abrogation for administrative convenience, the classical reading''s claim to represent the ''original'' resolution weakens, though its coordination function for legal certainty remains real regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naskh_kernel_reading_selection, conceptual, 'Whether classical abrogation is the historically primary reading of the kernel or a later institutional consolidation among competing readings.').

omega_variable(
    chronology_evidentiary_reliability,
    'How reliable is the occasion-of-revelation (asbab al-nuzul) chronology evidence that determines which verse is ''later'' and thus abrogating?',
    'Isnad-critical and historical-critical review of the hadith chains underlying disputed chronology claims, cross-referenced against independent early manuscript and codicological evidence.',
    'If chronology attributions for contested abrogation pairs are themselves weakly evidenced or retrojected, the entire supersession hierarchy for those pairs rests on contestable historical claims rather than settled fact, strengthening the case for the harmonization sibling reading in those specific instances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronology_evidentiary_reliability, empirical, 'Whether the chronological ordering claims that drive abrogation determinations are themselves well-evidenced.').

omega_variable(
    abrogation_beneficiary_capture_extent,
    'To what extent does the institutional benefit accruing to fiqh schools and chronology scholars from maintaining the abrogation framework bias their scholarly assessment of which readings count as valid evidence?',
    'Comparative study of scholars'' abrogation-list conclusions correlated with their institutional affiliation and career stakes, versus independent or cross-tradition scholars without such stakes.',
    'High correlation would support treating classical abrogation''s persistence partly as institutional self-interest (tangled_rope/false-summit dynamic); low correlation would support the framework resting primarily on textual-historical grounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abrogation_beneficiary_capture_extent, empirical, 'Whether beneficiary institutions'' scholarly conclusions are shaped by their stake in the framework''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__classical_abrogation, theater_ratio, 20, 0.14).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__classical_abrogation, theater_ratio, 40, 0.18).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__classical_abrogation, theater_ratio, 60, 0.21).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__classical_abrogation, theater_ratio, 80, 0.25).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__classical_abrogation, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nask_be_t20, naskh_principle__classical_abrogation, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(nask_be_t40, naskh_principle__classical_abrogation, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(nask_be_t60, naskh_principle__classical_abrogation, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(nask_be_t80, naskh_principle__classical_abrogation, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(nask_be_t100, naskh_principle__classical_abrogation, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nask_su_t20, naskh_principle__classical_abrogation, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(nask_su_t40, naskh_principle__classical_abrogation, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(nask_su_t60, naskh_principle__classical_abrogation, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(nask_su_t80, naskh_principle__classical_abrogation, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(nask_su_t100, naskh_principle__classical_abrogation, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the naskh_principle kernel: classical_abrogation (this story — fixed legal hierarchy, chronological supersession), contextual_harmonization (all verses remain valid within context, no supersession), and progressive_restriction (sequence reflects pedagogical tightening, not invalidation). Each reading has independently authored ε, beneficiaries, and victims per the ε-invariance principle; they are linked here rather than merged because measuring 'naskh' by classical-legal-hierarchy criteria versus contextual-validity criteria versus pedagogical-sequence criteria yields structurally different extraction profiles and different victim sets. The classical reading's beneficiaries (schools administering the hierarchy) are largely disjoint from who would benefit under the harmonization reading (exegetes preserving textual coherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
