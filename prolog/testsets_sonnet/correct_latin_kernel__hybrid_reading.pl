% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Humanist Latin Correctness Standard — Hybrid (Layered Reconstruction) Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the HYBRID reading of the contested 'correct
 *   Latin' kernel: it holds that medieval morphology represents genuine,
 *   unbroken linguistic continuity from Classical Latin (no correction
 *   needed, no corruption to speak of), while medieval syntax and lexicon
 *   represent a real divergence requiring textual recovery from the classical
 *   corpus. Reconstruction under this reading is therefore layered and
 *   partial — a 'partial reoccupation' that ratifies inherited inflectional
 *   systems while importing classical sentence structure and vocabulary. This
 *   is a structurally distinct claim from the continuity reading (which
 *   treats the whole system, syntax included, as organic evolution needing no
 *   external correction) and the discontinuity reading (which treats even
 *   morphology as requiring symbolic reoccupation because Classical and
 *   Medieval Latin are different systems). Each reading has a different
 *   beneficiary/victim structure and a different ε — they are separate
 *   constraints, linked here only by shared kernel membership.
 *
 * KEY AGENTS:
 *   - humanist_grammarians: agenda-setters who draw and enforce the morphology/syntax line
 *   - printing_press_editors: beneficiaries who operationalize the hybrid standard at scale
 *   - curial_chancery_officials: institutional beneficiaries who adopt the standard for prestige and interoperability
 *   - vernacular_notaries and provincial_clergy_latinists: payers whose morphology passes but whose syntax/lexicon is devalued
 *   - medieval_scholastic_writers: excluded, retroactively judged parties with no voice
 *   - textual_philologists: analytical observers documenting the actual pattern of continuity and divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.52).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Humanist Latin Correctness Standard — Hybrid (Layered Reconstruction) Reading").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '36cc0c81-aac4-425a-b49e-30013c14a7c3').
narrative_ontology:cs_kernel_codification('36cc0c81-aac4-425a-b49e-30013c14a7c3', fixed_text).
narrative_ontology:cs_authority_grounding('36cc0c81-aac4-425a-b49e-30013c14a7c3', lineage).
narrative_ontology:cs_interpretation_layer_present('36cc0c81-aac4-425a-b49e-30013c14a7c3').
narrative_ontology:cs_reading_relation('36cc0c81-aac4-425a-b49e-30013c14a7c3', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('36cc0c81-aac4-425a-b49e-30013c14a7c3', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('36cc0c81-aac4-425a-b49e-30013c14a7c3', foundational, morphological_continuity_requires_no_recovery).
narrative_ontology:cs_axiom_status(morphological_continuity_requires_no_recovery, holdable).
narrative_ontology:cs_axiom_grounding('36cc0c81-aac4-425a-b49e-30013c14a7c3', morphological_continuity_requires_no_recovery, empirically_contingent).
narrative_ontology:cs_axiom('36cc0c81-aac4-425a-b49e-30013c14a7c3', foundational, syntactic_lexical_divergence_constitutes_corruption).
narrative_ontology:cs_axiom_status(syntactic_lexical_divergence_constitutes_corruption, holdable).
narrative_ontology:cs_axiom_grounding('36cc0c81-aac4-425a-b49e-30013c14a7c3', syntactic_lexical_divergence_constitutes_corruption, conventional).
narrative_ontology:cs_reference_frame('36cc0c81-aac4-425a-b49e-30013c14a7c3', classical_ciceronian_corpus_baseline).
narrative_ontology:cs_drift_state('36cc0c81-aac4-425a-b49e-30013c14a7c3', high_medieval_scholastic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36cc0c81-aac4-425a-b49e-30013c14a7c3', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, humanist_grammarians).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, printing_press_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, curial_chancery_officials).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, vernacular_notaries).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, provincial_clergy_latinists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_scholastic_writers).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, classical_syntactic_norm).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, philological_recovery_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile grammars and commentaries that codify which forms count as correct Latin. They accept continuous medieval morphology (declensions, conjugation stems) as legitimate inheritance but flag medieval syntax (word order, subordination patterns) and lexicon (coined or repurposed vocabulary) as corruptions requiring recovery from classical texts. Their authority and careers depend on being the arbiters of this layered judgment; they move fluidly between courts, universities, and print houses.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, humanist_grammarians, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Produce editions that silently normalize medieval syntax and vocabulary toward classical models while leaving inherited morphology untouched, since morphological continuity requires no correction and printing efficiency rewards leaving it alone. They profit from selling 'purified' texts as authoritative and can relocate operations across print centers if local demand shifts.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, printing_press_editors, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, printing_press_editors, agenda_setter).

% Draft official documents in the newly standardized hybrid Latin, gaining prestige and interoperability across courts that recognize the humanist standard. They benefit from the layered correction because it lets them retain familiar inflectional patterns while adopting classical syntax markers that signal elite competence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, curial_chancery_officials, beneficiary,
    institutional, generational, constrained, continental).

% Draft contracts and local records in the Latin they learned through apprenticeship, which preserves medieval syntax and specialized medieval vocabulary for legal and commercial concepts with no classical equivalent. Under the hybrid standard their morphology passes but their sentence construction and terminology are marked as barbarous, devaluing their documents and training relative to humanist-schooled competitors.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_notaries, payer,
    moderate, biographical, constrained, regional).

% Preach and correspond in a Latin whose morphology is unimpeachable but whose syntax and technical theological vocabulary developed independently through centuries of scholastic use. They cannot easily retrain in classical syntax without access to humanist schooling concentrated in wealthy urban centers, so their competence is structurally downgraded by the same standard that validates their case endings.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, provincial_clergy_latinists, payer,
    powerless, biographical, trapped, regional).

% Already dead or professionally eclipsed by the time the hybrid standard consolidates; their syntactic innovations (relative clause structures, technical philosophical compounds) are precisely the material targeted for replacement even though it solved real expressive problems classical Latin lacked vocabulary for. They have no voice in the standard-setting process that reclassifies their work as corrupted.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_scholastic_writers, excluded,
    powerless, generational, trapped, continental).

% Study manuscript transmission to determine which medieval forms are organic continuations and which are innovations, informing (but not controlling) where the humanist line between 'legitimate inheritance' and 'corruption' gets drawn. They document the layered character of the standard without holding power to enforce or resist it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_philologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared elite written register across fragmented post-Carolingian polities by re-anchoring syntax and vocabulary to a stable classical corpus, while not requiring speakers to unlearn morphology that had never actually broken continuity — reducing the total relearning burden relative to a full discontinuity correction.
% TRANSFER_FUNCTION: Moves prestige, chancery employment, and textual authority from morphology-only competent regional Latinists (notaries, provincial clergy) toward humanist-trained writers who can additionally reproduce classical syntax and lexicon, concentrating gatekeeping power in humanist academies and print centers.
% ABSENT_VOICES: Medieval scholastic writers whose syntactic and lexical innovations solved genuine expressive problems (technical philosophical and legal vocabulary) are not consulted in the standard-setting process; their solutions are reclassified as corruption rather than evaluated on functional merit. Provincial clergy and vernacular notaries object informally but have no institutional forum to contest the hybrid line's placement.
% DISAPPEARANCE_RATIONALE: If the hybrid correctness standard vanished, chancery and print prestige hierarchies built on syntactic/lexical purism would collapse, provincial and scholastic Latin registers would regain parity with humanist Latin, and the humanist grammarian profession — whose authority rests on being the arbiter of the morphology/syntax split — would lose its distinguishing function.
% FOUNDING_PROBLEM: Post-classical Latin texts showed genuine morphological continuity but had diverged syntactically and lexically to the point that cross-regional intelligibility and access to the classical corpus were breaking down; some correction mechanism was needed to restore a shared elite register.
% FOUNDING_PROBLEM_CORROBORATION: Humanist grammarians and printing editors (the constraint's own beneficiaries) attest the founding problem is live and the hybrid correction remains necessary. Independent textual philologists, working from manuscript transmission evidence outside the humanist tradition's self-account, corroborate that morphological continuity was real but dispute that syntactic and lexical divergence constituted 'corruption' rather than functional adaptation — their evidence supports the diagnosis of a founding problem but not the value-laden framing of its solution.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and suppression (0.58) sit at moderate levels, lower than a pure discontinuity-style symbolic reoccupation project would generate, because the hybrid reading concedes genuine continuity in one linguistic domain (morphology) even as it imposes correction in others (syntax, lexicon) — the coordination function is real and partially self-limiting. Theater ratio (0.44) is elevated because a meaningful share of humanist correctness policing is performative signaling of classical competence rather than functionally necessary for intelligibility, particularly once print standardization had already achieved cross-regional legibility. Accessibility collapse (0.62) reflects that once the hybrid standard was established, alternative registers (scholastic Latin, notarial Latin) became genuinely hard to use for prestige purposes even though they remained functional. Resistance (0.55) captures the real, if diffuse, pushback from provincial and vernacular practitioners who never fully ceded the field.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist grammarian and printing editor seats, the standard reads as principled philological recovery restoring access to a shared classical inheritance. From the vernacular notary and provincial clergy seats, the same structure reads as an arbitrary, elite-controlled line that happens to validate exactly the competence they already have (morphology) while devaluing exactly the competence that distinguished their practical, locally-adapted Latin (syntax, technical vocabulary). The engine's per-seat computation should reflect this asymmetry without either seat's framing being treated as simply correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist grammarians and printing editors sit near the beneficiary end: they set the terms of the morphology/syntax split and capture the prestige and market value of being its arbiters and producers. Curial officials benefit as adopters who gain interoperability capital. Vernacular notaries and provincial clergy sit near the target end: their morphological competence is validated (a partial subsidy) but their syntactic and lexical competence is devalued (extraction), producing a mixed but net-negative directionality — exactly the layered structure the reading claims. Medieval scholastic writers, being temporally and institutionally excluded from the standard-setting process, are treated as full targets with no recourse; their exit option is 'trapped' because the judgment is retroactive and they cannot contest it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-regional intelligibility, access to classical corpus) was substantially real at the outset but had been largely resolved through print standardization and elite education networks well before the hybrid standard's disciplinary apparatus (theater_ratio climbing from 0.20 to 0.44) reached its mature form. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the continuity-preserving morphological judgment as pure extraction (it genuinely reflects a real linguistic fact and reduces relearning burden), and treating the syntax/lexicon correction as pure natural coordination (it demonstrably transfers prestige and employment toward humanist-trained elites at provincial and scholastic writers' expense). The hybrid reading's own internal logic — 'some forms legitimate, others corrupt' — is precisely what makes it tangled rather than pure in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_syntax_boundary_placement,
    'Is the line the hybrid reading draws between ''continuous morphology'' and ''corrupted syntax/lexicon'' a linguistically principled boundary, or a boundary placed where it happens to validate humanist competence while devaluing competing registers?',
    'Comparative corpus analysis of manuscript transmission across the morphology/syntax/lexicon dimensions, checking whether the degree of actual divergence from Classical Latin tracks the humanist correctness judgments or instead tracks which competence humanist-trained elites happened to possess.',
    'If the boundary tracks actual linguistic divergence, the hybrid reading''s partial-reoccupation claim is well-founded and closer to descriptive philology. If the boundary tracks elite competence rather than divergence, the ''hybrid'' framing is itself a constructed justification for a snare-like extraction dressed as principled recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_syntax_boundary_placement, empirical, 'Whether the morphology/syntax correctness boundary is linguistically principled or elite-competence-shaped.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence or institutional position led this story to adopt the hybrid reading rather than the continuity or discontinuity reading of the correct_latin_kernel, and would a different evidentiary emphasis (e.g., weighting scholastic vocabulary''s functional adequacy more heavily) shift the classification toward continuity_reading''s lower-extraction profile?',
    'Cross-reading comparison: hold ε and structural data for continuity_reading and discontinuity_reading alongside this story''s hybrid ε, and check whether independent philologists'' corpus judgments cluster around the hybrid boundary or scatter toward one of the other two readings.',
    'If independent judgments cluster elsewhere, the hybrid reading''s specific claim about the layered nature of the reconstruction is less robust than presented, and its intermediate tangled_rope classification may not be the most defensible single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Under-determination of which kernel reading best fits the manuscript evidence; alternative framings would shift ε and type.').

omega_variable(
    scholastic_lexicon_functional_value,
    'Did medieval scholastic and legal Latin''s syntactic and lexical innovations represent functional adaptations solving real expressive problems (as textual_philologists suggest) or genuine degradations of classical clarity (as humanist grammarians claimed)?',
    'Functional linguistic analysis of specific scholastic constructions (e.g., technical philosophical compounds, notarial legal formulae) against the expressive gaps they filled in Classical Latin vocabulary.',
    'If functional, the ''corruption'' framing for syntax/lexicon is itself contestable, weakening the hybrid reading''s asymmetric treatment of morphology versus syntax/lexicon and suggesting the true structure is closer to continuity_reading across all three domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholastic_lexicon_functional_value, empirical, 'Whether scholastic syntactic/lexical divergence was functional adaptation or degradation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__hybrid_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__hybrid_reading, theater_ratio, 100, 0.33).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__hybrid_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__hybrid_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement(corr_tr_t250, correct_latin_kernel__hybrid_reading, theater_ratio, 250, 0.43).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__hybrid_reading, theater_ratio, 300, 0.44).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__hybrid_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__hybrid_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__hybrid_reading, base_extractiveness, 150, 0.49).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__hybrid_reading, base_extractiveness, 200, 0.51).
narrative_ontology:measurement(corr_be_t250, correct_latin_kernel__hybrid_reading, base_extractiveness, 250, 0.52).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__hybrid_reading, base_extractiveness, 300, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t50, correct_latin_kernel__hybrid_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__hybrid_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__hybrid_reading, suppression_requirement, 150, 0.53).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__hybrid_reading, suppression_requirement, 200, 0.56).
narrative_ontology:measurement(corr_su_t250, correct_latin_kernel__hybrid_reading, suppression_requirement, 250, 0.57).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__hybrid_reading, suppression_requirement, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'correct/restored Latin.' continuity_reading treats the whole system as organically evolved and requiring only internal correction (lowest ε, closest to rope). discontinuity_reading treats Classical and Medieval Latin as fully distinct systems requiring total symbolic reoccupation from texts, including morphology (highest ε, closest to snare or tangled_rope with a wider victim set including notaries whose morphology would also be judged corrupt). This hybrid_reading occupies the middle: morphology is conceded as continuous (no correction, no extraction there) while syntax and lexicon are treated as requiring recovery (moderate extraction). All three share the same underlying kernel — the legitimacy question of what counts as correct Latin relative to the classical baseline — but instantiate different beneficiary/victim structures and different ε values, and must be read as a family, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
