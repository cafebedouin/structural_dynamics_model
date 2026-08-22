% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Classical-Norm Correctness Regime (Discontinuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   From the early humanists onward, European learned culture operated a
 *   correctness regime in which the classical corpus — Cicero, Caesar,
 *   Sallust, Virgil, Terence — was the sole arbiter of proper Latin. Medieval
 *   usage was classified as barbarous corruption rather than legitimate
 *   development, and repair proceeded by symbolic reoccupation: since the
 *   speech community that produced classical Latin was gone, correct form
 *   could only be recovered from the surviving textual symbols, then
 *   reimposed on writing, teaching, and editing. The regime performed real
 *   coordination (a unified learned medium, reliably established ancient
 *   texts) while extracting heavily from the medieval tradition it displaced:
 *   manuscript readings were emended away, scholastic idiom was stigmatized,
 *   and editorial authority over all Latin concentrated in the classicist
 *   profession. This story instantiates ONE reading of the
 *   correct_latin_kernel — the discontinuity_reading, which is also the
 *   regime's own self-understanding. Per the epsilon-invariance principle,
 *   the colloquial label 'correct Latin' decomposes into three structurally
 *   distinct constraints (this file plus the continuity and hybrid sibling
 *   stories), each with its own epsilon, beneficiary/victim structure, and
 *   classification, linked through the network. KEY AGENTS (by structural
 *   relationship): - classical_philologists: Agenda-setting administrator and
 *   collector (institutional/identity_locked) — runs the standard, sets
 *   curricula, adjudicates correctness - humanist_republic_of_letters:
 *   Primary beneficiary (powerful/constrained) — collects prestige and
 *   communicative payoff without running enforcement - elite_schoolmasters:
 *   Secondary administrator (organized/constrained) — drills the norm into
 *   each generation - monastic_scriptoria: Primary target (powerless/trapped)
 *   — holdings corrected without a seat in the conversation -
 *   scholastic_latin_writers: Primary target (powerless/trapped) — idiom
 *   judged posthumously, cannot answer - vernacular_scholars: Excluded voice
 *   (moderate/constrained) — would contest the regime's monopoly but stands
 *   outside it - modern_historical_linguists: Analytical observer
 *   (institutional/analytical) — assesses both systems from outside the
 *   regime
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.7).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.65).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical-Norm Correctness Regime (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '33c62d48-59b9-4578-bf4c-b6ccd48a0f18').
narrative_ontology:cs_kernel_codification('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', fixed_text).
narrative_ontology:cs_authority_grounding('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', lineage).
narrative_ontology:cs_interpretation_layer_present('33c62d48-59b9-4578-bf4c-b6ccd48a0f18').
narrative_ontology:cs_reading_relation('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', foundational, medieval_forms_are_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', medieval_forms_are_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_axiom('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', foundational, correctness_restored_only_through_texts).
narrative_ontology:cs_axiom_status(correctness_restored_only_through_texts, holdable).
narrative_ontology:cs_axiom_grounding('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', correctness_restored_only_through_texts, instrumental).
narrative_ontology:cs_reference_frame('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', golden_age_classical_usage).
narrative_ontology:cs_drift_state('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', post_comparative_philology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33c62d48-59b9-4578-bf4c-b6ccd48a0f18', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, humanist_republic_of_letters).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, monastic_scriptoria).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, scholastic_latin_writers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, elite_schoolmasters).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_corpus_supremacy_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, symbolic_reoccupation_method).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, linguistic_discontinuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit the ancient texts, write the grammars and dictionaries, set school and university syllabi, and adjudicate which forms count as correct Latin. Their chairs, editions, and professional standing depend on the classical corpus remaining the sole arbiter; stepping outside the standard would dissolve the expertise they embody.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, classical_philologists, beneficiary).

% Correspondents, poets, chancery officials, and churchmen who gain a shared elevated register and considerable prestige from classical mastery. They collect the communicative and status payoff of the standard without running its enforcement machinery; abandoning it would cost them the cultural capital that distinguishes them.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, humanist_republic_of_letters, beneficiary,
    powerful, biographical, constrained, continental).

% Teachers in gymnasia, Jesuit colleges, and university arts faculties who drill the classical forms into each generation of students. Administering the discipline is their livelihood and pedagogical identity; they profit from the standard's scarcity value while enforcing it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, elite_schoolmasters, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, elite_schoolmasters, beneficiary).

% Keepers of the medieval transmission — cathedral and monastery libraries and the copying houses around them. Their non-classical spellings, inflections, and idioms are systematically rewritten by editors working toward the ancient norm; they hold no seat in the correctness conversation, and their holdings are corrected regardless of their own judgment.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, monastic_scriptoria, payer,
    powerless, generational, trapped, continental).

% Theologians, jurists, and natural philosophers who wrote in the medieval learned register. Their works are edited against their own usage, their idiom is catalogued as barbarous, and they are barred from serving as models of good style; the judgment falls on them posthumously and they cannot answer it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, scholastic_latin_writers, payer,
    powerless, generational, trapped, continental).

% Thinkers writing in Italian, French, German, English, and other national languages who argue that learned authority need not route through classical Latinity, and later that the medieval traditions carry independent worth. They stand outside the correctness conversation, which is conducted entirely in and about the ancient norm.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_scholars, excluded,
    moderate, biographical, constrained, national).

% Students of Romance philology and medieval Latin who treat both periods as historical realities with their own systematic grammar. They assess the corrective regime from outside it, documenting what the emendation program preserved, what it altered, and what it destroyed.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, modern_historical_linguists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single pan-European written standard of learned Latin after spoken Latin had fragmented regionally — enabling mutually intelligible scholarly, legal, liturgical, and diplomatic communication, and giving editors a fixed yardstick for establishing the text of ancient authors.
% TRANSFER_FUNCTION: Moves textual and stylistic authority from the medieval transmission tradition (copying houses, scholastic authors) to the classical-philological establishment; moves the labor of generations of students into acquiring an artificial classical norm; moves editorial control over all Latin texts, ancient and medieval alike, to classicists.
% ABSENT_VOICES: The medieval parties being judged are structurally absent: the scholastic authors are dead and cannot defend their idiom, and the copying-house custodians were never consulted about the correction of their holdings. Vernacular scholars stand outside the conversation entirely. Even the ancient authors, whose usage is invoked as the yardstick, cannot confirm which attested variant they would endorse.
% DISAPPEARANCE_RATIONALE: If the classical-correctness regime vanished overnight, European learned writing would fragment back along regional medieval norms, editorial practice would lose its arbiter for establishing ancient texts, the gymnasium and college curricula built on classical mastery would collapse, and the professional authority of the philological establishment would evaporate — the whole architecture of learned exchange reorganizes around it.
% FOUNDING_PROBLEM: After the collapse of Roman educational infrastructure, the learned register drifted far from the ancient literary norm; by the late Middle Ages regional varieties of Latin threatened mutual unintelligibility among the educated, and the ancient texts themselves had become hard to read through accumulated alteration. The regime was built to restore a unified learned medium and recover direct access to ancient literature.
% FOUNDING_PROBLEM_CORROBORATION: That the divergence problem was real is corroborated from outside the benefiting parties: late-medieval chanceries and church administration complained about unintelligible regional Latinity, and modern Romance dialectology confirms the drift. But the specific discontinuity characterization — medieval forms as corruption rather than development — is attested almost exclusively within the classicist tradition itself; historical linguistics actively disputes it. Corroborated as to the problem, uncorroborated as to the corruption framing.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently: I claim tangled_rope because the structure possesses BOTH a genuine coordination function (the unified learned medium and the editorial yardstick are real achievements no participant disputes) AND asymmetric extraction through the same structure (the medieval tradition pays for the classical revival, with identifiable losers and no exit). The metrics describe the regime's actual operation. Extractiveness is high (0.70 at interval end) because the standard's benefits and costs land on different parties: the classical establishment gains authority while the medieval transmission bears irreversible correction. Suppression (0.65) is authored as a raw structural property — print gatekeeping, curricular compulsion, and editorial convention closed off the alternative of writing in the received medieval register — and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and continental scope, in the engine's computation. Accessibility_collapse is moderate (0.60): once the standard was understood, publishing in scholastic idiom became professionally impossible, but vernacular and national-language routes remained open, so alternatives collapsed only within the Latin channel. Resistance (0.55) is real: anti-Ciceronian baroque stylists, romantic defenders of national medieval traditions, and eventually the medieval-latinist counter-discipline. The temporal series run on one shared seven-point grid so every tracked metric is authored at every examined point. Extractiveness accumulates monotonically as canon formation, career structures, and curricular entrenchment layer rent onto the original coordination. Theater_ratio climbs from 0.15 to 0.46: early on the philological labor was overwhelmingly functional (establishing texts, writing grammars); by the nineteenth century a large share of activity is performative — verse composition exercises, Ciceronian prose as social accomplishment — as living command of the norm decayed while its maintenance rituals intensified. Suppression_requirement rises steadily as vernacular scholarship and medieval studies pressed against the regime's gatekeeping, tracking the hardening of enforcement machinery rather than mere shifts in extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat (classical_philologists, elite_schoolmasters), the regime is the discipline that makes their expertise possible and the learned world intelligible — their computed classification will sit nearer the coordination pole. From the target seats (monastic_scriptoria, scholastic_latin_writers), the same structure operates as enforced correction of their own usage with no exit and no hearing — nearer the extraction pole. The beneficiary seat (humanist_republic_of_letters) collects the payoff without administering the machinery, and the excluded seat (vernacular_scholars) experiences the regime primarily as a closed door. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: humanist_republic_of_letters and classical_philologists sit near the beneficiary end (low d, damped or inverted effective extraction), with the philologists' identity_locked exit keeping them structurally bound to the standard they administer. Victim declarations drive the opposite pole: monastic_scriptoria and scholastic_latin_writers are powerless and trapped — their holdings and reputations are corrected regardless of consent — placing them near the full-target end (high d, amplified effective extraction). Continental spatial scope makes verification of compliance harder, which the engine reflects as a modest upward scaling of effective extraction. Vernacular_scholars are excluded rather than coordinated: their exclusion is part of what the enforcement maintains. Modern_historical_linguists occupy the analytical seat and feed no directional arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both symmetrical misreadings. Calling the regime a snare would erase the genuine coordination achievement — pan-European learned discourse and reliably established ancient texts were real public goods the regime delivered, and participants were net beneficiaries in the medium itself. Calling it a rope would erase the destroyed medieval tradition, the posthumous condemnation of scholastic idiom, and the concentration of editorial authority — extraction through the very structure that coordinates. The R5 genealogy keeps the lifecycle honest: the founding problem (regional unintelligibility, inaccessible ancient texts) was live at founding and is now contested rather than dead — editing ancient texts still requires a classical yardstick, but national languages absorbed the general communication function. Because the status is contested rather than dead, the mismatch consumer should not fire the zombie flag; but the rising theater_ratio marks the direction of travel: as living classical competence decayed, maintenance grew increasingly performative. The regime is not yet a piton — concentrated beneficiaries (the philological establishment) still actively maintain it — but the trajectory documents the transition path a successor story should watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of correct_latin_kernel correctly describes the Classical-Medieval relation, and how would this constraint''s victim and beneficiary structure change under the sibling readings?',
    'Comparative per-reading analysis over the shared referent: document where each reading locates corruption, who it counts as harmed, and what each computes as the regime''s extraction, then assess which demarcation the linguistic evidence supports.',
    'Under continuity_reading the corruption-victim set empties and the regime reads as suppression of natural linguistic development; under hybrid_reading the victim set splits between the syntax/lexicon layers (genuinely recovered) and the morphology layer (where correction lacked warrant). The classification of this story is conditional on the discontinuity demarcation holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    corruption_demarcation_criterion,
    'By what principled criterion is a given medieval form a corruption rather than legitimate system-internal development?',
    'Systematic comparison of condemned medieval forms against the range of variation attested inside classical usage itself (inscriptions, subliterary texts, authorial inconsistency); forms falling within attested classical variation cannot be corruption by the reading''s own standard.',
    'If most condemned forms fall inside classical internal variation, the discontinuity premise narrows toward hybrid_reading and the emendation program loses warrant for exactly those corrections; the victim declarations would shrink accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_demarcation_criterion, empirical, 'Whether the corruption/development demarcation the reading requires is actually drawable.').

omega_variable(
    discovered_structure_vs_constructed_ideal,
    'Is the reconstructed classical norm a discovered historical system, or a constructed ideal stricter than the usage of any actual ancient community?',
    'Compare the codified norm (grammars, school models, Ciceronian canons) against unemended ancient usage corpora, including sermo cotidianus traces and non-literary registers the canon excludes.',
    'If the operative norm is a constructed ideal, the regime''s targets were corrected against a standard even the ancients did not uniformly meet, raising effective extraction beyond the authored estimate and strengthening the case that the coordination story covers a disciplinary program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_structure_vs_constructed_ideal, empirical, 'Whether the standard enforced was the recovered classical system or an idealized construct above it.').

omega_variable(
    emendation_evidence_loss,
    'How much independent linguistic evidence did classical-oriented emendation destroy before critical apparatuses began preserving rejected readings?',
    'Collate pre-humanist manuscript witnesses against early printed vulgates; quantify how many non-classical readings were silently normalized and whether they survive in apparatus or not at all.',
    'High loss strengthens the victim declarations (irreversible harm to the transmission tradition) and supports remedial diplomatic-edition norms; low loss softens the extraction assessment for the scribal seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emendation_evidence_loss, empirical, 'Magnitude of the evidentiary cost borne by the medieval transmission under the corrective regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t4, correct_latin_kernel__discontinuity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(corr_tr_t8, correct_latin_kernel__discontinuity_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(corr_tr_t12, correct_latin_kernel__discontinuity_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(corr_tr_t16, correct_latin_kernel__discontinuity_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__discontinuity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(corr_tr_t24, correct_latin_kernel__discontinuity_reading, theater_ratio, 24, 0.46).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(corr_be_t4, correct_latin_kernel__discontinuity_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(corr_be_t8, correct_latin_kernel__discontinuity_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(corr_be_t12, correct_latin_kernel__discontinuity_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(corr_be_t16, correct_latin_kernel__discontinuity_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__discontinuity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(corr_be_t24, correct_latin_kernel__discontinuity_reading, base_extractiveness, 24, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(corr_su_t4, correct_latin_kernel__discontinuity_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(corr_su_t8, correct_latin_kernel__discontinuity_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(corr_su_t12, correct_latin_kernel__discontinuity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(corr_su_t16, correct_latin_kernel__discontinuity_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__discontinuity_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(corr_su_t24, correct_latin_kernel__discontinuity_reading, suppression_requirement, 24, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle. continuity_reading (older scholastic self-understanding: natural evolution, internal correction) and this discontinuity_reading (humanist regime: distinct systems, symbolic reoccupation) disagree flatly about the Classical-Medieval relation and therefore assign different epsilon, different victim sets, and different classifications to the same historical material. hybrid_reading emerged as the moderated synthesis after comparative philology. This reading structurally influenced both siblings: its enforcement suppressed continuity-based practice (the continuity reading survives as a minority scholarly position rather than the operative regime), and its textual-recovery results created the conditions under which the hybrid reading became articulable. Each member of the family links to the others; no member should be evaluated in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
