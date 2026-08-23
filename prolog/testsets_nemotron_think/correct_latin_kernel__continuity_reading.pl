% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Medieval Latin as Natural Evolution of Classical Latin
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading asserts that Medieval Latin is simply Classical
 *   Latin after natural linguistic evolution — no rupture, no corruption,
 *   just the ordinary workings of language change. Humanist 'reconstruction'
 *   (Bembo, Erasmus, Valla) is reinterpreted not as recovery but as
 *   prescriptive purism: an imposition of a frozen classical norm on a
 *   living, evolved Latin. This reading functions as a constraint on Latin
 *   studies, philological method, and the valuation of medieval texts. It
 *   claims the authority of natural law (linguistic evolution is a Mountain),
 *   but its historical operation shows active enforcement (humanist
 *   curricula, printing standardization, academic gatekeeping), identifiable
 *   beneficiaries (humanist scholars, classicizing institutions, neolatin
 *   academies), and identifiable victims (medieval textual tradition,
 *   scholastic vocabulary, vernacular Latin users). The high extractiveness
 *   and suppression metrics reflect the real cost imposed on medieval
 *   Latinity by the classicizing standard; the theater ratio captures the
 *   genuine philological work mixed with the performative defense of a
 *   constructed norm.
 *
 * KEY AGENTS:
 *   - humanist_scholars: Primary beneficiary (institutional/mobile) — gain professional authority, editorial control, and cultural capital from enforcing the classical norm
 *   - renaissance_educational_institutions: Beneficiary/agenda_setter (institutional/biographical) — curricula built on the reconstructed norm secure patronage and legitimacy
 *   - medieval_latin_textual_tradition: Primary victim (organized/constrained) — its texts are re-edited, its vocabulary marginalized, its authority displaced
 *   - scholastic_philosophical_vocabulary: Victim (organized/identity_locked) — technical terminology developed over centuries is dismissed as 'barbarous' and replaced
 *   - vernacular_latin_users: Victim (powerless/trapped) — clergy, administrators, merchants using evolved Latin find their usage declared incorrect
 *   - medieval_manuscript_culture: Victim (organized/constrained) — transmission practices, glossing traditions, and textual habits are disrupted by humanist editorial norms
 *   - later_neolatin_academies: Beneficiary (institutional/generational) — inherit the normative standard as institutional capital
 *   - philological_observer: Observer (analytical/analytical) — sees the full structural asymmetry between the natural-law claim and the enforced norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.72).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Natural Evolution of Classical Latin").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).
domain_priors:emerges_naturally(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '3bc2d704-6147-4e86-bf72-9fd535632a0f').
narrative_ontology:cs_kernel_codification('3bc2d704-6147-4e86-bf72-9fd535632a0f', distributed).
narrative_ontology:cs_authority_grounding('3bc2d704-6147-4e86-bf72-9fd535632a0f', practice).
narrative_ontology:cs_interpretation_layer_present('3bc2d704-6147-4e86-bf72-9fd535632a0f').
narrative_ontology:cs_reading_relation('3bc2d704-6147-4e86-bf72-9fd535632a0f', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bc2d704-6147-4e86-bf72-9fd535632a0f', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3bc2d704-6147-4e86-bf72-9fd535632a0f', foundational, linguistic_continuity_is_normative).
narrative_ontology:cs_axiom_status(linguistic_continuity_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('3bc2d704-6147-4e86-bf72-9fd535632a0f', linguistic_continuity_is_normative, empirically_contingent).
narrative_ontology:cs_axiom('3bc2d704-6147-4e86-bf72-9fd535632a0f', foundational, prescriptive_reform_is_corruptive).
narrative_ontology:cs_axiom_status(prescriptive_reform_is_corruptive, holdable).
narrative_ontology:cs_axiom_grounding('3bc2d704-6147-4e86-bf72-9fd535632a0f', prescriptive_reform_is_corruptive, deontological).
narrative_ontology:cs_reference_frame('3bc2d704-6147-4e86-bf72-9fd535632a0f', living_latin_continuum).
narrative_ontology:cs_drift_state('3bc2d704-6147-4e86-bf72-9fd535632a0f', early_print_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('3bc2d704-6147-4e86-bf72-9fd535632a0f', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, renaissance_educational_institutions).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, classicizing_print_culture).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, later_neolatin_academies).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, medieval_latin_textual_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, scholastic_philosophical_vocabulary).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, vernacular_latin_users).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, medieval_manuscript_culture).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, linguistic_evolution_is_lawlike).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, classical_normativity_is_recoverable_from_usage).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, prescriptive_reform_is_corruptive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit, publish, and teach Latin texts under the classical norm. Gain professional prestige, patronage, and editorial authority from defining what counts as 'correct' Latin. Can move between courts, universities, and print shops — their Latin is portable capital.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_scholars, beneficiary,
    institutional, biographical, mobile, continental).

% Set curricula mandating Cicero and Virgil as exclusive models. Secure funding and legitimacy by aligning with the humanist standard. Compete with rival institutions on stylistic purity — the norm is their product and their gate.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, renaissance_educational_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, renaissance_educational_institutions, beneficiary).

% Centuries of manuscript transmission, glossing, and commentary practice. Humanist editors 'correct' medieval spellings, syntax, and vocabulary into classical forms, erasing the tradition's own editorial logic. Exit means abandoning the manuscript corpus — constrained by custodial duty.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_latin_textual_tradition, payer,
    organized, generational, constrained, continental).

% Technical Latin developed for theology, logic, metaphysics (quidditas, haecceitas, suppositio, etc.). Humanists dismiss this as 'barbarous' and replace it with classical circumlocutions. The vocabulary is constitutive of the scholastic identity — exit means ceasing to be a scholastic.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, scholastic_philosophical_vocabulary, payer,
    organized, generational, identity_locked, continental).

% Clergy, notaries, physicians, merchants using evolved Latin for daily professional work. Their usage is declared incorrect by grammars they cannot influence. Exit means abandoning their vocational language for the vernacular — trapped until vernacular institutions mature.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, vernacular_latin_users, payer,
    powerless, biographical, trapped, regional).

% Scriptoria, glossing traditions, textual habits (abbreviations, marginalia, compilation practices). Humanist printing replaces manuscript transmission with standardized editions; the culture's practices are devalued as 'corrupt.' Constrained by the material inheritance they steward.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_manuscript_culture, payer,
    organized, generational, constrained, continental).

% Inherit the classical norm as institutional capital (Crusca, Académie Française model). Police Latinity in diplomacy, science, and belles-lettres. The norm is their reason for existing — they arbitrate its application.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, later_neolatin_academies, beneficiary,
    institutional, generational, arbitrage, continental).

% Modern historical linguist / philologist analyzing the full structure. Sees the continuity claim as a reading, not a fact. No stake in the norm, no cost from its enforcement. Exit is analytical — the seat is defined by not being in the game.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, philological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transnational learned lingua franca for European scholarship, diplomacy, and science by stabilizing Latin around a single classical norm instead of fragmented medieval usages.
% TRANSFER_FUNCTION: Moves editorial authority, curricular control, and cultural capital from medieval textual communities (scholastic, monastic, vernacular) to humanist scholars and institutions, via the imposition of a classical purity standard that declares evolved usage incorrect.
% ABSENT_VOICES: Women Latinists (excluded from humanist academies and university chairs), non-European Latin users (Jesuit missions in Asia/Americas using evolved Latin), Jewish Latin writers (marginalized by both medieval and humanist Christian norms), and the vernacular languages themselves (displaced as Latin reasserts dominance in learned domains).
% DISAPPEARANCE_RATIONALE: If the classical norm and its enforcement vanished overnight, medieval Latin would continue evolving (as it did in scholastic universities into the 1700s), vernacular Latin would persist in administration and liturgy, and the humanist editorial industry would collapse. The transnational learned lingua franca would fragment — but that fragmentation is what the medieval period already had, and what the norm was built to solve.
% FOUNDING_PROBLEM: After the Carolingian Renaissance, Latin fragmented into regional usages; by 1300 no single Latin served as a reliable transnational standard for scholarship, law, or theology. A stable, teachable, universally recognized norm was needed.
% FOUNDING_PROBLEM_CORROBORATION: Humanist prefaces (Valla, Erasmus, Bembo) attest the problem was live. Medieval university statutes and scholastic commentaries attest the problem was already solved by existing pedagogical Latin. Later neolatin academies attest the problem persisted but the humanist solution became a new exclusion. No single corroboration outside the beneficiary set — the founding problem's status is itself a site of contest.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(correct_latin_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(correct_latin_kernel__continuity_reading),
    narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the classicizing norm extracts legitimacy, editorial control, and cultural capital from medieval Latin users while returning a 'purity' that never existed as a living standard. Suppression (0.72) is higher still because the constraint's persistence depended on active institutional enforcement: university statutes mandating Cicero-only curricula, printing privileges tied to humanist orthography, academic appointments conditional on stylistic conformity. Theater ratio (0.41) reflects the genuine philological achievements of humanists (textual criticism, manuscript collation) mixed with the performative staging of 'restoration' — the reconstruction theater. Accessibility collapse (0.62) is moderate-high: once the classical norm is internalized, medieval Latin appears as error rather than evolution, but the medieval tradition never fully disappears (scholastic Latin persists in universities into the 18th century). Resistance (0.58) is substantial: medievalists, humanist critics (e.g., Erasmus vs. the Ciceronians), and vernacular users all push back, but the institutional momentum of print and pedagogy prevails.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist/agenda_setter seat, the constraint is genuine coordination: a shared standard enables communication across Europe, enables textual recovery, solves the Babel of medieval usage. From the medieval/victim seats, the same structure is enforced extraction: their evolved, functional Latin is declared wrong, their texts are 'corrected' into a language no one spoke, their professional competence is devalued. The engine computes this divergence from the structural data — the continuity reading's Mountain claim masks the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars and renaissance institutions are structural beneficiaries: they collect the rents of normative authority (editorial prestige, curricular control, cultural capital) — directionality near 0.0. Medieval Latin textual tradition and scholastic vocabulary are structural targets: they bear the costs of displacement, re-editing, marginalization — directionality near 1.0. Vernacular Latin users are trapped targets with identity-locked exit: their Latin is their professional identity, exit means abandoning their vocational language — directionality amplified toward 1.0. Later neolatin academies inherit the beneficiary position. The philological observer sits at analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a transnational learned lingua franca after medieval dialectal fragmentation) was live in 1350-1450. By 1550, the classical norm had become the problem: it fossilized a living language, excluded vernacular users, and served as a gatekeeping mechanism for academic and ecclesiastical office. The arrangement persists (neolatin academies, classical philology's normative core) because the beneficiaries control the institutions that certify Latinity. The mandatrophy is unresolved: the coordination function (shared standard) was real but the extraction function (classical purity as exclusionary gate) has long since dominated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_standard,
    'Is the continuity thesis a genuine natural-law description of linguistic evolution, or a constructed epistemic standard that benefits classicizing institutions?',
    'Comparative analysis of linguistic change rates in Medieval vs. Classical Latin against attested natural language change in other documented continua; historical reconstruction of humanist editorial practices showing prescriptive intervention.',
    'If constructed, the Mountain claim collapses and the constraint reclassifies as extractive (tangled_rope or snare); if natural, the high suppression and extraction metrics require explanation as measurement artifacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_standard, conceptual, 'Whether the continuity reading describes a natural linguistic fact or institutes a normative standard').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the continuity, discontinuity, and hybrid readings structurally disagree — on the linguistic facts, the normative status of reconstruction, or the legitimacy of medieval innovation?',
    'Formal decomposition of each reading''s victim/beneficiary sets and coordination/extraction claims; the disagreement locus is the structural element whose value flips across readings.',
    'Locates the committer-axis contest precisely; determines whether readings are genuinely competing constraints (different ε, different victims) or interpretive frames on one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural location of disagreement among kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional enforcement of humanist norms in education/printing) or internalized (medievalists adopting classicizing standards as self-correction)?',
    'Post-reformation suppression trajectory: if suppression persists after humanist institutional control weakens, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after institutional exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the humanist reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 1350, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1350, correct_latin_kernel__continuity_reading, theater_ratio, 1350, 0.15).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1400, correct_latin_kernel__continuity_reading, theater_ratio, 1400, 0.22).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1450, correct_latin_kernel__continuity_reading, theater_ratio, 1450, 0.31).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1500, correct_latin_kernel__continuity_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1550, correct_latin_kernel__continuity_reading, theater_ratio, 1550, 0.41).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_tr_t1600, correct_latin_kernel__continuity_reading, theater_ratio, 1600, 0.41).

% Extraction over time
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1350, correct_latin_kernel__continuity_reading, base_extractiveness, 1350, 0.35).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1400, correct_latin_kernel__continuity_reading, base_extractiveness, 1400, 0.42).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1450, correct_latin_kernel__continuity_reading, base_extractiveness, 1450, 0.55).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1500, correct_latin_kernel__continuity_reading, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1550, correct_latin_kernel__continuity_reading, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_be_t1600, correct_latin_kernel__continuity_reading, base_extractiveness, 1600, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1350, correct_latin_kernel__continuity_reading, suppression_requirement, 1350, 0.4).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1400, correct_latin_kernel__continuity_reading, suppression_requirement, 1400, 0.52).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1450, correct_latin_kernel__continuity_reading, suppression_requirement, 1450, 0.61).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1500, correct_latin_kernel__continuity_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1550, correct_latin_kernel__continuity_reading, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement(correct_latin_kernel__continuity_reading_su_t1600, correct_latin_kernel__continuity_reading, suppression_requirement, 1600, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'correct Latin' kernel into three structurally distinct readings. The continuity reading claims natural-law status (Mountain) but operates with high extraction/suppression — FSM candidate. The discontinuity reading treats the classical norm as a constructed reoccupation (Snare/Tangled Rope). The hybrid reading splits the coordination/extraction across morphological vs. syntactic layers. They are linked because each reading's ε and victim sets differ — the kernel label 'correct Latin' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__continuity_reading, organized, 0.85).
constraint_indexing:directionality_override(correct_latin_kernel__continuity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
