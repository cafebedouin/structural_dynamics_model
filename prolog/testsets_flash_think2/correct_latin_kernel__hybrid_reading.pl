% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Hybrid Latinity Standard (Morphological Continuity, Syntactic/Lexical Reconstruction)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the 'correct Latin'
 *   kernel, which posits that while core Latin morphology remained continuous
 *   through the medieval period, its syntax and lexicon diverged
 *   significantly, necessitating 'textual recovery' and 'reconstruction' to
 *   restore a classical standard. This reading acknowledges some continuity
 *   but emphasizes the need for active intervention to 'correct' perceived
 *   corruptions. The constraint is claimed as a 'rope' to reflect its stated
 *   purpose of coordinating a shared, 'correct' standard for Latin, but its
 *   metrics (high extractiveness and suppression) reflect the reality of
 *   imposing this standard on existing linguistic practices.
 *
 * KEY AGENTS:
 *   - classical_philologists: Primary agenda setter and beneficiary (institutional/analytical) — defines and enforces the standard.
 *   - renaissance_humanists: Primary beneficiary (powerful/mobile) — championed the standard for intellectual authority.
 *   - medieval_latin_scribes: Primary target/payer (powerless/trapped) — their linguistic practices were devalued.
 *   - medieval_scholars: Secondary target/payer (moderate/constrained) — their work was subjected to 'correction'.
 *   - linguistic_historians: Analytical observer (analytical/universal) — analyze the constraint's historical impact.
 *   - continuity_advocates: Excluded voice (organized/constrained) — argue for the legitimacy of medieval Latin.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.65).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.7).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Hybrid Latinity Standard (Morphological Continuity, Syntactic/Lexical Reconstruction)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '3dd8bd22-c799-4036-b497-8bc21a1f1506').
narrative_ontology:cs_kernel_codification('3dd8bd22-c799-4036-b497-8bc21a1f1506', fixed_text).
narrative_ontology:cs_authority_grounding('3dd8bd22-c799-4036-b497-8bc21a1f1506', lineage).
narrative_ontology:cs_interpretation_layer_present('3dd8bd22-c799-4036-b497-8bc21a1f1506').
narrative_ontology:cs_reading_relation('3dd8bd22-c799-4036-b497-8bc21a1f1506', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3dd8bd22-c799-4036-b497-8bc21a1f1506', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('3dd8bd22-c799-4036-b497-8bc21a1f1506', foundational, classical_latin_as_normative_ideal).
narrative_ontology:cs_axiom_status(classical_latin_as_normative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('3dd8bd22-c799-4036-b497-8bc21a1f1506', classical_latin_as_normative_ideal, conventional).
narrative_ontology:cs_axiom('3dd8bd22-c799-4036-b497-8bc21a1f1506', foundational, morphological_continuity_with_classical).
narrative_ontology:cs_axiom_status(morphological_continuity_with_classical, holdable).
narrative_ontology:cs_axiom_grounding('3dd8bd22-c799-4036-b497-8bc21a1f1506', morphological_continuity_with_classical, empirically_contingent).
narrative_ontology:cs_reference_frame('3dd8bd22-c799-4036-b497-8bc21a1f1506', renaissance_philological_restoration).
narrative_ontology:cs_drift_state('3dd8bd22-c799-4036-b497-8bc21a1f1506', contemporary_historical_linguistics, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3dd8bd22-c799-4036-b497-8bc21a1f1506', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, renaissance_humanists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latin_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, teach, and enforce the 'correct' Latin standard based on reconstructed classical forms, particularly in syntax and lexicon. They benefit from the academic prestige and control over the curriculum that this standard confers.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, analytical, global).

% Were the original proponents and beneficiaries of this hybrid standard, using it to distinguish their scholarship and cultural project from the perceived 'barbarism' of medieval Latin. Their intellectual authority was enhanced by their mastery of reconstructed classical forms.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, renaissance_humanists, beneficiary,
    powerful, biographical, mobile, regional).

% Their linguistic practices, particularly in syntax and lexicon, were retrospectively deemed 'corrupt' or 'incorrect' by the hybrid standard. They bore the cost of this normative judgment, as their work was devalued or subjected to 'correction'.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latin_scribes, payer,
    powerless, biographical, trapped, local).

% Their intellectual output, written in the Latin of their time, was subjected to the prescriptive judgments of the hybrid standard. They faced the choice of having their work re-edited to conform or being seen as linguistically inferior.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_scholars, payer,
    moderate, biographical, constrained, regional).

% Analyze the historical evolution of Latin descriptively, often challenging the normative assumptions of the hybrid standard. They observe the constraint's operation and its impact on linguistic perception and pedagogy.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, universal).

% Argue that Medieval Latin represents a natural evolution of the language, not a corruption. Their perspective is often marginalized in pedagogical and philological circles dominated by the hybrid standard, effectively excluding them from setting the 'correct' Latin agenda.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, continuity_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent and historically grounded standard for Latin usage and interpretation, bridging the perceived gap between Classical and Medieval forms, particularly for scholarly and literary purposes.
% TRANSFER_FUNCTION: Transfers authority and prestige from contemporary or evolving Latin usage to a reconstructed Classical standard, requiring intellectual labor and conformity from users, and conferring academic capital on those who master and enforce the standard.
% ABSENT_VOICES: Those who viewed Medieval Latin as a legitimate, naturally evolved language in its own right, rather than a 'corruption' requiring 'recovery', would object to the prescriptive nature of this standard. Their descriptive linguistic perspective is often sidelined in favor of prescriptive philology.
% DISAPPEARANCE_RATIONALE: If this hybrid standard and its enforcement vanished overnight, the academic and pedagogical landscape of Latin studies would fundamentally shift. The emphasis on textual recovery and 'correctness' would diminish, potentially leading to a more descriptive, less prescriptive approach to historical Latin, and a re-evaluation of Medieval Latin's status. Curricula would change, and the careers built on enforcing this standard would lose their primary justification.
% FOUNDING_PROBLEM: The perceived degradation and divergence of Latin from its classical forms during the Middle Ages, leading to a desire among Renaissance humanists to restore a 'pure' or 'correct' Latin for scholarship, literature, and intellectual discourse.
% FOUNDING_PROBLEM_CORROBORATION: Renaissance humanists and early modern philologists attested to the problem of 'corrupt' Latin. Modern historical linguists, from outside the benefiting parties, largely view the 'degradation' narrative as a prescriptive bias, arguing for natural language change and challenging the premise that the founding problem was ever truly 'live' in a descriptive sense.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the standard imposes significant intellectual labor and conformity costs on those whose Latin deviates from the reconstructed ideal, while conferring substantial academic capital on its proponents. Suppression is also high (0.70) as alternative linguistic practices (e.g., descriptive study of Medieval Latin) are marginalized or dismissed as 'incorrect' within the dominant pedagogical framework. Theater ratio is moderate (0.40) reflecting the performative aspect of 'pure' Latin scholarship, where adherence to the reconstructed standard can sometimes overshadow communicative function. The metrics show a rise in extractiveness and suppression as the hybrid standard became more entrenched during the Renaissance and early modern period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical philologists and humanists, this standard is a necessary act of restoration and coordination, ensuring intellectual clarity and continuity with antiquity. From the perspective of medievalists and descriptive linguists, it functions as an imposition, devaluing a living linguistic tradition and extracting conformity through academic authority. The engine's classification as a Tangled Rope (derived from the metrics) would highlight this inherent tension between claimed coordination and actual extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and Renaissance humanists are clear beneficiaries, gaining prestige and control over the definition of 'correct' Latin. Medieval Latin scribes and scholars are targets, as their linguistic practices are deemed 'incorrect' and their work subjected to 'correction'. Linguistic historians act as observers, analyzing the constraint's impact without directly benefiting or paying. Continuity advocates are excluded, their alternative views suppressed by the dominant paradigm.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint is to 'restore' a 'correct' Latin. However, the 'founding problem' of Latin 'degradation' is contested by modern linguistics. The classification as a Tangled Rope (implied by the metrics) would prevent mislabeling this as pure coordination (a Rope) by highlighting the asymmetric extraction from those whose Latin was deemed 'corrupt'. The rising extractiveness and suppression over time indicate an enforcement ratchet, where the 'restoration' became an increasingly extractive and suppressive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid_reading'' of the ''correct_latin_kernel''?',
    'Analysis of historical philological texts and pedagogical practices to confirm the specific blend of morphological continuity and syntactic/lexical reconstruction as a distinct interpretive stance.',
    'If misidentified, the analysis of its relationship to sibling readings (continuity_reading, discontinuity_reading) would be flawed, leading to incorrect network and axiom evaluations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific interpretive stance of this kernel reading.').

omega_variable(
    linguistic_naturalness_of_morphology,
    'To what extent was the ''core morphology continuous'' due to natural linguistic inertia versus implicit prescriptive forces even in the medieval period?',
    'Detailed diachronic linguistic analysis of morphological change in Latin across various registers and regions, distinguishing between natural drift and conscious efforts to maintain classical forms.',
    'If morphological continuity was also significantly prescriptive, the ''hybrid'' claim of natural continuity would be weakened, potentially shifting the constraint closer to the ''discontinuity_reading'' in its overall extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_naturalness_of_morphology, empirical, 'Examines the naturalness vs. prescriptiveness of morphological continuity.').

omega_variable(
    reconstruction_as_imposition,
    'Is the ''textual recovery'' and ''reconstruction'' of syntax and lexicon a neutral scholarly endeavor or an inherently normative imposition on a living language?',
    'Comparative study of language standardization movements, analyzing the power dynamics and social consequences of prescriptive grammars versus descriptive linguistic approaches.',
    'If primarily an imposition, the ''coordination'' aspect of the constraint is further diminished, strengthening its classification as a Snare or a more extractive Tangled Rope. If genuinely neutral, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_as_imposition, conceptual, 'Assesses the normative nature of linguistic reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 1400, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin_kernel__hybrid_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(corr_tr_t1430, correct_latin_kernel__hybrid_reading, theater_ratio, 1430, 0.15).
narrative_ontology:measurement(corr_tr_t1460, correct_latin_kernel__hybrid_reading, theater_ratio, 1460, 0.2).
narrative_ontology:measurement(corr_tr_t1490, correct_latin_kernel__hybrid_reading, theater_ratio, 1490, 0.25).
narrative_ontology:measurement(corr_tr_t1520, correct_latin_kernel__hybrid_reading, theater_ratio, 1520, 0.3).
narrative_ontology:measurement(corr_tr_t1550, correct_latin_kernel__hybrid_reading, theater_ratio, 1550, 0.35).
narrative_ontology:measurement(corr_tr_t1580, correct_latin_kernel__hybrid_reading, theater_ratio, 1580, 0.38).
narrative_ontology:measurement(corr_tr_t1610, correct_latin_kernel__hybrid_reading, theater_ratio, 1610, 0.4).
narrative_ontology:measurement(corr_tr_t1640, correct_latin_kernel__hybrid_reading, theater_ratio, 1640, 0.4).
narrative_ontology:measurement(corr_tr_t1670, correct_latin_kernel__hybrid_reading, theater_ratio, 1670, 0.4).
narrative_ontology:measurement(corr_tr_t1700, correct_latin_kernel__hybrid_reading, theater_ratio, 1700, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin_kernel__hybrid_reading, base_extractiveness, 1400, 0.45).
narrative_ontology:measurement(corr_be_t1430, correct_latin_kernel__hybrid_reading, base_extractiveness, 1430, 0.5).
narrative_ontology:measurement(corr_be_t1460, correct_latin_kernel__hybrid_reading, base_extractiveness, 1460, 0.55).
narrative_ontology:measurement(corr_be_t1490, correct_latin_kernel__hybrid_reading, base_extractiveness, 1490, 0.6).
narrative_ontology:measurement(corr_be_t1520, correct_latin_kernel__hybrid_reading, base_extractiveness, 1520, 0.62).
narrative_ontology:measurement(corr_be_t1550, correct_latin_kernel__hybrid_reading, base_extractiveness, 1550, 0.64).
narrative_ontology:measurement(corr_be_t1580, correct_latin_kernel__hybrid_reading, base_extractiveness, 1580, 0.65).
narrative_ontology:measurement(corr_be_t1610, correct_latin_kernel__hybrid_reading, base_extractiveness, 1610, 0.65).
narrative_ontology:measurement(corr_be_t1640, correct_latin_kernel__hybrid_reading, base_extractiveness, 1640, 0.65).
narrative_ontology:measurement(corr_be_t1670, correct_latin_kernel__hybrid_reading, base_extractiveness, 1670, 0.65).
narrative_ontology:measurement(corr_be_t1700, correct_latin_kernel__hybrid_reading, base_extractiveness, 1700, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin_kernel__hybrid_reading, suppression_requirement, 1400, 0.3).
narrative_ontology:measurement(corr_su_t1430, correct_latin_kernel__hybrid_reading, suppression_requirement, 1430, 0.4).
narrative_ontology:measurement(corr_su_t1460, correct_latin_kernel__hybrid_reading, suppression_requirement, 1460, 0.5).
narrative_ontology:measurement(corr_su_t1490, correct_latin_kernel__hybrid_reading, suppression_requirement, 1490, 0.6).
narrative_ontology:measurement(corr_su_t1520, correct_latin_kernel__hybrid_reading, suppression_requirement, 1520, 0.65).
narrative_ontology:measurement(corr_su_t1550, correct_latin_kernel__hybrid_reading, suppression_requirement, 1550, 0.68).
narrative_ontology:measurement(corr_su_t1580, correct_latin_kernel__hybrid_reading, suppression_requirement, 1580, 0.7).
narrative_ontology:measurement(corr_su_t1610, correct_latin_kernel__hybrid_reading, suppression_requirement, 1610, 0.7).
narrative_ontology:measurement(corr_su_t1640, correct_latin_kernel__hybrid_reading, suppression_requirement, 1640, 0.7).
narrative_ontology:measurement(corr_su_t1670, correct_latin_kernel__hybrid_reading, suppression_requirement, 1670, 0.7).
narrative_ontology:measurement(corr_su_t1700, correct_latin_kernel__hybrid_reading, suppression_requirement, 1700, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, latin_pedagogy_standards).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, classical_text_editing_norms).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'correct_latin_kernel', each representing a distinct structural claim about the relationship between Classical and Medieval Latin. This 'hybrid_reading' attempts to reconcile continuity in morphology with discontinuity in syntax/lexicon, requiring active reconstruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
