% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Correct Latin: Discontinuity Reading (Classical as Preserved Text)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint represents the 'discontinuity' reading of correct Latin,
 *   which asserts that Classical Latin, as preserved in ancient texts, is the
 *   only 'correct' form, and medieval Latin is a corrupt deviation. This
 *   reading necessitates reconstruction from textual sources and declares a
 *   rupture between Classical and medieval forms. It is a Tangled Rope
 *   because it provides a coordination function (a shared, high-status
 *   standard) but also extracts from and suppresses alternative, continuously
 *   evolving forms of Latin, benefiting those who define and enforce the
 *   classical standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.65).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.78).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Correct Latin: Discontinuity Reading (Classical as Preserved Text)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, 'f02812f4-6f6e-4ac0-9e09-74dbb42943ce').
narrative_ontology:cs_kernel_codification('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', fixed_text).
narrative_ontology:cs_authority_grounding('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', lineage).
narrative_ontology:cs_interpretation_layer_present('f02812f4-6f6e-4ac0-9e09-74dbb42943ce').
narrative_ontology:cs_reading_relation('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', foundational, classical_latin_is_normative_ideal).
narrative_ontology:cs_axiom_status(classical_latin_is_normative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', classical_latin_is_normative_ideal, conventional).
narrative_ontology:cs_axiom('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', foundational, medieval_latin_is_corrupt_deviation).
narrative_ontology:cs_axiom_status(medieval_latin_is_corrupt_deviation, holdable).
narrative_ontology:cs_axiom_grounding('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', medieval_latin_is_corrupt_deviation, conventional).
narrative_ontology:cs_reference_frame('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', renaissance_humanist_restoration).
narrative_ontology:cs_drift_state('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', contemporary_linguistic_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f02812f4-6f6e-4ac0-9e09-74dbb42943ce', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, renaissance_humanists).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, living_latin_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the standards of 'correct' Latin based on ancient texts. Their professional identity and academic authority are built upon the premise of a rupture between Classical and medieval Latin, and the necessity of textual reconstruction.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, global).

% Historically established and propagated this reading, gaining intellectual prestige and patronage by 'restoring' Latin to its perceived classical purity. Their legacy benefits from the continued adherence to this discontinuity.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, renaissance_humanists, beneficiary,
    powerful, generational, mobile, continental).

% Their work on medieval texts is often implicitly or explicitly devalued as 'corrupt' or 'deviant' by this reading. They must either conform to classical standards in their own writing or defend the legitimacy of medieval forms against a dominant paradigm.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_scholars, payer,
    moderate, biographical, constrained, global).

% Those who learned Latin through continuous, evolving traditions (e.g., ecclesiastical Latin) find their usage deemed 'incorrect' or 'barbaric'. Their linguistic identity is challenged, and they are pressured to adopt an artificial, reconstructed form.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, living_latin_speakers, payer,
    powerless, immediate, identity_locked, local).

% Analyze the historical evolution of Latin without normative judgment, often finding evidence for continuity and gradual change rather than sharp rupture. Their analytical findings frequently challenge the prescriptive claims of this reading.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, high-status linguistic standard for scholarly and literary Latin, ensuring intelligibility across time and space by referencing a fixed, prestigious historical period.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural capital from medieval and living traditions to the textual authority of Classical antiquity, enforced by philological gatekeepers.
% ABSENT_VOICES: Medieval scribes and grammarians, who saw their Latin as a living, evolving language, are absent from the conversation. If present, they would argue for the legitimacy of their usage as a natural continuation of Latin.
% DISAPPEARANCE_RATIONALE: If the 'discontinuity' reading vanished, the entire field of Latin studies would undergo a profound reorientation. Medieval Latin would be re-evaluated as a legitimate stage of linguistic development, pedagogical approaches would shift, and the authority of classical philologists would be significantly diminished, leading to a reorganization of academic hierarchies and research priorities.
% FOUNDING_PROBLEM: The perceived 'decline' and 'corruption' of Latin during the Middle Ages, leading to a desire to restore the language to its perceived golden age of Classical antiquity for clarity, elegance, and intellectual rigor.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists and some literary scholars attest the problem is still live, emphasizing the aesthetic and grammatical superiority of Classical forms. Linguistic historians and medievalists, from outside the benefiting parties, attest that the 'corruption' narrative is largely a prescriptive judgment, not a descriptive linguistic reality, and that the problem is largely 'dead' as a genuine linguistic issue, persisting primarily as a cultural preference.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because it devalues and marginalizes a vast body of legitimate linguistic practice and scholarship (medieval Latin), imposing a high cost on those who do not conform to the reconstructed classical ideal. Suppression is also high (0.78) due to the institutional power of classical philology in academic and cultural spheres, actively enforcing the 'correct' standard and suppressing alternatives through pedagogical methods, publication gatekeeping, and scholarly prestige. The theater ratio (0.40) reflects that while there is genuine scholarly work in textual criticism, a significant portion of the effort is performative maintenance of the 'purity' narrative, rather than purely descriptive linguistic analysis. The slight decrease in extractiveness and suppression towards the end of the interval reflects growing academic recognition of medieval Latin's legitimacy, though the core constraint remains strong.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical philologists, this constraint is a necessary Rope for preserving a high standard of Latin. From the perspective of medieval Latin scholars, it operates as a Snare, devaluing their field and imposing an artificial standard. The engine's classification as Tangled Rope reflects this hybrid nature, with both coordination and significant extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and Renaissance humanists are beneficiaries and agenda-setters, as their authority and intellectual projects are grounded in this reading. Medieval Latin scholars and living Latin speakers are payers, bearing the cost of having their linguistic practices deemed 'incorrect' or 'corrupt'. Linguistic historians act as observers, often providing evidence that challenges the foundational premises of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_prescriptivism_vs_descriptivism,
    'Is the ''correctness'' of Classical Latin a descriptive linguistic fact or a prescriptive cultural judgment?',
    'Analysis of linguistic change over time, comparing the ''rules'' of Classical Latin to the actual usage patterns in medieval texts, and examining the social functions of both forms without normative bias.',
    'If primarily a prescriptive judgment, the constraint''s extractiveness and suppression are higher, as it enforces an arbitrary standard. If a descriptive fact, the constraint is closer to a Mountain, reflecting inherent linguistic properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linguistic_prescriptivism_vs_descriptivism, conceptual, 'Ambiguity between linguistic description and cultural prescription.').

omega_variable(
    identity_lock_of_philologists,
    'To what extent is the identity of classical philologists fused with the ''discontinuity'' reading, making alternative framings an existential threat to their professional identity?',
    'Sociological study of academic disciplines, analysis of career paths, and responses to challenges to the ''discontinuity'' narrative. Observe if scholars who embrace continuity are marginalized.',
    'If identity-locked, the resistance to re-evaluation is higher, and the constraint''s persistence is more inertial, even if its functional justification erodes. This would amplify the effective suppression for those challenging the paradigm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_philologists, empirical, 'Professional identity fusion with the constraint''s core premise.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (academic gatekeeping, publication bias) or internalized (medievalists self-censoring or adopting classical norms)?',
    'Post-exit suppression trajectory: if medievalists continue to prioritize classical norms even after institutional barriers are removed, reclassify as partially internalized. Qualitative interviews with scholars.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — scholars carry the suppression with them after exit, making the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1400, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin__discontinuity_reading, theater_ratio, 1400, 0.2).
narrative_ontology:measurement(corr_tr_t1600, correct_latin__discontinuity_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(corr_tr_t1800, correct_latin__discontinuity_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(corr_tr_t2024, correct_latin__discontinuity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin__discontinuity_reading, base_extractiveness, 1400, 0.5).
narrative_ontology:measurement(corr_be_t1600, correct_latin__discontinuity_reading, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(corr_be_t1800, correct_latin__discontinuity_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement(corr_be_t2024, correct_latin__discontinuity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin__discontinuity_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement(corr_su_t1600, correct_latin__discontinuity_reading, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(corr_su_t1800, correct_latin__discontinuity_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(corr_su_t2024, correct_latin__discontinuity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'correct_latin' kernel. Its ε value reflects the specific structural claims of the discontinuity reading, which differ significantly from the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
