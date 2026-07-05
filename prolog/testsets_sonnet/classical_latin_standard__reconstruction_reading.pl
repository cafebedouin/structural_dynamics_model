% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Humanist Philological Reconstruction Standard for Correct Latin
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the reconstruction reading of the
 *   classical_latin_standard kernel: correct Latin is held to be exclusively
 *   the Classical form recoverable through philological archaeology, and
 *   medieval/ecclesiastical usage is treated as corruption to be
 *   discontinuously rejected rather than a legitimate developmental stage.
 *   This is a distinct constraint from the continuity_reading (which holds
 *   transmitted usage as legitimate by definition) and the hybrid_reading
 *   (which admits both textual fidelity and post-Classical
 *   technical/ecclesiastical development) — the three readings have different
 *   beneficiary/victim structures and different ε values, and are linked here
 *   only through network.affects_constraints and
 *   cs_structure.reading_relations, never merged.
 *
 * KEY AGENTS:
 *   - humanist_philologists: agenda_setter/beneficiary (institutional/arbitrage) — set and administer the standard
 *   - renaissance_academies: beneficiary (institutional/arbitrage) — institutional prestige from the standard
 *   - medieval_trained_clergy: payer (moderate/constrained) — delegitimized despite unchanged competence
 *   - vernacular_educated_notaries: payer (powerless/trapped) — permanently excluded from the credential
 *   - ecclesiastical_latin_users: payer/excluded (organized/constrained) — centuries of usage indicted wholesale
 *   - historians_of_language: observer (analytical) — documents the redistribution of authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.72).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Humanist Philological Reconstruction Standard for Correct Latin").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'bc6ba30b-a9dd-489f-a149-3d6754150bf7').
narrative_ontology:cs_kernel_codification('bc6ba30b-a9dd-489f-a149-3d6754150bf7', fixed_text).
narrative_ontology:cs_authority_grounding('bc6ba30b-a9dd-489f-a149-3d6754150bf7', expertise).
narrative_ontology:cs_interpretation_layer_present('bc6ba30b-a9dd-489f-a149-3d6754150bf7').
narrative_ontology:cs_reading_relation('bc6ba30b-a9dd-489f-a149-3d6754150bf7', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('bc6ba30b-a9dd-489f-a149-3d6754150bf7', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('bc6ba30b-a9dd-489f-a149-3d6754150bf7', foundational, textual_recovery_is_sole_legitimate_authority).
narrative_ontology:cs_axiom_status(textual_recovery_is_sole_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('bc6ba30b-a9dd-489f-a149-3d6754150bf7', textual_recovery_is_sole_legitimate_authority, conventional).
narrative_ontology:cs_axiom('bc6ba30b-a9dd-489f-a149-3d6754150bf7', foundational, medieval_drift_constitutes_corruption_not_development).
narrative_ontology:cs_axiom_status(medieval_drift_constitutes_corruption_not_development, holdable).
narrative_ontology:cs_axiom_grounding('bc6ba30b-a9dd-489f-a149-3d6754150bf7', medieval_drift_constitutes_corruption_not_development, empirically_contingent).
narrative_ontology:cs_reference_frame('bc6ba30b-a9dd-489f-a149-3d6754150bf7', ciceronian_augustan_textual_corpus).
narrative_ontology:cs_drift_state('bc6ba30b-a9dd-489f-a149-3d6754150bf7', post_humanist_revival, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('bc6ba30b-a9dd-489f-a149-3d6754150bf7', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, renaissance_academies).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_textbook_publishers).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_trained_clergy).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, chancery_scribes).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, vernacular_educated_notaries).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_users).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, classical_ciceronian_norm_as_true_latin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in the recovery of Classical manuscripts and grammar, they set the standard by which Latin is now judged correct: fidelity to Ciceronian/Augustan usage recovered through textual collation. They administer the new pedagogy, write the grammars that fix the standard, and staff the academies and chanceries that require it. Their expertise is the sole credential that satisfies the new bar, and their careers and institutional positions are built on maintaining the discontinuity claim — that medieval Latin was corruption, not development.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, humanist_philologists, beneficiary).

% Adopt the reconstruction standard as their institutional identity and market themselves on producing 'true' Latinists. They gain prestige, patronage, and enrollment by positioning themselves as custodians of the recovered classical form against the discredited medieval schools.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, renaissance_academies, beneficiary,
    institutional, generational, arbitrage, continental).

% Produce and sell the new grammars, dictionaries, and corrected editions required to teach and verify the reconstructed standard. Demand for their products depends entirely on the standard's continued authority and on medieval-trained competitors being treated as illegitimate.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_textbook_publishers, beneficiary,
    organized, generational, mobile, continental).

% Learned Latin through centuries of unbroken ecclesiastical and monastic transmission — a living, functional register adequate to liturgy, correspondence, and law. Under the new standard their Latin is reclassified as corrupt, their training devalued, and their authority to write or judge Latin usage undermined. Retraining under humanist masters late in a career is costly and often impractical; many simply lose standing without any change in their actual competence.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_trained_clergy, payer,
    moderate, biographical, constrained, regional).

% Produce administrative and legal Latin in a practice-based register optimized for clarity and legal precedent, not classical elegance. The reconstruction standard delegitimizes their formulary conventions as barbarisms, threatening their employability against newly credentialed humanist secretaries who can invoke the classical norm as a professional distinction.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, chancery_scribes, payer,
    moderate, biographical, constrained, regional).

% Learned functional Latin through apprenticeship and local practice rather than formal philological training. They have no access to the manuscript collation, Greek philology, or classical curriculum that the new standard requires, and no realistic path to acquiring it; their exclusion from 'correct' Latin is closer to permanent than for better-resourced peers.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, vernacular_educated_notaries, payer,
    powerless, biographical, trapped, local).

% The institutional Church has centuries of liturgical and doctrinal Latin built on continuous usage. The reconstruction standard implicitly indicts that entire corpus as corrupted, forcing an uncomfortable choice between defending inherited practice against secular philological authority or conceding that centuries of ecclesiastical Latin were simply wrong. Their objection — that transmitted, functioning Latin cannot be dismissed wholesale as error — is rarely granted a hearing in humanist academic circles.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_users, payer,
    organized, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_users, excluded).

% Study the standard's construction and its social effects without a stake in either camp; document how the discontinuity claim functioned to redistribute prestige and gatekeeping power toward the newly credentialed humanist class.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, historians_of_language, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored reference point (Classical usage as attested in collated manuscripts) against which any claim about correct Latin can in principle be checked, replacing an unbounded and regionally fragmented set of local usages with one recoverable standard.
% TRANSFER_FUNCTION: Moves professional legitimacy, teaching positions, patronage, and the authority to certify correct usage away from medieval-trained clergy, scribes, and notaries and toward humanist philologists, the academies that employ them, and the publishers who supply their materials.
% ABSENT_VOICES: Medieval-trained clergy, chancery scribes, and especially vernacular-educated notaries who cannot access philological training have no seat in the humanist academies where the standard is set; their functional, transmitted Latin is judged in absentia and found wanting by a body they were never invited to address.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished, medieval and ecclesiastical Latin usage would regain unquestioned legitimacy overnight, humanist credentialing would lose its distinguishing function, textbook and grammar markets built on 'correction' would collapse, and displaced clergy and scribes would recover professional standing without having changed their practice at all.
% FOUNDING_PROBLEM: Humanist scholars observed genuine discontinuities between Classical usage (as attested in newly recovered manuscripts) and contemporary medieval Latin, and sought a principled way to recover and teach the earlier form for scholarly and aesthetic purposes.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists themselves attest the problem is live and unresolved — medieval drift is real and correction is ongoing scholarly work. Historians of language, writing from outside the humanist institutional apparatus, corroborate that genuine textual discontinuity exists but dispute that this warrants delegitimizing functioning transmitted registers; no corroboration from outside the humanist tradition supports the further claim that medieval and ecclesiastical Latin were simply corrupt rather than legitimately developed.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 by interval end) because the standard does not merely describe a textual fact but actively redistributes professional legitimacy: it creates a credentialing bottleneck (philological training) that a large existing population of competent Latin users cannot pass regardless of their functional competence. Suppression is high (0.78) because the standard's authority depends on actively labeling alternatives as 'corruption' rather than merely being the best available description — this delegitimization is the suppression mechanism, not a byproduct of it. Theater ratio is moderate (0.42) reflecting that some fraction of 'correction' work is performative purism (obsessive rejection of well-functioning medieval forms with no communicative deficit) layered onto genuine philological scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist philologist's seat this is straightforwardly Rope: it solves a genuine coordination problem (what does Cicero's Latin actually look like, and how do we teach it faithfully) with minimal coercion — anyone can, in principle, learn the classical grammar. From the medieval clergy or notary's seat the same structure computes as extractive: their working competence, built over a lifetime of practice, is retroactively reclassified as error by a standard they had no part in setting and often cannot access the training to satisfy. The engine should register this divergence rather than resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and the academies/publishers built around them are declared beneficiaries: the standard is their credential, their curriculum, their market. Medieval clergy, chancery scribes, and notaries are declared victims: the standard's sole effect on their situation is negative reclassification without any change in their actual linguistic performance. Vernacular-educated notaries receive the lowest exit_options rating (trapped) because unlike beneficiaries with institutional mobility, they lack any realistic path to the manuscript-based training the standard requires — their directionality sits closest to full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine attested discontinuity between Classical and medieval usage — is real and was live at the standard's founding. But whether that founding problem justifies wholesale delegitimization of subsequently-developed, functionally adequate registers is exactly the site of mandatrophy risk: the standard's own beneficiaries attest the founding problem remains urgent (an easy claim to make when urgency justifies one's own institutional position), while outside observers (historians of language) corroborate the discontinuity but not the delegitimization inference. The disappearance_verdict of world_rearranges (rather than world_unchanged) signals this is NOT a dead mandate merely persisting by inertia — real institutional structures (academies, credentialing, publishing markets) depend on it, which is why this reading computes as tangled_rope rather than mountain or piton: it has a genuine coordination kernel (textual fidelity is a real and checkable thing) wrapped in asymmetric extraction (delegitimizing functioning alternative registers) sustained by active enforcement (credentialing gates, institutional hiring, curriculum control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_discontinuity_vs_constructed_break,
    'Is the discontinuity between Classical and medieval Latin usage a genuine linguistic fact recoverable by philology, or is the DEGREE of discontinuity asserted by humanist reconstructionists itself partly a rhetorical construction serving the interests of a new credentialing class?',
    'Comparative corpus linguistics measuring actual continuity/discontinuity in morphology, syntax, and lexicon between well-attested Classical texts and medieval administrative/ecclesiastical Latin, independent of humanist scholarly framing; cross-check against historians of language operating outside humanist institutional incentives.',
    'If discontinuity is substantially overstated, the reconstruction reading''s coordination claim weakens relative to its extraction, pushing the computed type toward snare; if discontinuity is as severe as claimed, the coordination function is more genuinely load-bearing and the tangled_rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_discontinuity_vs_constructed_break, empirical, 'Whether the reconstruction reading''s discontinuity claim is empirically accurate or partly self-serving.').

omega_variable(
    kernel_reading_selection_evidence,
    'What historical and institutional evidence favors treating the reconstruction reading as the operative reading of the classical_latin_standard kernel, rather than the continuity or hybrid readings, for a given institutional context (e.g. a 15th-century Italian humanist academy vs. a contemporary papal chancery)?',
    'Institutional-history analysis of which reading each named academy, chancery, or educational body actually enforced, and whose interests that enforcement served, cross-referenced against the humanist self-narrative claiming universal applicability.',
    'If the reconstruction reading was locally dominant in humanist academic contexts but the hybrid or continuity readings remained operative in ecclesiastical/administrative contexts throughout the same period, that would confirm the kernel genuinely supports multiple coexisting readings rather than reconstruction superseding the others universally — supporting the coexists_with relation over any foreclosure claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'What grounds the choice to read this kernel via reconstruction rather than continuity or hybrid framings in a given institutional setting.').

omega_variable(
    beneficiary_class_permanence,
    'Does the humanist philological gatekeeping class established by this standard eventually stabilize into ordinary academic specialization (a Rope-like steady state), or does it persist indefinitely as an extraction mechanism against non-credentialed practitioners?',
    'Longitudinal tracking of credentialing barriers and institutional hiring practices in Latin-adjacent fields over subsequent centuries; check whether vernacular/practice-based Latin competence ever regains institutional legitimacy or whether the gate hardens.',
    'If the gate softens over time as classical training diffuses broadly, the tangled_rope classification may drift toward rope; if it hardens into permanent credentialing barriers, it may drift toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_permanence, empirical, 'Whether the humanist credentialing gate is a transitional feature or a permanent extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__reconstruction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__reconstruction_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__reconstruction_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__reconstruction_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__reconstruction_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__reconstruction_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__reconstruction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__reconstruction_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__reconstruction_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__reconstruction_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__reconstruction_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__reconstruction_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__reconstruction_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__reconstruction_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__reconstruction_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the natural-language concept 'correct Latin' per the ε-invariance principle: this story (reconstruction_reading) is the most extractive member, with a manufactured victim class (medieval-trained practitioners) absent from continuity_reading and only partly present in hybrid_reading. All three share the classical_latin_standard kernel but instantiate structurally distinct constraints with distinct ε values, distinct beneficiary/victim sets, and (likely) distinct computed types — do not average or merge them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
