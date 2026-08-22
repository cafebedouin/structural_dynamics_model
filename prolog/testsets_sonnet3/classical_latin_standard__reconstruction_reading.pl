% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Humanist Reconstruction Standard for Correct Latin
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This constraint captures the reconstruction reading of the contested
 *   'correct Latin' kernel: the humanist philological position that Classical
 *   (roughly Ciceronian) Latin, recoverable only through manuscript
 *   archaeology and textual comparison, is the sole legitimate standard, and
 *   that a millennium of continuous liturgical, administrative, and
 *   pedagogical Latin usage constitutes 'corruption' to be corrected rather
 *   than legitimate development. The standard emerged from genuine scholarly
 *   observation of divergence but hardened into an institutional apparatus
 *   that redistributes prestige and employment from practice-trained Latin
 *   users toward a new credentialed philological elite and the printing
 *   economy built around them. Two sibling readings of the same kernel —
 *   continuity_reading (living practice as the standard) and hybrid_reading
 *   (textual fidelity plus legitimate technical/ecclesiastical development) —
 *   are NOT part of this story; they are separate constraints with their own
 *   ε values, linked here only for network and commentary purposes.
 *
 * KEY AGENTS:
 *   - humanist_philologists: agenda_setter/beneficiary (institutional/arbitrage) — sets and profits from the correction criteria
 *   - classical_academies: beneficiary (institutional/arbitrage) — builds prestige on the standard
 *   - renaissance_printing_houses: beneficiary (organized/mobile) — commercializes corrected editions
 *   - medieval_trained_clergy: payer (moderate/trapped) — devalued practice-competence
 *   - notarial_and_chancery_scribes: payer (moderate/constrained) — technical Latin delegitimized
 *   - provincial_grammar_teachers: payer (powerless/trapped) — curriculum obsolescence
 *   - vernacular_ecclesiastical_writers: payer (powerless/trapped) — locally functional Latin devalued
 *   - philological_review_councils: observer (institutional/analytical) — later adjudicators of the standard's justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.71).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Humanist Reconstruction Standard for Correct Latin").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '53fda077-6d33-4987-82d5-df409222ce6c').
narrative_ontology:cs_kernel_codification('53fda077-6d33-4987-82d5-df409222ce6c', fixed_text).
narrative_ontology:cs_authority_grounding('53fda077-6d33-4987-82d5-df409222ce6c', expertise).
narrative_ontology:cs_interpretation_layer_present('53fda077-6d33-4987-82d5-df409222ce6c').
narrative_ontology:cs_reading_relation('53fda077-6d33-4987-82d5-df409222ce6c', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('53fda077-6d33-4987-82d5-df409222ce6c', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('53fda077-6d33-4987-82d5-df409222ce6c', foundational, textual_exemplars_are_sole_legitimate_evidence).
narrative_ontology:cs_axiom_status(textual_exemplars_are_sole_legitimate_evidence, holdable).
narrative_ontology:cs_axiom_grounding('53fda077-6d33-4987-82d5-df409222ce6c', textual_exemplars_are_sole_legitimate_evidence, conventional).
narrative_ontology:cs_axiom('53fda077-6d33-4987-82d5-df409222ce6c', foundational, discontinuous_rupture_can_ground_legitimate_authority).
narrative_ontology:cs_axiom_status(discontinuous_rupture_can_ground_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('53fda077-6d33-4987-82d5-df409222ce6c', discontinuous_rupture_can_ground_legitimate_authority, instrumental).
narrative_ontology:cs_reference_frame('53fda077-6d33-4987-82d5-df409222ce6c', ciceronian_classical_exemplar).
narrative_ontology:cs_drift_state('53fda077-6d33-4987-82d5-df409222ce6c', post_critical_edition_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53fda077-6d33-4987-82d5-df409222ce6c', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_academies).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, renaissance_printing_houses).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_trained_clergy).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, notarial_and_chancery_scribes).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, provincial_grammar_teachers).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, vernacular_ecclesiastical_writers).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, textual_priority_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, ciceronian_purity_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in collating manuscripts and comparing usage against Cicero, Caesar, and Virgil, they set the criteria by which any given Latin usage is judged 'correct' or 'corrupt.' Their expertise is the scarce credential the standard requires, so tightening the standard increases demand for their teaching, editing, and correction services. They can move between courts, universities, and printing houses that all want their imprimatur.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, humanist_philologists, beneficiary).

% New humanist schools and academies build their entire curriculum and prestige around teaching the reconstructed Classical norm. They recruit students and patrons on the promise of restoring 'true' Latin, and their institutional standing rises as medieval Latin is reclassified as barbarism to be corrected.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_academies, beneficiary,
    institutional, generational, arbitrage, continental).

% Print and sell corrected editions of Classical texts, grammars, and dictionaries built around the reconstructed standard. Every text previously copied in medieval Latin becomes a candidate for a new, purchasable, 'corrected' edition, creating a recurring commercial market out of the delegitimization of the old forms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, renaissance_printing_houses, beneficiary,
    organized, biographical, mobile, continental).

% Spent years mastering the Latin of the liturgy, canon law, and scholastic disputation as it was actually practiced and transmitted. Under the reconstruction standard, this hard-won competence is publicly reclassified as ignorant corruption. They cannot simply relearn Classical Latin at their age or within their institutional role without incurring humiliation and retraining cost, and their sermons, glosses, and administrative Latin are now cited as evidence of decline.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_trained_clergy, payer,
    moderate, biographical, trapped, regional).

% Draft legal, administrative, and diplomatic documents in a functional Latin evolved for precision in contracts and record-keeping. The reconstruction standard treats their technical vocabulary and syntax as barbarisms, threatening their employability wherever employers begin demanding humanist-trained secretaries instead.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, notarial_and_chancery_scribes, payer,
    moderate, biographical, constrained, regional).

% Taught generations of students the grammar manuals descended from late-antique and medieval tradition. As the reconstructed standard spreads from urban humanist centers, their curriculum is declared obsolete and their own Latin marked as substandard, undermining their livelihood without any path to retrain that does not require relocating to a center of humanist learning.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, provincial_grammar_teachers, payer,
    powerless, biographical, trapped, local).

% Compose devotional and administrative texts in the living ecclesiastical Latin of their communities, a form intelligible and functional for its local audience. The reconstruction standard has no use for local intelligibility as a criterion of correctness, so their entire output is retroactively devalued regardless of its communicative success.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, vernacular_ecclesiastical_writers, payer,
    powerless, biographical, trapped, local).

% The doctrine that written Classical exemplars, not living practice, are the sole legitimate arbiter of correctness. It is vindicated by every act of correction performed under this standard, though it collects no rents itself — it is the justification structure the beneficiary class invokes.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, textual_priority_doctrine, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__reconstruction_reading, textual_priority_doctrine).

% Later editorial and academic bodies that adjudicate disputes about which manuscript readings are authoritative and periodically revisit whether the standard's exclusions were philologically justified or served institutional interests. They can commission comparative textual studies but do not administer the standard day to day.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, philological_review_councils, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, textually-anchored reference point for 'correct' Latin so that scholars across regions and generations can converge on shared norms of usage, citation, and pedagogy rather than each locality maintaining its own drifted standard.
% TRANSFER_FUNCTION: Moves prestige, employment, and institutional legitimacy away from practice-trained Latin users (clergy, scribes, teachers) toward humanist-trained philologists and the schools, print shops, and patrons aligned with the reconstructed standard.
% ABSENT_VOICES: Medieval grammarians and the compilers of the very manuals now being displaced are dead and cannot answer the charge of 'corruption'; living practitioners whose Latin is functional but non-Classical are rarely invited into the philological councils that set the standard, since participation itself requires the credential the standard privileges.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished, the humanist academies' distinguishing claim to prestige collapses, the market for 'corrected' printed editions shrinks sharply, and medieval-trained clergy and scribes would no longer face systematic devaluation of their working competence — institutional Latin usage would likely re-converge toward whatever form best served local communicative function.
% FOUNDING_PROBLEM: Humanist scholars observed genuine divergence between Ciceronian usage and the Latin actually used in medieval administration, liturgy, and scholarship, and sought a stable, textually-verifiable reference point instead of an uncontrolled patchwork of regional drift.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists and academies themselves attest the founding problem (uncontrolled corruption) as ongoing and urgent. Outside the beneficiary set, later comparative linguists and historians of the medieval Latin tradition attest that medieval Latin was not disordered decay but a functionally coherent, regionally stable register serving its own communicative purposes — corroboration for the 'dead as decay-narrative, live only as institutional prestige claim' reading comes from this external historiographical literature, not from the humanist tradition itself.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.32 to 0.71 across the interval as the reconstruction standard moves from a scholarly proposal to an institutionally enforced credentialing system — the same trajectory as suppression (0.40 to 0.78), reflecting the increasing delegitimization of alternative Latin forms as printing and academy networks scale. Theater ratio climbs more modestly (0.20 to 0.42): a real philological coordination function persists (textual comparison genuinely resolves some usage disputes), but an increasing share of correction activity performs prestige-signaling ('purity' policing) rather than resolving actual communicative ambiguity. Accessibility collapse (0.62) is high but not near-mountain levels because functional alternatives (medieval Latin, vernacular ecclesiastical Latin) persist in practice even as they are delegitimized in prestige terms — the collapse is normative/institutional, not logical or physical. Resistance (0.58) reflects genuine pushback from displaced practitioners, though asymmetric power limits its effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist philologist's seat, the standard is restorative coordination: recovering a lost, superior order from chaos. From the medieval clergy or provincial teacher's seat, the same standard is an imposed reclassification of their competence as ignorance, enforced by institutions they cannot access. The engine computes these divergent per-seat classifications from the structural power/exit data; the claim (tangled_rope) reflects the analytical synthesis, not either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and the academies/printers aligned with them sit near the full-beneficiary end: they set the criteria, control the credentialing apparatus, and capture the commercial and institutional rents of 'correction.' Medieval clergy, scribes, teachers, and vernacular writers sit near the full-target end: their existing competence is retroactively reclassified as deficient by a standard they had no role in setting, and their exit options are narrow because relocation to humanist centers or full retraining is often infeasible given age, institutional role, or resources. The doctrine itself (textual_priority_doctrine) is listed as a non-agent beneficiary — it is vindicated by the standard's operation but collects no rents directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled regional divergence in Latin usage) was a real coordination problem at the outset, which is why this constraint is authored as tangled_rope rather than pure snare — there is a genuine coordination function (a stable textual reference point) alongside the asymmetric extraction. But the founding_problem_status is contested: outside historiography suggests medieval Latin was not disordered decay but a coherent, functionally adequate register, meaning the 'corruption' framing was partly retrofitted to justify an emerging philological gatekeeping class. Treating this as tangled_rope rather than snare prevents both over-reading it as pure invented extraction (there was a real divergence problem) and under-reading it as innocent coordination (the standard's persistence depends on active suppression of alternative legitimacy claims, and it names an identifiable beneficiary class).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_development_framing,
    'Is medieval Latin''s divergence from Classical norms best understood as corruption (decay from a superior original) or as legitimate linguistic development (natural evolution of a living language)?',
    'Comparative historical linguistics assessing whether medieval Latin functioned as a stable, internally coherent register serving its communicative context, versus evidence of genuine communicative breakdown or internal incoherence attributable to the divergence itself.',
    'If medieval Latin was coherent and functional, the reconstruction standard''s founding justification is substantially a retrofitted cover story for an emerging credentialing class, strengthening the tangled_rope/extraction reading. If genuine incoherence is documented, the coordination justification is stronger and closer to the story''s own framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_development_framing, conceptual, 'Whether the standard''s ''corruption'' framing is empirically grounded or retrofitted.').

omega_variable(
    kernel_reading_locus_of_disagreement,
    'Across the three readings of the classical_latin_standard kernel, is the disagreement located in what counts as evidence for correctness (textual vs. practice-based), or in whether discontinuity itself is legitimate (rejecting vs. incorporating medieval development)?',
    'Structural comparison of the three readings'' axioms: identify whether the continuity_reading and hybrid_reading reject the evidentiary primacy of texts, or instead accept textual evidence but weight practice-based transmission as co-authoritative.',
    'If the disagreement is purely evidentiary, the readings might in principle be reconciled by better textual-practice synthesis (supporting hybrid_reading as a genuine middle path). If the disagreement is about the legitimacy of discontinuity itself, no evidentiary advance resolves it — the readings reflect a values conflict about whether historical rupture can create legitimate authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_disagreement, conceptual, 'Where exactly the reconstruction, continuity, and hybrid readings diverge structurally.').

omega_variable(
    credentialing_class_permanence,
    'Once established, does the humanist philological gatekeeping class persist even if the original textual-recovery project is largely completed and stable?',
    'Longitudinal institutional analysis: track whether philological credentialing requirements loosen once a stable critical edition consensus is reached, or whether the credentialing apparatus finds new correction targets to sustain its function.',
    'If the apparatus persists past its founding problem''s resolution by finding new targets, this supports reclassification toward piton (inertial gatekeeping) rather than tangled_rope (live coordination-plus-extraction) in later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_class_permanence, empirical, 'Whether the philological gatekeeping function outlives its founding textual-recovery problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__reconstruction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__reconstruction_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(clas_tr_t120, classical_latin_standard__reconstruction_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(clas_tr_t160, classical_latin_standard__reconstruction_reading, theater_ratio, 160, 0.41).
narrative_ontology:measurement(clas_tr_t200, classical_latin_standard__reconstruction_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__reconstruction_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__reconstruction_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(clas_be_t120, classical_latin_standard__reconstruction_reading, base_extractiveness, 120, 0.66).
narrative_ontology:measurement(clas_be_t160, classical_latin_standard__reconstruction_reading, base_extractiveness, 160, 0.7).
narrative_ontology:measurement(clas_be_t200, classical_latin_standard__reconstruction_reading, base_extractiveness, 200, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__reconstruction_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__reconstruction_reading, suppression_requirement, 80, 0.66).
narrative_ontology:measurement(clas_su_t120, classical_latin_standard__reconstruction_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(clas_su_t160, classical_latin_standard__reconstruction_reading, suppression_requirement, 160, 0.76).
narrative_ontology:measurement(clas_su_t200, classical_latin_standard__reconstruction_reading, suppression_requirement, 200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the single natural-language label 'correct Latin' per the ε-invariance principle: reconstruction_reading (this story, tangled_rope, high extraction via credentialing class formation), continuity_reading (living-practice standard, expected lower extraction, likely rope or piton depending on institutional capture), and hybrid_reading (textual-plus-technical-development synthesis, expected moderate extraction as a negotiated middle position). Each carries its own ε, beneficiary/victim structure, and classification; they are linked via network edges rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
