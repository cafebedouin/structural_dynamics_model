% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Continuity Doctrine of Correct Latin (Internal-Correction Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   Between the Carolingian reform of letters and the humanist challenge, the
 *   learned West operated under a continuity settlement: written Latin was
 *   held to be one language developing lawfully from its classical ancestor,
 *   so correct Latin meant conformity to the transmitted grammatical
 *   tradition (Donatus, Priscian, and their gloss), and the repair of damaged
 *   texts meant correction by the language's own analogical principles rather
 *   than importation of antique forms. This story authors that settlement as
 *   the continuity_reading of the correct_latin_kernel: medieval innovations
 *   count as legitimate developments, and the humanist program of recovering
 *   a classical standard counts as prescriptive purism. The claim/metrics
 *   division is deliberate: the claimed type states what the structure looks
 *   like from this reading's seat, while the metrics describe the
 *   settlement's actual operation across the interval, including the
 *   authority rents and defensive enforcement that accumulated inside it.
 *   Sibling readings are separate files; their deltas are recorded in the
 *   omegas and the network note.
 *
 * KEY AGENTS:
 *   - latin_grammatical_masters: Agenda-setter with a collecting second seat (organized/constrained) - administers the corrective standard and collects the deference, fees, and final-word authority it generates
 *   - ecclesiastical_chanceries: Primary beneficiary (institutional/constrained) - house Latin validated without any classicizing cost
 *   - monastic_scriptoria: Secondary beneficiary (organized/constrained) - copying-and-correction conventions legitimated down the copy chain
 *   - scholastic_theologians: Identity-fused beneficiary (organized/identity_locked) - technical coinages certified as lawful growth of the language
 *   - classical_recovery_advocates: Challenger-payer (moderate/constrained) - program ruled out of order as purism by the bench it must persuade
 *   - provincial_notaries_and_lower_clergy: Diffuse payer (powerless/trapped) - bear correction costs with no voice in fixing the norm
 *   - vernacular_literates: Excluded rival (moderate/mobile) - would contest the standard's monopoly but stand outside the conversation
 *   - modern_historical_linguists: Analytical observer (analytical/analytical) - sees the settlement's descriptive core and its authority-serving surplus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.42).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Continuity Doctrine of Correct Latin (Internal-Correction Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, 'f73111a3-f5a4-42ff-8a4c-4a3905e62bd2').
narrative_ontology:cs_kernel_codification('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', fixed_text).
narrative_ontology:cs_authority_grounding('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', lineage).
narrative_ontology:cs_interpretation_layer_present('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2').
narrative_ontology:cs_reading_relation('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', foundational, learned_latin_one_continuous_system).
narrative_ontology:cs_axiom_status(learned_latin_one_continuous_system, holdable).
narrative_ontology:cs_axiom_grounding('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', learned_latin_one_continuous_system, empirically_contingent).
narrative_ontology:cs_axiom('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', secondary, reconstruction_is_internal_correction).
narrative_ontology:cs_axiom_status(reconstruction_is_internal_correction, holdable).
narrative_ontology:cs_axiom_grounding('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', reconstruction_is_internal_correction, instrumental).
narrative_ontology:cs_reference_frame('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', unbroken_living_grammatical_tradition).
narrative_ontology:cs_drift_state('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', humanist_philological_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f73111a3-f5a4-42ff-8a4c-4a3905e62bd2', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, latin_grammatical_masters).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, ecclesiastical_chanceries).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, monastic_scriptoria).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, scholastic_theologians).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, classical_recovery_advocates).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, provincial_notaries_and_lower_clergy).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, priscian_grammatical_authority).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, internal_analogy_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach Donatus and Priscian as the living norm of written Latin, examine candidates for orders and offices, and pronounce on disputed usages in schools and scriptoria. Their office, income, and standing exist inside the transmitted curriculum; leaving it would mean abandoning the discipline that constitutes their position. They collect fees, deference, and the final word on correctness.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, latin_grammatical_masters, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, latin_grammatical_masters, beneficiary).

% Produce most of Europe's charters, bulls, and administrative records in Latin shaped by house practice rather than classical models. The doctrine of unbroken development spares them the cost of rewriting their registers to an antique measure and shields their archives from charges of corruption. Adopting an external standard would devalue centuries of records and require retraining entire staffs.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, ecclesiastical_chanceries, beneficiary,
    institutional, generational, constrained, continental).

% Copy, correct, and annotate manuscripts under rules handed down within the cloister. Corrections made by internal analogy accumulate authority as they pass down the generations of copies. They hold few of the classical books a recovery program would treat as touchstones, so a standard drawn from such texts would place judgment permanently beyond their walls.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, monastic_scriptoria, beneficiary,
    organized, generational, constrained, regional).

% Write a technical Latin thick with coined terms - essentia, quidditas, haecceitas - and compressed argumentative syntax. Certification of these coinages as lawful growth rather than decay comes from the continuity doctrine. Their careers, disputations, and self-understanding are built in this register; writing otherwise would mean ceasing to speak their discipline.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, scholastic_theologians, beneficiary,
    organized, biographical, identity_locked, continental).

% Hunt classical manuscripts, measure current usage against Cicero, Livy, and Virgil, and propose emendations on antiquarian grounds. Inside the reigning regime their proposals arrive pre-refuted: judged purism, affectation, or failure to grasp the language's lawful development. They cannot take their case to the vernacular without losing the learned audience the case addresses.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_recovery_advocates, payer,
    moderate, biographical, constrained, continental).

% Draw up wills, charters, and parish records in Latin learned thinly and locally. Metropolitan examiners flag their spellings and constructions as barbarisms; they bear the cost of correction yet have no voice in fixing the norm, and their work cannot function in any other written medium.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, provincial_notaries_and_lower_clergy, payer,
    powerless, immediate, trapped, local).

% Poets and clerks writing in Tuscan, French, and German argue that the mother tongue can carry learned and literary weight - Dante's De vulgari eloquentia is the program statement. They stand outside the conversation that defines written Latin correctness, while their success steadily shrinks the territory that conversation governs.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, vernacular_literates, excluded,
    moderate, generational, mobile, regional).

% Reconstruct the actual development of Latin into the Romance languages and the learned register's real continuities and ruptures from the documentary record. They can see what the continuity doctrine got right, where it served office rather than description, and what the humanist recovery genuinely restored. They hold no stake in either side's victory.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, latin_grammatical_masters).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single writable and readable standard of learned Latin across a continent whose spoken varieties had diverged beyond mutual comprehension, so that records, instruction, and manuscripts remain portable from one end of Latin Christendom to the other.
% TRANSFER_FUNCTION: Moves correction authority and linguistic prestige toward the transmitting institutions - schools, chanceries, scriptoria - and moves compliance costs outward: peripheral writers conform their usage, challengers pay entry costs to a bench staffed by their rivals, and texts corrected under the tradition carry its imprimatur back down the copy chain.
% ABSENT_VOICES: Vernacular literates with a rival account of what a supralocal standard should be; Greek-speaking scholars whose ancient texts could serve as external checks, largely unreachable in the West before the fifteenth century; lay readers of charters and records, who consume the standard's output with no seat in defining it.
% DISAPPEARANCE_RATIONALE: Overnight loss of the shared corrective standard would fracture learned communication within a generation: chancery practice would regionalize, university instruction would lose its common textbook language, and manuscript transmission would splinter into mutually hardening local written varieties - the humanist recovery program would have nothing continental left to recover.
% FOUNDING_PROBLEM: After the western empire's political fragmentation and the divergence of spoken Romance, learned writing needed a stable supralocal standard; the grammatical tradition answered by treating Latin as one continuous language whose rules the schools preserved and applied.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the humanists themselves - the settlement's fiercest critics - kept writing Latin for everything that mattered, which concedes the supralocal-medium problem was real; Dante's De vulgari eloquentia argues the need for a supra-regional literary medium while disputing which language supplies it. No party in the dispute denies the founding problem; they divide over which standard solves it.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.42: the settlement delivered a real service - one writable, readable standard for a continent whose speech had diverged past mutual comprehension - but the service carried a growing surcharge: correction authority concentrated in the schools, chancery and cloister output gained immunity from antiquarian criticism, and rival programs paid entry costs set by their competitors' judges. Suppression (0.58) is structural rather than incidental: the standard persisted because the machinery enforcing it - curriculum, examination, the expanding catalogue of barbarisms - actively closed the space where an antiquarian standard could be argued, not because alternatives failed on their merits. Theater_ratio climbs from 0.15 to 0.45: early correction compared witnesses and restored sense; by the fourteenth century much correction was citation ritual, appealing to Priscian's authority to ratify whatever the house style already did. Accessibility_collapse stays moderate (0.40): the vernaculars rose beside Latin, classical fragments circulated, and a determined writer could reach several registers - the standard never approached the totality of a natural law. Resistance averages 0.52 and spikes at interval end as the Italian humanists mount the first organized challenge. All three series share one seven-point grid (t=0..600, roughly 800-1400 CE) so no metric is sampled against another's gaps; suppression_requirement is tracked because the story's enforcement picture genuinely changes - the machinery hardened defensively as the challenge grew, expanding the barbarism catalogue and tightening examination precisely when compliance was eroding.
 *
 * PERSPECTIVAL GAP:
 *   From the masters' seat the settlement is simply what learning is: there is one Latin, the grammarians preserve it, and correction is obedience to it. From the chancery and scriptorium seats it is insurance - their ordinary output passes as the language itself. From the notary's seat it is a levy imposed by people he will never meet on the only writing he can do. From the recovery advocate's seat it is a closed court in which his evidence is inadmissible by rule. Same nominal literacy, four different arrangements - the divergence is driven entirely by position relative to the standard-setting machinery and by what each seat's career has fused with, not by any difference in what the written language actually is.
 *
 * DIRECTIONALITY LOGIC:
 *   The four beneficiary-declared groups sit near the subsidized pole: masters lowest (they set the rules and collect from them), chanceries and scriptoria close behind (validated output, no classicizing cost), theologians somewhat higher (they gain certification but also absorb the standard's rigidity as their prose ages badly). The two victim-declared groups sit near the target pole: notaries highest - powerless, trapped, taxed without representation; recovery advocates nearly as high - moderate power blunted by constrained exit, since the vernacular route forfeits the learned audience their program addresses. Vernacular literates are excluded rather than placed: their exclusion is the enforcement object's shadow, not a position inside the derivation. Modern historical linguists sit at the analytical pole with no directional stake. Gain_flow names the masters because the settlement's scarcest product - the authority to say what the language is - demonstrably accrues to their seat; the chanceries and scriptoria receive validation of output they would produce anyway.
 *
 * MANDATROPHY ANALYSIS:
 *   The settlement was born solving a live problem - after spoken Romance diverged, learned writing needed a supralocal norm, and the transmitted grammar supplied one at low overhead. That founding problem never died inside the interval, so there is no mandate corpse to bury, and the classification resists both misreadings the period invites. Reading the settlement as pure coordination hides the rents (masters' authority, chancery immunity, the suppression of the antiquarian program); reading it as pure predation hides the service (without the shared norm, no charter reads in Palermo that was written in York). The tangled_rope claim holds both truths. The theater series raises the inertial-shell question - by 1400 much correction was performance - but the function never fully atrophied before external displacement arrived, so the honest verdict is degraded-but-operative rather than vestigial. Had the interval run past 1450, the residual continuity apparatus after the humanist takeover would be a strong candidate for the inertial category: administered, defended, and no longer the operative standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    descriptive_vs_prescriptive_continuity,
    'Is the continuity doctrine primarily a description of linguistic reality (learned Latin genuinely one evolving system) or an instrument protecting the authority of the transmitting institutions?',
    'Correlate the grammatical tradition''s norms against dated documentary usage: where the tradition tracks living usage it is descriptive; where it condemns widespread competent usage to defend curricular authority it is prescriptive.',
    'If predominantly prescriptive, effective extraction exceeds the authored base and the arrangement trends toward the snare end of the hybrid range; if descriptive, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_vs_prescriptive_continuity, empirical, 'Whether the continuity doctrine describes linguistic fact or defends institutional authority.').

omega_variable(
    kernel_sibling_polarity_flip,
    'This story instantiates the continuity_reading of correct_latin_kernel; what structural elements would change under the sibling readings, and where exactly does the disagreement sit?',
    'Author discontinuity_reading and hybrid_reading as separate stories: discontinuity_reading relocates credit to the humanist recovery (classical_recovery_advocates flip from suppressed challengers to agents of repair) and re-bases epsilon on a loss-and-recovery arrangement; hybrid_reading splits the challenger seat between classicizing emenders and continuity defenders and layers the warrant for reconstruction. The disagreement is located in the identity claim - whether medieval and classical Latin are one system - from which the method claim (internal correction versus textual reoccupation) follows.',
    'Classifications are not comparable across readings; adopting a sibling flips the polarity of the challenger seat and changes which standing arrangement epsilon describes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_polarity_flip, conceptual, 'Committer structure: one reading of correct_latin_kernel among three, with polarity-flipping siblings.').

omega_variable(
    internal_correction_circularity,
    'Can correction by internal analogy converge on authorial wording, or does it converge on the correcting tradition''s own norms - repairing the text into the tradition''s image?',
    'Collate internally corrected witnesses against independently established archetypes: measure whether corrections cluster on tradition-marked forms regardless of what the archetype reads.',
    'If circular, the doctrine''s repair function partially fails at its own declared task and the theater_ratio understates dysfunction; if not, internal correction is a genuine repair mechanism and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_correction_circularity, empirical, 'Circularity risk of internal-analogy textual correction.').

omega_variable(
    terminal_phase_disposition,
    'Was the late-interval continuity apparatus drifting toward inertial maintenance - function persisting mainly as performance - before the humanist displacement, or did its correction function remain substantively operative to the end?',
    'Compare the functional yield of fourteenth-century correction campaigns (sense-restoring emendations per manuscript) against earlier centuries, controlling for differential manuscript survival.',
    'A steep functional decline supports a piton-drift counterfactual had the humanist displacement not intervened; a flat profile supports persistence as a live hybrid arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_phase_disposition, empirical, 'Whether the tradition was drifting inertial prior to external displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__continuity_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__continuity_reading, theater_ratio, 200, 0.24).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__continuity_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement(corr_tr_t400, correct_latin_kernel__continuity_reading, theater_ratio, 400, 0.36).
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__continuity_reading, theater_ratio, 500, 0.41).
narrative_ontology:measurement(corr_tr_t600, correct_latin_kernel__continuity_reading, theater_ratio, 600, 0.45).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__continuity_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__continuity_reading, base_extractiveness, 200, 0.33).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__continuity_reading, base_extractiveness, 300, 0.37).
narrative_ontology:measurement(corr_be_t400, correct_latin_kernel__continuity_reading, base_extractiveness, 400, 0.39).
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__continuity_reading, base_extractiveness, 500, 0.41).
narrative_ontology:measurement(corr_be_t600, correct_latin_kernel__continuity_reading, base_extractiveness, 600, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__continuity_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__continuity_reading, suppression_requirement, 200, 0.44).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__continuity_reading, suppression_requirement, 300, 0.49).
narrative_ontology:measurement(corr_su_t400, correct_latin_kernel__continuity_reading, suppression_requirement, 400, 0.53).
narrative_ontology:measurement(corr_su_t500, correct_latin_kernel__continuity_reading, suppression_requirement, 500, 0.56).
narrative_ontology:measurement(corr_su_t600, correct_latin_kernel__continuity_reading, suppression_requirement, 600, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes into three structurally distinct constraints corresponding to the three readings of correct_latin_kernel. Each reading assigns a different epsilon referent (the standing arrangement it takes Latin's history to be), a different beneficiary/victim polarity, and a different warrant for reconstruction. This file instantiates the continuity_reading; the upstream continuity claim is typically cited as evidence within the hybrid reading's morphology layer, while the discontinuity reading cites the same documentary record against it. Cross-file deltas are documented in omega kernel_sibling_polarity_flip.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
