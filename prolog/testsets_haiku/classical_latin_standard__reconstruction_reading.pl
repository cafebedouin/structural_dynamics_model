% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Classical Latin Standard (Reconstruction Reading)
 *   domain: institutional/educational/intellectual
 *
 * SUMMARY:
 *   The reconstruction reading of the Classical Latin standard kernel asserts
 *   that correct Latin is the form recoverable from surviving Classical texts
 *   through philological archaeology, and that medieval development should be
 *   rejected as corrupted drift. This reading was instantiated by Renaissance
 *   humanists beginning in the 14th century and became institutionally
 *   dominant by the early 17th century. The constraint operates by
 *   systematically delegitimizing medieval forms and training—what had been
 *   living, functional practice becomes classified as error. The reading
 *   forecloses the continuity reading (which holds that practice-based
 *   transmission is a legitimate source of correctness) within any single
 *   institutional framework, because the two readings make contradictory
 *   claims about what counts as a valid source of linguistic authority.
 *
 * KEY AGENTS:
 *   - humanist_philologists: institutional agenda-setters controlling the philological method and certification of correctness; beneficiaries of the new gatekeeping structure.
 *   - medieval_ecclesiastical_users: powerful but constrained payers bearing the cost of delegitimization; cannot exit without disrupting inherited institutional texts and liturgy.
 *   - institutional_practitioners_trained_in_medieval_forms: identity-locked payers whose professional standing collapses once their fluency is redefined as incompetence.
 *   - emerging_humanist_students: mobile beneficiaries positioned to monopolize high-status roles under the new standard.
 *   - manuscript_custodians: observers who hold the material basis of the philological standard.
 *   - vernacular_language_advocates: structurally excluded from contesting the assumption that Latin must remain authoritative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.79).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.82).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Standard (Reconstruction Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "institutional/educational/intellectual").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9').
narrative_ontology:cs_kernel_codification('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', fixed_text).
narrative_ontology:cs_authority_grounding('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', extraction).
narrative_ontology:cs_interpretation_layer_present('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9').
narrative_ontology:cs_reading_relation('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', foundational, correctness_derived_from_classical_textual_recovery).
narrative_ontology:cs_axiom_status(correctness_derived_from_classical_textual_recovery, holdable).
narrative_ontology:cs_axiom_grounding('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', correctness_derived_from_classical_textual_recovery, empirically_contingent).
narrative_ontology:cs_axiom('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', foundational, medieval_development_constitutes_corruption_not_legitimate_change).
narrative_ontology:cs_axiom_status(medieval_development_constitutes_corruption_not_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', medieval_development_constitutes_corruption_not_legitimate_change, conventional).
narrative_ontology:cs_reference_frame('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', classical_textual_authority_recovered).
narrative_ontology:cs_drift_state('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', contemporary_humanist_ascendancy_1550, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60b8ff70-3d0a-4cd5-98f2-5023f8bb7ea9', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_educational_institutions).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_ecclesiastical_users).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, institutional_practitioners_trained_in_medieval_forms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, institutional_practitioners_trained_in_medieval_forms).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, emerging_humanist_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Philological scholars trained in textual reconstruction and Classical manuscript interpretation. They set the standard for 'correct' Latin by recovering Classical forms from surviving texts, declaring medieval usage corrupted and illegitimate. They control the certification of Latinity through university curricula, printed grammars, and scholarly networks. The recovery method itself becomes a credential—only those trained in philological method can adjudicate correctness.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Church and monastic institutions whose Latin practice evolved over centuries to serve liturgical, theological, and administrative functions. Their forms—ablative constructions, gerundive usage, neologisms for Christian concepts—are now labeled 'incorrect' and 'corrupted' despite serving their domain coherently. They cannot simply abandon their institutional Latin without disrupting centuries of inherited texts, liturgy, and scholarly tradition. Exit means accepting systematic delegitimization or investing heavily in retraining.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_ecclesiastical_users, payer,
    powerful, generational, constrained, continental).

% Scribes, copyists, theologians, and administrators trained in living medieval Latin traditions who encounter their fluency suddenly redefined as incompetence. Their professional identity and authority—rooted in mastery of the forms they practiced—collapses once the humanist standard takes hold. They bear both the cost of retraining and the loss of social standing. Some benefit from the new standard if they adopt it, but most face identity-dissolution rather than reskilling opportunity.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, institutional_practitioners_trained_in_medieval_forms, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, institutional_practitioners_trained_in_medieval_forms, beneficiary).

% Younger scholars and clerks in urban centers who learn philological method early and gain social capital by mastering the newly legitimized standard. They are positioned to monopolize high-status roles in education, diplomacy, and institutional correspondence once the reconstruction standard dominates. Their mobility comes from having learned the 'correct' form first.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, emerging_humanist_students, beneficiary,
    moderate, biographical, mobile, continental).

% Librarians, archivists, and manuscript collectors who hold and preserve the texts on which the reconstruction standard depends. They are gatekeepers to the material basis of the humanist claim: without access to these manuscripts, the philological method cannot operate. They observe the constraint rather than enforce or bear it directly.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, manuscript_custodians, observer,
    organized, generational, analytical, continental).

% Early advocates for writing and intellectual work in emerging vernacular languages would argue for relaxing the Latin standard entirely or recognizing regional linguistic variation as legitimate. Their position—that Latin need not be a universal standard—is structurally excluded from the debate because the constraint operates within the premise that Latin remains authoritative.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, vernacular_language_advocates, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, reference-based standard for correct Latin by grounding legitimacy in surviving Classical texts rather than in evolving practice or doctrinal innovation. Enables scholars across regions to communicate using a shared, material-anchored form recoverable from a fixed archive rather than relying on local or institutional variants.
% TRANSFER_FUNCTION: Transfers authority and social capital from medieval institutional practitioners and church authority to humanist scholars trained in philological reconstruction. Moves the gatekeeping function from transmission-based (what the monastery taught) to artifact-based (what the texts say). Extracts acceptance of 'incorrect' status from those trained in the delegitimized forms.
% ABSENT_VOICES: Vernacular language advocates and the bulk of practicing Latin users in medieval institutions cannot articulate the case for legitimacy of institutional drift or regional variation—the constraint operates by excluding the very idea that such variation could be correct. Practitioners whose fluency is now redefined as error have little institutional platform to contest the philological judgment.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished, institutional Latin would revert to practice-based authority; medieval forms would re-stabilize as legitimate; humanist gatekeeping would collapse; pedagogy would return to transmission from master to student rather than textbook and grammar; social standing would no longer require philological training. The entire intellectual ecosystem reorganizes around which authority structure—texts or practice—is taken as legitimate.
% FOUNDING_PROBLEM: Renaissance humanists encountered a working Latin tradition that had diverged substantially from surviving Classical texts. They faced the question: is correctness defined by living practice or by fidelity to the attested Classical form? They resolved it by asserting the Classical form as the legitimate standard and the medieval practice as corruption requiring correction.
% FOUNDING_PROBLEM_CORROBORATION: Humanists attest the founding problem as an urgent need for philological purity and recovery of authentic Classical knowledge. Medieval practitioners and later scholars of medieval Latin attest that the medieval forms were functional, coherent, and legitimate developments serving real institutional needs. Modern linguistics attests that 'corruption' is a value judgment, not a structural fact—drift and development are normal linguistic processes, not failures.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).

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
 *   Extractiveness rises from 0.12 to 0.79 over 250 years as the humanist standard diffuses from university centers into institutional practice. The initial low extractiveness reflects the reading's emergence in a context where medieval forms still dominate; the steady climb tracks the systematic delegitimization of existing institutional practice and the capture of educational authority. Suppression requirement tracks extraction closely but runs 6–10 points higher throughout, reflecting the active enforcement needed to prevent reversion to practice-based authority—schools must actively police against medieval forms, churches must justify their inherited usages, practitioners must be retrained or excluded. Theater ratio stays moderate (0.08–0.41) because the philological method has a genuine epistemological function (artifacts do anchor the standard), but an increasing share of enforcement activity is pure gatekeeping (preventing medieval forms from circulating, managing credential competition) rather than knowledge production. All measurements are authored on a single shared time grid: every metric carries a value at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The humanist philologist seat experiences the constraint as genuine knowledge recovery and intellectual progress—the 'correct' form is out there in the texts, and scholarship consists in recovering it. From the medieval institutional user's perspective, the same structure is systematic delegitimization and extraction of existing authority. From the practitioner's perspective, it is identity-fusion (their professional self is rooted in mastery of the now-'incorrect' forms) followed by identity-loss. The engine computes per-seat directionality from the structural data: humanists land near d=0.0 (full beneficiary), medieval users near d=1.0 (full target), practitioners at high d with identity_locked exit amplifying extraction. This divergence is the measurement the constraint family exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists benefit from the standard by capturing gatekeeping authority and social capital; their exit options are arbitrage-grade (they can move between humanist networks and maintain status). Medieval ecclesiastical users pay the cost of delegitimization; they are constrained in exit because abandoning their inherited Latin means disrupting centuries of texts and practice. Practitioners trained in medieval forms face identity_locked exit: their professional identity was constituted through mastery of the forms now declared incorrect. The institutional practitioners' directionality is amplified toward the target end (high d, near 1.0) precisely because identity-locking prevents the kind of reskilling or arbitrage that would reduce extraction. For this constraint, d derivation chains through: (beneficiary/victim declaration) → (institutional power level) → (exit_options, especially identity_locked) → (effective directionality toward target), with amplification by scope (continental institutional diffusion makes verification/escape harder).
 *
 * MANDATROPHY ANALYSIS:
 *   The reconstruction reading instantiates genuine tangled_rope structure: it solves a real coordination problem (how to anchor Latin in a shared standard without relying on local transmission chains that diverge over time) AND it extracts heavily from medieval practitioners by delegitimizing their forms. The coordination function is not cover—humanist networks genuinely needed a material-anchored standard to communicate across regions. But the extraction is not incidental—the gatekeeping class (humanist philologists) captures authority that had belonged to ecclesiastical and monastic institutions, and they maintain it by continuously suppressing alternative sources of legitimacy (practice-based transmission, institutional custom, regional development). The suppression measurement (0.82 by 1600) is high because the constraint persists only through active enforcement: without schools teaching the Classical standard, without manuscripts being edited to elevate Classical forms, without social penalties for medieval usage, the medieval practice-based reading would re-stabilize. Mandatrophy does not apply: the founding problem (what is the legitimate source of Latin correctness?) remains live and contested across the entire interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_status_of_recovery,
    'Is the ''Classical Latin'' recovered through philological method a real historical object, or a reconstruction artifact shaped by the method''s assumptions about corruption and textual authority?',
    'Comparison of Classical forms across heterogeneous textual traditions and genres; assessment of whether the ''Classical'' form is stable across sources or is a consensus artifact of humanist method. Modern comparative historical linguistics can test whether the recovered form would have been intelligible to its own speakers or is an idealized construct.',
    'If reconstruction is a genuine historical recovery, the reading''s claim to authentic authority is strengthened. If it is method-dependent, the reading is less a return to origins than a creation of a new standard projected backward onto Classical texts. This affects the legitimacy of the suppression: suppressing medieval forms is justified by fidelity to a real standard only if that standard is genuinely recovered, not constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_status_of_recovery, empirical, 'Whether Classical Latin is a discovered historical form or a reconstruction artifact.').

omega_variable(
    committer_kernel_reading_ambiguity,
    'Is the reconstruction reading held by humanist philologists as a factual claim about which form is historically correct, or is it adopted as a strategic normative commitment to establish gatekeeping authority?',
    'Analysis of humanist texts, correspondence, and institutional decisions: do humanists claim recovery of a historical fact, or do they argue that Classical forms should be privileged for intellectual coherence and institutional discipline? Do early humanists express doubt about the recovery, and does doubt decrease as the standard becomes institutionalized?',
    'If the reading is genuinely epistemic (humanists believe they are recovering a real historical form), the gatekeeping effect is secondary to knowledge work. If it is strategic (humanists adopt the Classical standard to establish institutional authority), the reading is best classified as snare rather than tangled_rope—the coordination story (textual anchoring) becomes cover for extraction (authority capture). The measurement difference is whether the suppression is understood as correction of error or as elimination of competitors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_ambiguity, conceptual, 'Whether the reconstruction reading is held as epistemic recovery or strategic authority-capture.').

omega_variable(
    institutional_identity_fusion_depth,
    'For practitioners trained in medieval Latin, is the identity-locking to medieval forms a deep cognitive/professional fusion that persists after the forms lose institutional recognition, or is it a contingent role-specific dependency that dissolves with institutional context?',
    'Post-institutional trajectory analysis: what happens to practitioners'' self-conception and utility after they exit institutional roles? Do they continue to defend medieval forms as legitimate, or do they internalize the humanist judgment and experience their former expertise as ignorance? Does the internalization persist after institutional pressure ends?',
    'If identity-fusion is deep and persistent, the suppression operates at the level of identity-destruction and carries traumatic weight—the constraint extracts the target''s sense of professional competence. If identity-locking is contingent on role, suppression is reversible and primarily institutional rather than internalized. High internalization suggests the constraint persists partly through internalized suppression rather than external enforcement alone, which increases effective extraction from the target''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_fusion_depth, empirical, 'Depth and persistence of identity-fusion to delegitimized medieval forms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 1350, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t1350, classical_latin_standard__reconstruction_reading, theater_ratio, 1350, 0.08).
narrative_ontology:measurement_basis(clas_tr_t1350, projected).
narrative_ontology:measurement(clas_tr_t1400, classical_latin_standard__reconstruction_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement_basis(clas_tr_t1400, observed).
narrative_ontology:measurement(clas_tr_t1450, classical_latin_standard__reconstruction_reading, theater_ratio, 1450, 0.24).
narrative_ontology:measurement_basis(clas_tr_t1450, observed).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__reconstruction_reading, theater_ratio, 1500, 0.33).
narrative_ontology:measurement_basis(clas_tr_t1500, observed).
narrative_ontology:measurement(clas_tr_t1550, classical_latin_standard__reconstruction_reading, theater_ratio, 1550, 0.38).
narrative_ontology:measurement_basis(clas_tr_t1550, observed).
narrative_ontology:measurement(clas_tr_t1600, classical_latin_standard__reconstruction_reading, theater_ratio, 1600, 0.41).
narrative_ontology:measurement_basis(clas_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t1350, classical_latin_standard__reconstruction_reading, base_extractiveness, 1350, 0.12).
narrative_ontology:measurement_basis(clas_be_t1350, projected).
narrative_ontology:measurement(clas_be_t1400, classical_latin_standard__reconstruction_reading, base_extractiveness, 1400, 0.31).
narrative_ontology:measurement_basis(clas_be_t1400, observed).
narrative_ontology:measurement(clas_be_t1450, classical_latin_standard__reconstruction_reading, base_extractiveness, 1450, 0.54).
narrative_ontology:measurement_basis(clas_be_t1450, observed).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__reconstruction_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement_basis(clas_be_t1500, observed).
narrative_ontology:measurement(clas_be_t1550, classical_latin_standard__reconstruction_reading, base_extractiveness, 1550, 0.76).
narrative_ontology:measurement_basis(clas_be_t1550, observed).
narrative_ontology:measurement(clas_be_t1600, classical_latin_standard__reconstruction_reading, base_extractiveness, 1600, 0.79).
narrative_ontology:measurement_basis(clas_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t1350, classical_latin_standard__reconstruction_reading, suppression_requirement, 1350, 0.15).
narrative_ontology:measurement_basis(clas_su_t1350, projected).
narrative_ontology:measurement(clas_su_t1400, classical_latin_standard__reconstruction_reading, suppression_requirement, 1400, 0.38).
narrative_ontology:measurement_basis(clas_su_t1400, observed).
narrative_ontology:measurement(clas_su_t1450, classical_latin_standard__reconstruction_reading, suppression_requirement, 1450, 0.59).
narrative_ontology:measurement_basis(clas_su_t1450, observed).
narrative_ontology:measurement(clas_su_t1500, classical_latin_standard__reconstruction_reading, suppression_requirement, 1500, 0.71).
narrative_ontology:measurement_basis(clas_su_t1500, observed).
narrative_ontology:measurement(clas_su_t1550, classical_latin_standard__reconstruction_reading, suppression_requirement, 1550, 0.79).
narrative_ontology:measurement_basis(clas_su_t1550, observed).
narrative_ontology:measurement(clas_su_t1600, classical_latin_standard__reconstruction_reading, suppression_requirement, 1600, 0.82).
narrative_ontology:measurement_basis(clas_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.18).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel comprises three readings: reconstruction_reading (this constraint, asserting Classical form as recovered from texts), continuity_reading (asserting living practice as legitimate), and hybrid_reading (asserting both Classical norms and legitimate post-Classical development in specialized domains). These are not the same constraint viewed from different angles—they have different beneficiary/victim structures, different suppression mechanisms, different terminal types. The ε values differ substantially: reconstruction_reading instantiates high extraction (systematic delegitimization of institutional practice), continuity_reading instantiates lower extraction (practice-based authority is inclusive and requires no gatekeeping class), hybrid_reading occupies middle ground (acknowledges both sources of legitimacy but creates boundary-management overhead). The three readings compete within the intellectual history of Latin studies; only one can dominate institutional authority at any moment, though all three remain live as scholarly positions. Linked via network.affects_constraints: reconstruction_reading influences the other two by creating the institutional landscape against which they must argue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
