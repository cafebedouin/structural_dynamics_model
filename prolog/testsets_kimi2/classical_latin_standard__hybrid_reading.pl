% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Classical Latin Hybrid Standard: Classical Fidelity with Post-Classical Accommodation
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the hybrid_reading of the classical_latin_standard
 *   kernel. It holds that correct Latin requires fidelity to Classical
 *   grammatical and stylistic norms while granting legitimacy to specific
 *   post-Classical developments in technical and ecclesiastical domains. The
 *   kernel is contested by three readings: continuity_reading (all natural
 *   drift is legitimate), hybrid_reading (this constraint), and
 *   reconstruction_reading (only archaeologically recoverable Classical usage
 *   is valid). The hybrid reading reduces the victim set relative to pure
 *   reconstruction by accommodating medieval coinages in science and liturgy,
 *   but it maintains extraction by stigmatizing forms it classifies as
 *   barbarisms.
 *
 * KEY AGENTS:
 *   - Classical pedagogical academies (agenda_setter/institutional): set curricula, certify competence, and enforce the boundary between legitimate accommodation and barbarism.
 *   - Institutional Latin users (beneficiary/institutional): ecclesiastical and scientific communities that gain a stable, authoritative language integrating Classical prestige with domain-specific vocabulary.
 *   - Barbarism-committing speakers (payer/moderate): bear costs of correction and exclusion when their usage falls outside the hybrid norm.
 *   - Medieval continuity advocates (excluded/organized): contest the privileging of Classical antiquity over later natural development.
 *   - Reconstructionist philologists (observer/organized): critique the hybrid standard as insufficiently pure from an analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.56).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.58).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Hybrid Standard: Classical Fidelity with Post-Classical Accommodation").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '21d6d60d-79c6-44ca-879d-c6a7e9b1b37c').
narrative_ontology:cs_kernel_codification('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', fixed_text).
narrative_ontology:cs_authority_grounding('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', lineage).
narrative_ontology:cs_interpretation_layer_present('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c').
narrative_ontology:cs_reading_relation('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', foundational, classical_textual_priority).
narrative_ontology:cs_axiom_status(classical_textual_priority, holdable).
narrative_ontology:cs_axiom_grounding('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', classical_textual_priority, deontological).
narrative_ontology:cs_axiom('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', foundational, partial_postclassical_accommodation).
narrative_ontology:cs_axiom_status(partial_postclassical_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', partial_postclassical_accommodation, conventional).
narrative_ontology:cs_reference_frame('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', classical_textual_fidelity).
narrative_ontology:cs_drift_state('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', post_medieval_institutionalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('21d6d60d-79c6-44ca-879d-c6a7e9b1b37c', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_latin_users).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, barbarism_committing_speakers).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_textual_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, legitimate_technical_ecclesiastical_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, academies, and examination boards that define curricula and certify proficiency in Latin. They enforce the hybrid standard by requiring Classical grammatical norms while permitting established post-Classical technical and ecclesiastical vocabulary. Their authority rests on continuity with the Classical textual tradition.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_pedagogical_academies, agenda_setter,
    institutional, generational, constrained, continental).

% Ecclesiastical bodies, scientific nomenclature committees, and legal institutions that use Latin as a working language. They rely on a stable standard that grants legitimacy to their post-Classical technical terminology while preserving the cultural authority of Classical forms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, institutional_latin_users, beneficiary,
    institutional, generational, constrained, global).

% Students, amateur Latinists, and scholars whose usage incorporates forms deemed barbarisms rather than legitimate post-Classical developments. Their usage is subject to correction and exclusion from publication when it deviates from the hybrid norm, and they have limited ability to exit because formal Latin competence is assessed against this standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, barbarism_committing_speakers, payer,
    moderate, biographical, constrained, national).

% Scholars and communities who regard all natural post-Classical linguistic drift as legitimate and reject the privileging of Classical antiquity. Their perspective is not represented in standard-setting institutions that enforce the hybrid norm.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_continuity_advocates, excluded,
    organized, generational, constrained, continental).

% Scholars who argue that only strictly recoverable Classical usage is valid and who critique the hybrid standard as insufficiently pure. They do not control the institutional standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, reconstructionist_philologists, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a trans-temporal and trans-national linguistic standard that enables communication across ecclesiastical, scientific, and academic domains without requiring full classical purity.
% TRANSFER_FUNCTION: Moves legitimacy and institutional authority from Classical textual sources to modern users who conform to the hybrid norm; moves stigma and exclusion toward practitioners whose deviations fall outside the accommodated domains.
% ABSENT_VOICES: Medieval continuity advocates, who would argue for unconditional acceptance of all historical drift, and strict reconstructionists, who would reject all post-Classical innovation as corruption. Both are partially excluded from normative institutions.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, ecclesiastical and scientific institutions might fragment between a purist Classical revival and an uncodified living-language approach; the direction of rearrangement would be disputed between continuity and reconstruction factions.
% FOUNDING_PROBLEM: The collapse of Roman antiquity left Latin without a living native speaker community, yet the Church and emerging scholarly disciplines needed a stable, authoritative language for transregional communication and textual access.
% FOUNDING_PROBLEM_CORROBORATION: Church councils and Renaissance humanists attest to the need for a usable post-Classical standard. Reconstructionist philologists dispute that the hybrid compromise was necessary, arguing direct return to Classical sources was always sufficient.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, contested).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.56) because the constraint genuinely coordinates communication across centuries and domains, but it also asymmetrically delegitimizes non-conforming speakers. Suppression is moderate (0.58): enforcement is real (academic gatekeeping, liturgical regulation) but partial because some post-Classical forms are actively legitimized. Theater_ratio (0.38) reflects that a substantial fraction of normative activity performs classical purity rather than serving live communicative needs. Accessibility_collapse (0.62) is moderately high because alternatives (full medieval Latin, reconstructed demotic Latin) lack institutional standing once the hybrid standard is understood. Resistance (0.42) reflects ongoing contestation from both continuity and reconstruction factions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (academies) experiences the constraint as a necessary coordination mechanism preserving textual access and institutional continuity. The payer seat (non-conforming speakers) experiences it as an exclusionary gate that extracts legitimacy from their practice. The beneficiary seat (institutional users) experiences reduced extraction because the standard accommodates their technical vocabulary. The engine computes these divergences from structural data; the authored claim (tangled_rope) reflects the combined picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Latin users are declared beneficiaries because the standard subsidizes their domain-specific usage with Classical legitimacy, giving them low directionality. Classical pedagogical academies are agenda-setters who administer the constraint; their exit is constrained by their institutional role but they do not bear extraction. Barbarism-committing speakers are declared victims (role: payer) because the constraint extracts legitimacy from their practice and channels correction costs onto them, yielding high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mandatrophy mislabeling by explicitly acknowledging the coordination function (trans-temporal communication) alongside the extraction function (barbarism stigmatization). A pure snare reading would miss the genuine accommodation of technical and ecclesiastical vocabulary; a pure rope reading would miss the asymmetric cost borne by excluded speakers. The tangled_rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_boundary_arbitrariness,
    'Is the boundary between legitimized post-Classical technical/ecclesiastical forms and stigmatized barbarisms structurally principled, or does it track the historical power of the institutions that produced each form?',
    'Historical sociolinguistic analysis comparing the institutional power of domains whose vocabulary was accommodated versus those whose forms were rejected.',
    'If the boundary tracks institutional power rather than linguistic principle, the constraint''s extraction is higher than its coordination function suggests, shifting it toward snare. If principled, the hybrid accommodation is genuinely coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_boundary_arbitrariness, empirical, 'Whether legitimacy boundary is principled or power-driven').

omega_variable(
    kernel_reading_stability,
    'Would adoption of the continuity_reading or reconstruction_reading by currently hybrid institutions eliminate the coordination problem entirely, or merely redistribute extraction?',
    'Comparative institutional analysis of communities operating under each reading: measure communicative breakdown, exclusion rates, and standardization success.',
    'If the hybrid reading is the only viable coordination solution, its extraction is the price of the coordination. If another reading solves coordination with less extraction, the hybrid reading is more extractive than necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether hybrid reading is uniquely necessary for coordination').

omega_variable(
    authority_grounding_ambiguity,
    'Does the standard''s authority derive from recoverable Classical textual evidence, or from the self-reinforcing institutional lineage of the academies that transmit it?',
    'Philological audit tracing specific normative claims to identifiable Classical sources versus institutional tradition.',
    'If authority is primarily institutional, the constraint is more extractive and the Classical textual fidelity claim functions as legitimizing theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, empirical, 'Whether authority is textual or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(classical_latin_hybrid_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(classical_latin_hybrid_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(classical_latin_hybrid_tr_t200, classical_latin_standard__hybrid_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(classical_latin_hybrid_tr_t300, classical_latin_standard__hybrid_reading, theater_ratio, 300, 0.36).
narrative_ontology:measurement(classical_latin_hybrid_tr_t400, classical_latin_standard__hybrid_reading, theater_ratio, 400, 0.38).
narrative_ontology:measurement(classical_latin_hybrid_tr_t500, classical_latin_standard__hybrid_reading, theater_ratio, 500, 0.39).
narrative_ontology:measurement(classical_latin_hybrid_tr_t600, classical_latin_standard__hybrid_reading, theater_ratio, 600, 0.38).

% Extraction over time
narrative_ontology:measurement(classical_latin_hybrid_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(classical_latin_hybrid_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(classical_latin_hybrid_be_t200, classical_latin_standard__hybrid_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(classical_latin_hybrid_be_t300, classical_latin_standard__hybrid_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(classical_latin_hybrid_be_t400, classical_latin_standard__hybrid_reading, base_extractiveness, 400, 0.56).
narrative_ontology:measurement(classical_latin_hybrid_be_t500, classical_latin_standard__hybrid_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(classical_latin_hybrid_be_t600, classical_latin_standard__hybrid_reading, base_extractiveness, 600, 0.56).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(classical_latin_standard__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the classical_latin_standard kernel, decomposed per the epsilon-invariance principle. Sibling readings instantiate structurally distinct constraints from the same natural-language concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
