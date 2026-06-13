% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__incidental_collection_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 Incidental Collection and Warrantless Query
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint describes the 'incidental collection' reading of FISA
 *   Section 702, which permits U.S. intelligence agencies to retain and query
 *   communications of U.S. persons collected without a warrant, so long as
 *   the initial collection targeted a non-U.S. person abroad for foreign
 *   intelligence purposes. This interpretation allows for 'backdoor searches'
 *   where FBI domestic investigators can access this database without a
 *   warrant, effectively circumventing Fourth Amendment protections for U.S.
 *   persons. The constraint is actively enforced by intelligence agencies and
 *   the FISA Court, but faces significant resistance from civil liberties
 *   advocates and some members of Congress.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.7).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection and Warrantless Query").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'f4ce6ddf-4086-43a4-b41d-3ee0f968caa1').
narrative_ontology:cs_kernel_codification('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', fixed_text).
narrative_ontology:cs_authority_grounding('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', lineage).
narrative_ontology:cs_interpretation_layer_present('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1').
narrative_ontology:cs_reading_relation('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', foundational, foreign_intelligence_purpose_justifies_warrantless_query).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_warrantless_query, holdable).
narrative_ontology:cs_axiom_grounding('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', foreign_intelligence_purpose_justifies_warrantless_query, conventional).
narrative_ontology:cs_axiom('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', foundational, incidental_collection_does_not_trigger_fourth_amendment_warrant).
narrative_ontology:cs_axiom_status(incidental_collection_does_not_trigger_fourth_amendment_warrant, holdable).
narrative_ontology:cs_axiom_grounding('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', incidental_collection_does_not_trigger_fourth_amendment_warrant, conventional).
narrative_ontology:cs_reference_frame('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', post_9_11_executive_authority).
narrative_ontology:cs_drift_state('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', contemporary_privacy_advocacy_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('f4ce6ddf-4086-43a4-b41d-3ee0f968caa1', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the surveillance programs under FISA Section 702, collect foreign intelligence, and justify the retention and querying of incidentally collected U.S. person data as essential for national security. They set the internal minimization procedures and interpret the scope of 'foreign intelligence purpose'.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Access the database of incidentally collected U.S. person communications for domestic investigations without needing a probable cause warrant, leveraging the foreign intelligence authority. They benefit from this 'backdoor search' capability.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators, beneficiary,
    institutional, biographical, constrained, national).

% Have their communications retained and queried by intelligence agencies without their knowledge or a warrant, simply because they communicated with a foreign target. They bear the cost of privacy erosion and potential misuse of their data, with no practical means of opting out or challenging the collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, global).

% Actively challenge the legality and constitutionality of this interpretation of Section 702 through litigation, public advocacy, and lobbying Congress. They bear the cost of sustained effort against a powerful, secretive apparatus, with limited success in altering the core practice.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, payer,
    organized, generational, constrained, national).

% Reviews and approves the government's Section 702 targeting and minimization procedures, but does not issue warrants for individual U.S. person queries. Its oversight is limited by the classified nature of the programs and its institutional role.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Enacted FISA Section 702 and periodically reauthorizes it. Members are divided on the 'incidental collection' reading, with some seeking reforms to protect U.S. person privacy and others prioritizing intelligence capabilities. They have the power to amend the statute but face political and national security pressures.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).
:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is substantial because it permits warrantless access to sensitive U.S. person data, infringing on privacy rights. Suppression (0.7) is high due to the classified nature of the collection and querying processes, making it difficult for affected U.S. persons to know they are being surveilled or to challenge it. Theater ratio (0.2) is low, as the collection and querying are genuinely functional for intelligence purposes, though the 'incidental' justification is often seen as a legal fiction. Accessibility collapse (0.6) is moderate, as legal challenges and legislative efforts exist, but are often unsuccessful. Resistance (0.4) is present from civil liberties groups and some legislators, but has not yet fundamentally altered the practice.
 *
 * PERSPECTIVAL GAP:
 *   National security agencies and FBI domestic investigators experience this as a vital tool for protecting national security, enabling efficient intelligence gathering and domestic threat detection. U.S. persons and civil liberties advocates experience it as a significant erosion of Fourth Amendment rights, an unchecked expansion of government surveillance, and a violation of privacy. The FISA Court and Congress occupy an intermediate position, attempting to balance these competing interests, but often deferring to executive branch claims of necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and FBI domestic investigators are clear beneficiaries (d=0.0-0.1) as they gain access to vast amounts of data without the burden of warrants. U.S. persons whose communications are incidentally collected are clear victims (d=0.9-1.0) as their data is retained and queried without their knowledge or consent, and with no practical exit. Civil liberties advocates are also victims (d=0.8-0.9) as their mission to protect constitutional rights is undermined. The FISA Court and Congress are agenda-setters/observers (d=0.4-0.6), tasked with oversight but often constrained by classified information and political pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it purports to solve a genuine coordination problem (gathering foreign intelligence) but does so with significant asymmetric extraction (warrantless surveillance of U.S. persons). The coordination story (national security) is used to justify the extraction. If the 'foreign intelligence purpose' justification were to atrophy, or if the incidental collection became the primary target, it would shift towards a Snare. The ongoing debate and legal challenges prevent it from being a Piton, as there are still concentrated beneficiaries actively maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fisa_702_kernel_reading_identification,
    'Is this constraint a genuine interpretation of FISA Section 702, or an overreach of statutory authority?',
    'Supreme Court ruling on the constitutionality of warrantless U.S. person queries under Section 702, or legislative amendment explicitly prohibiting such queries.',
    'If ruled an overreach, the constraint would be reclassified as a Snare; if upheld, it would remain a Tangled Rope, but with a stronger claim to legitimacy within the current legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fisa_702_kernel_reading_identification, conceptual, 'This constraint is the ''incidental_collection_reading'' of the ''fisa_702_statutory_text'' kernel. It permits retention and warrantless query of incidentally collected U.S. person communications for foreign intelligence purposes, a reading contested by other interpretations.').

omega_variable(
    foreign_intelligence_purpose_ambiguity,
    'How broadly can ''foreign intelligence purpose'' be interpreted to justify querying incidentally collected U.S. person data?',
    'Clearer statutory definition of ''foreign intelligence purpose'' or judicial precedent establishing strict limits on its application to U.S. person data.',
    'A narrow interpretation would reduce extractiveness and suppression for U.S. persons; a broad interpretation would maintain or increase them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_ambiguity, empirical, 'The scope of ''foreign intelligence purpose'' is critical to the constraint''s operation and its impact on U.S. persons.').

omega_variable(
    fourth_amendment_displacement,
    'To what extent does this reading displace traditional Fourth Amendment warrant requirements for U.S. person data?',
    'Judicial review explicitly affirming or denying the applicability of the Fourth Amendment''s warrant clause to queries of incidentally collected U.S. person data under Section 702.',
    'If the Fourth Amendment is deemed fully applicable, the constraint''s legal basis for warrantless queries would collapse, leading to reclassification as a Snare or its dissolution. If deemed inapplicable, the current extractive structure would be legally entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fourth_amendment_displacement, conceptual, 'This reading''s core premise is that administrative minimization procedures suffice, displacing the Fourth Amendment''s warrant requirement for incidentally collected U.S. person data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_warrant_requirement).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_court_oversight_authority).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
