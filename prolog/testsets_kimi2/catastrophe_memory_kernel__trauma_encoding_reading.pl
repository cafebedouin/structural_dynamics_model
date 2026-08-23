% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Trauma Encoding as Intergenerational Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the trauma_encoding_reading of the
 *   catastrophe_memory_kernel: the claim that ritual mourning practice
 *   functions primarily as an intergenerational trauma-transmission
 *   mechanism, encoding collective catastrophe into affective memory to serve
 *   as an early-warning system against recurrence. The standing arrangement
 *   under contest is the ritual obligation itself â the repeated,
 *   emotionally intense commemorative practice that imposes psychological
 *   costs on descendants while producing collective vigilance. The reading
 *   treats the ritual as a tangled rope: it genuinely coordinates
 *   threat-perception across time, but it asymmetrically extracts from the
 *   same descendants who bear its costs, requiring active social and ritual
 *   enforcement to persist. Sibling readings (boundary_maintenance,
 *   survival_competence, symbol_continuity) are acknowledged but not
 *   described inside this constraint; they are separate files in the same
 *   family.
 *
 * KEY AGENTS:
 *   - ritual_specialists: agenda_setter (moderate/identity_locked) â maintain and officiate the ritual, derive authority from lineage to the founding catastrophe
 *   - descendant_community: dual-position beneficiary/payer (moderate/identity_locked) â receive vigilance but bear trauma; cannot exit without severing belonging
 *   - trauma_therapists: excluded (organized/analytical) â would object to harm but are outside ritual frame
 *   - memory_studies_scholars: observer (organized/analytical) â analyze structural effects without participating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Trauma Encoding as Intergenerational Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650').
narrative_ontology:cs_kernel_codification('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', fixed_text).
narrative_ontology:cs_authority_grounding('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', lineage).
narrative_ontology:cs_interpretation_layer_present('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650').
narrative_ontology:cs_reading_relation('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_axiom('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', foundational, traumatic_affect_required_for_warning).
narrative_ontology:cs_axiom_status(traumatic_affect_required_for_warning, holdable).
narrative_ontology:cs_axiom_grounding('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', traumatic_affect_required_for_warning, empirically_contingent).
narrative_ontology:cs_axiom('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', secondary, descendants_bear_ritual_memory_duty).
narrative_ontology:cs_axiom_status(descendants_bear_ritual_memory_duty, holdable).
narrative_ontology:cs_axiom_grounding('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', descendants_bear_ritual_memory_duty, deontological).
narrative_ontology:cs_reference_frame('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', catastrophic_origins_vigilance).
narrative_ontology:cs_drift_state('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', contemporary_secular_memorial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bbbd3f7a-fdbc-47f9-85f5-6e581b8f8650', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, intergenerational_threat_vigilance).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_recurrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the ritual practice across generations, officiating at memorial ceremonies and interpreting the obligations of remembrance. Their authority derives from continuity with the founding catastrophe and the duty to preserve its memory. Exit would mean abandoning their vocational and communal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_specialists, agenda_setter,
    moderate, generational, identity_locked, national).

% Participate in ritual mourning and commemoration, receiving the encoded warning about collective threats while bearing the psychological burden of transmitted trauma. Their communal identity is fused with the ritual; exit risks social exclusion and loss of belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community, payer).

% Would frame the ritual transmission as psychologically harmful and advocate for trauma-informed alternatives, but are outside the ritual authority structure and rarely consulted in communal decisions about memory practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_therapists, excluded,
    organized, biographical, analytical, national).

% Analyze the ritual as a mechanism of collective memory, debating whether the trauma-encoding function serves survival or perpetuates unnecessary suffering. They do not participate in the ritual but study its structural effects.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, memory_studies_scholars, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits collective threat-perception across generations, maintaining early-warning capacity against recurring persecution or catastrophe by encoding danger-signals in ritual form.
% TRANSFER_FUNCTION: Moves psychological vigilance and trauma-load from the generation that experienced catastrophe to subsequent generations, via repetitive ritual practice that reactivates grief and hypervigilance.
% ABSENT_VOICES: Mental health professionals and trauma-therapists who would frame the transmission as psychologically harmful; secular memorial advocates who would separate memory from ritual obligation; younger descendants who might reject the trauma frame but are identity-locked into participation.
% DISAPPEARANCE_RATIONALE: Without the ritual encoding, the intergenerational threat-vigilance mechanism would weaken; collective memory would shift to secular or individualized forms, and the early-warning function would dissipate, rearranging how the community relates to its past and future threats.
% FOUNDING_PROBLEM: The community faced recurrent catastrophic threats and needed a mechanism to transmit danger-recognition across generations without relying on institutional archives that could be destroyed or suppressed.
% FOUNDING_PROBLEM_CORROBORATION: Historians and genocide scholars outside the ritual community attest to the original catastrophes; trauma-studies researchers and secular memory advocates contest whether the ongoing ritual transmission is still necessary for safety or has become self-perpetuating.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-to-high because the ritual imposes a sustained psychological burden â hypervigilance, grief activation, identity-fusion with catastrophe â on descendants for the sake of a collective good that could arguably be served by less costly means. Suppression (0.55) reflects active social enforcement: ritual participation is socially compulsory within the community, and exit triggers shame and exclusion, though outright coercion is rare. Theater ratio (0.25) is low-moderate; while some ritual performance is symbolic, the affective load is real and functional. Accessibility collapse (0.65) is substantial because secular or therapeutic alternatives to trauma-transmission are delegitimized within the ritual frame. Resistance (0.35) is moderate; younger generations and secularists push back, but identity-lock dampens organized resistance. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_specialists experience the constraint as a sacred duty and a necessary defense against oblivion; the descendant_community experiences it as both protective (vigilance) and burdensome (trauma). The engine computes divergent per-seat types from this structural asymmetry â the agenda_setter seat may compute toward rope-like coordination, while the payer seat computes toward snare-like extraction, yielding tangled_rope at the aggregate level.
 *
 * DIRECTIONALITY LOGIC:
 *   The descendant_community is declared in both beneficiaries and victims because the same structural arrangement moves both a coordination benefit (threat-vigilance) and an extraction cost (psychological trauma) to the same group. Their identity_locked exit options amplify the effective extraction: they cannot easily leave without severing communal belonging. Ritual_specialists are not declared as beneficiaries because their primary structural relation is administration, not rent collection; the coordination gains are diffuse (collective vigilance).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both a genuine coordination function and asymmetric extraction. If the trauma cost were negligible, the constraint would be a rope (pure coordination of memory). If there were no genuine threat-vigilance function, it would be a snare (pure extraction under cover of memory). The authored metrics claim a genuine but costly coordination function, mandating tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the trauma-encoding function of ritual the primary structural role, or is it secondary to boundary-maintenance or symbol-continuity?',
    'Comparative ethnographic analysis measuring whether ritual communities without recent catastrophe maintain the same trauma-laden intensity, which would support boundary-maintenance over trauma-encoding.',
    'If trauma-encoding is secondary, this reading''s epsilon is overstated and the constraint may reclassify toward identity_coordination with lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether trauma-encoding is the primary or secondary function of the ritual').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social sanction, exclusion) or internalized (descendants believe the ritual obligation is constitutive of identity)?',
    'Post-exit observation: if descendants who leave the ritual community continue to experience psychological pressure and guilt, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    vigilance_trauma_boundary,
    'Does the ritual''s threat-vigilance function require the traumatic affective load, or could the same information be transmitted without psychological burden?',
    'Comparison with secular memorial education programs that transmit similar historical threat-information without ritual obligation or trauma-encoding.',
    'If vigilance is separable from trauma, the constraint is more extractive than coordination; if inseparable, the extraction is inherent to the coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vigilance_trauma_boundary, conceptual, 'Whether vigilance can be separated from traumatic affect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trauma_enc_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trauma_enc_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(trauma_enc_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(trauma_enc_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(trauma_enc_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(trauma_enc_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(trauma_enc_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(trauma_enc_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(trauma_enc_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(trauma_enc_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(trauma_enc_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(trauma_enc_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__trauma_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four structurally distinct constraints per the epsilon-invariance principle. The trauma_encoding_reading isolates the warning-system function and its associated extraction costs; other readings isolate boundary maintenance, survival competence, and symbolic continuity functions, each with different epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
