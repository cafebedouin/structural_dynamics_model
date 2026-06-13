% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Length Judicial Deference (Ambiguity Reading)
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   copyright_constitutional_mandate. The kernel is the constitutional text's
 *   'limited times' clause and Congress's power to set copyright term length.
 *   This reading (judicial_ambiguity_reading) instantiates a specific
 *   judicial posture: courts apply rational basis review, which defers to
 *   Congress's judgment on term length so long as a rational basis exists.
 *   Under this reading, 'limited times' is not a substantive ceiling but a
 *   zone of legislative discretion. The constraint's structural content is
 *   the deferential review posture itself — it permits repeated term
 *   extensions without judicial invalidation. Sibling readings
 *   (corporate_enclosure_reading, public_scaffold_reading) constitute
 *   different constitutional interpretations with different beneficiary
 *   structures and extraction profiles; they are separate constraint stories,
 *   not views of this one.
 *
 * KEY AGENTS:
 *   - Congress: sets term length, benefits from deference authority
 *   - Federal courts: apply rational basis review, defer to Congress
 *   - Copyright holders (corporate): benefit from extended monopoly, lobby Congress
 *   - Authors and creators (organized): benefit from extended terms, provide legislative testimony
 *   - Libraries, educators, cultural producers: bear costs of monopoly extension and limited public-domain access
 *   - Constitutional fixity claim (abstraction): carries the normative proposition that 'limited times' should constrain term length; this claim is deprioritized under the reading
 *   - Progressive constitutional scholars: excluded from decision-making by deferential review standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.28).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Length Judicial Deference (Ambiguity Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property/constitutional_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7').
narrative_ontology:cs_kernel_codification('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', fixed_text).
narrative_ontology:cs_authority_grounding('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', lineage).
narrative_ontology:cs_interpretation_layer_present('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7').
narrative_ontology:cs_reading_relation('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', foundational, rational_basis_deference_legitimate).
narrative_ontology:cs_axiom_status(rational_basis_deference_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', rational_basis_deference_legitimate, deontological).
narrative_ontology:cs_axiom('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', foundational, limited_times_means_legislative_discretion).
narrative_ontology:cs_axiom_status(limited_times_means_legislative_discretion, holdable).
narrative_ontology:cs_axiom_grounding('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', limited_times_means_legislative_discretion, conventional).
narrative_ontology:cs_reference_frame('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', rational_basis_deference_posture).
narrative_ontology:cs_drift_state('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50e5644e-a9d9-47b0-abd0-6d9b6cfbbfe7', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at 2024) because the constraint does extract authority from constitutional limits and channels it toward congressional discretion, but the underlying public goods (authorship incentives, copyright infrastructure) retain some real coordination function. The measurement series show extractiveness accumulating over the 1970–2024 interval: rational basis deference enabled the Sonny Bono Copyright Term Extension Act (1998) and subsequent extensions, each of which increased monopoly duration without judicial constraint. Theater ratio rises modestly (0.08 to 0.19) as the constitutional justification — 'limited times' means Congress can set limits — becomes increasingly decoupled from practice (terms approach perpetuity for corporate holders). Suppression is low-to-moderate (0.28 at 2024) because the constraint relies more on judicial passivity than on active suppression; public-domain advocates can argue their case, but rational basis review makes their arguments structurally weightless in court. Accessibility collapse is moderate (0.61) because alternatives to copyright — compulsory licensing, fair use, orphan-work exceptions — persist as legal safety valves, but they are constrained by extended terms. Resistance is high (0.72) because numerous constituencies (libraries, digital innovators, academic institutions, progressive jurists) actively contest term extensions, but they meet weak judicial resistance because of the deferential review standard.
 *
 * PERSPECTIVAL GAP:
 *   Congress and institutional copyright holders experience this constraint as a coordination success: it permits clear, predictable policy-setting. Courts experience it as institutional deference. Cultural producers, libraries, and progressive scholars experience it as extraction: their access rights are sacrificed to monopoly extension, and their constitutional claims are excluded from adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority sits at d near 0 (beneficiary: the constraint secures its discretion). Copyright holders (powerful) sit at d around 0.1–0.2 (beneficiaries: they secure extended terms). Authors and creators (organized) sit at d around 0.2 (mixed: they benefit from extensions, but constrained exit limits their ability to exit if terms become onerous). Libraries and cultural producers (organized) sit at d around 0.7–0.8 (payers: extended monopoly is a cost, constrained exit). Public-domain beneficiaries (powerless, excluded) sit at d near 1.0 (full targets: they bear the cost of delayed access, trapped exit). The constitutional fixity claim (non-agent) is not a seat but carries the normative proposition that 'limited' should constrain — under this reading, that proposition's weight is deprioritized, which is an extraction from the claim itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping copyright genuinely temporary — was live at the constraint's inception. By 2024, the founding problem is contested: copyright holders and Congress attest it is still live (extensions are necessary for author survival in modern markets); public-interest constituencies and economists attest it is dead (terms have extended far beyond reasonable author-incentive requirements; further extensions serve rent-seeking, not incentives). Judicial deference enables this drift without constitutional invalidation: rational basis review requires only a rational basis to exist, not that it be empirically true. The constraint permits a scaffold-to-enclosure transition: rational basis review was originally a limited deference doctrine that would validate some extensions while preserving the possibility of invalidating egregious ones. Over 54 years, it has become near-complete deference, enabling seven major extensions without judicial scrutiny. The mandatrophy resolution is: the constraint itself is not defunct (rational basis review still operates), but the founding problem it ostensibly protects has become contestable, and the judicial deference posture has enabled term length to drift toward the corporate-enclosure reading without constitutional invalidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_basis_floor_opacity,
    'Is rational basis review a stable legal standard that permits meaningful constraint on congressional discretion, or a floor so permissive that nearly any legislative action survives?',
    'Case law analysis: examine how often rational basis review results in invalidation across all domains (copyright and beyond). If the invalidation rate approaches zero, the standard is effectively permissive without constraint. Cross-jurisdictional comparison: examine whether other democracies'' deferential standards produce measurably different outcomes.',
    'If rational basis review is truly permissive, the constraint is closer to pure deference (benign rope) than to judicial review with meaningful floor. If the standard retains latent gatekeeping power, it is a contested rope with capacity to invalidate if Congress strays far enough. This affects whether the constraint enables enclosure or sets a genuine if high bar.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_basis_floor_opacity, empirical, 'Whether rational basis review has substantive gatekeeping power or is effectively permissive.').

omega_variable(
    limited_times_semantic_ambiguity,
    'Does the constitutional text''s ''limited times'' language inherently support the judicial_ambiguity_reading (Congress decides the limit), or does it inherently foreclose it in favor of a substantive ceiling reading?',
    'Originalist linguistic analysis: examine 18th-century usage of ''limited'' in temporal contexts. Did the Framers intend ''limited'' to mean Congress could redefine limits, or to mean a natural-law or implicit ceiling applied by courts? Historical correspondence and statutory precedent from the Founding era.',
    'If ''limited'' inherently forecloses the judicial_ambiguity reading (i.e., the reading is a later constitutional drift, not the original understanding), the reading itself is contested and vulnerable to reframing. If ''limited'' genuinely is ambiguous, the reading gains epistemic legitimacy as one live interpretation among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_semantic_ambiguity, conceptual, 'Whether the constitutional text itself supports or forecloses the judicial ambiguity interpretation.').

omega_variable(
    incentive_mechanism_empirical_drift,
    'Do term extensions beyond a certain threshold (e.g., life of author + 50 years) actually increase authorship incentives, or do they primarily transfer wealth to copyright holders without marginal incentive effect?',
    'Econometric analysis of publication/creation rates against term length in different jurisdictions and historical periods. Controlled comparison between regimes with shorter and longer terms, holding other factors (technology, education, market size) constant. Survey evidence on author perception of term length as an incentive factor.',
    'If marginal incentive effect is negligible beyond ~life+50 years, the rational basis for recent extensions (incentivizing authorship) becomes empirically weak. This does not invalidate extensions under rational basis review, but it increases the plausibility of the public_scaffold_reading: extensions serve wealth transfer, not the constitutional purpose. It strengthens the case for the judicial_ambiguity_reading to be reconsidered as a cover story for enclosure rather than genuine legislative discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_mechanism_empirical_drift, empirical, 'Whether term extensions produce measurable marginal incentive returns or serve primarily as wealth transfer.').

omega_variable(
    reading_institutional_capture,
    'To what extent does the judicial_ambiguity_reading itself constitute institutional capture — a posture adopted by courts partly because copyright holders have superior lobbying power and legislative voice?',
    'Historical analysis of judicial reasoning: did courts adopt rational basis deference because the legal standard genuinely supports it, or because copyright-holder lobbying made aggressive review politically costly? Comparison with other deferential review standards in areas with less organized beneficiary power (e.g., labor rights, where judicial deference is lower despite comparable textual ambiguity).',
    'If the reading is partly a product of capture, it is not a neutral interpretation of the constitutional text but a contestable institutional arrangement. This would strengthen the omega_c case that the reading is one side of a live, ongoing dispute, not a settled constitutional matter. It would shift the constraint''s description: from ''courts apply deferential review to congressional judgment'' to ''courts defer to Congress partly because Congress is more responsive to organized copyright interests.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_institutional_capture, conceptual, 'Whether the deferential review posture is a canonical legal interpretation or a product of institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(copy_tr_t1984, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1984, 0.11).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.14).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2024, 0.19).

% Extraction over time
narrative_ontology:measurement(copy_be_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(copy_be_t1984, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1984, 0.24).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.32).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(copy_su_t1984, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1984, 0.18).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.21).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.08).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the copyright_constitutional_mandate kernel. The judicial_ambiguity_reading treats term length as a zone of legislative discretion (rational basis deference). The public_scaffold_reading treats 'limited times' as a substantive constitutional ceiling meant to preserve the public domain. The corporate_enclosure_reading treats term length as a property right requiring maximal extension. Each reading has distinct beneficiary structures, epsilon values, and mandatrophy implications. This story (judicial_ambiguity) sits structurally between the other two: it permits extensions (closer to enclosure in practice) while maintaining formal fidelity to 'limited' (ostensibly preserving the public_scaffold principle). The three readings are not observations of a single constraint from different angles — they are distinct constraints emanating from the contested kernel. Their relationship is interdependence: the plausibility of judicial deference depends on whether courts can convince themselves that the founding problem is still live (a public_scaffold claim) rather than dead (an enclosure claim). Judicial ambiguity enables the transition from scaffold to enclosure without constitutional crisis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, analytical, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
