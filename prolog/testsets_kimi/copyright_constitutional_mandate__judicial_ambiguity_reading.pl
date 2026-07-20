% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Judicial Deference Doctrine (Rational Basis Review)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The judicial_ambiguity_reading of the copyright_constitutional_mandate
 *   kernel treats the Copyright Clause's 'limited times' and 'promote the
 *   progress' language as a standard of review rather than a substantive
 *   ceiling. Under this reading, federal courts defer to Congress via
 *   rational basis review, treating copyright term length as a zone of
 *   legislative discretion. The constraint is the doctrinal practice of
 *   deference itself. It coordinates separation of powers by preventing
 *   courts from micromanaging copyright policy, while simultaneously
 *   extracting from constitutional fixity: by refusing to enforce a hard
 *   limit, the doctrine enables repeated term extensions that shift wealth
 *   from the public domain to incumbent holders. This constraint is
 *   structurally distinct from the public_scaffold_reading (which reads the
 *   clause as a temporary monopoly strictly subordinate to public enrichment)
 *   and the corporate_enclosure_reading (which reads it as a property right
 *   deserving maximal protection).
 *
 * KEY AGENTS:
 *   - congressional_authority: Primary beneficiary and agenda setter (institutional/constrained) â gains discretion to set terms without judicial veto.
 *   - content_industries: Secondary beneficiary (powerful/constrained) â captures legislative extensions made possible by deference.
 *   - federal_judiciary: Agenda setter (institutional/constrained) â administers the rational basis standard and enforces deference.
 *   - general_public: Primary payer (powerless/trapped) â loses access to works kept out of the public domain.
 *   - future_creators: Secondary payer (moderate/constrained) â faces enlarged monopoly fields and higher licensing costs.
 *   - public_domain_advocates: Excluded (moderate/mobile) â would argue for a justiciable limit but lack effective voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Judicial Deference Doctrine (Rational Basis Review)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd').
narrative_ontology:cs_kernel_codification('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', fixed_text).
narrative_ontology:cs_authority_grounding('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', lineage).
narrative_ontology:cs_interpretation_layer_present('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd').
narrative_ontology:cs_reading_relation('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', foundational, legislative_discretion_on_term_length).
narrative_ontology:cs_axiom_status(legislative_discretion_on_term_length, holdable).
narrative_ontology:cs_axiom_grounding('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', legislative_discretion_on_term_length, conventional).
narrative_ontology:cs_axiom('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', foundational, rational_basis_as_sufficient_scrutiny).
narrative_ontology:cs_axiom_status(rational_basis_as_sufficient_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', rational_basis_as_sufficient_scrutiny, conventional).
narrative_ontology:cs_reference_frame('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', constitutional_limited_times_ceiling).
narrative_ontology:cs_drift_state('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', post_eldred_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16e3bd49-a73d-4a0d-a0d0-4dab9839dcbd', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, content_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises discretion under Article I to set copyright term length without facing meaningful judicial invalidation. Benefits from institutional autonomy and reduced inter-branch friction, but remains constrained by electoral cycles and lobbying pressures from content industries.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, beneficiary).

% Lobby for and receive repeated term extensions and statutory enforcement expansions. Business models depend on long monopoly terms. Exit is constrained because competitive pressure requires maximizing IP holdings, though firms can diversify across media markets.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, content_industries, beneficiary,
    powerful, generational, constrained, national).

% Administers the rational basis standard of review for copyright term legislation. Bound by precedent and separation-of-powers norms to defer to Congress; lower courts are tightly constrained by Supreme Court doctrine. Maintains institutional legitimacy by avoiding entanglement in statutory economic policy.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Pays higher prices and loses access to works kept out of the public domain by term extensions. Cannot opt out of the copyright system; access to culture and knowledge is delayed indefinitely for the duration of legislative monopolies.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public, payer,
    powerless, civilizational, trapped, national).

% Face thickened monopoly fields and higher licensing burdens when building upon existing works. Individual creators lack leverage to challenge statutory frameworks and must navigate a more enclosed commons.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    moderate, biographical, constrained, national).

% Argue for a justiciable constitutional ceiling on copyright terms and an expanded public domain. File amicus briefs and publish critiques but lack standing or electoral weight to alter the doctrinal standard.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, excluded,
    moderate, civilizational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, content_industries).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates powers by assigning copyright duration policy to the legislature and limiting judicial review to rational basis scrutiny, thereby avoiding judicial micromanagement of complex political-economic allocations and reducing inter-branch friction.
% TRANSFER_FUNCTION: Transfers interpretive authority over the Limited Times Clause from the judiciary to Congress, enabling legislative extension of monopoly terms without constitutional invalidation; the cost is borne by the public domain and future creators who lose access to expired works and face enlarged monopoly fields.
% ABSENT_VOICES: Constitutional originalists who read 'limited times' as a justiciable ceiling, future creators who lack standing to challenge term extensions before harm materializes, and public-domain advocates whose arguments are heard but structurally excluded from effective influence.
% DISAPPEARANCE_RATIONALE: If courts suddenly applied strict scrutiny or a fixed interpretive limit to copyright term length, numerous statutory extensions would face invalidation, the legislative bargaining environment around copyright would shift, and the content industry's reliance on long terms would be destabilized.
% FOUNDING_PROBLEM: Early federal courts needed a doctrinal framework for reviewing copyright statutes that respected congressional authority under Article I while avoiding micromanagement of statutory economic policy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside the benefiting parties attest that modern rational basis deference departs from nineteenth-century precedent (e.g., Wheaton v. Peters) which treated the Copyright Clause as a substantive limit; independent historical analysis and amicus briefs from library and public-domain organizations corroborate the shift from judicial enforcement to legislative discretion.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.45) because the doctrine does not itself collect rents; it is a procedural enabler that licenses downstream extraction by Congress and industry. Suppression is moderate (0.60) because the constraint's persistence depends on courts actively dismissing constitutional challenges and denying standing to public-domain claimants. Theater ratio is moderate (0.40): the separation-of-powers rhetoric is genuine and functional, but an increasing share of deference practice is performative abdication that avoids engaging with the constitutional text. Accessibility collapse (0.65) is substantial because once rational basis review is accepted, challengers cannot articulate a standard that would invalidate any term extension. Resistance (0.45) reflects persistent but unsuccessful academic and advocacy critique.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary seat experiences this constraint as coordination: it solves a separation-of-powers problem by restraining courts from policy-making. The payer seats experience it as extraction: the same restraint removes the only institutional check on legislative enclosure. The engine computes this divergence from the structural data without requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal_judiciary and congressional_authority sit near the beneficiary end: the doctrine expands their institutional autonomy and reduces friction between branches. Content_industries are direct beneficiaries of the legislative extensions the doctrine enables. General_public and future_creators are structural targets: they bear the cost of deferred public-domain entry and thickened monopoly. Public_domain_advocates are excluded from effective influence, mapping to high directionality toward the target end despite their moderate organizational power.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the doctrine as either pure coordination (rope) or pure extraction (snare). It has a genuine coordination functionâpreventing judicial entanglement in statutory economic policyâwhile also producing asymmetric extraction by disabling constitutional limits on term length. If the coordination function atrophied and only the extraction remained (e.g., if courts openly admitted they would never invalidate any term), the constraint would degrade toward a piton or snare; the temporal measurements show modest drift in that direction without having crossed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_deference_necessity,
    'Is rational basis deference on copyright term length a structurally necessary separation-of-powers doctrine, or a constructed ambiguity that licenses legislative extraction?',
    'Comparative analysis of jurisdictions with substantive judicial review of copyright duration limits; if those jurisdictions experience judicial micromanagement without public benefit, deference is necessary. If they maintain stable creative industries with shorter terms, deference is constructed.',
    'If necessary, the constraint is coordination-dominant; if constructed, it is extraction-dominant and approaches snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_necessity, conceptual, 'Whether the deference doctrine is a necessary structural feature or an enabler of extraction.').

omega_variable(
    term_extension_foreclosure,
    'Does the doctrine effectively foreclose any constitutional ceiling on copyright term length, or merely raise the burden of proof for challengers?',
    'Tracking future constitutional challenges: if no plausible term extension can be crafted that courts would invalidate, the doctrine has collapsed into a self-executing license.',
    'If foreclosure is total, effective extraction is higher than base epsilon suggests because accessibility collapse is complete. If partial, some constitutional constraint remains operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_extension_foreclosure, empirical, 'Whether rational basis review leaves any operative constitutional limit.').

omega_variable(
    congressional_discretion_beneficiary,
    'Does congressional authority benefit as an institution from this deference, or is the beneficiary structurally the content industry that captures the legislative process?',
    'Campaign finance and lobbying disclosure analysis mapping copyright term extension votes to industry contributions; if Congress as an institution gains no net power (because it is captured), the named beneficiary is a fiction and the real beneficiary is the content industry.',
    'Would reclassify the beneficiary structure and directionality: congressional_authority shifts from beneficiary to captured agenda_setter, raising extraction and altering seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_discretion_beneficiary, empirical, 'Whether the institutional beneficiary is genuine or a cover for industry capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(copy_tr_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(copy_tr_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(copy_tr_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(copy_be_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(copy_be_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(copy_be_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(copy_su_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(copy_su_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(copy_su_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel copyright_constitutional_mandate. The judicial_ambiguity_reading instantiates the doctrinal deference that enables legislative drift; it is decomposed from the public_scaffold_reading (which treats copyright as temporary means to public domain ends) and the corporate_enclosure_reading (which treats copyright as maximal property) because each reading emits a structurally distinct constraint with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
