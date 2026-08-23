% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright Constitutional Mandate â Public Scaffold Reading
 *   domain: legal/political
 *
 * SUMMARY:
 *   This constraint instantiates the public_scaffold_reading of the
 *   copyright_constitutional_mandate kernel. The reading treats Article I,
 *   Section 8, Clause 8 of the U.S. Constitution as establishing a
 *   transitional coordination mechanism: Congress may grant temporary
 *   exclusive rights to authors and inventors, but only as a means to promote
 *   the progress of science and useful arts and ultimately enrich the public
 *   domain. The monopoly is instrumental and carries a built-in sunset
 *   (limited times). Sibling readings include the
 *   corporate_enclosure_reading, which treats copyright as a maximal property
 *   right demanding perpetual extension, and the judicial_ambiguity_reading,
 *   which treats term length as a legislative discretion subject to minimal
 *   judicial review. The metrics are authored independently of the claimed
 *   scaffold type and show moderate drift toward extraction and theatricality
 *   as enclosure interests capture the legislative process.
 *
 * KEY AGENTS:
 *   - general_public: Primary beneficiary (moderate/mobile) â gains public domain access and fair use rights.
 *   - subsequent_authors: Secondary beneficiary (moderate/mobile) â builds upon the promised commons.
 *   - federal_legislature: Agenda-setter (institutional/constrained) â sets terms within constitutional bounds.
 *   - copyright_holders: Instrumental beneficiary (powerful/constrained) â receives temporary exclusivity, lobbies for extension.
 *   - federal_judiciary: Analytical observer (institutional/analytical) â interprets the Progress Clause.
 *   - future_generations: Excluded voice (powerless/trapped) â stands to inherit the public domain but has no present representation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.4).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.45).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright Constitutional Mandate â Public Scaffold Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "legal/political").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '53ab516b-4d35-43c3-b3ee-87832cb9f477').
narrative_ontology:cs_kernel_codification('53ab516b-4d35-43c3-b3ee-87832cb9f477', fixed_text).
narrative_ontology:cs_authority_grounding('53ab516b-4d35-43c3-b3ee-87832cb9f477', lineage).
narrative_ontology:cs_interpretation_layer_present('53ab516b-4d35-43c3-b3ee-87832cb9f477').
narrative_ontology:cs_reading_relation('53ab516b-4d35-43c3-b3ee-87832cb9f477', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('53ab516b-4d35-43c3-b3ee-87832cb9f477', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('53ab516b-4d35-43c3-b3ee-87832cb9f477', foundational, copyright_instrumental_to_public_domain).
narrative_ontology:cs_axiom_status(copyright_instrumental_to_public_domain, holdable).
narrative_ontology:cs_axiom_grounding('53ab516b-4d35-43c3-b3ee-87832cb9f477', copyright_instrumental_to_public_domain, conventional).
narrative_ontology:cs_axiom('53ab516b-4d35-43c3-b3ee-87832cb9f477', foundational, limited_times_requires_effective_sunset).
narrative_ontology:cs_axiom_status(limited_times_requires_effective_sunset, holdable).
narrative_ontology:cs_axiom_grounding('53ab516b-4d35-43c3-b3ee-87832cb9f477', limited_times_requires_effective_sunset, conventional).
narrative_ontology:cs_reference_frame('53ab516b-4d35-43c3-b3ee-87832cb9f477', limited_times_public_domain).
narrative_ontology:cs_drift_state('53ab516b-4d35-43c3-b3ee-87832cb9f477', post_term_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53ab516b-4d35-43c3-b3ee-87832cb9f477', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, general_public).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, subsequent_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, progress_clause_limited_times).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_domain_enrichment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate intended beneficiary of the copyright system. Gains access to works after the limited monopoly expires, and benefits during the term from fair use and the constitutional requirement that monopoly serve public enrichment rather than private enclosure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, general_public, beneficiary,
    moderate, civilizational, mobile, national).

% Build upon prior works once they enter the public domain. Rely on the promise that copyright is temporary and instrumental, which reduces long-term clearance costs and sustains a cultural commons for derivative creation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, subsequent_authors, beneficiary,
    moderate, generational, mobile, national).

% Holds constitutional authority to grant exclusive rights only for limited times to promote progress. Sets copyright terms and scope, subject to political pressure from enclosure industries but textually constrained by the Progress Clause.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, federal_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Receive temporary exclusive rights to reproduce and distribute works. Under this reading they are instrumental beneficiaries of a transitional coordination mechanism, not the ultimate intended beneficiaries, though they lobby for term extensions that would undermine the scaffold.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders, beneficiary,
    powerful, biographical, constrained, national).

% Interprets the Progress Clause and evaluates whether congressional action remains within limited times and serves the public good. Can enforce or erode the scaffold reading through statutory interpretation and constitutional review.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Stand to benefit most from a robust public domain but have no seat at the legislative or judicial table. Their interests are systematically discounted in present-value political economy because they do not yet exist to advocate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public goods problem in creative production by granting temporary exclusive rights that incentivize authorship, while guaranteeing eventual release into the public domain so knowledge and culture remain accessible for future use and building.
% TRANSFER_FUNCTION: Moves temporary exclusive rights over reproduction and distribution from the public domain into the hands of creators; after the constitutionally limited term, the protected work returns to the public domain.
% ABSENT_VOICES: Future generations who would inherit a rich public domain are structurally unrepresented; public-domain advocacy groups and access-to-knowledge scholars are present in discourse but politically marginalized relative to enclosure industries.
% DISAPPEARANCE_RATIONALE: If the temporary-monopoly framework vanished overnight, existing copyrighted works would immediately enter the public domain, creative-industry business models would shift away from statutory exclusivity, and the incentive structure for new production would reorganize around patronage, markets, and commons-based mechanisms.
% FOUNDING_PROBLEM: Creative and useful works are non-rivalrous and non-excludable, leading to potential underproduction if creators cannot recover costs, yet perpetual private enclosure withholds knowledge from society.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and legal scholars outside the copyright industry attest that the public goods problem is real but that current statutory terms vastly exceed empirical estimates of the necessary incentive window; legislative history and amicus briefs from library and educational sectors corroborate the public-good framing from non-beneficiary seats.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.40) because the temporary monopoly inherently restricts public access to works during the term, which is a real cost, but it is bounded by the constitutional sunset. Suppression (0.45) reflects the active enforcement infrastructure needed to maintain temporary exclusivity against infringement. Theater_ratio (0.30 and rising) captures the growing gap between the public-good rhetoric and the legislative practice of repeated term extension. Accessibility_collapse (0.50) reflects that fair use and public domain alternatives remain legally available but are increasingly complex to navigate. Resistance (0.55) registers the active pushback from public-domain advocates and access movements against enclosure. The measurement series share a single time grid and show drift consistent with scaffold capture.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading, the primary divergence is between the diffuse public beneficiaries (low d â subsidized by the promise of eventual open access) and the concentrated copyright holders (low-to-moderate d â subsidized by the temporary grant but structurally opposed to the sunset). The federal legislature sits at a different power and time-horizon level: its generational horizon and constrained political exit produce a distinct computed seat. The excluded future_generations seat would compute as high-d target if it were present, but its structural absence is precisely what the reading identifies as the regime's democratic deficit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the general public and subsequent authors (low d â the constraint is designed to enrich them over the long run). Copyright holders receive the temporary monopoly (low-to-moderate d â they are instrumentally subsidized). No victims are declared, consistent with the coordination-regime framing. The directional asymmetry is modest because the scaffold is meant to be mutually beneficial during the term and overwhelmingly beneficial after sunset; extraction is bounded by the limited-times requirement.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling the temporary monopoly as pure extraction (snare) or as an unconditional coordination mechanism (rope). The built-in sunset clause â 'limited times' â is the structural marker that this is transitional support, not a steady-state property regime. Mandatrophy risk is tracked through the temporal measurements: rising theater_ratio and extractiveness indicate that the sunset is becoming performative rather than functional. If the founding problem (underprovision of public goods) is solved but the monopoly persists indefinitely, the constraint would drift toward piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_domain_beneficiary_ambiguity,
    'Does the copyright system as currently administered structurally benefit the public domain, or have term extensions and enclosure mechanisms inverted the beneficiary structure?',
    'Empirical measurement of effective copyright terms versus public domain entry rates; judicial adoption of public-scaffold canons of construction in statutory interpretation.',
    'If the public domain is not the effective beneficiary, this reading''s classification as scaffold fails and the constraint drifts toward tangled_rope or snare under the same constitutional text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_beneficiary_ambiguity, empirical, 'Whether the public domain remains the actual beneficiary of copyright law.').

omega_variable(
    sunset_clause_erosion,
    'Has the limited times requirement functionally eroded to the point that the sunset clause no longer disciplines the monopoly grant?',
    'Constitutional challenge establishing a finite upper bound on copyright term; comparative analysis of international term limits and legislative extension patterns.',
    'If limited times is effectively unlimited, the scaffold has lost its transitional character and become a permanent extraction structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_erosion, empirical, 'Whether the constitutional sunset remains effective.').

omega_variable(
    kernel_reading_location,
    'How would the classification of the copyright constitutional mandate change if the corporate_enclosure_reading or judicial_ambiguity_reading were adopted instead of the public_scaffold_reading?',
    'Compare the compiled constraints across the three readings: corporate_enclosure would reclassify beneficiaries to copyright_holders and raise epsilon substantially; judicial_ambiguity would deflate epsilon by removing judicial enforcement of the public-good purpose.',
    'Selection of reading determines whether the constraint is a scaffold (public-scaffold), a snare (corporate-enclosure), or a diffuse deference regime (judicial-ambiguity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location of this reading within the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(copy_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(copy_tr_t50, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(copy_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(copy_be_t50, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(copy_su_t40, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(copy_su_t50, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the copyright_constitutional_mandate constraint family. The kernel (Article I, Section 8, Clause 8) is read by public_scaffold_reading as a temporary coordination mechanism (scaffold), by corporate_enclosure_reading as a property-rights snare, and by judicial_ambiguity_reading as a deference regime. The epsilon values and beneficiary structures differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
