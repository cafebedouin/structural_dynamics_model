% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign Target Statutory Constraint (Strict Reading)
 *   domain: constitutional/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint story instantiates the foreign_target_strict_reading of
 *   the FISA Section 702 statutory kernel. The reading holds that the
 *   statutory text — requiring that targets be non-U.S. persons reasonably
 *   believed to be located abroad, mandating minimization procedures that
 *   delete incidentally collected U.S. person communications, and prohibiting
 *   FBI queries of the 702 database for domestic criminal investigations —
 *   operates as a genuine coordination mechanism (rope) for foreign
 *   intelligence. The statutory design enables collection at scale against
 *   foreign adversaries while embedding structural protections for U.S.
 *   persons that this reading argues satisfy Fourth Amendment requirements.
 *   The claimed type is rope (coordination with built-in safeguards); the
 *   authored metrics reflect low extractiveness from U.S. persons (ε=0.15),
 *   low suppression (statutory/judicial framework, not coercion), and low
 *   theater (the framework is functional, not performative). The engine will
 *   compute per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.2).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign Target Statutory Constraint (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'd217425e-ba42-41b9-a9c2-51cea488e842').
narrative_ontology:cs_kernel_codification('d217425e-ba42-41b9-a9c2-51cea488e842', fixed_text).
narrative_ontology:cs_authority_grounding('d217425e-ba42-41b9-a9c2-51cea488e842', lineage).
narrative_ontology:cs_interpretation_layer_present('d217425e-ba42-41b9-a9c2-51cea488e842').
narrative_ontology:cs_reading_relation('d217425e-ba42-41b9-a9c2-51cea488e842', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('d217425e-ba42-41b9-a9c2-51cea488e842', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('d217425e-ba42-41b9-a9c2-51cea488e842', foundational, statutory_text_sufficient_for_fourth_amendment).
narrative_ontology:cs_axiom_status(statutory_text_sufficient_for_fourth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('d217425e-ba42-41b9-a9c2-51cea488e842', statutory_text_sufficient_for_fourth_amendment, conventional).
narrative_ontology:cs_axiom('d217425e-ba42-41b9-a9c2-51cea488e842', secondary, minimization_requires_deletion).
narrative_ontology:cs_axiom_status(minimization_requires_deletion, holdable).
narrative_ontology:cs_axiom_grounding('d217425e-ba42-41b9-a9c2-51cea488e842', minimization_requires_deletion, conventional).
narrative_ontology:cs_axiom('d217425e-ba42-41b9-a9c2-51cea488e842', secondary, domestic_queries_categorically_prohibited).
narrative_ontology:cs_axiom_status(domestic_queries_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('d217425e-ba42-41b9-a9c2-51cea488e842', domestic_queries_categorically_prohibited, conventional).
narrative_ontology:cs_reference_frame('d217425e-ba42-41b9-a9c2-51cea488e842', statutory_foreign_target_framework).
narrative_ontology:cs_drift_state('d217425e-ba42-41b9-a9c2-51cea488e842', post_snowden_reauthorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d217425e-ba42-41b9-a9c2-51cea488e842', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_targets).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_foreign_target_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, minimization_as_deletion).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, domestic_query_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose international communications may be incidentally collected under 702; protected by statutory targeting restrictions (foreign target requirement), minimization procedures requiring deletion of incidentally collected U.S. person data, and categorical prohibition on FBI queries of the 702 database for domestic criminal investigations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, biographical, constrained, national).

% NSA, CIA, and FBI foreign intelligence components that operate the 702 collection program; the statutory framework authorizes collection against non-U.S. persons abroad while imposing targeting, minimization, and query restrictions that structure their operations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community, beneficiary).

% Non-U.S. persons abroad who are the legitimate targets of 702 collection; they bear the surveillance burden as the objects of foreign intelligence gathering, with no access to U.S. statutory or constitutional protections.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_targets, payer,
    powerless, biographical, trapped, global).

% FBI domestic law enforcement division that is categorically prohibited under this reading from querying the 702 database for domestic criminal investigations; loses an investigative tool but is constrained by the statutory foreign-target-only design.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic, payer,
    institutional, biographical, constrained, national).

% Foreign Intelligence Surveillance Court that reviews and approves annual certifications, targeting procedures, and minimization procedures; provides judicial oversight of the statutory framework's implementation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Legislative body that enacted FISA Section 702 (2008) and has reauthorized it multiple times (2012, 2017, 2023); sets the statutory framework and can amend the foreign target requirement, minimization standards, and query restrictions.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables foreign intelligence collection against non-U.S. persons abroad at the speed and scale required by modern threats, while statutorily constraining that authority through the foreign target requirement, minimization procedures, and domestic query prohibitions to protect U.S. persons' Fourth Amendment interests.
% TRANSFER_FUNCTION: Moves foreign intelligence value from foreign targets (non-U.S. persons abroad) to the U.S. intelligence community; moves privacy protections to U.S. persons through statutory targeting restrictions, mandatory deletion of incidentally collected U.S. person communications, and categorical bar on FBI domestic queries of the 702 database.
% ABSENT_VOICES: Foreign targets (non-U.S. persons abroad) have no representation in the U.S. legislative or judicial process that authorizes and oversees their surveillance; privacy advocates and civil liberties organizations who argue that statutory protections are insufficient in practice due to compliance incidents and broad targeting certifications are excluded from the classified FISC proceedings that interpret the statute.
% DISAPPEARANCE_RATIONALE: If the 702 statutory framework vanished overnight, foreign intelligence collection would revert to either executive authority under Article II (less constrained, no statutory minimization) or traditional FISA Title I warrants (more constrained, slower, individualized); U.S. persons would lose statutory minimization protections and the categorical domestic query prohibition, relying solely on the Fourth Amendment as interpreted by the constitutional_floor_reading.
% FOUNDING_PROBLEM: Post-9/11 intelligence gap: foreign adversaries increasingly used U.S. communications infrastructure (email, social media, cloud services) to communicate, but traditional FISA Title I required individualized probable cause warrants for each target — too slow and narrow for foreign intelligence collection at scale against non-U.S. persons abroad.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community and executive branch attest the problem remains live, citing encryption proliferation and adversary use of U.S. platforms; the 9/11 Commission Report and multiple PCLOB reports corroborate the original intelligence gap; civil liberties groups and some legislators contest whether the current threat environment justifies 702's scope versus narrower alternatives.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the statutory framework's primary operation is foreign intelligence collection against legitimate foreign targets; U.S. persons are structurally protected by targeting restrictions, minimization-as-deletion, and domestic query prohibition. The 0.15 reflects residual risk from compliance incidents and the inherent privacy impact of incidental collection even with minimization. Suppression is low (0.20) because the constraint operates through statutory authority and FISC oversight, not through coercive suppression of alternatives — the constitutional_floor_reading and incidental_collection_reading remain live contestations. Theater ratio is low (0.10) because the minimization and query restrictions are operationally implemented, not merely performative. Accessibility collapse is moderate (0.30) because alternative frameworks (constitutional floor, incidental collection) remain structurally available. Resistance is moderate (0.40) reflecting ongoing litigation, legislative reform proposals, and compliance controversies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (foreign_targets, fbi_domestic) and beneficiary seats (us_persons, intelligence_community) will compute differently: from the intelligence community's position, the arrangement is a functional coordination mechanism they built and operate under law; from foreign targets' position, it is extraction without representation; from FBI domestic's position, it is a constraint that forecloses a useful tool. The engine computes this divergence from the structural data — the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are structural beneficiaries (d near 0.0): the statute affirmatively protects them through foreign target requirement, minimization-as-deletion, and domestic query ban. Intelligence community is agenda-setter/beneficiary (d near 0.0): they receive the foreign intelligence value authorized by the statute. Foreign targets are payers (d near 1.0): they bear the surveillance burden as legitimate targets. FBI domestic is a payer (d ~0.7): categorically prohibited from using 702 for domestic cases, losing an investigative tool. FISC and Congress are observers/agenda-setters with analytical exit. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign intelligence gap against adversaries using U.S. infrastructure) remains live per intelligence community and PCLOB corroboration. The arrangement has not atrophied into a piton — it is actively used, reauthorized, and modified. However, the tension between the statutory design (this reading) and operational practice (documented compliance incidents, FBI query controversies) creates a mandatrophy risk: if practice drifts toward the incidental_collection_reading while the statutory text remains unchanged, the constraint becomes a false summit — claimed as protective rope, operating as extractive snare. The omega variables capture this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (foreign_target_strict_reading) of the contested kernel fisa_702_statutory_text; what are the structural consequences of treating this reading as a standalone ε-invariant constraint?',
    'Compare engine classifications across all three kernel readings (this reading, incidental_collection_reading, constitutional_floor_reading) to measure how the same statutory text produces divergent constraint types from different interpretive commitments.',
    'If this reading computes as rope/mountain while sibling readings compute as snare/tangled_rope, the kernel''s classification is reading-dependent — confirming the ε-invariance principle that the label ''FISA 702'' covers multiple structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading decomposition: whether statutory text alone suffices as a constraint or requires constitutional supplementation.').

omega_variable(
    minimization_implementation_gap,
    'Does statutory minimization in practice achieve deletion of incidentally collected U.S. person communications, or does it function as access restriction with retention (as the incidental_collection_reading claims)?',
    'Declassified FISC opinions, PCLOB reports, and ODNI transparency reports on compliance incidents involving retention of U.S. person data beyond minimization timelines.',
    'If minimization is access-restriction-with-retention, the constraint''s extractiveness from U.S. persons is higher than 0.15 and the domestic query prohibition is undermined by data availability — shifting classification toward tangled_rope or snare for the us_persons seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_implementation_gap, empirical, 'Whether minimization procedures achieve statutory deletion or de facto retention.').

omega_variable(
    fbi_query_compliance,
    'Does the FBI in practice comply with the categorical prohibition on querying the 702 database for domestic criminal investigations, or do ''foreign intelligence purpose'' justifications function as a loophole?',
    'DOJ OIG audits, FISC compliance opinions, and congressional oversight hearings on FBI 702 query practices (e.g., 2018-2024 query count disclosures, ''U.S. person query'' metrics).',
    'If FBI domestic queries for criminal investigations occur under foreign intelligence pretexts, the fbi_domestic seat''s directionality shifts toward beneficiary (they gain access) and the us_persons seat''s extraction rises — the constraint operates as tangential_rope for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fbi_query_compliance, empirical, 'Whether the domestic query prohibition is structurally enforced or nominal.').

omega_variable(
    extraction_stability_under_compliance_incidents,
    'Does the ε=0.15 assessment hold given documented compliance incidents (overcollection, retention violations, improper queries), or does the accumulation of incidents indicate structural extraction higher than the statutory design suggests?',
    'Longitudinal analysis of compliance incident rates, severity, and remediation across reauthorization cycles (2008, 2012, 2017, 2023); compare to statutory design parameters.',
    'If compliance incidents represent structural drift rather than deviations, the constraint''s true extractiveness for us_persons may be 0.30-0.40, reclassifying the us_persons seat from rope to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_stability_under_compliance_incidents, empirical, 'Whether documented compliance incidents reflect operational noise or structural extraction drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 15, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 3, 0.18).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 15, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This reading, the incidental_collection_reading, and the constitutional_floor_reading form the fisa_702_statutory_text constraint family. This reading claims the statutory text itself (foreign target requirement, minimization-as-deletion, domestic query prohibition) is a sufficient coordination mechanism (rope) protecting U.S. persons. The incidental_collection_reading claims the same statute permits retention and query of incidental collection, making it extractive (tangled_rope/snare). The constitutional_floor_reading claims the Constitution requires a warrant regardless of statute, making the statutory framework a false summit (mountain claim masking extraction). The three readings have divergent ε values (0.15, ~0.45, ~0.60) and divergent victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__foreign_target_strict_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
