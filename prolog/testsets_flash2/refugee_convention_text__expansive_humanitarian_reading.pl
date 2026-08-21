% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention: Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive humanitarian reading' of the
 *   1951 Refugee Convention, which interprets 'well-founded fear of
 *   persecution' to include generalized violence and persecution by non-state
 *   actors, and 'particular social group' to encompass gender, LGBTQ+, and
 *   clan-based persecution. It views the Convention as an unbendable
 *   humanitarian mandate requiring broad protection. This is one reading of
 *   the 'refugee_convention_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.45).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.3).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention: Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '2f60c05e-63c3-4b9a-a16b-49a863f4835e').
narrative_ontology:cs_kernel_codification('2f60c05e-63c3-4b9a-a16b-49a863f4835e', fixed_text).
narrative_ontology:cs_authority_grounding('2f60c05e-63c3-4b9a-a16b-49a863f4835e', lineage).
narrative_ontology:cs_interpretation_layer_present('2f60c05e-63c3-4b9a-a16b-49a863f4835e').
narrative_ontology:cs_reading_relation('2f60c05e-63c3-4b9a-a16b-49a863f4835e', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f60c05e-63c3-4b9a-a16b-49a863f4835e', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('2f60c05e-63c3-4b9a-a16b-49a863f4835e', foundational, non_refoulement_absolute_humanitarian_duty).
narrative_ontology:cs_axiom_status(non_refoulement_absolute_humanitarian_duty, holdable).
narrative_ontology:cs_axiom_grounding('2f60c05e-63c3-4b9a-a16b-49a863f4835e', non_refoulement_absolute_humanitarian_duty, deontological).
narrative_ontology:cs_axiom('2f60c05e-63c3-4b9a-a16b-49a863f4835e', foundational, persecution_includes_generalized_violence_non_state_actors).
narrative_ontology:cs_axiom_status(persecution_includes_generalized_violence_non_state_actors, holdable).
narrative_ontology:cs_axiom_grounding('2f60c05e-63c3-4b9a-a16b-49a863f4835e', persecution_includes_generalized_violence_non_state_actors, empirically_contingent).
narrative_ontology:cs_reference_frame('2f60c05e-63c3-4b9a-a16b-49a863f4835e', universal_humanitarian_protection_framework).
narrative_ontology:cs_drift_state('2f60c05e-63c3-4b9a-a16b-49a863f4835e', contemporary_migration_crises_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2f60c05e-63c3-4b9a-a16b-49a863f4835e', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_advocacy_groups).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_organizations).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_facing_persecution).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, universal_human_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing generalized violence, non-state persecution, or persecution based on gender, LGBTQ+, or clan identity, seeking protection under an expansive interpretation of the Convention. Their lives depend on this reading being upheld.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_facing_persecution, payer,
    powerless, immediate, trapped, global).

% Organizations whose mandate is to protect refugees and promote human rights. This reading aligns with their core mission and provides a legal framework for their advocacy and direct assistance work.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_advocacy_groups, beneficiary,
    organized, generational, constrained, global).

% International bodies and NGOs that monitor human rights compliance. This expansive reading strengthens the international human rights framework and provides a basis for holding states accountable.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_organizations, beneficiary,
    institutional, generational, constrained, global).

% Signatory states to the Refugee Convention, tasked with implementing its provisions. Under this reading, they bear the responsibility of broadly interpreting 'well-founded fear' and 'particular social group,' potentially increasing their asylum caseloads and administrative burdens.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum, agenda_setter,
    institutional, biographical, constrained, national).

% Bodies that interpret and apply international law, including the Refugee Convention. This reading guides their jurisprudence towards broader protection and holds states to higher humanitarian standards.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Political factions and legal scholars who prioritize state sovereignty and border control, advocating for a narrow interpretation of refugee protection. This reading directly challenges their policy preferences and legal arguments.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_advocates, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to provide protection to individuals fleeing persecution by establishing a common understanding of who qualifies as a refugee, ensuring that states share the responsibility of humanitarian protection.
% TRANSFER_FUNCTION: Transfers the burden of protection from individuals fleeing persecution to signatory states, and from states with restrictive interpretations to those upholding broader humanitarian principles.
% ABSENT_VOICES: Individuals and groups who would advocate for even broader interpretations of protection, such as those seeking refuge from climate change or economic hardship, are not explicitly covered by this reading but would push for its further expansion.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, many asylum seekers currently protected would lose their legal basis for protection, leading to mass refoulement, increased human suffering, and a significant weakening of the international human rights framework. States would revert to more restrictive interpretations, and the global burden of displacement would shift dramatically.
% FOUNDING_PROBLEM: The post-WWII need to provide legal protection to individuals fleeing persecution, ensuring they would not be returned to places where their lives or freedom were threatened.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, refugee advocacy groups, and international legal scholars corroborate that the problem of persecution and forced displacement remains live and complex, requiring robust interpretation of the Convention. While states of asylum may contest the scope, the underlying problem is widely acknowledged.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while the reading aims to protect, it imposes significant burdens on states of asylum, which can be seen as a form of extraction from their sovereign discretion and resources. Suppression is low (0.30) as this reading is actively championed by powerful advocacy groups and international bodies, but it faces resistance from states seeking to limit their obligations. Theater ratio is low (0.10) as the humanitarian intent is genuine, though its implementation is often contested.
 *
 * PERSPECTIVAL GAP:
 *   States of asylum experience this reading as a constraint on their sovereignty and a burden on their resources, leading to higher perceived extraction. Asylum seekers and their advocates experience it as a vital protection mechanism, perceiving it as a pure coordination or even a subsidy. International courts and tribunals, as interpreters, aim for a balanced application but are often influenced by the humanitarian imperative.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers are the primary targets of persecution, and this reading aims to subsidize their protection (d near 0.0). Refugee and human rights advocacy groups are beneficiaries as this reading aligns with their mission. States of asylum are agenda-setters who bear the costs of implementation (d near 1.0). Restrictive sovereignty advocates are excluded, as this reading directly opposes their views.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_persecution_definition,
    'To what extent does ''well-founded fear'' genuinely encompass generalized violence and non-state persecution, and ''particular social group'' include gender, LGBTQ+, and clan-based persecution, as a matter of international customary law?',
    'Further development of state practice and opinio juris, as evidenced by national court decisions, state legislation, and international legal instruments, or a definitive advisory opinion from the International Court of Justice.',
    'If these expansive interpretations become universally recognized as customary law, the constraint''s legitimacy and enforcement would strengthen, reducing resistance from states. If not, the reading remains a contested interpretation, subject to ongoing political and legal challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_persecution_definition, empirical, 'Ambiguity regarding the customary international law status of expansive persecution definitions.').

omega_variable(
    refoulement_interdiction_scope,
    'Does the principle of non-refoulement, under this reading, unequivocally prohibit interdiction at sea and offshore processing that prevents substantive asylum claims?',
    'Jurisprudence from international human rights courts (e.g., ECtHR, Inter-American Court of Human Rights) specifically addressing the extraterritorial application of non-refoulement to interdiction and offshore processing regimes.',
    'A clear prohibition would significantly increase the burden on states to process claims on their territory or in safe third countries, strengthening the protection for asylum seekers. Ambiguity allows states to continue practices that circumvent substantive assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refoulement_interdiction_scope, conceptual, 'Uncertainty regarding the application of non-refoulement to contemporary border control practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.3).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_law).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, migration_governance_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'refugee_convention_text' kernel. It focuses on the expansive humanitarian interpretation, while 'restrictive_sovereignty_reading' and 'procedural_integrity_reading' offer alternative interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
