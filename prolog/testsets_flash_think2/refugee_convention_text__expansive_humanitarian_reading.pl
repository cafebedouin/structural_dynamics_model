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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention: Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the 'expansive humanitarian reading' of the
 *   1951 Refugee Convention, which interprets its provisions as an unbendable
 *   humanitarian mandate requiring broad protection. This includes
 *   recognizing 'well-founded fear' in cases of generalized violence and
 *   non-state persecution, and encompassing gender, LGBTQ+, and clan-based
 *   persecution within 'particular social group' definitions. From this
 *   reading's perspective, the current state of affairs is highly extractive
 *   from refugees due to states' failures to uphold this expansive mandate,
 *   requiring significant suppression to maintain more restrictive practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention: Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '0458451e-8aaa-4960-b319-78589611d1cc').
narrative_ontology:cs_kernel_codification('0458451e-8aaa-4960-b319-78589611d1cc', fixed_text).
narrative_ontology:cs_authority_grounding('0458451e-8aaa-4960-b319-78589611d1cc', lineage).
narrative_ontology:cs_interpretation_layer_present('0458451e-8aaa-4960-b319-78589611d1cc').
narrative_ontology:cs_reading_relation('0458451e-8aaa-4960-b319-78589611d1cc', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('0458451e-8aaa-4960-b319-78589611d1cc', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('0458451e-8aaa-4960-b319-78589611d1cc', foundational, refoulement_is_absolute_and_expansive).
narrative_ontology:cs_axiom_status(refoulement_is_absolute_and_expansive, holdable).
narrative_ontology:cs_axiom_grounding('0458451e-8aaa-4960-b319-78589611d1cc', refoulement_is_absolute_and_expansive, deontological).
narrative_ontology:cs_axiom('0458451e-8aaa-4960-b319-78589611d1cc', foundational, humanitarian_protection_is_primary_purpose).
narrative_ontology:cs_axiom_status(humanitarian_protection_is_primary_purpose, holdable).
narrative_ontology:cs_axiom_grounding('0458451e-8aaa-4960-b319-78589611d1cc', humanitarian_protection_is_primary_purpose, deontological).
narrative_ontology:cs_axiom('0458451e-8aaa-4960-b319-78589611d1cc', secondary, non_state_persecution_is_valid_fear).
narrative_ontology:cs_axiom_status(non_state_persecution_is_valid_fear, holdable).
narrative_ontology:cs_axiom_grounding('0458451e-8aaa-4960-b319-78589611d1cc', non_state_persecution_is_valid_fear, empirically_contingent).
narrative_ontology:cs_axiom('0458451e-8aaa-4960-b319-78589611d1cc', secondary, generalized_violence_constitutes_fear).
narrative_ontology:cs_axiom_status(generalized_violence_constitutes_fear, holdable).
narrative_ontology:cs_axiom_grounding('0458451e-8aaa-4960-b319-78589611d1cc', generalized_violence_constitutes_fear, empirically_contingent).
narrative_ontology:cs_reference_frame('0458451e-8aaa-4960-b319-78589611d1cc', universal_humanitarian_protection_framework).
narrative_ontology:cs_drift_state('0458451e-8aaa-4960-b319-78589611d1cc', contemporary_migration_crises, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0458451e-8aaa-4960-b319-78589611d1cc', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, humanitarian_advocates).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_bodies).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, refugees_and_asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugees_and_asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing persecution who rely on the Convention for protection. From this reading's perspective, they are victims of states' failures to uphold the expansive mandate, bearing the costs of non-protection, but are also the intended beneficiaries of the mandate itself.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugees_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, refugees_and_asylum_seekers, beneficiary).

% Signatories to the Convention, responsible for implementing its provisions. From this reading's perspective, they resist the broad interpretation, often failing to provide protection, but also bear the costs of compliance when they do.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum, payer).

% The UN Refugee Agency, mandated to supervise the application of the Convention. It advocates for an expansive interpretation and monitors state compliance, but lacks direct enforcement power.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, unhcr, observer).

% NGOs and legal aid organizations that work to uphold and expand refugee protection. They benefit from the Convention's existence as a legal framework for their advocacy, even as they contest its restrictive application.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, humanitarian_advocates, beneficiary,
    organized, biographical, constrained, global).

% Bodies like the European Court of Human Rights or UN treaty bodies that interpret and apply human rights law, often reinforcing an expansive reading of refugee protection. They benefit from the Convention's framework for their work.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_bodies, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_bodies, observer).

% Political actors and movements that advocate for restrictive immigration policies and national sovereignty over international obligations. They are structurally excluded from the normative framework of this expansive reading, as their core premises are rejected.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, anti_immigrant_political_movements, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common international standard for the protection of individuals fleeing persecution, aiming to prevent states from externalizing refugee burdens onto others without consequence and ensuring a baseline of humanitarian treatment.
% TRANSFER_FUNCTION: Ideally, transfers the burden of protection from persecuted individuals to states of asylum, and from states with less capacity to the international community. In practice, it often transfers the burden of non-compliance and precariousness back to refugees.
% ABSENT_VOICES: Restrictive sovereignty advocates and anti-immigrant political movements are excluded from the normative framing of this reading. They would argue for maximum state discretion and minimal international obligation, directly contradicting the expansive humanitarian mandate.
% DISAPPEARANCE_RATIONALE: If this expansive reading of the Convention vanished overnight, states would have significantly less legal obligation to protect refugees, leading to increased refoulement, human rights abuses, and a collapse of the international protection regime, forcing millions into even more precarious situations globally.
% FOUNDING_PROBLEM: The mass displacement and persecution of individuals during and after WWII, and the failure of states to provide adequate protection, leading to immense human suffering and a lack of international legal recourse for those fleeing violence.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR reports, human rights organizations (e.g., Amnesty International, Human Rights Watch), academic studies on forced migration, and direct testimony from refugees consistently corroborate the ongoing nature of mass displacement and persecution, affirming the continued relevance of the founding problem.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the costs borne by refugees due to states' non-compliance with the expansive mandate, including refoulement, detention, and denial of access to asylum. Suppression (0.85) is high because states actively resist this broad interpretation through legal challenges, policy changes, and physical barriers, requiring significant coercive force to maintain restrictive regimes. The theater ratio (0.60) indicates that while states often perform compliance with international law, a substantial portion of their actions undermine the spirit of the expansive mandate. Accessibility collapse (0.90) is severe for refugees, who have few safe and legal alternatives to seeking asylum. Resistance (0.70) is high from states against this interpretation, but also from humanitarian advocates pushing for it.
 *
 * PERSPECTIVAL GAP:
 *   States of asylum experience this constraint as a burdensome obligation that infringes on sovereignty, leading to resistance and attempts to narrow its scope. Refugees, conversely, experience the gap between the mandate's promise and its often-restrictive implementation as a source of profound extraction and vulnerability. Humanitarian advocates see it as a vital, though contested, tool for protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Refugees and asylum seekers are the primary targets of extraction when states fail to uphold the mandate (high d), but are also the intended beneficiaries. States of asylum are the primary payers, bearing the burden of providing protection (high d from their perspective), but also act as agenda-setters in interpreting and enforcing the Convention. UNHCR and international human rights bodies are beneficiaries of the mandate's existence, as it provides the framework for their work (low d). Anti-immigrant political movements are excluded from this reading's normative framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent reading of the Refugee Convention, or merely an advocacy position?',
    'Analysis of international jurisprudence and state practice: if courts and international bodies consistently apply this interpretation, it is a genuine reading. If it remains primarily an advocacy position without legal uptake, it is not.',
    'If a genuine reading, its structural properties (extractiveness, suppression) are valid measures of the gap between mandate and practice. If merely advocacy, its classification as a Tangled Rope might overstate its legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing a legal reading from an advocacy position.').

omega_variable(
    scope_of_well_founded_fear,
    'To what extent does ''well-founded fear'' genuinely encompass generalized violence and non-state persecution in state practice?',
    'Empirical analysis of asylum claim adjudication rates and legal precedents in various jurisdictions for cases involving generalized violence or non-state actors.',
    'If these categories are widely recognized, the extractiveness from refugees is primarily due to other factors. If they are consistently rejected, the extractiveness is directly tied to this definitional contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_well_founded_fear, empirical, 'Empirical scope of ''well-founded fear'' in practice.').

omega_variable(
    refoulement_violations_measurement,
    'How frequently do interdiction and offshore processing practices constitute refoulement violations under this expansive reading?',
    'Legal analysis of specific cases and policies against the criteria of non-refoulement, combined with empirical data on outcomes for individuals subjected to these practices.',
    'A high frequency of violations would significantly increase the measured extractiveness and suppression, reinforcing the Tangled Rope classification. A low frequency would suggest less extraction from these specific practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refoulement_violations_measurement, empirical, 'Quantifying refoulement violations in interdiction/offshore processing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.4).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'refugee_convention_text' kernel. This 'expansive humanitarian reading' emphasizes broad protection, contrasting with the 'restrictive sovereignty reading' (prioritizing state discretion) and the 'procedural integrity reading' (focusing on fair process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
