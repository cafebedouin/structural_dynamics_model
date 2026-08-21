% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a 'restrictive sovereignty' reading of the
 *   1951 Refugee Convention and its 1967 Protocol. Under this reading, the
 *   Convention serves as a minimum floor for international obligations, but
 *   states retain maximum discretion in its interpretation and application.
 *   Key terms like 'well-founded fear' are narrowly construed to require
 *   individualized persecution proof, excluding generalized violence or
 *   non-state persecution. 'Particular social group' is limited to immutable
 *   characteristics, and high admissibility screening, offshore processing,
 *   and strict border controls are deemed permissible. This reading
 *   prioritizes state sovereignty and border control over expansive
 *   humanitarian protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '9ac24d8b-d9c8-4ee7-9180-0893b396b015').
narrative_ontology:cs_kernel_codification('9ac24d8b-d9c8-4ee7-9180-0893b396b015', fixed_text).
narrative_ontology:cs_authority_grounding('9ac24d8b-d9c8-4ee7-9180-0893b396b015', extraction).
narrative_ontology:cs_interpretation_layer_present('9ac24d8b-d9c8-4ee7-9180-0893b396b015').
narrative_ontology:cs_reading_relation('9ac24d8b-d9c8-4ee7-9180-0893b396b015', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ac24d8b-d9c8-4ee7-9180-0893b396b015', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('9ac24d8b-d9c8-4ee7-9180-0893b396b015', foundational, state_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(state_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9ac24d8b-d9c8-4ee7-9180-0893b396b015', state_sovereignty_supremacy, conventional).
narrative_ontology:cs_axiom('9ac24d8b-d9c8-4ee7-9180-0893b396b015', foundational, individualized_persecution_proof).
narrative_ontology:cs_axiom_status(individualized_persecution_proof, holdable).
narrative_ontology:cs_axiom_grounding('9ac24d8b-d9c8-4ee7-9180-0893b396b015', individualized_persecution_proof, conventional).
narrative_ontology:cs_reference_frame('9ac24d8b-d9c8-4ee7-9180-0893b396b015', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('9ac24d8b-d9c8-4ee7-9180-0893b396b015', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ac24d8b-d9c8-4ee7-9180-0893b396b015', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, border_agencies).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_principle).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, national_security_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States interpret the Convention to maximize their discretion over who is admitted and under what conditions. They benefit from reduced obligations and control over borders, often citing national security and economic concerns. They actively enforce border controls and restrictive asylum procedures.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, beneficiary).

% These agencies implement the restrictive interpretations of the Convention, managing border controls, detention centers, and asylum processing. They bear the operational costs of enforcement but gain institutional power and resources from their role in managing migration flows.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_agencies, agenda_setter,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_agencies, payer).

% Individuals fleeing persecution who seek protection under the Convention. They face high barriers to entry, rigorous and often skeptical screening processes, and the burden of proving individualized persecution, often without adequate legal support. Their options are limited to the asylum process or dangerous irregular migration.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, local).

% Those who meet the narrow criteria of 'well-founded fear' and 'particular social group' under this reading. Even when recognized, they often face precarious legal status, limited integration opportunities, and the constant threat of deportation if conditions in their home country are deemed to have improved.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugees, payer,
    powerless, biographical, identity_locked, global).

% Organizations and individuals who monitor state compliance with international human rights law, including the Refugee Convention. They document abuses, provide legal aid, and advocate for more expansive and humanitarian interpretations, but have no direct power to alter state policy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Bodies like the European Court of Human Rights or the International Court of Justice that can issue rulings on state obligations under international law. However, their jurisdiction is often limited, and enforcement depends on state cooperation, which can be resisted by states prioritizing sovereignty.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a minimum international legal floor for states to manage cross-border movements of people fleeing persecution, aiming to prevent unilateral burden-shifting and ensure some level of international cooperation on migration flows.
% TRANSFER_FUNCTION: Transfers the burden of proof and the risk of non-protection onto asylum seekers, while transferring discretion and control over borders to sovereign states. It also transfers the costs of enforcement and detention to states, but these are often offset by perceived benefits of border control.
% ABSENT_VOICES: Refugee-led organizations and stateless persons are largely absent from the formal interpretive processes, though their experiences are documented by human rights advocates. Their perspectives would highlight the human cost of restrictive interpretations and the need for broader protection criteria.
% DISAPPEARANCE_RATIONALE: If the Convention vanished overnight, states would likely revert to purely national, potentially more restrictive, and uncoordinated migration policies. This would lead to increased human rights violations at borders, greater chaos in migration flows, and a breakdown of any international burden-sharing mechanisms, however minimal.
% FOUNDING_PROBLEM: To provide a legal framework for the protection of persons fleeing persecution, particularly in the aftermath of World War II, to prevent states from simply turning away those in need of safety and to establish a shared, albeit minimal, international responsibility.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and border agencies (beneficiaries) often argue the founding problem of managing migration and security remains live, justifying restrictive measures. Human rights advocates and some international legal scholars (outside the benefiting parties) contend that the original humanitarian intent is severely undermined by current interpretations, rendering the problem 'dead' in its original spirit for many asylum seekers.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because this reading allows states to significantly limit their obligations, effectively extracting control and discretion from asylum seekers who bear the costs of restrictive policies. Suppression is very high (0.85) due to active enforcement through border controls, detention, and complex legal procedures designed to deter and exclude. Theater ratio (0.45) is moderate-high, reflecting the gap between the Convention's stated humanitarian aims and the actual restrictive practices, where some procedural elements are maintained performatively while the substantive protection is eroded. Accessibility collapse (0.65) is substantial as alternatives to the state-controlled asylum process are severely limited, and resistance (0.70) is high from advocates and some legal challenges, but often ineffective against state power.
 *
 * PERSPECTIVAL GAP:
 *   Sovereign states and border agencies perceive this reading as a necessary framework for managing national security and migration flows, balancing humanitarian concerns with practical realities. Asylum seekers and refugees, however, experience it as a formidable barrier, a system designed to exclude rather than protect, where the 'minimum floor' is often below a meaningful threshold of safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and their border agencies are the primary beneficiaries, gaining discretion, control, and reduced obligations (low directionality). Asylum seekers and refugees are the clear targets, bearing the costs of exclusion, detention, and the burden of proof (high directionality). Human rights advocates and international courts act as observers, attempting to influence the interpretation but not directly benefiting or paying in the same structural sense.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights how the Convention's original mandate to protect refugees can be reinterpreted to serve state interests in border control. The 'minimum floor' coordination function persists, but the 'maximum sovereign discretion' component allows for significant extraction, preventing the constraint from atrophying into a Piton. Instead, it remains a Tangled Rope, actively maintained and enforced to manage both international cooperation and national interests, even as its humanitarian function is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    well_founded_fear_scope,
    'Does ''well-founded fear of persecution'' genuinely require individualized proof, or can it encompass generalized violence and non-state persecution, as argued by other readings?',
    'Judicial rulings from higher international courts or a new international protocol explicitly clarifying the scope of ''well-founded fear'' to include broader contexts.',
    'If generalized violence is included, the victim set would expand significantly, increasing state obligations and reducing extraction from asylum seekers. If individualized proof is reaffirmed, the current restrictive interpretation is solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(well_founded_fear_scope, conceptual, 'Ambiguity in the scope of ''well-founded fear'' and its implications for protection.').

omega_variable(
    particular_social_group_definition,
    'Is ''particular social group'' strictly limited to immutable characteristics, or can it evolve to include groups defined by gender, sexual orientation, or clan affiliation, as argued by other readings?',
    'Evolution of state practice and international jurisprudence, or a new interpretive guidance from UNHCR or other authoritative bodies.',
    'An expansive definition would broaden the protected categories, increasing state obligations and reducing extraction. A narrow definition reinforces the restrictive approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_definition, conceptual, 'Ambiguity in the definition of ''particular social group'' and its implications for protection.').

omega_variable(
    sovereign_discretion_vs_international_obligation,
    'What is the true balance between state sovereignty in migration control and international obligations under the Convention? Is ''maximum sovereign discretion'' compatible with the Convention''s object and purpose?',
    'A definitive advisory opinion from the International Court of Justice or a new, widely ratified international treaty that rebalances these principles.',
    'If discretion is found to be overly broad, states would face increased pressure to align practices with humanitarian principles. If current discretion is upheld, the restrictive reading gains further legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_discretion_vs_international_obligation, preference, 'The fundamental tension between state sovereignty and international human rights obligations in refugee law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.2).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.45).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, national_asylum_laws).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, international_human_rights_treaties).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, eu_asylum_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'refugee_convention_text' kernel, focusing on state sovereignty and restrictive interpretation. It is linked to the 'expansive_humanitarian_reading' and 'procedural_integrity_reading' as sibling interpretations of the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
