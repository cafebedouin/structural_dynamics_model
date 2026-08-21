% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate: Dual Obligation for Indigenous Rights
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates a reading of the Balfour Mandate
 *   instruments that emphasizes the equal or superior obligation to protect
 *   existing Arab civil/political rights and land tenure, subordinating the
 *   'national home' clause to self-determination norms and
 *   minority-protection principles. From this perspective, the Mandate was a
 *   Tangled Rope, genuinely attempting coordination but with significant
 *   extraction from Zionist aspirations, enforced by British administration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.8).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.9).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.8).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate: Dual Obligation for Indigenous Rights").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e350351d-7c47-4dba-b6f5-0c3105010463').
narrative_ontology:cs_kernel_codification('e350351d-7c47-4dba-b6f5-0c3105010463', formalized).
narrative_ontology:cs_authority_grounding('e350351d-7c47-4dba-b6f5-0c3105010463', lineage).
narrative_ontology:cs_interpretation_layer_present('e350351d-7c47-4dba-b6f5-0c3105010463').
narrative_ontology:cs_reading_relation('e350351d-7c47-4dba-b6f5-0c3105010463', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('e350351d-7c47-4dba-b6f5-0c3105010463', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('e350351d-7c47-4dba-b6f5-0c3105010463', foundational, indigenous_rights_supremacy).
narrative_ontology:cs_axiom_status(indigenous_rights_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e350351d-7c47-4dba-b6f5-0c3105010463', indigenous_rights_supremacy, deontological).
narrative_ontology:cs_axiom('e350351d-7c47-4dba-b6f5-0c3105010463', foundational, self_determination_principle).
narrative_ontology:cs_axiom_status(self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('e350351d-7c47-4dba-b6f5-0c3105010463', self_determination_principle, conventional).
narrative_ontology:cs_reference_frame('e350351d-7c47-4dba-b6f5-0c3105010463', league_of_nations_mandate_principles).
narrative_ontology:cs_drift_state('e350351d-7c47-4dba-b6f5-0c3105010463', post_balfour_declaration_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e350351d-7c47-4dba-b6f5-0c3105010463', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from land transfer restrictions and political recognition of their majority status, but constrained by British ultimate authority and Zionist pressure. They sought to leverage this reading for greater self-determination.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    powerful, biographical, constrained, regional).

% Benefited from the legal protection of their land tenure and civil rights, which aimed to prevent dispossession. However, they faced ongoing pressure from Zionist immigration and British policies that often undermined these protections in practice.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, generational, constrained, local).

% Paid the cost of restricted land acquisition, limited immigration, and political subordination to the Arab majority. They actively resisted these constraints, viewing them as impediments to establishing a Jewish proto-state.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, constrained, global).

% Tasked with implementing the dual obligation, they were constrained by the need to protect Arab rights while also facilitating a Jewish national home. This often placed them in an untenable position, caught between conflicting demands and international legal principles.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, payer).

% The ultimate authority granting the mandate, observing its implementation and receiving reports. From this reading's perspective, the League was responsible for upholding the principles of self-determination and minority protection, but had limited direct enforcement capacity.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, diffuse).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the administration of Palestine under international law, ensuring the protection of existing civil and political rights and land tenure for the non-Jewish population, while also facilitating the establishment of a Jewish national home without prejudice to these rights.
% TRANSFER_FUNCTION: Restricts the transfer of land from Arab to Jewish ownership and limits Jewish immigration to prevent demographic displacement, thereby preserving the existing demographic and land tenure balance for the Arab population. It transfers political legitimacy and a path to self-determination to the Arab majority.
% ABSENT_VOICES: Palestinian Arab nationalists advocating for immediate, unfettered self-determination and an end to British rule, as well as those who rejected any notion of a Jewish national home, were largely excluded from the formal interpretive process of the Mandate instruments.
% DISAPPEARANCE_RATIONALE: If this interpretation of the Mandate vanished, the legal basis for protecting Arab land and political rights would disappear, leading to accelerated land transfers, unrestricted immigration, and a rapid demographic shift, fundamentally altering the political and social landscape of Palestine.
% FOUNDING_PROBLEM: To reconcile the Balfour Declaration's promise of a Jewish national home with the League of Nations' commitment to self-determination and the protection of existing non-Jewish populations in mandated territories, preventing dispossession and ensuring equitable treatment under international law.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian Arab leaders and international legal scholars consistently argued that the founding problem of protecting indigenous rights remained live and was being undermined. Zionist organizations and many British officials attested the problem was being addressed, or that the 'national home' aspect took precedence, leading to a contested status.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.80) because this reading imposes substantial limitations on Zionist goals of land acquisition and demographic transformation, effectively extracting these opportunities. Suppression is very high (0.90) as it requires active British enforcement to restrict immigration and land transfers against strong Zionist pressure. Theater ratio is low (0.20) because, from this reading's perspective, the protective measures were a genuine, albeit often contested, function of the Mandate, not merely performative. Resistance is high (0.80) due to consistent Zionist opposition to these limitations.
 *
 * PERSPECTIVAL GAP:
 *   The British administrators, caught between conflicting obligations, would experience this as a highly constrained and costly coordination problem. Zionist organizations would experience it as an extractive snare, actively suppressing their legitimate aspirations. Palestinian Arab communities would see it as a necessary, though often imperfect, protective mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are beneficiaries, as the constraint aims to protect their existing rights and land. Zionist organizations are targets, as the constraint actively limits their aspirations. British administrators are both agenda-setters and payers, as they are tasked with enforcing this difficult balance and bear the costs of managing the inherent conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_hierarchy_ambiguity,
    'Does the ''national home'' clause inherently subordinate to or stand co-equal with the protection of existing non-Jewish rights within the Mandate instruments?',
    'Adjudication by an international court with binding authority, or a clear, unambiguous amendment to the Mandate text.',
    'If subordinate, this reading is strengthened and its internal coherence improved; if co-equal, the mandate''s internal contradictions are amplified, making consistent enforcement of this reading more difficult and increasing its perceived theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_hierarchy_ambiguity, conceptual, 'Ambiguity in the hierarchical relationship between the two core obligations of the Mandate.').

omega_variable(
    effective_land_protection_empirical,
    'To what extent were land transfer restrictions and immigration quotas *actually* effective in protecting Arab land tenure and preventing demographic displacement throughout the Mandate period?',
    'Comprehensive historical and demographic analysis of land ownership changes and immigration statistics during the Mandate, accounting for both legal and illegal transfers/entries.',
    'If found largely ineffective, the constraint''s actual suppression and extractiveness (from the Zionist perspective) would be lower, and its theater_ratio higher, indicating a failure of the stated protective function and a shift towards a more performative role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_land_protection_empirical, empirical, 'Empirical effectiveness of protective measures for indigenous rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1935, 0.17).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1940, 0.19).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1925, 0.65).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1930, 0.7).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1935, 0.75).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1940, 0.78).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1925, 0.75).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1930, 0.8).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1935, 0.85).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1940, 0.88).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
