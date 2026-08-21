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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate: Dual Obligation for Indigenous Rights
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Balfour Declaration
 *   and the League of Nations Mandate for Palestine, emphasizing the 'dual
 *   obligation' to protect existing Arab civil/political rights and land
 *   tenure, and subordinating the 'national home' concept to
 *   self-determination norms and minority protection. This reading posits
 *   that the mandate instruments imposed an equal or superior obligation to
 *   safeguard indigenous rights, implying restrictions on land transfers and
 *   immigration to prevent demographic displacement. The constraint is framed
 *   as a Tangled Rope because it attempts to coordinate conflicting claims
 *   but results in asymmetric extraction, requiring active enforcement to
 *   maintain a precarious balance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.78).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.65).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate: Dual Obligation for Indigenous Rights").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'c72916a1-3db1-4482-9ac6-362dce10f7d7').
narrative_ontology:cs_kernel_codification('c72916a1-3db1-4482-9ac6-362dce10f7d7', formalized).
narrative_ontology:cs_authority_grounding('c72916a1-3db1-4482-9ac6-362dce10f7d7', lineage).
narrative_ontology:cs_interpretation_layer_present('c72916a1-3db1-4482-9ac6-362dce10f7d7').
narrative_ontology:cs_reading_relation('c72916a1-3db1-4482-9ac6-362dce10f7d7', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('c72916a1-3db1-4482-9ac6-362dce10f7d7', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('c72916a1-3db1-4482-9ac6-362dce10f7d7', foundational, indigenous_rights_supremacy).
narrative_ontology:cs_axiom_status(indigenous_rights_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c72916a1-3db1-4482-9ac6-362dce10f7d7', indigenous_rights_supremacy, deontological).
narrative_ontology:cs_axiom('c72916a1-3db1-4482-9ac6-362dce10f7d7', foundational, self_determination_principle).
narrative_ontology:cs_axiom_status(self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('c72916a1-3db1-4482-9ac6-362dce10f7d7', self_determination_principle, deontological).
narrative_ontology:cs_reference_frame('c72916a1-3db1-4482-9ac6-362dce10f7d7', international_law_of_mandates_indigenous_rights).
narrative_ontology:cs_drift_state('c72916a1-3db1-4482-9ac6-362dce10f7d7', mandate_implementation_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c72916a1-3db1-4482-9ac6-362dce10f7d7', '').
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

% Benefit from land transfer restrictions and immigration quotas designed to protect their existing land tenure and demographic majority. They advocate for self-determination and representative government based on their numerical superiority.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, regional).

% Their land tenure and civil/political rights are theoretically protected by the mandate's dual obligation. They experience the direct impact of land sales and immigration, and their ability to resist is tied to the enforcement of these protections.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of restrictions on land acquisition and immigration quotas, which impede their goal of establishing a Jewish majority and proto-state. They actively lobby against these interpretations and seek to circumvent them.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, constrained, global).

% Are constrained by the dual obligation, which makes it difficult to satisfy Zionist demands for land and immigration while upholding Arab rights. They face pressure from both sides and from international bodies, leading to a complex and often contradictory administration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, payer,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, agenda_setter).

% The international body that granted the mandate and theoretically oversees its implementation. It receives reports and petitions, but its enforcement power is limited, making it an observer rather than a direct enforcer of this specific reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the establishment of a 'national home for the Jewish people' with the protection of existing non-Jewish civil and religious rights in Palestine, under international supervision.
% TRANSFER_FUNCTION: Transfers land and political control from the indigenous Arab population to Jewish immigrants and institutions, but with significant legal and political friction due to the mandated protections for Arab rights and tenure.
% ABSENT_VOICES: The full spectrum of Palestinian Arab nationalist movements, particularly those advocating for immediate independence and rejection of the 'national home' concept, were largely excluded from the formal drafting and early interpretive processes, though their resistance was a constant factor.
% DISAPPEARANCE_RATIONALE: If this interpretation of the mandate (prioritizing indigenous rights) had been consistently and robustly enforced, the demographic and political landscape of Palestine would have developed very differently, likely preventing large-scale land transfers and maintaining an Arab majority, fundamentally altering the path to statehood for both populations.
% FOUNDING_PROBLEM: To reconcile the British commitment to a 'national home for the Jewish people' with the existing rights and aspirations of the indigenous Arab population in Palestine, following the collapse of the Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely corroborate that the founding problem of reconciling these two conflicting obligations was never truly resolved by the mandate's implementation. The problem's 'death' is marked by the 1948 war and subsequent events, which rendered the mandate's original framework obsolete, though its legacy continues to shape the conflict. Independent legal analyses from outside the benefiting parties (e.g., UN reports, academic studies) support this assessment.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because, despite the stated protections, the overall trajectory of the mandate's implementation led to significant land alienation and demographic shifts, extracting resources and political power from the Arab population. Suppression (0.65) reflects the British administration's efforts to manage Arab resistance and Zionist pressure, often through coercive measures, while attempting to uphold some semblance of order. The theater ratio (0.20) indicates that while some genuine efforts were made to protect Arab rights, a substantial portion of the administrative activity served to manage the inherent contradictions of the mandate rather than fully resolve them. The slight decrease in suppression and theater towards 1948 reflects the breakdown of British authority and the escalation of direct conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian Arabs, this reading represents a minimal, often unfulfilled, protection against a larger extractive project. From the Zionist perspective, it is an obstacle to their legitimate aspirations. British administrators experience it as an unworkable compromise. The engine's classification will highlight how this 'coordination' mechanism ultimately served to manage, rather than resolve, fundamental conflicts, leading to high extraction for those whose rights were supposedly protected.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are beneficiaries of this reading, as it theoretically protects their rights and land, though the actual benefits were often undermined. Zionist organizations are victims, as this reading constrains their ability to achieve their goals of land acquisition and demographic transformation. British administrators are also victims, caught between conflicting obligations and facing resistance from both sides, making their position highly constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_intended_enforcement,
    'To what extent did British administrative practice genuinely enforce the ''dual obligation'' to protect Arab rights, versus merely managing the appearance of compliance?',
    'Detailed historical analysis of land transfer records, immigration policies, and judicial rulings, comparing stated policy with actual outcomes and enforcement actions.',
    'If enforcement was largely performative, the constraint''s effective extractiveness for Palestinian Arabs would be higher, and its classification would lean more towards Snare. If genuine, it would reinforce the Tangled Rope classification, highlighting the inherent conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_vs_intended_enforcement, empirical, 'Assessing the gap between the stated dual obligation and its practical implementation.').

omega_variable(
    legitimacy_of_dual_obligation,
    'Was the ''dual obligation'' a coherent and implementable legal framework, or an inherently contradictory political compromise designed to fail?',
    'Legal-historical analysis of international law principles at the time, combined with counterfactual modeling of alternative mandate structures. This is a conceptual question about the mandate''s internal consistency.',
    'If inherently contradictory, the constraint''s high extractiveness and suppression are structural features of its design, not merely failures of implementation. This would strengthen the argument for it being a Snare from the outset, rather than a failed Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_dual_obligation, conceptual, 'Coherence and implementability of the dual obligation framework.').

omega_variable(
    impact_of_external_pressure,
    'How did external pressures (e.g., Zionist lobbying, Arab revolts, League of Nations scrutiny) influence the British administration''s interpretation and enforcement of the dual obligation?',
    'Archival research into British government documents, diplomatic correspondence, and records of international bodies to trace the causal links between external pressure and policy shifts.',
    'Understanding these dynamics would clarify the ''active enforcement'' component: was it primarily internal to the mandate''s logic, or a response to external forces? This could refine the suppression metric and the role of ''british_administrators'' as both agenda-setters and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_external_pressure, empirical, 'Role of external pressures in shaping mandate implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(balf_tr_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1940, 0.25).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1922, 0.6).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1928, 0.68).
narrative_ontology:measurement(balf_be_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1934, 0.75).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1940, 0.79).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1922, 0.55).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1928, 0.6).
narrative_ontology:measurement(balf_su_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1934, 0.68).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1940, 0.72).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
