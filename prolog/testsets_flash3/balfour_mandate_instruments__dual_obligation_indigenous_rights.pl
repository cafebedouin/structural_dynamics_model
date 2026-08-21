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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual Obligation for Indigenous Rights (Palestinian Reading)
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Balfour Declaration
 *   and the League of Nations Mandate for Palestine, emphasizing the 'dual
 *   obligation' to protect the existing civil and political rights and land
 *   tenure of the non-Jewish (primarily Arab) population. In this reading,
 *   the 'national home for the Jewish people' is subordinated to these
 *   indigenous rights, implying restrictions on land transfers and
 *   immigration to prevent demographic displacement and ensure the path to
 *   self-determination for the Arab majority. This reading views the mandate
 *   as a Tangled Rope, providing a coordination function (international
 *   oversight, protection of rights) but with significant, actively enforced
 *   extraction from Zionist organizations and constraints on British
 *   administrators seeking to fulfill Zionist aims.
 *
 * KEY AGENTS:
 *   - palestinian_arab_elites: Beneficiary (organized/constrained)
 *   - palestinian_arab_communities: Beneficiary (powerless/trapped)
 *   - zionist_organizations: Payer (powerful/constrained)
 *   - british_administrators: Agenda Setter (institutional/constrained)
 *   - league_of_nations_permanent_mandates_commission: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.78).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.65).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual Obligation for Indigenous Rights (Palestinian Reading)").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'a6d44358-fb78-4d67-b137-ec0c12c72523').
narrative_ontology:cs_kernel_codification('a6d44358-fb78-4d67-b137-ec0c12c72523', fixed_text).
narrative_ontology:cs_authority_grounding('a6d44358-fb78-4d67-b137-ec0c12c72523', lineage).
narrative_ontology:cs_interpretation_layer_present('a6d44358-fb78-4d67-b137-ec0c12c72523').
narrative_ontology:cs_reading_relation('a6d44358-fb78-4d67-b137-ec0c12c72523', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('a6d44358-fb78-4d67-b137-ec0c12c72523', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('a6d44358-fb78-4d67-b137-ec0c12c72523', foundational, indigenous_rights_primacy).
narrative_ontology:cs_axiom_status(indigenous_rights_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a6d44358-fb78-4d67-b137-ec0c12c72523', indigenous_rights_primacy, deontological).
narrative_ontology:cs_axiom('a6d44358-fb78-4d67-b137-ec0c12c72523', foundational, land_tenure_inviolability).
narrative_ontology:cs_axiom_status(land_tenure_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('a6d44358-fb78-4d67-b137-ec0c12c72523', land_tenure_inviolability, conventional).
narrative_ontology:cs_reference_frame('a6d44358-fb78-4d67-b137-ec0c12c72523', international_law_indigenous_protection_framework).
narrative_ontology:cs_drift_state('a6d44358-fb78-4d67-b137-ec0c12c72523', post_1948_state_formation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a6d44358-fb78-4d67-b137-ec0c12c72523', '').
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

% Benefit from land transfer restrictions and political safeguards intended to protect their existing rights and majority status, which they see as a path to self-determination. Their ability to fully realize these benefits is constrained by British administrative power and Zionist pressure.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, regional).

% Their land tenure and civil rights are theoretically protected by the mandate's dual obligation. However, they are largely disempowered in practice, relying on the British administration to enforce these protections against Zionist land acquisition and immigration, often with limited success.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    powerless, generational, trapped, local).

% Bear the costs of restrictions on land acquisition, immigration quotas, and the subordination of the 'national home' concept to existing Arab rights. They actively resist these constraints, viewing them as impediments to their goal of establishing a Jewish state.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, constrained, global).

% Are tasked with implementing the mandate's dual and often contradictory obligations. They are constrained by the need to balance Arab rights with Zionist aspirations, often facing pressure from both sides and international bodies. Their actions are frequently perceived as insufficient by both parties.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Oversees the British administration's adherence to the mandate terms, including the protection of indigenous rights. They receive petitions and reports, providing a formal, though often limited, avenue for accountability and interpretation of the mandate's obligations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the establishment of a 'national home for the Jewish people' with the protection of the civil and religious rights of existing non-Jewish communities, under international supervision.
% TRANSFER_FUNCTION: Transfers political legitimacy and international recognition to the concept of protecting indigenous rights and land tenure, theoretically limiting the transfer of land and sovereignty to Zionist entities. It also transfers administrative burden and political friction to the British Mandatory power.
% ABSENT_VOICES: Palestinian Arab representatives, who consistently rejected the mandate system itself and demanded immediate self-determination, were largely excluded from the drafting of the mandate instruments and their subsequent interpretation by the Mandatory power, beyond formal petitions.
% DISAPPEARANCE_RATIONALE: If this interpretation of the mandate's dual obligation vanished, the legal and political basis for protecting Palestinian Arab rights and land tenure would be severely undermined, accelerating demographic and territorial transformation in favor of Zionist aspirations, leading to a fundamentally different historical trajectory for the region.
% FOUNDING_PROBLEM: The problem of reconciling the promise of a 'national home for the Jewish people' with the existing rights and self-determination aspirations of the indigenous Arab population in Palestine, following the collapse of the Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is widely considered dead by historians and international legal scholars, as the mandate system itself ended in 1948, and the subsequent conflict and state formation rendered the original 'reconciliation' framework obsolete. Independent historical analysis and UN resolutions corroborate this status, despite ongoing political claims by some parties.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.78) is high because this reading imposes significant limitations on Zionist aspirations, extracting from their goals of rapid land acquisition and demographic transformation. Suppression (0.65) reflects the active enforcement by the British (albeit often inconsistently) of land transfer regulations and immigration quotas, which suppressed Zionist expansion. The theater ratio (0.40) indicates that while some genuine administrative effort went into protecting Arab rights, a substantial portion of the 'protection' was performative, designed to appease international opinion and Arab resistance without fundamentally altering the pro-Zionist trajectory of British policy. The rising extractiveness and suppression over time reflect increasing Arab resistance and the British attempts to manage the conflict through more explicit, though often ineffective, restrictions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian Arab beneficiaries, this reading of the mandate represents a (flawed) attempt at protection and a basis for future self-determination. From the Zionist organizations' perspective, it is an extractive constraint that impedes their national project. British administrators experience it as a complex, contradictory, and politically costly obligation. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are beneficiaries (low d) as the constraint theoretically protects their status and rights. Zionist organizations are targets (high d) as the constraint actively limits their objectives. British administrators are agenda-setters but also targets (moderate-high d) as they are constrained by the dual obligation and face extraction in terms of political capital and administrative burden from both sides. The League of Nations acts as an analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of the mandate, while intended to coordinate conflicting claims, ultimately failed to resolve the underlying tensions, leading to its effective mandatrophy. The classification as a Tangled Rope prevents mislabeling it as pure extraction (Snare) by acknowledging its genuine, albeit often unfulfilled, coordination function of protecting indigenous rights under international law. However, the high extractiveness and suppression, coupled with the contested status of its founding problem, highlight its eventual failure to sustain legitimate coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    british_enforcement_consistency,
    'To what extent did British administrators consistently and effectively enforce the mandate''s provisions for protecting Arab civil/political rights and land tenure, versus yielding to Zionist pressure or strategic imperial interests?',
    'Detailed archival research into British administrative records, land registry data, and internal policy debates, comparing stated policy with actual implementation outcomes over time.',
    'If enforcement was consistently weak or selectively applied against Arab interests, the effective extractiveness from Zionist organizations would be lower, and the effective suppression of Arab agency higher, potentially reclassifying the constraint closer to a Snare for Arabs or a Rope for Zionists. If enforcement was robust, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(british_enforcement_consistency, empirical, 'Ambiguity regarding the actual consistency and effectiveness of British enforcement of indigenous rights protections.').

omega_variable(
    self_determination_vs_national_home_priority,
    'Is the principle of self-determination for the indigenous population inherently superior to the right to establish a national home for another people, or are these rights of equal moral weight requiring a negotiated balance?',
    'Conceptual analysis within international legal theory and political philosophy, examining the hierarchy of rights in colonial contexts and the status of indigenous populations.',
    'If self-determination is inherently superior, this reading''s emphasis on indigenous rights is normatively reinforced, and any deviation from it by the Mandatory power is a clear violation. If they are of equal weight, the mandate''s inherent contradiction is highlighted, and the ''tangled'' nature of the rope is more about an impossible balancing act than asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_vs_national_home_priority, conceptual, 'The normative hierarchy between indigenous self-determination and the right to a national home.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1922, 0.3).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1928, 0.35).
narrative_ontology:measurement(balf_tr_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1934, 0.4).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1940, 0.45).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1922, 0.7).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1928, 0.72).
narrative_ontology:measurement(balf_be_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1934, 0.75).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1940, 0.77).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1922, 0.55).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1928, 0.6).
narrative_ontology:measurement(balf_su_t1934, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1934, 0.65).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1940, 0.68).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel. This reading emphasizes the protection of indigenous Arab rights and land tenure, subordinating the 'national home' concept. It is linked to the 'jewish_national_home_primacy' reading (which prioritizes Zionist aims) and the 'mandatory_interpretive_discretion' reading (which focuses on British administrative autonomy), as they all derive from the same foundational texts but interpret them differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
