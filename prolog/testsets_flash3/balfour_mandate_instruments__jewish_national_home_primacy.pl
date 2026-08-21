% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents the 'Jewish National Home Primacy' reading of
 *   the Balfour Declaration and the League of Nations Mandate for Palestine.
 *   Under this interpretation, the Mandate instruments were understood to
 *   direct demographic and territorial transformation to establish Jewish
 *   sovereignty, with the 'national home' interpreted as a proto-state
 *   requiring facilitated land access, immigration, and Jewish institutional
 *   supremacy. This reading led to policies that systematically favored
 *   Zionist aims, often at the expense of the existing Palestinian Arab
 *   population. The constraint is classified as a Tangled Rope due to its
 *   dual function of coordinating the 'national home' project while
 *   simultaneously extracting resources and rights from the indigenous
 *   population through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.85).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.78).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'c2885d5c-8f4e-42e3-a529-2da9546a4723').
narrative_ontology:cs_kernel_codification('c2885d5c-8f4e-42e3-a529-2da9546a4723', fixed_text).
narrative_ontology:cs_authority_grounding('c2885d5c-8f4e-42e3-a529-2da9546a4723', lineage).
narrative_ontology:cs_interpretation_layer_present('c2885d5c-8f4e-42e3-a529-2da9546a4723').
narrative_ontology:cs_reading_relation('c2885d5c-8f4e-42e3-a529-2da9546a4723', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('c2885d5c-8f4e-42e3-a529-2da9546a4723', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('c2885d5c-8f4e-42e3-a529-2da9546a4723', foundational, jewish_national_home_as_proto_state).
narrative_ontology:cs_axiom_status(jewish_national_home_as_proto_state, holdable).
narrative_ontology:cs_axiom_grounding('c2885d5c-8f4e-42e3-a529-2da9546a4723', jewish_national_home_as_proto_state, conventional).
narrative_ontology:cs_axiom('c2885d5c-8f4e-42e3-a529-2da9546a4723', foundational, demographic_transformation_as_mandate_goal).
narrative_ontology:cs_axiom_status(demographic_transformation_as_mandate_goal, holdable).
narrative_ontology:cs_axiom_grounding('c2885d5c-8f4e-42e3-a529-2da9546a4723', demographic_transformation_as_mandate_goal, conventional).
narrative_ontology:cs_reference_frame('c2885d5c-8f4e-42e3-a529-2da9546a4723', balfour_declaration_intent).
narrative_ontology:cs_drift_state('c2885d5c-8f4e-42e3-a529-2da9546a4723', post_un_partition_plan, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c2885d5c-8f4e-42e3-a529-2da9546a4723', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained quasi-governmental status under Mandate Article 4, enabling them to facilitate Jewish immigration, acquire land, and develop a parallel state apparatus. Directly benefited from policies prioritizing Jewish settlement and institutional growth.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    institutional, generational, arbitrage, regional).

% Benefited from facilitated immigration, land access, and the establishment of a 'national home' that offered refuge and a path to self-determination. Their demographic growth was a direct aim of the Mandate's interpretation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, regional).

% Experienced systematic land dispossession through policies that favored Jewish land acquisition, often without adequate protection for their traditional tenure or economic viability. Their land was seen as available for transfer to the 'national home'.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, trapped, local).

% Their political representation was structurally downgraded, and their demands for self-determination were consistently subordinated to the 'national home' project. They faced active suppression of their political organizing and resistance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    powerless, generational, constrained, regional).

% Subjected to policies that prioritized Jewish demographic growth and institutional development, leading to their marginalization, economic displacement, and the denial of their collective political rights in their homeland.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Administered the Mandate, actively implementing policies that facilitated Jewish immigration and land acquisition, and suppressed Arab resistance, consistent with the 'national home' primacy interpretation. Held ultimate authority but was influenced by Zionist lobbying and geopolitical considerations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate the establishment of a 'Jewish national home' in Palestine by providing a legal framework for Zionist institutions to build infrastructure, facilitate immigration, and acquire land, under British administration.
% TRANSFER_FUNCTION: Transferred land, political influence, and demographic advantage from the existing Palestinian Arab population to Zionist institutions and Jewish migrants, under the legal authority of the British Mandate.
% ABSENT_VOICES: The vast majority of the Palestinian Arab population, whose political aspirations and land rights were systematically subordinated, were excluded from meaningful participation in the Mandate's governance and interpretation. Their voices were largely heard only through resistance and protest, which were often suppressed.
% DISAPPEARANCE_RATIONALE: If this interpretation of the Mandate had vanished, the demographic and territorial transformation of Palestine would not have occurred as it did. British policy would have had to prioritize existing inhabitants' rights, altering the trajectory of state formation and land ownership, leading to a fundamentally different political and social landscape.
% FOUNDING_PROBLEM: The problem was how to implement the Balfour Declaration's promise of a 'national home for the Jewish people' in Palestine, a territory already inhabited by a majority Arab population, while maintaining British imperial interests.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and their supporters attest that the problem of establishing and securing a Jewish national home remains live. Palestinian Arab voices and international legal scholars, from outside the benefiting parties, attest that the problem was fundamentally one of colonial imposition and dispossession, which remains unresolved.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the policies systematically transferred land and political power to one group. Suppression is also high (0.78) due to the active enforcement by the British Mandatory Power against Arab resistance and political organizing. The theater ratio is moderate (0.20) as the 'civilizing mission' and 'dual obligation' narratives served as a partial cover for the primary goal of establishing the national home. Accessibility collapse is significant (0.70) as alternatives for Palestinian Arabs to assert self-determination or protect land rights were systematically undermined. Resistance was high (0.75) reflecting the continuous Arab opposition to these policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions and Jewish migrants, this interpretation of the Mandate was a legitimate and necessary coordination mechanism for building a national home. From the perspective of Palestinian Arabs, it was a coercive and extractive colonial imposition. The British Mandatory Power, as the agenda-setter, navigated these conflicting claims while largely implementing policies aligned with the primacy reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are clear beneficiaries, as the constraint directly facilitated their goals (low d). Palestinian Arab landholders and political leadership are clear victims, bearing the costs of land dispossession and political marginalization (high d). The British Mandatory Power, while an agenda-setter, also faced costs in terms of administering a contested territory and suppressing resistance, placing its d closer to symmetric but still benefiting from imperial control.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'Jewish National Home Primacy' reading of the Mandate did not suffer from mandatrophy during its operational period (1922-1948) because its core mandate – the establishment of the national home – remained a live and actively pursued objective. The constraint's persistence was driven by the active pursuit of this goal, not by inertia or theatrical maintenance. The high extractiveness and suppression were integral to achieving this mandate, rather than symptoms of its decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_obligation_vs_primacy,
    'To what extent did the British Mandatory Power genuinely attempt to balance the ''dual obligation'' to the Jewish national home and the civil and religious rights of existing non-Jewish communities, versus prioritizing the national home?',
    'Analysis of British policy documents, administrative decisions, and resource allocation patterns, particularly in areas of land sales, immigration quotas, and political representation, compared against stated ''dual obligation'' principles.',
    'If a genuine attempt at balance is found, the extractiveness of this reading might be slightly lower, and the constraint might lean more towards a Tangled Rope with a stronger coordination component. If primacy was consistently prioritized, the classification as a highly extractive Tangled Rope (or even Snare) is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_obligation_vs_primacy, empirical, 'Ambiguity regarding the British interpretation and implementation of the ''dual obligation'' clause.').

omega_variable(
    legitimacy_of_mandate_authority,
    'Was the League of Nations Mandate system, as applied to Palestine, a legitimate exercise of international law or a continuation of colonial practices under a new guise?',
    'Historical and legal analysis of the Mandate''s origins, the exclusion of indigenous self-determination, and its operational outcomes compared against evolving international norms of self-determination and anti-colonialism.',
    'If deemed illegitimate, the entire constraint system''s foundational authority is undermined, potentially reclassifying it as a Snare from its inception, as its coordination function would be seen as a cover for colonial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_mandate_authority, conceptual, 'The fundamental legitimacy of the Mandate system itself as a framework for state formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(balf_tr_t1934, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1940, 0.25).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.75).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1928, 0.8).
narrative_ontology:measurement(balf_be_t1934, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1934, 0.85).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1940, 0.88).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.65).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1928, 0.7).
narrative_ontology:measurement(balf_su_t1934, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1934, 0.78).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1940, 0.82).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'balfour_mandate_instruments' kernel, focusing on the primacy of the Jewish national home. It is structurally distinct from the 'dual_obligation_indigenous_rights' and 'mandatory_interpretive_discretion' readings, which emphasize different aspects of the Mandate's obligations and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
