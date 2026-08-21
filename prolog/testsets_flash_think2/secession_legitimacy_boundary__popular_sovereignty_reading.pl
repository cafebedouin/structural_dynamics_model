% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Popular Sovereignty in Secession
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'popular_sovereignty_reading' of
 *   the 'secession_legitimacy_boundary' kernel. It posits that a democratic
 *   majority within a provincial boundary holds ultimate sovereignty, and a
 *   referendum result is self-legitimating for secession. While claimed as a
 *   'rope' from the perspective of the provincial majority (as it coordinates
 *   their collective action), the metrics reflect the high extraction and
 *   suppression imposed on the federal government and other affected parties
 *   (like indigenous treaty holders) by this unilateral claim to sovereignty.
 *   The gap between the claimed type and the high metrics is deliberate,
 *   highlighting the contested nature of this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.7).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.8).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Popular Sovereignty in Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '7cd443d9-9214-4ce7-8606-1a4ac1e25357').
narrative_ontology:cs_kernel_codification('7cd443d9-9214-4ce7-8606-1a4ac1e25357', formalized).
narrative_ontology:cs_authority_grounding('7cd443d9-9214-4ce7-8606-1a4ac1e25357', practice).
narrative_ontology:cs_interpretation_layer_present('7cd443d9-9214-4ce7-8606-1a4ac1e25357').
narrative_ontology:cs_reading_relation('7cd443d9-9214-4ce7-8606-1a4ac1e25357', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('7cd443d9-9214-4ce7-8606-1a4ac1e25357', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cd443d9-9214-4ce7-8606-1a4ac1e25357', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('7cd443d9-9214-4ce7-8606-1a4ac1e25357', foundational, popular_will_is_supreme_within_province).
narrative_ontology:cs_axiom_status(popular_will_is_supreme_within_province, holdable).
narrative_ontology:cs_axiom_grounding('7cd443d9-9214-4ce7-8606-1a4ac1e25357', popular_will_is_supreme_within_province, deontological).
narrative_ontology:cs_axiom('7cd443d9-9214-4ce7-8606-1a4ac1e25357', secondary, provincial_boundaries_define_sovereign_unit).
narrative_ontology:cs_axiom_status(provincial_boundaries_define_sovereign_unit, holdable).
narrative_ontology:cs_axiom_grounding('7cd443d9-9214-4ce7-8606-1a4ac1e25357', provincial_boundaries_define_sovereign_unit, conventional).
narrative_ontology:cs_reference_frame('7cd443d9-9214-4ce7-8606-1a4ac1e25357', unfettered_provincial_self_determination).
narrative_ontology:cs_drift_state('7cd443d9-9214-4ce7-8606-1a4ac1e25357', contemporary_federal_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7cd443d9-9214-4ce7-8606-1a4ac1e25357', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, other_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of citizens within a provincial boundary who, through a democratic referendum, assert their right to self-determination and secession. They are the primary beneficiaries of this reading, as it empowers their collective will.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority, agenda_setter,
    powerful, biographical, mobile, regional).

% The central authority of the federal state, which would lose territory, resources, and political authority if a province seceded based on popular will. This reading subordinates its constitutional and territorial claims to provincial popular sovereignty.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Other constituent units of the federal state that might be negatively impacted by the secession of a resource-rich or strategically important province, or by the precedent set for their own minorities. They bear the cost of a diminished federal union.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, other_provinces, payer,
    organized, generational, constrained, national).

% First Nations and other Indigenous groups whose ancestral lands and treaty rights often span or predate provincial and federal boundaries. This reading, by prioritizing provincial popular will, often subordinates or ignores their claims to self-determination and territorial integrity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, excluded,
    powerless, civilizational, trapped, local).

% International bodies, legal scholars, and human rights organizations that monitor and comment on self-determination processes, often balancing principles of territorial integrity with democratic rights.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a clear, democratic mechanism for a provincial population to collectively determine its political status and potentially secede from a federal state, thereby coordinating their collective action towards self-determination.
% TRANSFER_FUNCTION: Transfers ultimate political authority, and potentially significant economic resources and territory, from the federal government to the seceding provincial entity. It also transfers the burden of managing indigenous relations and other federal responsibilities to the new entity.
% ABSENT_VOICES: Indigenous treaty holders are often structurally excluded from the provincial referendum process, despite their inherent sovereignty and treaty rights being directly impacted. Federal minorities within the province who do not support secession may also find their voices marginalized.
% DISAPPEARANCE_RATIONALE: If the principle of provincial popular sovereignty as self-legitimating for secession vanished, the mechanism for provincial self-determination would become highly ambiguous or non-existent. This would lead to severe political instability, potential conflict over sovereignty, and a fundamental re-evaluation of federal-provincial relations, as the path to exit would be blocked or unclear.
% FOUNDING_PROBLEM: To resolve tensions within federal states by providing a democratic and legitimate pathway for distinct provincial populations to exercise self-determination, preventing prolonged political deadlock or violent conflict over sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists specializing in federalism and secession, international law scholars on self-determination, and historical precedents of referenda in federal states (e.g., Quebec, Scotland) corroborate the ongoing relevance of this problem and the need for clear mechanisms, even if the specific reading is contested.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.7) and `suppression` (0.8) are high because this reading, while empowering the provincial majority, simultaneously extracts territory and authority from the federal government and suppresses the claims of other stakeholders, particularly indigenous groups whose rights are often ignored. The `theater_ratio` is low (0.1) as the mechanism (referendum) is direct and functional, not performative. `Accessibility_collapse` is moderate (0.6) as it collapses federal alternatives to preventing secession. `Resistance` is high (0.7) from federal and indigenous parties who contest this reading. The `claimed_type` of 'rope' reflects the provincial majority's perspective that this is a legitimate coordination mechanism for their self-determination.
 *
 * PERSPECTIVAL GAP:
 *   The provincial majority experiences this constraint as a legitimate and empowering 'rope' for self-determination. In contrast, the federal government and indigenous treaty holders experience it as a highly extractive and suppressive 'snare' that undermines their own sovereignty and rights. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The `provincial_majority` is the clear beneficiary, gaining political autonomy and control over resources. The `federal_government` and `other_provinces` are victims, losing territory, resources, and federal cohesion. `Indigenous_treaty_holders` are also victims, as their pre-existing rights are often subordinated to provincial popular will. `International_observers` maintain an analytical stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_popular_sovereignty,
    'Is ''ultimate sovereignty'' truly held by a provincial majority, or is it constrained by the existing federal constitution and its foundational principles?',
    'A definitive ruling by a supreme court on the legality of unilateral provincial secession, or a constitutional amendment explicitly defining the process.',
    'If constitutional supremacy is affirmed, this reading''s legitimacy is severely undermined, shifting its classification towards a ''snare'' for the federal government. If popular sovereignty is affirmed, the ''rope'' classification for the provincial majority is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_popular_sovereignty, conceptual, 'Ambiguity between popular will and constitutional text as ultimate authority.').

omega_variable(
    indigenous_rights_subordination,
    'Does provincial popular sovereignty legitimately supersede pre-existing Indigenous treaty rights and their inherent right to self-determination over their traditional territories?',
    'Legal challenges by Indigenous nations to provincial secession claims, or international legal rulings on the scope of self-determination in multi-national contexts.',
    'If Indigenous rights are found to supersede provincial popular will, the ''excluded'' status of Indigenous treaty holders becomes a critical flaw, potentially reclassifying the constraint as a ''snare'' due to its unacknowledged victims. If provincial will is upheld, the extraction from Indigenous groups is legitimized within this framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_rights_subordination, empirical, 'The extent to which provincial popular sovereignty impacts Indigenous self-determination.').

omega_variable(
    definition_of_provincial_boundaries,
    'Are the current administrative provincial boundaries the legitimate and immutable definition of the ''people'' exercising self-determination, or are they arbitrary colonial constructs?',
    'Historical and anthropological research into pre-colonial territorial claims, or a re-negotiation of internal borders based on self-identified communities.',
    'If boundaries are found to be arbitrary, the legitimacy of the ''provincial majority'' as a sovereign unit is weakened, potentially leading to internal divisions within the seceding entity and complicating the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_provincial_boundaries, conceptual, 'The legitimacy of administrative boundaries in defining a sovereign people.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2010, 0.69).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
