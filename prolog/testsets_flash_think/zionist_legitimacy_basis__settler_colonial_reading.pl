% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis: Settler-Colonial Reading
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story analyzes Zionism as a European settler-colonial
 *   movement, focusing on its structural role in establishing an ethno-state
 *   through the systematic displacement and dispossession of indigenous
 *   Palestinians. This reading emphasizes the colonial framework as
 *   constitutive of the movement's historical trajectory and ongoing
 *   operation, rather than viewing indigenous displacement as an incidental
 *   outcome. The constraint's persistence relies on active enforcement and
 *   the suppression of alternatives for the indigenous population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.92).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis: Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'a7a77ea2-5658-4ec7-91e6-c3d706a3d65a').
narrative_ontology:cs_kernel_codification('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', formalized).
narrative_ontology:cs_authority_grounding('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', extraction).
narrative_ontology:cs_interpretation_layer_present('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a').
narrative_ontology:cs_reading_relation('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', foundational, indigenous_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', indigenous_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', foundational, settler_state_formation_is_extractive).
narrative_ontology:cs_axiom_status(settler_state_formation_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', settler_state_formation_is_extractive, conventional).
narrative_ontology:cs_reference_frame('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', european_colonial_expansion_framework).
narrative_ontology:cs_drift_state('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', contemporary_decolonial_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7a77ea2-5658-4ec7-91e6-c3d706a3d65a', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, state_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, settler_colonial_theory).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, postcolonial_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from land acquisition, state protection, and the establishment of an ethno-state. Their identity and security are deeply intertwined with the persistence of the settler-colonial project, making exit from the framework difficult without fundamental societal change.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers, beneficiary,
    powerful, generational, constrained, national).

% The institutional embodiment of the ethno-state, actively enforcing policies that facilitate settlement, control indigenous populations, and maintain the demographic and territorial status quo. Its legitimacy and existence are tied to the success of the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Bear the primary costs of displacement, dispossession, loss of sovereignty, and ongoing military occupation or discriminatory laws. Their alternatives for self-determination and return are systematically suppressed, leaving them trapped within the colonial structure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, regional).

% Observes the conflict, often issuing condemnations or resolutions, but frequently fails to effectively intervene to alter the fundamental settler-colonial dynamics. Its actions are often constrained by geopolitical interests.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_community, observer,
    institutional, generational, analytical, global).

% Analyze and theorize the historical and ongoing dynamics of settler-colonialism, providing the academic framework for this reading. They aim to expose the structural mechanisms of displacement and extraction.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, settler_colonial_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint coordinates the establishment and maintenance of a new society and state for Zionist settlers, including resource allocation, security provision, and the construction of a national identity, often at the expense of indigenous populations.
% TRANSFER_FUNCTION: Transfers land, resources, sovereignty, and self-determination from indigenous Palestinians to Zionist settlers and the State of Israel, facilitated by legal, military, and demographic mechanisms.
% ABSENT_VOICES: Indigenous Palestinian voices advocating for decolonization, the right of return, and full self-determination are often marginalized, suppressed, or delegitimized within dominant international and Israeli discourse. Their perspectives are systematically excluded from the agenda-setting process.
% DISAPPEARANCE_RATIONALE: If the settler-colonial basis of the state and its associated enforcement mechanisms vanished overnight, the entire political, demographic, and territorial structure of the region would undergo a fundamental and rapid reorganization, leading to decolonization and the potential return of displaced populations.
% FOUNDING_PROBLEM: The perceived need for a secure homeland for Jewish people in response to historical antisemitism and persecution, which this reading frames as being pursued through a process of indigenous displacement and colonial state-building.
% FOUNDING_PROBLEM_CORROBORATION: The historical problem of antisemitism and the desire for a Jewish homeland are widely attested by Jewish communities globally. However, the settler-colonial nature of the movement and its impact on indigenous populations are corroborated by indigenous Palestinian narratives, human rights organizations, and a growing body of academic work in settler-colonial studies, which challenge the self-justifying narratives of the benefiting parties.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the core function of the constraint is the transfer of land, resources, and sovereignty from indigenous Palestinians to the settler population and the state. Suppression is also very high (0.88) due to the active military, legal, and demographic measures employed to control and dispossess indigenous populations, and to prevent their return or self-determination. Theater ratio is moderate (0.45) as justifications for these actions often invoke security, historical rights, or national liberation narratives, which, from this reading, serve to mask the underlying extractive and suppressive mechanisms. Accessibility collapse is near total (0.90) for indigenous Palestinians, as their fundamental rights and alternatives are systematically denied. Resistance is high (0.85) reflecting the ongoing and sustained struggle by indigenous Palestinians against this colonial structure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (State of Israel) and beneficiaries (Zionist settlers) would perceive this constraint as a legitimate national project, possibly a 'rope' or 'scaffold' for national self-determination and security. In contrast, the payers (indigenous Palestinians) experience it as a 'snare' of pure extraction and oppression. The engine's computation of per-seat classification from the structural data will highlight this profound divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist settlers and the State of Israel are the primary beneficiaries, gaining land, resources, and state power. Indigenous Palestinians are the primary targets/victims, suffering displacement, dispossession, and loss of self-determination. The international community and settler-colonial scholars act as observers, with varying degrees of influence and analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'snare' prevents mislabeling the constraint as a 'rope' or 'scaffold' for national liberation or security, which are often the stated coordination functions. By focusing on the structural outcomes of indigenous displacement and dispossession, it highlights that any coordination for the settler population is achieved through asymmetric extraction and active suppression of the indigenous population, rather than through mutual benefit or temporary support. The coordination story is identified as a cover for pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''settler_colonial_reading'' of the ''zionist_legitimacy_basis'' kernel?',
    'Analysis of the core premises and structural outcomes emphasized by this reading compared to the definitions of settler-colonialism and the other kernel readings.',
    'If misidentified, the entire analysis of Zionism''s structural legitimacy would be flawed, leading to incorrect classification and policy recommendations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific kernel reading being instantiated.').

omega_variable(
    national_liberation_vs_settler_colonialism,
    'Does the ''national_liberation_reading'' of Zionism genuinely coexist with or is it foreclosed by the ''settler_colonial_reading''?',
    'Examination of whether a movement can simultaneously be a national liberation for one group and a settler-colonial project for another, particularly when it involves indigenous displacement. This requires a conceptual analysis of definitional boundaries.',
    'If ''forecloses'', the two readings are fundamentally contradictory within a single analytical framework. If ''coexists_with'', they represent different, potentially overlapping, analytical lenses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_liberation_vs_settler_colonialism, conceptual, 'Ambiguity regarding the logical compatibility of national liberation and settler-colonial framings for Zionism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of indigenous Palestinians primarily structural (external barriers) or internalized (cognitive patterns)?',
    'Post-exit suppression trajectory: if suppression of agency and self-determination persists after the formal colonial mechanisms are removed (e.g., in diaspora communities), reclassify as partially internalized. Otherwise, it is predominantly structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit. This would imply a deeper, more insidious form of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for indigenous Palestinians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1882, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1882, 0.3).
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.35).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.42).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(zion_be_t1882, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1882, 0.6).
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.7).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1882, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1882, 0.55).
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.65).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_occupation_of_palestinian_territories).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'zionist_legitimacy_basis' kernel. Its structural analysis of Zionism as a settler-colonial movement directly influences and is influenced by related constraints concerning Palestinian rights and Israeli policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
