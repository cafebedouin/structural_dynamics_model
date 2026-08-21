% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint describes the 'existential matrix' reading of territorial
 *   sovereignty, where legitimacy is not derived from juridical claims but
 *   from the perceived necessity of territorial control for collective
 *   survival and identity. This framing renders conflict fundamentally
 *   zero-sum, making legal or historical arguments secondary to demographic
 *   and military dominance. The constraint is claimed as a Snare because it
 *   actively extracts from the subordinate group and suppresses alternatives,
 *   despite being framed by its beneficiaries as an unavoidable 'natural'
 *   state of conflict. The high extractiveness and suppression reflect the
 *   ongoing, violent nature of the conflict it describes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.98).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.99).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '5235a6f6-a388-42ff-ab2f-b94dcd694065').
narrative_ontology:cs_kernel_codification('5235a6f6-a388-42ff-ab2f-b94dcd694065', implicit).
narrative_ontology:cs_authority_grounding('5235a6f6-a388-42ff-ab2f-b94dcd694065', extraction).
narrative_ontology:cs_interpretation_layer_present('5235a6f6-a388-42ff-ab2f-b94dcd694065').
narrative_ontology:cs_reading_relation('5235a6f6-a388-42ff-ab2f-b94dcd694065', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5235a6f6-a388-42ff-ab2f-b94dcd694065', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('5235a6f6-a388-42ff-ab2f-b94dcd694065', foundational, territorial_control_is_existential_precondition).
narrative_ontology:cs_axiom_status(territorial_control_is_existential_precondition, holdable).
narrative_ontology:cs_axiom_grounding('5235a6f6-a388-42ff-ab2f-b94dcd694065', territorial_control_is_existential_precondition, empirically_contingent).
narrative_ontology:cs_axiom('5235a6f6-a388-42ff-ab2f-b94dcd694065', foundational, identity_expression_requires_sovereign_territory).
narrative_ontology:cs_axiom_status(identity_expression_requires_sovereign_territory, holdable).
narrative_ontology:cs_axiom_grounding('5235a6f6-a388-42ff-ab2f-b94dcd694065', identity_expression_requires_sovereign_territory, deontological).
narrative_ontology:cs_reference_frame('5235a6f6-a388-42ff-ab2f-b94dcd694065', zero_sum_survival_imperative).
narrative_ontology:cs_drift_state('5235a6f6-a388-42ff-ab2f-b94dcd694065', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5235a6f6-a388-42ff-ab2f-b94dcd694065', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethno_national_group).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_political_factions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_ethno_national_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_advocates).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_frameworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perceives territorial control as non-negotiable for its collective survival and identity. Any compromise is seen as an existential threat. Benefits from the zero-sum framing as it justifies maximalist claims and military action. Identity is fused with the land.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethno_national_group, agenda_setter,
    institutional, generational, identity_locked, national).

% Experiences the constraint as a constant threat to its own survival and identity, facing displacement, loss of land, and suppression of self-determination. Trapped by the conflict, with no viable exit from the territory or the zero-sum dynamic.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_ethno_national_group, payer,
    powerless, generational, trapped, national).

% Benefit from the existential framing by consolidating power, mobilizing support, and delegitimizing compromise. Their political survival is tied to the perpetuation of the zero-sum conflict. They actively enforce the narrative of existential threat.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_political_factions, beneficiary,
    organized, biographical, constrained, national).

% Attempt to promote dialogue and compromise, but are marginalized and delegitimized by the dominant existential narrative. Their efforts are seen as naive or traitorous by hardline factions, making their influence minimal.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_advocates, excluded,
    moderate, biographical, constrained, global).

% Provide alternative bases for legitimacy (e.g., UN resolutions, Geneva Conventions) but are dismissed as irrelevant or hostile by those operating within the existential matrix. Their juridical arguments are seen as secondary to the 'facts on the ground' and the imperative of survival.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_frameworks, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of the dominant group by providing a coherent, albeit zero-sum, framework for understanding territorial conflict, justifying resource allocation towards security and expansion, and maintaining internal cohesion against perceived external threats.
% TRANSFER_FUNCTION: Transfers land, resources, and security from the subordinate group to the dominant group, while also transferring political capital and legitimacy to hardline factions within the dominant group.
% ABSENT_VOICES: Any voice advocating for shared sovereignty, territorial compromise, or a non-zero-sum resolution is systematically excluded or silenced, as such proposals are framed as existential threats to either or both groups. International legal bodies are also effectively absent from the operative decision-making matrix.
% DISAPPEARANCE_RATIONALE: If this existential reading of sovereignty vanished, the entire framework of conflict would collapse. Legal and historical arguments would gain salience, territorial compromise would become thinkable, and the political landscape would be fundamentally reshaped, likely leading to new forms of governance and resource distribution.
% FOUNDING_PROBLEM: The perceived existential threat to a people's collective survival and identity in a contested territory, leading to a zero-sum competition for control.
% FOUNDING_PROBLEM_CORROBORATION: Both dominant and subordinate ethno-national groups, despite their opposing positions, attest to the live nature of their respective existential threats, often citing historical traumas and ongoing conflicts. This shared perception of existential vulnerability, though interpreted differently, corroborates the problem's persistence from outside any single benefiting party.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.95) and suppression (0.98) are extremely high because this reading describes a state of perpetual, zero-sum conflict where one group's gain is another's existential loss, enforced through military and demographic means. Resistance (0.99) is also extremely high, as the subordinate group actively resists its own suppression and extraction. Theater ratio (0.85) is high because legal and diplomatic efforts are largely performative, serving to legitimize pre-existing power dynamics rather than genuinely resolving the underlying existential conflict. Accessibility collapse (0.9) is high because the zero-sum framing makes any alternative to territorial control seem impossible or suicidal.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dominant group, this is a necessary, almost 'mountain-like' struggle for survival. From the subordinate group, it is a pure snare, actively extracting their land and future. The engine's classification as a Snare reflects the objective structural extraction and suppression, regardless of the dominant group's internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant ethno-national group and hardline political factions are beneficiaries (d near 0.0) as they gain territorial control and political power from this framing. The subordinate ethno-national group is the primary victim (d near 1.0), bearing the full cost of displacement, loss of land, and suppression. Peace advocates and international law frameworks are excluded (d near 0.5, but effectively ignored), as their non-zero-sum approaches are incompatible with the constraint's core logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare, not a Mountain, because its persistence depends on active enforcement and suppression of alternatives, not natural law. The 'existential' framing is a powerful justification for extraction, but it is a constructed narrative, not an unchangeable physical limit. The high theater ratio indicates that much of the 'diplomacy' and 'negotiation' around this conflict serves to maintain the zero-sum dynamic rather than resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_constructed_threat,
    'To what extent is the ''existential threat'' a genuine, irreducible condition of survival, versus a constructed narrative maintained by political actors to justify maximalist claims?',
    'Analysis of historical periods of coexistence and cooperation, and examination of political rhetoric for evidence of threat inflation or manufactured crises. If periods of non-zero-sum interaction are possible, the threat is at least partly constructed.',
    'If largely constructed, the constraint''s extractiveness is even more clearly a product of political will rather than necessity, strengthening its Snare classification. If genuinely irreducible, it would lean towards a Mountain, but its active enforcement and victim set would still pull it towards a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_vs_constructed_threat, conceptual, 'Distinguishing genuine existential threat from politically constructed narratives.').

omega_variable(
    identity_fusion_vs_political_interest,
    'Is the ''identity-locked'' exit option for the dominant group primarily due to genuine identity fusion with the land, or is it a strategic stance maintained by political elites whose power depends on this fusion?',
    'Sociological studies of identity formation, public opinion surveys on willingness to compromise under different security guarantees, and analysis of elite discourse for instrumentalization of identity.',
    'If primarily strategic, the ''identity-locked'' status is less an intrinsic property of the group and more a product of political manipulation, making the constraint''s suppression more clearly a tool of elite extraction. If genuine, it highlights the deep-seated nature of the conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_political_interest, empirical, 'Source of identity-locked exit: genuine fusion or political instrumentalization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if a subordinate group gains autonomy but still operates under the psychological shadow of the zero-sum framing), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resolution harder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.7).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.75).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 20, 0.8).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.82).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.84).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 50, 0.85).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 20, 0.91).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.94).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 20, 0.94).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.96).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.97).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel. This 'existential_matrix_reading' emphasizes survival and identity as the basis for territorial claims, making conflict zero-sum. It contrasts with the 'covenant_continuity_reading' (divine promise, historical presence) and the 'self_determination_reading' (demographic majority, modern principles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
