% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Presumptive Illegitimacy of Borders (Freedom of Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'freedom of movement' reading of the
 *   border legitimacy kernel, asserting that freedom of movement is a human
 *   right and borders are presumptively illegitimate restrictions. From this
 *   perspective, border enforcement is a highly extractive and suppressive
 *   mechanism, creating victims not only among those seeking entry but also
 *   among certain segments of the destination state's population who bear the
 *   economic and social costs of restricted labor markets and welfare
 *   systems. The constraint is claimed as a 'snare' due to its high
 *   extractiveness and suppression, which are actively maintained by state
 *   enforcement agencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.92).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Presumptive Illegitimacy of Borders (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8').
narrative_ontology:cs_kernel_codification('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', formalized).
narrative_ontology:cs_authority_grounding('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', extraction).
narrative_ontology:cs_interpretation_layer_present('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8').
narrative_ontology:cs_reading_relation('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', foundational, freedom_of_movement_is_a_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_human_right, holdable).
narrative_ontology:cs_axiom_grounding('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', freedom_of_movement_is_a_human_right, deontological).
narrative_ontology:cs_axiom('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', foundational, borders_are_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(borders_are_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', borders_are_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', universal_human_rights_framework).
narrative_ontology:cs_drift_state('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', contemporary_nation_state_system, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('50ce8bb1-c1cc-42ef-a95a-f6d50521d6e8', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, citizens_of_wealthier_states).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_states).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals attempting to cross borders without legal authorization, facing physical danger, exploitation, detention, and deportation. They bear the direct costs of border enforcement and are denied opportunities for a better life.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants_seeking_entry, payer,
    powerless, immediate, trapped, global).

% Government bodies responsible for border patrol, immigration enforcement, and national security. They benefit from increased budgets, expanded powers, and political legitimacy derived from 'securing' borders. They actively enforce the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Segments of the population in destination countries who perceive benefits from restricted immigration, such as maintaining cultural homogeneity, reducing perceived competition for jobs, or preserving social welfare systems. They support border enforcement policies.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizens_of_wealthier_states, beneficiary,
    organized, biographical, mobile, national).

% Workers in destination countries whose wages or employment opportunities are negatively impacted by the artificial scarcity of labor created by border restrictions, or by the exploitation of undocumented workers in shadow economies. They bear indirect economic costs.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_in_destination_states, payer,
    moderate, biographical, constrained, national).

% Individuals relying on social welfare programs in destination countries who are often scapegoated in anti-immigrant narratives, leading to political pressure to reduce benefits or restrict access, even if immigration has no direct impact on their situation. They bear social and political costs.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states, payer,
    powerless, biographical, constrained, national).

% Organizations and individuals who monitor border policies, document abuses, and advocate for the rights of migrants. They challenge the legitimacy of current border regimes and seek to expand freedom of movement.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, state_enforcement_agencies).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of this reading, the constraint primarily serves to coordinate the exclusion of certain populations and the maintenance of state power, rather than solving a genuine collective action problem for all parties. Any 'coordination' is for the benefit of the excluding parties.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and safety from migrants seeking entry to state enforcement agencies (in terms of power and budget) and to citizens of wealthier states (in terms of perceived security and economic stability). It also transfers economic and social costs to displaced workers and welfare recipients within destination states.
% ABSENT_VOICES: The voices of those who would benefit from open borders (e.g., global poor, entrepreneurs seeking new markets, families separated by borders) are largely absent from policy-making discussions, or are actively suppressed by state narratives and enforcement. Their exclusion is fundamental to the constraint's operation.
% DISAPPEARANCE_RATIONALE: If borders were to vanish overnight, global labor markets would reconfigure, populations would shift, and the concept of national sovereignty would be fundamentally altered. The world would rearrange itself dramatically as people moved to optimize their opportunities and well-being.
% FOUNDING_PROBLEM: The constraint of national borders was historically established to define territorial sovereignty, control populations, and manage resources within a defined geographic area, often in the context of nation-state formation and inter-state conflict.
% FOUNDING_PROBLEM_CORROBORATION: State actors and proponents of national sovereignty attest that the founding problem of territorial control and national security is still live. However, human rights advocates and proponents of freedom of movement argue that the original problem has been superseded by global human rights norms and economic interdependence, and that the constraint now serves primarily to maintain inequality. Independent international legal scholars and economists often corroborate the latter view, highlighting the disjunction between historical justifications and contemporary impacts.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the constraint imposes significant costs on migrants (lost opportunities, danger, exploitation) and distorts labor markets in destination states. Suppression (0.92) is very high, reflecting the coercive force of state border regimes (walls, patrols, detention, deportation) and the lack of legal alternatives for entry. Theater ratio (0.1) is low, as border enforcement is a direct, functional exercise of state power, not primarily performative. Accessibility collapse (0.75) is substantial, as legal avenues for entry are severely limited for many. Resistance (0.8) is high, reflecting ongoing efforts by migrants and advocates to challenge border regimes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of migrants, the constraint is a clear snare, imposing immense costs and denying fundamental rights. From the perspective of state enforcement agencies, it is a necessary enforcement mechanism, perhaps even a mountain (if sovereignty is taken as absolute). Citizens of wealthier states may experience it as a rope, providing security and economic stability, while displaced workers and welfare recipients within those states may experience it as a snare dueating to labor market distortions and increased competition for resources.
 *
 * DIRECTIONALITY LOGIC:
 *   State enforcement agencies are clear beneficiaries (d near 0.0) as they maintain their power and budgets through border control. Citizens of wealthier states are also beneficiaries (d near 0.1-0.2) as they perceive benefits from controlled migration. Migrants seeking entry are the primary victims (d near 1.0), bearing the full brunt of the constraint. Displaced workers and welfare recipients in destination states are also victims (d near 0.8-0.9) due to the economic and social impacts of restricted labor mobility and the political narratives surrounding migration.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the 'mandate' of border control (if it ever had one beyond pure exclusion) has atrophied, and the constraint now primarily serves extractive functions. The high extractiveness and suppression, coupled with the identification of multiple victim groups, prevent mislabeling this as a coordination mechanism. The persistence of the constraint is due to the concentrated benefits for state power and certain citizen groups, despite the widespread costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_social_construct,
    'Is freedom of movement an inherent natural right, or a socially constructed right contingent on state recognition?',
    'Philosophical consensus on the grounding of human rights, or a global legal framework explicitly codifying it as a natural right.',
    'If a natural right, the constraint''s extractiveness and suppression are inherent violations; if socially constructed, its legitimacy is contingent on the political will of states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_social_construct, conceptual, 'Ambiguity of the grounding of freedom of movement.').

omega_variable(
    border_legitimacy_kernel_reading,
    'This constraint is one reading of the ''border_legitimacy'' kernel. How would the classification change under the ''sovereignty_reading'' or ''humanitarian_obligation_reading''?',
    'Adopting a different reading of the kernel would shift the declared beneficiaries/victims and thus the computed extractiveness and classification. The ''sovereignty_reading'' would likely reduce extractiveness for state actors and increase it for migrants, while the ''humanitarian_obligation_reading'' would introduce conditional legitimacy.',
    'The ''sovereignty_reading'' would likely classify border enforcement as a ''rope'' or ''mountain'' for citizens and ''snare'' for migrants, but with lower overall extractiveness. The ''humanitarian_obligation_reading'' would introduce a ''scaffold'' element for refugees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(border_legitimacy_kernel_reading, conceptual, 'This constraint is the ''freedom_of_movement_reading'' of the ''border_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__freedom_of_movement_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__freedom_of_movement_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__freedom_of_movement_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 30, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_legitimacy' kernel, each with distinct structural properties and classifications. This reading emphasizes the human right to freedom of movement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
