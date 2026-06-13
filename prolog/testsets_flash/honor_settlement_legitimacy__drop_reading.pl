% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Persistence of Dueling in Residual Honor Cultures (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the persistence of dueling as a fringe practice
 *   within specific social and geographic niches where traditional honor
 *   cultures retained informal legitimacy, despite formal legal proscription.
 *   It is a reading of the 'honor_settlement_legitimacy' kernel, focusing on
 *   the 'drop' scenario where the practice did not vanish entirely but
 *   receded to specific, resilient pockets. The constraint is claimed as a
 *   Tangled Rope because it still provides a coordination function for
 *   honor-bound individuals while extracting high costs, and requires active
 *   enforcement (both legal and cultural) to maintain its precarious balance.
 *
 * KEY AGENTS:
 *   - residual_honor_culture_adherents: Beneficiary (moderate/identity_locked) — upholds honor, gains social cohesion
 *   - honor_culture_gatekeepers: Agenda Setter (organized/constrained) — preserves cultural framework, maintains authority
 *   - duelists: Payer (powerless/trapped) — bears personal risk and legal consequences
 *   - families_of_duelists: Payer (powerless/constrained) — bears social stigma, trauma, economic hardship
 *   - state_legal_authorities: Agenda Setter (institutional/analytical) — enforces proscription, suppresses practice
 *   - broader_society: Observer (organized/mobile) — views dueling as anachronistic, largely unaffected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.6).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.7).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Persistence of Dueling in Residual Honor Cultures (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '62815b6e-d086-4d8f-b1b8-50d45e7dd3e6').
narrative_ontology:cs_kernel_codification('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', implicit).
narrative_ontology:cs_authority_grounding('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', practice).
narrative_ontology:cs_interpretation_layer_present('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6').
narrative_ontology:cs_reading_relation('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', foundational, honor_demands_personal_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_personal_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', honor_demands_personal_satisfaction, conventional).
narrative_ontology:cs_axiom('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', secondary, state_law_subordinate_to_honor_code).
narrative_ontology:cs_axiom_status(state_law_subordinate_to_honor_code, holdable).
narrative_ontology:cs_axiom_grounding('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', state_law_subordinate_to_honor_code, conventional).
narrative_ontology:cs_reference_frame('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', residual_honor_code_integrity).
narrative_ontology:cs_drift_state('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', mid_20th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62815b6e-d086-4d8f-b1b8-50d45e7dd3e6', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_culture_adherents).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_culture_gatekeepers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duelists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who maintain a strong personal and social identity tied to traditional honor codes, for whom dueling, though legally proscribed, remains a legitimate (if dangerous) means of settling certain disputes. They benefit from the social cohesion and status derived from upholding these norms, even if it entails risk.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_culture_adherents, beneficiary,
    moderate, biographical, identity_locked, local).

% Elders, community leaders, or influential figures within residual honor cultures who tacitly or explicitly sanction dueling as a means of upholding honor, often by mediating challenges or ensuring adherence to ritual. They maintain their authority by preserving the cultural framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_gatekeepers, agenda_setter,
    organized, generational, constrained, local).

% Individuals who engage in duels, facing severe personal risk (injury, death) and legal consequences. They are often compelled by social pressure within their honor culture, with few acceptable alternatives for resolving perceived slights.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duelists, payer,
    powerless, immediate, trapped, local).

% Bear the social stigma, emotional trauma, and economic hardship resulting from duels, whether through loss of life, injury, or legal penalties. They are often bound by the same honor codes that compel dueling, limiting their ability to object or exit.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, families_of_duelists, payer,
    powerless, generational, constrained, local).

% Enforce laws against dueling, viewing it as a criminal act. Their efforts are aimed at suppressing the practice, but they face challenges in communities where honor codes retain strong informal authority, leading to a persistent, though fringe, practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Views dueling as an anachronistic and barbaric practice, largely irrelevant to modern social norms. They are largely unaffected by its fringe persistence but may occasionally be drawn into public discourse when incidents occur.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, broader_society, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a ritualized, if dangerous, mechanism for settling disputes and restoring perceived honor within specific cultural niches, preventing open-ended feuds or unresolved slights that could destabilize social order within those groups.
% TRANSFER_FUNCTION: Transfers the burden of dispute resolution and honor maintenance onto individuals (duelists) and their families, while preserving the social capital and authority of honor culture gatekeepers. It also transfers legal risk from the collective to the individual.
% ABSENT_VOICES: The victims of dueling (injured parties, widows, orphans) are often silenced by the very honor code that compels the practice, or by fear of further social ostracization. They would argue for alternative, non-violent means of dispute resolution and a redefinition of honor.
% DISAPPEARANCE_RATIONALE: If the informal legitimacy of dueling vanished overnight, residual honor cultures would face a crisis in dispute resolution, potentially leading to less ritualized violence or a complete collapse of their internal social order. The state's legal authority would be strengthened, but the underlying cultural dynamics would need new outlets.
% FOUNDING_PROBLEM: The need for a definitive, public means of resolving grave insults and maintaining social standing in societies where personal honor was paramount and state legal systems were either weak or not seen as legitimate arbiters of such disputes.
% FOUNDING_PROBLEM_CORROBORATION: Within residual honor cultures, the problem of maintaining honor and resolving slights is still considered live, as attested by adherents and gatekeepers. State legal authorities, while condemning dueling, acknowledge the persistence of honor-based conflicts in certain communities, corroborating the continued existence of the underlying social problem, even if the 'solution' is proscribed.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is high due to the severe personal and social costs borne by duelists and their families, often leading to death or legal penalties. Suppression (0.7) is also high, reflecting both the active legal enforcement by the state and the powerful social pressure within honor cultures that compels participation and silences dissent. The theater ratio (0.4) indicates that while the practice is genuinely functional for maintaining honor within its niche, a significant portion of its persistence involves performative adherence to a fading tradition, often in secret or with ritualized elements that mask its illegality. The declining extractiveness and rising suppression over the interval reflect the increasing legal pressure and the shrinking social space for dueling.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of residual honor culture adherents and gatekeepers, dueling, though dangerous, is a necessary (if regrettable) mechanism for maintaining social order and personal dignity, thus appearing as a coordination function. From the perspective of duelists and their families, it is a coercive, high-cost obligation with few alternatives, appearing as pure extraction. State legal authorities view it as a criminal act to be suppressed, seeing no legitimate coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor culture adherents and gatekeepers are beneficiaries (d near 0.0-0.2) as they derive social cohesion and authority from the practice. Duelists and their families are targets (d near 0.8-1.0) as they bear the direct costs and risks. State legal authorities are agenda-setters, attempting to suppress the practice (d near 0.5, as they expend resources to enforce but also benefit from upholding the rule of law).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (settling honor disputes) is still 'live' within its specific cultural niche, preventing a full mandatrophy resolution. However, the increasing suppression and declining extractiveness over time suggest a slow process of functional atrophy, where the costs of maintaining the practice are rising relative to its diminishing social utility, pushing it towards a Piton-like state for the broader society, even if it remains a Tangled Rope for its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_suppression,
    'What proportion of the measured suppression is due to state legal enforcement versus internalized cultural pressure within honor cultures?',
    'Comparative analysis of dueling incidence and enforcement outcomes in jurisdictions with varying legal stringency but similar honor cultures, or ethnographic studies of internal social sanctions.',
    'If cultural pressure is dominant, the constraint is more deeply embedded and resistant to external legal remedies; if legal enforcement is dominant, changes in state policy would have a more direct impact on its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_legal_suppression, empirical, 'Distinguishing sources of suppression.').

omega_variable(
    honor_redefinition_potential,
    'To what extent can the concept of ''honor'' be redefined within these residual cultures to exclude dueling as a legitimate means of settlement?',
    'Longitudinal ethnographic studies of cultural evolution in honor-bound communities, or analysis of successful historical redefinitions of honor in similar contexts.',
    'If redefinition is possible, the constraint could transition towards a Rope or even disappear as the underlying cultural axiom shifts; if honor is rigidly tied to dueling, the constraint will remain highly resistant to change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_redefinition_potential, conceptual, 'Flexibility of honor concept.').

omega_variable(
    drop_vs_contraction_distinction,
    'Is the persistence of dueling truly a ''drop'' (fringe survival) or is it better understood as a ''contraction'' (a broader cultural shift making it unthinkable, with only isolated, pathological exceptions)?',
    'Detailed historical and anthropological case studies comparing the normative status of dueling in these niches versus its cognitive status in the broader society, focusing on the ''thinkable'' vs. ''unthinkable'' threshold.',
    'If it''s a ''drop'', the honor culture remains a live, if marginalized, normative system. If it''s a ''contraction'', the persistence is an anomaly against a fundamentally altered cultural landscape, suggesting a different classification for the broader phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_distinction, conceptual, 'Distinguishing between fringe persistence and cognitive elimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__drop_reading, theater_ratio, 1875, 0.3).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(hono_tr_t1925, honor_settlement_legitimacy__drop_reading, theater_ratio, 1925, 0.45).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.5).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1875, 0.65).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(hono_be_t1925, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1925, 0.55).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1875, 0.6).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(hono_su_t1925, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1925, 0.75).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_settlement_legitimacy' kernel, focusing on the 'drop' scenario where dueling persisted as a fringe practice. It is linked to sibling readings that emphasize broader cultural contraction or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
