% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'indigenous continuity' reading of
 *   territorial legitimacy, which frames the 1948 events as the Nakba
 *   (catastrophe) and views the Israeli state as a settler-colonial entity.
 *   From this perspective, legitimacy derives from continuous indigenous
 *   habitation and anti-colonial self-determination, demanding Palestinian
 *   sovereignty over all of historic Palestine and the right of return for
 *   1948 refugees. The high extractiveness and suppression metrics reflect
 *   the ongoing costs borne by the Palestinian people under the current
 *   contested arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.9).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.95).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).
domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'b1a7223a-3596-4f95-89c6-60fa093a0bb7').
narrative_ontology:cs_kernel_codification('b1a7223a-3596-4f95-89c6-60fa093a0bb7', implicit).
narrative_ontology:cs_authority_grounding('b1a7223a-3596-4f95-89c6-60fa093a0bb7', practice).
narrative_ontology:cs_interpretation_layer_present('b1a7223a-3596-4f95-89c6-60fa093a0bb7').
narrative_ontology:cs_reading_relation('b1a7223a-3596-4f95-89c6-60fa093a0bb7', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('b1a7223a-3596-4f95-89c6-60fa093a0bb7', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('b1a7223a-3596-4f95-89c6-60fa093a0bb7', foundational, indigenous_rights_are_inalienable).
narrative_ontology:cs_axiom_status(indigenous_rights_are_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('b1a7223a-3596-4f95-89c6-60fa093a0bb7', indigenous_rights_are_inalienable, deontological).
narrative_ontology:cs_axiom('b1a7223a-3596-4f95-89c6-60fa093a0bb7', foundational, settler_colonialism_is_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonialism_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('b1a7223a-3596-4f95-89c6-60fa093a0bb7', settler_colonialism_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('b1a7223a-3596-4f95-89c6-60fa093a0bb7', pre_1948_palestinian_sovereignty).
narrative_ontology:cs_drift_state('b1a7223a-3596-4f95-89c6-60fa093a0bb7', contemporary_international_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b1a7223a-3596-4f95-89c6-60fa093a0bb7', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, zionist_movement_supporters).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, zionist_movement_supporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous population, they are the primary beneficiaries of the principle of continuous habitation and self-determination. However, they are also the victims of the ongoing denial of this principle, bearing the costs of displacement, occupation, and statelessness. Their identity is deeply tied to the land and the right of return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, payer).

% The Israeli state, from this reading's perspective, is the settler-colonial entity whose legitimacy is challenged. It actively enforces the current territorial arrangements and would bear the costs (loss of territory, demographic shift) if the indigenous continuity principle were fully realized. It frames its existence as a security necessity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_state, payer).

% Comprises various states and international bodies that observe, debate, and occasionally intervene in the conflict. While some elements acknowledge indigenous rights, the dominant discourse often prioritizes state sovereignty or security, leading to a contested and often contradictory stance on this reading's claims. They could enforce or deny the constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_community, observer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, international_community, agenda_setter).

% These individuals and groups globally support the Israeli state's claims and narrative, benefiting from its existence and the associated ideological framework. They would bear significant ideological and political costs if the indigenous continuity reading were to prevail, as it fundamentally challenges their foundational narrative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, zionist_movement_supporters, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, zionist_movement_supporters, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational principle for territorial legitimacy based on historical indigenous presence and anti-colonial self-determination, aiming to coordinate international law and state practice around principles of historical justice and decolonization.
% TRANSFER_FUNCTION: Transfers claims of sovereignty, the right of return for 1948 refugees, and control over historic Palestine to the Palestinian people, implicitly transferring territorial control and resources from the Israeli state.
% ABSENT_VOICES: The voices of indigenous peoples globally, whose historical claims to land and self-determination are often suppressed or ignored in international discourse, would strongly corroborate this reading. Their experiences of settler-colonialism and dispossession resonate directly with the Palestinian narrative.
% DISAPPEARANCE_RATIONALE: If the principle of indigenous continuity and anti-colonial self-determination vanished, the entire framework for challenging settler-colonial states and advocating for historical justice would collapse. This would fundamentally alter international law, political movements, and the very concept of legitimate territorial claims, particularly for post-colonial nations.
% FOUNDING_PROBLEM: The historical injustice of settler-colonialism and the displacement of indigenous populations, specifically the 1948 Nakba and subsequent occupation, which denied the Palestinian people their right to self-determination and continuous habitation on their ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the region, international human rights organizations (e.g., Amnesty International, Human Rights Watch), UN resolutions on Palestinian rights, and numerous independent legal scholars corroborate the ongoing nature of the displacement, occupation, and denial of self-determination. This corroboration comes from outside the immediate Palestinian beneficiary group.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.90) reflects the profound and ongoing dispossession, displacement, and denial of rights experienced by the Palestinian people. Suppression (0.95) is extremely high due to the military occupation, legal frameworks, and political actions that actively prevent the realization of Palestinian self-determination and the right of return. The theater ratio is low (0.10) because this reading is a deeply held, actively pursued claim of justice, not a performative or atrophied function. The claimed type is 'mountain' because, from this reading's perspective, indigenous rights and anti-colonial principles are fundamental, unchangeable truths of justice.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and its supporters fundamentally reject this reading, framing their presence as a security necessity or based on international partition. They would view this constraint as a political claim, not a natural law, and would emphasize their own historical and security narratives. The Palestinian people, conversely, view this as an inherent, unalienable right grounded in historical fact and international law, seeing any alternative as a perpetuation of injustice. The engine's computation of a 'false summit mountain' for this claimed type, given the declared beneficiaries, captures this inherent contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are both the beneficiaries of the principle of indigenous continuity (as it asserts their fundamental rights) and the victims of its ongoing violation (as they bear the costs of its denial). The Israeli state, as the entity whose legitimacy is challenged by this reading, would be the primary 'payer' if this constraint were enforced, losing its current territorial claims and control. The international community and Zionist movement supporters occupy complex positions, acting as both observers and potential enforcers/challengers, with varying degrees of benefit from or cost to the current arrangement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_political_claim,
    'Is the principle of indigenous continuity and anti-colonial self-determination a natural law of justice, or a political claim subject to negotiation and compromise?',
    'Philosophical and legal consensus on the universality and inalienability of indigenous rights, or a shift in international legal precedent that explicitly codifies or rejects such claims as foundational.',
    'If affirmed as natural law, the constraint''s ''mountain'' classification is strengthened, implying its unchangeability. If reclassified as a political claim, its persistence becomes contingent on power dynamics and negotiation, potentially shifting its classification to a ''snare'' or ''tangled_rope'' depending on enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_political_claim, conceptual, 'Ambiguity between inherent justice and political construct.').

omega_variable(
    right_of_return_feasibility,
    'What are the practical and demographic implications of implementing the right of return for 1948 refugees, and how would this impact the existing population?',
    'Detailed demographic studies, infrastructure planning, and political agreements on phased implementation and integration, or the establishment of a truth and reconciliation commission to address historical grievances and resettlement options.',
    'If implementation is deemed practically impossible or leads to severe destabilization, it could weaken the international community''s willingness to enforce this aspect of the constraint, potentially leading to a ''piton'' classification for the right of return itself. If feasible, it strengthens the constraint''s overall force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Practicality of implementing the right of return.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(terr_tr_t75, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 75, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 45, 0.85).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(terr_be_t75, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 75, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 60, 0.93).
narrative_ontology:measurement(terr_su_t75, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 75, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The concept of 'territorial legitimacy' is decomposed into three distinct readings: indigenous continuity, partition, and security necessity. Each reading constitutes a separate constraint with its own structural properties and classification, reflecting the contested nature of the underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
