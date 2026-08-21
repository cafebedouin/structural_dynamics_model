% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination in Palestine (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the liberal-nationalist reading of Jewish
 *   self-determination in Palestine, which posits that the Jewish people have
 *   a collective right to statehood in their ancestral homeland, but this
 *   right must be exercised in a manner that respects the co-equal
 *   self-determination rights of the Palestinian people. This reading
 *   typically advocates for a two-state solution or a binational framework,
 *   implying territorial compromise and power-sharing. The extractiveness is
 *   moderate because it acknowledges the need for concessions from the Jewish
 *   side, but still imposes a cost on Palestinians by requiring them to
 *   accept a Jewish state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.3).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '7daa3511-353d-407d-a244-b6ce6ab0e1ff').
narrative_ontology:cs_kernel_codification('7daa3511-353d-407d-a244-b6ce6ab0e1ff', formalized).
narrative_ontology:cs_authority_grounding('7daa3511-353d-407d-a244-b6ce6ab0e1ff', lineage).
narrative_ontology:cs_interpretation_layer_present('7daa3511-353d-407d-a244-b6ce6ab0e1ff').
narrative_ontology:cs_reading_relation('7daa3511-353d-407d-a244-b6ce6ab0e1ff', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('7daa3511-353d-407d-a244-b6ce6ab0e1ff', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7daa3511-353d-407d-a244-b6ce6ab0e1ff', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7daa3511-353d-407d-a244-b6ce6ab0e1ff', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('7daa3511-353d-407d-a244-b6ce6ab0e1ff', foundational, jewish_people_have_national_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_people_have_national_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('7daa3511-353d-407d-a244-b6ce6ab0e1ff', jewish_people_have_national_self_determination_right, deontological).
narrative_ontology:cs_axiom('7daa3511-353d-407d-a244-b6ce6ab0e1ff', foundational, palestinian_people_have_co_equal_national_self_determination_right).
narrative_ontology:cs_axiom_status(palestinian_people_have_co_equal_national_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('7daa3511-353d-407d-a244-b6ce6ab0e1ff', palestinian_people_have_co_equal_national_self_determination_right, deontological).
narrative_ontology:cs_reference_frame('7daa3511-353d-407d-a244-b6ce6ab0e1ff', liberal_democratic_nation_state_model).
narrative_ontology:cs_drift_state('7daa3511-353d-407d-a244-b6ce6ab0e1ff', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7daa3511-353d-407d-a244-b6ce6ab0e1ff', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As a collective, they assert their right to self-determination and statehood in their ancestral homeland, viewing it as a necessary condition for security and cultural flourishing. This reading acknowledges the need for territorial compromise and a just resolution for Palestinians.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation, beneficiary,
    institutional, generational, constrained, national).

% They are recognized as co-equal claimants to self-determination in the same territory. This reading implies a need for partition or a binational framework, which would require Palestinians to accept a Jewish state alongside their own, or within a shared framework, entailing significant territorial and political concessions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation, payer,
    organized, generational, trapped, national).

% Advocates for a two-state solution or a binational state based on principles of self-determination for both peoples, human rights, and international law. They seek to mediate a resolution that balances competing national claims.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_international_community, agenda_setter,
    institutional, generational, mobile, global).

% Reject the premise of co-equal Palestinian national rights and any territorial compromise, often on religious or security grounds. They are excluded from the liberal-nationalist framing's core assumptions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, right_wing_israeli_factions, excluded,
    powerful, generational, identity_locked, national).

% Reject the legitimacy of a Jewish state in any form, asserting exclusive Palestinian sovereignty over the entire territory. They are excluded from the liberal-nationalist framing's core assumptions of mutual recognition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_rejectionist_factions, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Jewish people to exercise national self-determination and establish a secure homeland, while simultaneously acknowledging and accommodating the self-determination rights of the Palestinian people in the same territory, aiming for a peaceful coexistence.
% TRANSFER_FUNCTION: Transfers sovereign control over a portion of the ancestral homeland to the Jewish people, in exchange for recognition of Palestinian rights and a commitment to a just resolution, potentially involving territorial partition or power-sharing.
% ABSENT_VOICES: Hardline factions on both Israeli and Palestinian sides are excluded from this liberal-nationalist discourse, as their maximalist claims fundamentally reject the premise of mutual recognition and compromise central to this reading. They would argue that the constraint is either too weak (from the Israeli right) or illegitimate (from the Palestinian rejectionists).
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist reading of Jewish self-determination vanished, the framework for a two-state solution or binational state would collapse. The international community would lose its primary diplomatic tool, and the conflict would likely escalate into more zero-sum, maximalist claims from all sides, leading to profound political and social reorganization.
% FOUNDING_PROBLEM: The historical statelessness and persecution of the Jewish people, coupled with their deep historical and religious connection to the land of Israel, necessitated a secure national home, while also recognizing the indigenous Palestinian population's ties to the same land.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Jewish security and self-determination remains live, attested by global Jewish communities and international bodies. The problem of Palestinian self-determination and displacement is also live, attested by the Palestinian people and numerous international human rights organizations. The challenge is how to reconcile these two live claims.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the inherent tension of two national claims over the same territory, but tempered by the liberal-nationalist commitment to mutual recognition and compromise. Suppression (0.30) is present due to the historical and ongoing enforcement of state borders and security measures, but it is lower than more maximalist readings because this framework theoretically allows for Palestinian agency and self-determination. Theater ratio (0.10) is low, as the core claims are genuinely pursued, though often with difficulty. The temporal measurements reflect periods of conflict (e.g., 1967) increasing extractiveness and suppression, and periods of peace efforts (e.g., 1993) where they might temporarily decrease, before settling into a more stable, moderately extractive state.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people (as a nation), this constraint is a legitimate exercise of self-determination, a 'rope' that secures their collective future. From the perspective of the Palestinian people (as a nation), it is a 'tangled rope' or 'snare' that imposes a cost on their own self-determination, even if it offers a path to statehood. The liberal international community views it as a 'rope' for conflict resolution. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people, as a nation, are the primary beneficiaries, as the constraint legitimizes their statehood. The Palestinian people, as a nation, are the payers, as they bear the cost of territorial division or shared sovereignty. The liberal international community acts as an agenda-setter, attempting to enforce a framework of mutual recognition. Hardline factions on both sides are excluded, as their positions are incompatible with the core tenets of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_compromise_feasibility,
    'Is genuine territorial compromise, sufficient to satisfy both Jewish and Palestinian national aspirations, empirically feasible given demographic realities and security concerns?',
    'Successful implementation of a two-state solution or a binational framework that is accepted by both populations and proves sustainable over decades.',
    'If feasible, the liberal-nationalist reading''s ''rope'' classification is strengthened. If infeasible, the reading''s coordination function is undermined, pushing it towards a ''tangled rope'' or ''snare'' as the costs on Palestinians become unresolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_feasibility, empirical, 'Empirical feasibility of territorial compromise for mutual self-determination.').

omega_variable(
    co_equal_self_determination_sincerity,
    'Is the commitment to co-equal Palestinian self-determination genuinely held by the Jewish national collective, or is it a rhetorical cover for maintaining a dominant position?',
    'Long-term observation of policy decisions, resource allocation, and public discourse within the Jewish national collective, particularly regarding settlement expansion, annexation, and Palestinian rights.',
    'If the commitment is insincere, the constraint''s extractiveness and suppression are higher than stated, and its classification shifts towards ''snare''. If sincere, the ''rope'' classification is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_equal_self_determination_sincerity, empirical, 'Sincerity of commitment to co-equal self-determination.').

omega_variable(
    liberal_nationalist_framing_legitimacy,
    'Does the liberal-nationalist framing adequately address the historical injustices and power asymmetries inherent in the conflict, or does it inadvertently legitimize an ongoing settler-colonial dynamic?',
    'Critical analysis from postcolonial scholars and Palestinian voices, assessing whether the proposed solutions genuinely dismantle power imbalances or merely reconfigure them.',
    'If the framing is found to perpetuate injustice, its legitimacy as a ''rope'' is undermined, and it may be reclassified as a ''tangled rope'' or even a ''snare'' from a critical perspective. If it genuinely offers a path to equity, its ''rope'' classification is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_nationalist_framing_legitimacy, conceptual, 'Legitimacy of liberal-nationalist framing in addressing historical injustices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.48).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2014, 0.38).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_palestinian_peace_process).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, international_law_on_occupation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
