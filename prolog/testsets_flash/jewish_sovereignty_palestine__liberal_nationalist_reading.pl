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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination in Palestine (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'liberal nationalist' reading of Jewish
 *   sovereignty in Palestine. It asserts the Jewish people's right to
 *   collective self-determination, legitimately exercised through statehood
 *   in their ancestral homeland, but crucially acknowledges the co-equal
 *   self-determination claims of Palestinians. This reading therefore
 *   necessitates a framework of territorial compromise, such as partition or
 *   a binational state, to accommodate both national aspirations. It is a
 *   contested claim, requiring active enforcement to maintain its boundaries
 *   against both internal (more maximalist Zionist) and external (Palestinian
 *   rejectionist) challenges.
 *
 * KEY AGENTS:
 *   - jewish_collective_as_nation: Primary beneficiary (institutional/generational) — exercises self-determination
 *   - palestinian_collective_as_nation: Primary victim (institutional/generational) — bears costs of territorial compromise
 *   - israeli_state_institutions: Agenda setter (institutional/generational) — administers and enforces the state's existence
 *   - international_community: Observer (institutional/civilizational) — adjudicates legitimacy and mediates conflict
 *   - religious_zionist_factions: Excluded (organized/generational) — reject territorial compromise, push for maximalist claims
 *   - post_zionist_intellectuals: Observer (analytical/generational) — critique the national framework's impact on civic equality
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
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1').
narrative_ontology:cs_kernel_codification('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', formalized).
narrative_ontology:cs_authority_grounding('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', lineage).
narrative_ontology:cs_interpretation_layer_present('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1').
narrative_ontology:cs_reading_relation('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', foundational, universal_national_self_determination).
narrative_ontology:cs_axiom_status(universal_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', universal_national_self_determination, deontological).
narrative_ontology:cs_axiom('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', foundational, co_equal_rights_of_indigenous_populations).
narrative_ontology:cs_axiom_status(co_equal_rights_of_indigenous_populations, holdable).
narrative_ontology:cs_axiom_grounding('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', co_equal_rights_of_indigenous_populations, deontological).
narrative_ontology:cs_reference_frame('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', two_state_solution_framework).
narrative_ontology:cs_drift_state('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', contemporary_political_reality, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6cc08bbc-1c7a-493e-888e-cb6b3b7ceae1', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, universal_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, right_to_national_homeland).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Jewish people to exercise collective self-determination and establish statehood in their ancestral homeland, while simultaneously attempting to accommodate the co-equal self-determination claims of the Palestinian people through territorial compromise or shared governance.
% TRANSFER_FUNCTION: Transfers sovereign control over a portion of the ancestral homeland to the Jewish collective, in exchange for a commitment to a framework that acknowledges Palestinian rights, thereby transferring a portion of Palestinian self-determination potential to the Jewish state.
% ABSENT_VOICES: Religious Zionist factions, who reject territorial compromise on theological grounds, and Palestinian rejectionist groups, who deny the legitimacy of Jewish statehood, are both excluded from the liberal nationalist discourse, as their maximalist positions undermine the possibility of a negotiated settlement.
% DISAPPEARANCE_RATIONALE: If this specific liberal nationalist reading vanished, the political landscape would immediately polarize further. Without the framework of co-equal claims and compromise, the conflict would likely escalate, with maximalist claims from both sides becoming dominant, leading to a complete reorganization of diplomatic efforts and potentially increased violence.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness and persecution, coupled with the aspiration for national self-determination in their ancestral homeland, alongside the existing Palestinian Arab population's own national aspirations and presence in that land.
% FOUNDING_PROBLEM_CORROBORATION: The problem of competing national claims over the same territory is widely attested by international bodies (e.g., UN resolutions, peace process frameworks), numerous academic studies in political science and history, and the ongoing conflict itself. While the specific 'liberal nationalist' solution is contested, the underlying problem it seeks to address is undeniably live and corroborated by external observers.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate two competing national claims (Jewish and Palestinian self-determination) but inherently involves asymmetric extraction (Palestinians must compromise on territory or sovereignty for Jewish statehood). Extractiveness (0.45) is moderate, reflecting the expectation of territorial compromise, but not zero. Suppression (0.30) is present due to the need for active enforcement to maintain borders and manage conflict arising from competing claims. Theater ratio (0.10) is low, as the state's functions are largely genuine, though the 'coordination' aspect is heavily contested. The slight increase in extractiveness and suppression over time reflects the historical difficulty of achieving equitable compromise and the ongoing need for enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish collective, this constraint is a legitimate exercise of national rights, a hard-won Rope. From the perspective of the Palestinian collective, it is a Snare, as it imposes a compromise on their own self-determination. The liberal nationalist reading attempts to bridge this gap by acknowledging both claims, but the structural reality of competing claims over finite territory means one party's gain is necessarily another's loss, leading to a Tangled Rope classification from an analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective is the primary beneficiary (d=0.0-0.2) as the constraint legitimizes their statehood. The Palestinian collective is the primary target (d=0.8-1.0) as they must accept territorial compromise. Israeli state institutions act as the agenda-setter (d=0.1-0.3), enforcing the state's existence and its boundaries. The international community is an analytical observer (d=0.5). Religious Zionist factions are excluded (d=0.9-1.0) as their maximalist claims are incompatible with the liberal nationalist framework's need for compromise. Post-Zionist intellectuals are also observers (d=0.5), analyzing the constraint's long-term effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the core problem of competing national claims remains live. However, the 'liberal nationalist' framing itself could become a form of mandatrophy if it ceases to genuinely pursue equitable compromise and instead becomes a rhetorical cover for continued expansion or denial of Palestinian rights. The classification as Tangled Rope, rather than a pure Rope, acknowledges this inherent tension and the risk of drift towards a Snare if the coordination function atrophies while extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_historical_contingency,
    'Is the Jewish right to self-determination in Palestine a universal natural right, or a historically contingent claim requiring negotiation with existing populations?',
    'International legal consensus on the application of self-determination principles in contested territories, and the outcome of negotiations between claimant groups.',
    'If a natural right, the constraint''s legitimacy is inherent and extractiveness is minimized (costs are ''tragic necessity''); if historically contingent, extractiveness is higher (costs are ''unjust imposition'') and requires active justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_historical_contingency, conceptual, 'Ambiguity between inherent right and negotiated claim.').

omega_variable(
    co_equal_claimants_vs_subordinate_status,
    'Does this reading genuinely treat Palestinians as co-equal self-determination claimants, or does it implicitly subordinate their claim to the Jewish claim?',
    'Analysis of policy outcomes: whether proposed solutions (e.g., partition, binationalism) are genuinely equitable and implementable, or if they consistently result in Palestinian dispossession or limited sovereignty.',
    'If co-equal, the constraint functions as a genuine (if difficult) coordination problem; if subordinate, it functions as a snare, using the language of rights to mask extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_claimants_vs_subordinate_status, empirical, 'Whether Palestinian self-determination is genuinely co-equal or implicitly subordinated.').

omega_variable(
    liberal_nationalist_vs_other_readings,
    'This constraint is the ''liberal_nationalist_reading'' of the ''jewish_sovereignty_palestine'' kernel. How would the classification change under a ''settler_colonial_reading'' or ''religious_zionist_reading''?',
    'Comparing the structural properties (beneficiaries, victims, extractiveness, suppression) of this reading with those of the sibling readings, as instantiated in their respective constraint stories.',
    'A ''settler_colonial_reading'' would likely yield higher extractiveness and suppression, classifying as a Snare. A ''religious_zionist_reading'' would likely claim Mountain status with lower extractiveness, but potentially higher suppression if it denies Palestinian claims entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liberal_nationalist_vs_other_readings, conceptual, 'Impact of alternative kernel readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jerusalem_status_quo).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jewish_sovereignty_palestine' kernel. Each reading represents a distinct structural claim with different beneficiaries, victims, and extractiveness profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
