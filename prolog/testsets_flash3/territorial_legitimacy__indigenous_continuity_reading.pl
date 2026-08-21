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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Territorial Legitimacy via Indigenous Continuity (Nakba Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents a reading of territorial legitimacy grounded
 *   in continuous indigenous habitation and anti-colonial self-determination,
 *   specifically framing 1948 as the Nakba (catastrophe) rather than a
 *   legitimate partition. From this perspective, the Israeli state is a
 *   settler-colonial entity, and Palestinian sovereignty over all of historic
 *   Palestine, including the right of return for 1948 refugees, is
 *   structurally central. The constraint is claimed as a Snare because its
 *   persistence relies on active suppression and the denial of indigenous
 *   rights, with identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.95).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.98).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.99).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity (Nakba Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '9cb5ce95-32c6-465d-a3cb-60687bb542f4').
narrative_ontology:cs_kernel_codification('9cb5ce95-32c6-465d-a3cb-60687bb542f4', distributed).
narrative_ontology:cs_authority_grounding('9cb5ce95-32c6-465d-a3cb-60687bb542f4', extraction).
narrative_ontology:cs_reading_relation('9cb5ce95-32c6-465d-a3cb-60687bb542f4', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('9cb5ce95-32c6-465d-a3cb-60687bb542f4', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('9cb5ce95-32c6-465d-a3cb-60687bb542f4', foundational, indigenous_sovereignty_primacy).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9cb5ce95-32c6-465d-a3cb-60687bb542f4', indigenous_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('9cb5ce95-32c6-465d-a3cb-60687bb542f4', foundational, right_of_return_absolute).
narrative_ontology:cs_axiom_status(right_of_return_absolute, holdable).
narrative_ontology:cs_axiom_grounding('9cb5ce95-32c6-465d-a3cb-60687bb542f4', right_of_return_absolute, deontological).
narrative_ontology:cs_reference_frame('9cb5ce95-32c6-465d-a3cb-60687bb542f4', pre_nakba_palestinian_sovereignty).
narrative_ontology:cs_drift_state('9cb5ce95-32c6-465d-a3cb-60687bb542f4', contemporary_occupation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9cb5ce95-32c6-465d-a3cb-60687bb542f4', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, zionist_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous population, they bear the full cost of dispossession, displacement, and denial of self-determination. Their existence is continuously suppressed, and their right of return is denied. Exit means abandoning their identity and ancestral lands.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% The Israeli state, from this reading, is the settler-colonial entity whose existence is predicated on the dispossession of the Palestinian people. It actively enforces the constraint through military occupation, legal frameworks, and demographic policies. Its legitimacy is derived from the denial of indigenous rights.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, regional).

% Advocates within the international community who support Palestinian self-determination and the right of return. They observe the ongoing dispossession and advocate for international legal remedies, but their power to enforce change is limited.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_community_proponents, observer,
    organized, generational, analytical, global).

% Benefits from the establishment and maintenance of the Israeli state, which is seen as the fulfillment of its ideological goals. Its identity is deeply intertwined with the existence and expansion of the state, making any alternative difficult to conceive.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, zionist_movement, beneficiary,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the constraint does not solve a genuine coordination problem for the Palestinian people; rather, it coordinates the dispossession and control of indigenous land and population by a settler-colonial power.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the Palestinian people to the Israeli state and the Zionist movement, along with the denial of the right of return for refugees.
% ABSENT_VOICES: The voices of displaced Palestinians, those living under occupation, and those denied return are systematically suppressed or marginalized in international forums that do not fully adopt this reading. Their narratives are often reframed or dismissed as partisan rather than foundational.
% DISAPPEARANCE_RATIONALE: If this constraint (the denial of indigenous continuity and self-determination) disappeared, the entire political and territorial arrangement of historic Palestine would be fundamentally reordered. The Israeli state's legitimacy would collapse, the right of return would be enacted, and a new sovereign entity representing the Palestinian people would emerge.
% FOUNDING_PROBLEM: The problem this constraint was built to 'solve' (from the perspective of its beneficiaries) was the establishment of a Jewish state in historic Palestine, requiring the displacement and subjugation of the indigenous Palestinian population.
% FOUNDING_PROBLEM_CORROBORATION: The Palestinian people and their advocates attest that the problem of dispossession and denial of self-determination remains acutely live. International human rights organizations and UN resolutions (e.g., Resolution 194 on the right of return) corroborate the ongoing nature of the problem, from outside the benefiting parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint fundamentally denies the self-determination and land rights of an entire indigenous population. Suppression is also extremely high (0.98) due to ongoing military occupation, legal discrimination, and active efforts to prevent the return of refugees. Theater ratio is low (0.1) as there is little performative cover for the core extractive function; the conflict is overt. Resistance is high (0.99) reflecting continuous Palestinian struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and Zionist movement would experience this as a legitimate, even necessary, constraint for their existence and security, while the Palestinian people experience it as an existential snare. The engine's classification will highlight this divergence, showing a claimed 'snare' from the indigenous continuity reading, contrasting sharply with 'rope' or 'scaffold' classifications from other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are the primary targets and victims (d=1.0), bearing the full costs of dispossession. The Israeli state is the agenda-setter and primary beneficiary (d=0.0), actively enforcing the constraint to maintain its territorial control and demographic policies. The Zionist movement is also a beneficiary, with its identity deeply tied to the constraint's persistence. International proponents of Palestinian rights act as observers, while those who uphold the Israeli state's legitimacy (from other readings) are implicitly excluded from this reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the conventional sense, as its 'mandate' (from the perspective of its beneficiaries) is the ongoing maintenance of a settler-colonial project. The problem it 'solves' for its beneficiaries is continuously 'live' as long as the indigenous population resists. The classification as a Snare prevents mislabeling this as a coordination problem with a decaying mandate; its function is extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_legitimacy,
    'Is the historical narrative of continuous indigenous habitation and the Nakba the sole legitimate basis for territorial sovereignty, or do other historical claims (e.g., religious, historical presence) hold equal weight?',
    'International legal consensus on the primacy of indigenous rights and anti-colonial principles over other historical claims, or a negotiated political settlement that explicitly prioritizes one narrative.',
    'If indigenous continuity is universally recognized as primary, this reading''s claim to full Palestinian sovereignty is strengthened. If other claims are given equal weight, the territorial outcome becomes a contested negotiation rather than a clear right.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrative_legitimacy, conceptual, 'The foundational historical narrative grounding territorial legitimacy.').

omega_variable(
    right_of_return_feasibility,
    'Is the full right of return for all 1948 refugees practically feasible and compatible with a future political settlement, or would it necessitate a different form of statehood?',
    'Detailed demographic and logistical studies, combined with political negotiations on the structure of a future state that accommodates all populations.',
    'If full right of return is deemed feasible within a single state, this reading''s vision of a unified Palestine is reinforced. If not, it might lead to a re-evaluation of the political structure (e.g., confederation, two-state solution with compensation) even within an indigenous rights framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Practical and political feasibility of the right of return.').

omega_variable(
    settler_colonial_designation,
    'Is the designation of the Israeli state as a ''settler-colonial entity'' an accurate and universally accepted legal and historical classification, or is it a contested political framing?',
    'Broad international legal and academic consensus on the application of settler-colonial theory to the Israeli-Palestinian context, or a ruling by an international court of justice.',
    'If universally accepted, it strengthens the legal and moral basis for the indigenous continuity reading and its implications for state legitimacy. If contested, it remains a powerful but not universally binding analytical framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_colonial_designation, conceptual, 'The legal and historical classification of the Israeli state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.92).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1987, 0.93).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.94).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2014, 0.95).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.95).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1987, 0.96).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.97).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2014, 0.98).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. This 'indigenous_continuity_reading' directly challenges the premises of the 'partition_reading' and the 'security_necessity_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
