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
 *   human_readable: Territorial Legitimacy: Indigenous Continuity Reading
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'indigenous continuity' reading of
 *   territorial legitimacy, which views the events of 1948 as the Nakba
 *   (catastrophe) and asserts the continuous right of the Palestinian people
 *   to self-determination and sovereignty over all of historic Palestine.
 *   From this perspective, the Israeli state is a settler-colonial entity,
 *   and its existence is fundamentally illegitimate. The constraint's high
 *   extractiveness and suppression reflect the ongoing dispossession and
 *   denial of rights experienced by the Palestinian people. The claimed type
 *   is 'snare' because the coordination story (e.g., security for the Israeli
 *   state) is seen as a cover for pure extraction and suppression of the
 *   indigenous population.
 *
 * KEY AGENTS:
 *   - palestinian_people: Primary target (powerless/identity_locked) — bears full extraction
 *   - israeli_state: Agenda setter (institutional/constrained) — enforces the dispossession
 *   - international_community_supporters: Beneficiary (organized/mobile) — benefits from upholding international law
 *   - international_community_opponents: Excluded (institutional/arbitrage) — actively undermines Palestinian claims
 *   - human_rights_organizations: Observer (moderate/analytical) — documents violations
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
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy: Indigenous Continuity Reading").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '8ae04861-977b-4b56-8e56-c4ea678fb500').
narrative_ontology:cs_kernel_codification('8ae04861-977b-4b56-8e56-c4ea678fb500', distributed).
narrative_ontology:cs_authority_grounding('8ae04861-977b-4b56-8e56-c4ea678fb500', distributed).
narrative_ontology:cs_reading_relation('8ae04861-977b-4b56-8e56-c4ea678fb500', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('8ae04861-977b-4b56-8e56-c4ea678fb500', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('8ae04861-977b-4b56-8e56-c4ea678fb500', foundational, indigenous_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('8ae04861-977b-4b56-8e56-c4ea678fb500', indigenous_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('8ae04861-977b-4b56-8e56-c4ea678fb500', foundational, id_1948_nakba_is_foundational_injustice).
narrative_ontology:cs_axiom_status(id_1948_nakba_is_foundational_injustice, holdable).
narrative_ontology:cs_axiom_grounding('8ae04861-977b-4b56-8e56-c4ea678fb500', id_1948_nakba_is_foundational_injustice, empirically_contingent).
narrative_ontology:cs_reference_frame('8ae04861-977b-4b56-8e56-c4ea678fb500', pre_1948_indigenous_sovereignty).
narrative_ontology:cs_drift_state('8ae04861-977b-4b56-8e56-c4ea678fb500', contemporary_occupation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8ae04861-977b-4b56-8e56-c4ea678fb500', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, international_community_supporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous population, they bear the full cost of dispossession, displacement, and denial of self-determination. Their identity is deeply tied to the land, making exit unthinkable. They are the primary target of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, payer,
    powerless, generational, identity_locked, regional).

% The state that, from this reading's perspective, enforces the dispossession and maintains control over the territory. Its legitimacy is derived from a settler-colonial project, and it actively suppresses Palestinian claims to sovereignty and return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% States, organizations, and individuals who support Palestinian rights and self-determination. They benefit from upholding international law and anti-colonial principles, but their support often faces political and diplomatic constraints.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_community_supporters, beneficiary,
    organized, biographical, mobile, global).

% States and organizations that actively oppose or undermine Palestinian claims, often supporting the Israeli state's narrative. They are excluded from the moral and legal framework of indigenous continuity but actively shape the geopolitical context.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_community_opponents, excluded,
    institutional, biographical, arbitrage, global).

% Document and report on human rights violations, displacement, and denial of rights, providing evidence that supports the indigenous continuity reading. They operate as analytical observers, but their reports can influence international opinion and policy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, human_rights_organizations, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that the constraint's function is to coordinate the continuous indigenous habitation and self-determination of the Palestinian people, ensuring their rights are upheld against colonial displacement.
% TRANSFER_FUNCTION: The constraint, as read, transfers sovereignty, land, and the right to self-determination from the Palestinian people to a settler-colonial entity, while denying the right of return for refugees.
% ABSENT_VOICES: The voices of the dispossessed and displaced Palestinian refugees, particularly those from 1948, are often marginalized or silenced in international discourse, preventing their direct claims to return and sovereignty from being fully heard.
% DISAPPEARANCE_RATIONALE: If the indigenous continuity reading of territorial legitimacy were universally adopted and enforced overnight, the entire geopolitical structure of the region would fundamentally rearrange. The Israeli state's current territorial claims and governance would be delegitimized, leading to a complete re-evaluation of borders, citizenship, and the right of return for millions of refugees. This would entail a massive shift in power, land ownership, and national identity.
% FOUNDING_PROBLEM: The problem this reading addresses is the historical and ongoing dispossession of the indigenous Palestinian population, the denial of their right to self-determination, and the establishment of a settler-colonial state on their land, particularly the events of 1948 (Nakba).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by numerous UN resolutions, international human rights organizations (e.g., Amnesty International, Human Rights Watch), historical accounts from Palestinian and international scholars, and the continuous resistance and advocacy of the Palestinian people themselves. This corroboration comes from sources outside the direct beneficiaries of the current arrangement.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness is extremely high (0.95) because the constraint fundamentally denies the self-determination and territorial rights of an entire indigenous population, leading to ongoing dispossession and violence. Suppression is also extremely high (0.98) due to the active military, legal, and political mechanisms used to maintain control and prevent Palestinian return or sovereignty. Theater ratio is low (0.1) because the core function of the constraint, from this reading, is direct extraction and suppression, with minimal performative cover. Accessibility collapse is high (0.9) as alternatives for Palestinian self-determination are systematically dismantled. Resistance is high (0.99) reflecting the continuous struggle for liberation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian people, the constraint is a snare of pure extraction and suppression. From the perspective of the Israeli state, it is framed as a security necessity or a legitimate outcome of international partition. The engine's classification will highlight this divergence, showing a snare from the Palestinian seat and potentially a different type from other seats, based on their declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are the full target (d=1.0) as they bear the entire cost of dispossession and have no exit options due to identity-lock. The Israeli state, as the enforcer and beneficiary of the current territorial arrangement, sits at the beneficiary end (d=0.0). International community supporters, while advocating for Palestinian rights, are not directly extracted from, placing them closer to symmetric or beneficiary depending on their active involvement. International community opponents are structurally excluded from this reading's moral framework, but their actions contribute to the constraint's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the constraint's mandate (e.g., 'security' or 'partition') is a cover for ongoing settler-colonial extraction. The high extractiveness and suppression, coupled with the 'snare' classification, prevent mislabeling this as a coordination mechanism. The persistence of the 'founding problem' (dispossession) as 'live' for the Palestinian people, despite the 'dead' or 'contested' status from other readings, highlights the ongoing nature of the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_indigenous_continuity,
    'Is this constraint a genuine expression of indigenous continuity and anti-colonial self-determination, or is it a political claim framed as such?',
    'Historical and legal analysis of indigenous rights frameworks, comparative studies of settler-colonial contexts, and verification of continuous habitation claims.',
    'If verified as genuine, it strengthens the moral and legal imperative for Palestinian sovereignty. If found to be primarily a political framing, it would weaken its claim to foundational legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_indigenous_continuity, conceptual, 'This constraint is one reading of the ''territorial_legitimacy'' kernel, specifically the ''indigenous_continuity_reading''. Sibling readings (''partition_reading'', ''security_necessity_reading'') would structurally alter the beneficiary/victim sets and the claimed legitimacy basis.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if Palestinian refugees were granted right of return but still faced internal barriers to self-governance), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and displacement.').


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

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, right_of_return_for_palestinian_refugees).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, israeli_settlement_expansion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
