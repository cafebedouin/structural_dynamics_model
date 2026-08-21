% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework (1967 Borders Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'two-state coexistence' reading of the
 *   'territorial_legitimacy_dual' kernel, which advocates for mutual
 *   recognition of Israeli and Palestinian legitimacy, with 1967 borders as
 *   the basis for partition, a limited right of return for Palestinians, and
 *   security cooperation. It is a framework for managing an intractable
 *   conflict, requiring significant concessions from all parties and active
 *   enforcement against rejectionist elements. The metrics reflect the
 *   ongoing costs and coercive elements necessary to maintain this compromise
 *   in the face of persistent resistance.
 *
 * KEY AGENTS:
 *   - israeli_state_security: Beneficiary/Agenda-setter (institutional/constrained)
 *   - palestinian_state_sovereignty: Beneficiary/Payer (institutional/constrained)
 *   - international_community: Agenda-setter/Beneficiary/Observer (institutional/analytical)
 *   - palestinian_refugees_maximalist: Payer/Excluded (powerless/trapped)
 *   - israeli_settlers_maximalist: Payer/Excluded (organized/identity_locked)
 *   - rejectionist_factions_both_sides: Excluded/Payer (organized/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework (1967 Borders Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '0a8aeebc-90e2-4a6b-9207-9303b6c26d7c').
narrative_ontology:cs_kernel_codification('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', formalized).
narrative_ontology:cs_authority_grounding('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', lineage).
narrative_ontology:cs_interpretation_layer_present('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c').
narrative_ontology:cs_reading_relation('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', foundational, mutual_recognition_of_national_rights).
narrative_ontology:cs_axiom_status(mutual_recognition_of_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', mutual_recognition_of_national_rights, deontological).
narrative_ontology:cs_axiom('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', foundational, territorial_partition_on_1967_lines).
narrative_ontology:cs_axiom_status(territorial_partition_on_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', territorial_partition_on_1967_lines, conventional).
narrative_ontology:cs_reference_frame('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', oslo_accords_framework).
narrative_ontology:cs_drift_state('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', contemporary_stalemate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a8aeebc-90e2-4a6b-9207-9303b6c26d7c', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_security).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_sovereignty).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_community).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_maximalist).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_maximalist).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, rejectionist_factions_both_sides).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the framework's emphasis on security cooperation and mutual recognition, which aims to end existential threats. Bears the cost of relinquishing maximal territorial claims beyond the 1967 lines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_security, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_security, agenda_setter).

% Benefits from the promise of statehood and self-determination. Bears the cost of accepting 1967 borders and a limited right of return, which means relinquishing claims to 1948 territories and full refugee return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_sovereignty, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_sovereignty, payer).

% Actively promotes and seeks to enforce this framework as a path to regional stability and international law compliance. Benefits from reduced conflict and humanitarian crises, but bears the cost of diplomatic effort and potential aid commitments.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_community, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, international_community, beneficiary).

% Bears the cost of the limited right of return, which is seen as a betrayal of their historical claims and displacement trauma. They are largely excluded from the framework's negotiation and implementation, with no viable alternative for their maximalist demands.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_maximalist, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_maximalist, excluded).

% Bears the cost of potential withdrawal from settlements beyond the 1967 lines, which they view as ancestral or divinely promised land. Their identity is deeply tied to the land, making exit unthinkable and resistance high.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_maximalist, payer,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_maximalist, excluded).

% Actively reject the premise of mutual recognition and compromise, viewing the framework as illegitimate or a betrayal. They bear the costs of being marginalized and suppressed by the framework's enforcement mechanisms, but their identity is fused with the maximalist claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, rejectionist_factions_both_sides, excluded,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, rejectionist_factions_both_sides, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a mutually recognized framework for two sovereign states, resolving the zero-sum conflict over land and identity through partition along 1967 lines, a limited right of return, and security cooperation.
% TRANSFER_FUNCTION: Transfers territorial control and sovereign rights based on 1967 lines, and limits the right of return for Palestinian refugees, in exchange for security guarantees and mutual recognition of national rights.
% ABSENT_VOICES: Maximalist factions on both sides, who reject the premise of mutual recognition and compromise, are structurally excluded from the framework's negotiation and implementation. They would argue for exclusive claims to the entire territory.
% DISAPPEARANCE_RATIONALE: If this framework and its underlying principles vanished overnight, the conflict would revert to a zero-sum struggle, likely escalating violence, destabilizing the region, and forcing international actors to intervene more directly or withdraw entirely, leading to a complete reorganization of regional geopolitics.
% FOUNDING_PROBLEM: The intractable, violent conflict between two national movements claiming the same land, leading to cycles of war, occupation, and displacement, with no mutually acceptable resolution.
% FOUNDING_PROBLEM_CORROBORATION: International diplomatic consensus, numerous UN resolutions, and the ongoing humanitarian and security crises attest to the founding problem's persistence and the urgent need for a framework to manage it. This is corroborated by independent human rights organizations and geopolitical analysts.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The framework's extractiveness (0.70 at end) is high because it demands significant concessions from both Israeli and Palestinian maximalist claims, imposing costs on those who must relinquish historical or ideological positions. Suppression (0.80 at end) is also high, reflecting the active enforcement required to contain rejectionist factions and prevent a return to full-scale conflict. Theater ratio (0.30 at end) is moderate, as diplomatic efforts and security cooperation are real, but often involve performative elements to maintain international legitimacy despite limited progress. Resistance (0.75) is consistently high due to deeply entrenched maximalist narratives on both sides. The increasing trend in extractiveness and suppression over time reflects the growing difficulty of maintaining the framework amidst ongoing challenges and lack of full implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international community and moderate factions, this framework is a necessary and just compromise for peace and security. However, from the perspective of maximalist Palestinian refugees and Israeli settlers, it represents an unacceptable imposition and a betrayal of fundamental rights or historical claims. The engine's per-seat classification will highlight this divergence, showing it as a coordination mechanism for some and an extractive snare for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state security and Palestinian state sovereignty are beneficiaries in that the framework offers a path to secure existence and recognized statehood, respectively, though they bear costs of compromise. The international community benefits from regional stability and acts as an agenda-setter. Palestinian refugees and Israeli settlers, particularly those with maximalist claims, are primary targets (payers) as they bear the direct costs of territorial partition and limited right of return. Rejectionist factions are excluded and bear the costs of being actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's mandate to resolve the conflict remains live, as the founding problem persists. Mandatrophy is not resolved because the constraint is still actively pursued and enforced, despite its challenges. The rising extractiveness and suppression indicate that the costs of maintaining this 'compromise' are increasing, suggesting a potential drift towards a more extractive form if not successfully implemented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_achievability_vs_maximalism,
    'Is the two-state coexistence framework genuinely achievable given the persistent strength of maximalist readings on both sides, or is it perpetually undermined by them?',
    'Empirical observation of sustained, successful implementation of key framework components (e.g., border demarcation, security arrangements, refugee resettlement) over a decade, or a definitive collapse of diplomatic efforts.',
    'If perpetually undermined, the framework''s effective extractiveness and suppression are higher than measured, as it imposes costs without delivering its promised coordination benefits. If achievable, the current high extractiveness is a transitional cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_achievability_vs_maximalism, empirical, 'Whether the framework''s coordination function can overcome maximalist resistance.').

omega_variable(
    limited_right_of_return_justice,
    'Is the ''limited right of return'' a just and equitable compromise for Palestinian refugees, or does it constitute an imposed cost that violates fundamental rights?',
    'Resolution depends on a preference-based judgment regarding the balance of historical claims, national self-determination, and individual rights, potentially informed by international legal precedents on refugee status and compensation.',
    'If viewed as unjust, the framework''s extraction from Palestinian refugees is amplified, and its coordination function is fundamentally compromised by an ethical violation. If viewed as just, the extraction is a necessary cost of a broader peace.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_right_of_return_justice, preference, 'Ethical evaluation of the limited right of return.').

omega_variable(
    security_cooperation_symmetry,
    'Is the security cooperation envisioned by the framework truly mutual and balanced, or does it disproportionately benefit one side''s security at the expense of the other''s sovereignty or freedom of action?',
    'Detailed empirical analysis of security arrangements, including resource allocation, intelligence sharing, and operational control, assessed by independent security experts and human rights monitors.',
    'If disproportionately beneficial to one side, the framework''s coordination claim is weakened, and its effective extraction from the disadvantaged party is higher than measured, indicating a hidden snare within the coordination structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cooperation_symmetry, empirical, 'Assessment of the balance of benefits and costs in security cooperation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, regional_stability_middle_east).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy_dual' kernel, each representing a distinct structural claim about the Israeli-Palestinian conflict. This 'two-state coexistence' reading focuses on mutual recognition and partition, influencing and being influenced by the maximalist 'zionist_refuge' and 'palestinian_autochthony' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
