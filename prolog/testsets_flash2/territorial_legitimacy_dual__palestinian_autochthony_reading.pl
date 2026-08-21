% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony and Right of Return
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Palestinian autochthony' reading
 *   of the broader 'territorial legitimacy' kernel. It frames Palestinian
 *   legitimacy as grounded in continuous habitation, the trauma of
 *   displacement (especially 1948), and the non-negotiable right of return.
 *   This reading views the ongoing territorial reduction and denial of return
 *   as severe, actively enforced deprivation, making the constraint a snare
 *   from the perspective of Palestinian victims. The high extractiveness and
 *   suppression reflect the ongoing dispossession and the active enforcement
 *   required to maintain the status quo against Palestinian claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.95).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.9).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, 'c312d05a-52f5-4629-a08d-1148901f118f').
narrative_ontology:cs_kernel_codification('c312d05a-52f5-4629-a08d-1148901f118f', distributed).
narrative_ontology:cs_authority_grounding('c312d05a-52f5-4629-a08d-1148901f118f', practice).
narrative_ontology:cs_interpretation_layer_present('c312d05a-52f5-4629-a08d-1148901f118f').
narrative_ontology:cs_reading_relation('c312d05a-52f5-4629-a08d-1148901f118f', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('c312d05a-52f5-4629-a08d-1148901f118f', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('c312d05a-52f5-4629-a08d-1148901f118f', foundational, continuous_habitation_grants_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_grants_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c312d05a-52f5-4629-a08d-1148901f118f', continuous_habitation_grants_sovereignty, deontological).
narrative_ontology:cs_axiom('c312d05a-52f5-4629-a08d-1148901f118f', foundational, right_of_return_is_inalienable).
narrative_ontology:cs_axiom_status(right_of_return_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('c312d05a-52f5-4629-a08d-1148901f118f', right_of_return_is_inalienable, deontological).
narrative_ontology:cs_reference_frame('c312d05a-52f5-4629-a08d-1148901f118f', pre_1948_palestinian_sovereignty).
narrative_ontology:cs_drift_state('c312d05a-52f5-4629-a08d-1148901f118f', contemporary_occupation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c312d05a-52f5-4629-a08d-1148901f118f', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced from their ancestral lands in 1948 and subsequent conflicts, they bear the direct cost of dispossession and are denied the right to return. Their identity is deeply tied to their original homes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military occupation, facing restrictions on movement, land confiscation, and limited self-governance. They bear the costs of territorial reduction and contested sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, constrained, local).

% While not directly suffering territorial loss, they benefit from the preservation of a collective identity and the moral claim to a homeland, which fuels political advocacy and cultural cohesion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora, beneficiary,
    moderate, generational, mobile, global).

% Articulates and defends the narrative of continuous habitation and right of return, using it as a foundational claim for statehood and international recognition. Their legitimacy is tied to upholding these claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership, agenda_setter,
    organized, biographical, constrained, regional).

% The primary counter-party whose legitimacy is contested by this reading. It actively enforces policies that prevent the right of return and maintain control over disputed territories. It is excluded from the internal logic of this Palestinian reading.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, excluded,
    institutional, generational, analytical, national).

% Document and report on human rights violations related to displacement, occupation, and denial of return, often affirming the legal basis for the right of return under international law.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective Palestinian identity and political struggle around shared historical trauma, continuous connection to land, and the aspiration for return and self-determination.
% TRANSFER_FUNCTION: Transfers moral and political legitimacy from the historical presence and ongoing suffering of Palestinians to their claims for statehood and the right of return, while denying legitimacy to counter-claims.
% ABSENT_VOICES: The Israeli state and its supporters are structurally excluded from this narrative's internal logic; they would argue for their own historical and security claims, which are foreclosed by this reading's foundational axioms.
% DISAPPEARANCE_RATIONALE: If this foundational narrative vanished, Palestinian collective identity and political aspirations would fragment, the right of return would lose its moral and legal force, and the international discourse on the conflict would fundamentally shift, leading to a complete rearrangement of political positions and alliances.
% FOUNDING_PROBLEM: The dispossession and displacement of Palestinians in 1948 and subsequent conflicts, leading to a stateless population denied access to their ancestral lands.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian refugees and residents of occupied territories directly attest to the ongoing nature of the problem. International human rights organizations and UN resolutions corroborate the legal and humanitarian aspects of the displacement and the right of return, from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because this reading centers the profound loss of land, sovereignty, and self-determination experienced by Palestinians. Suppression is also very high (0.9) due to the active military and political enforcement mechanisms preventing the right of return and maintaining control over occupied territories. Theater ratio is low (0.1) as the claims are direct and the struggle is overt, with little performative cover for the core issues. Accessibility collapse is high (0.85) because, from this perspective, there are no acceptable alternatives to the right of return and full self-determination; any compromise is seen as further capitulation. Resistance is high (0.9) reflecting the continuous and often violent struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian refugees and residents, the constraint is a pure snare, actively extracting their land and rights through coercive force. For the Palestinian leadership, it is a foundational narrative that, while rooted in immense suffering, provides the basis for political action and international advocacy, making it a form of identity coordination that benefits their political project. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees and residents of occupied territories are the primary targets (d=1.0) as they bear the direct costs of displacement and occupation. The Palestinian diaspora and political leadership are beneficiaries (d=0.0-0.2) as this narrative provides a unifying framework for their identity and political agency, even as they advocate for those directly suffering. The Israeli state is structurally excluded from this reading's internal logic, as its legitimacy is fundamentally contested.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_contestation,
    'To what extent is the historical narrative of continuous habitation and exclusive autochthony empirically verifiable and universally accepted, versus being a constructed political claim?',
    'Comprehensive, independent historical and archaeological research, accepted by all parties, establishing the demographic and cultural continuity of Palestinian presence prior to 1948.',
    'If the narrative is widely corroborated, it strengthens the moral and legal force of the right of return. If it is found to be substantially contested or selectively presented, it could weaken the international consensus around the Palestinian claim, shifting the constraint''s perceived legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrative_contestation, empirical, 'Contestation over the historical basis of Palestinian autochthony.').

omega_variable(
    right_of_return_feasibility,
    'What are the practical and demographic implications of implementing the full right of return for all 1948 refugees and their descendants, and how does this impact the viability of a future Palestinian state?',
    'Detailed demographic and logistical studies, coupled with political negotiations on phased implementation and compensation mechanisms.',
    'If full implementation is deemed demographically or logistically impossible without fundamentally altering the character of the Israeli state, it could force a re-evaluation of the ''non-negotiable'' aspect of the right of return within this reading, potentially shifting the constraint towards a more ''constrained'' or ''negotiated'' outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, preference, 'Feasibility and implications of the right of return.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers, military occupation) or internalized (cognitive patterns of despair, identity fusion with victimhood)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism (occupation, denial of return) is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making political mobilization harder even if external barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.92).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2014, 0.94).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.88).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2014, 0.9).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, international_humanitarian_law_application).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel. It focuses on Palestinian autochthony and the right of return, which directly contests the 'Zionist refuge' reading and complicates the 'two-state coexistence' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
