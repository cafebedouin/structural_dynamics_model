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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony and Right of Return (Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'palestinian_autochthony_reading'
 *   of the 'territorial_legitimacy_dual' kernel. From this perspective, the
 *   constraint is the ongoing denial of Palestinian legitimacy, continuous
 *   habitation, and the right of return, enforced by the Israeli state. It
 *   frames the 1948 displacement as an ongoing injustice requiring remedy,
 *   views territorial reduction as severe deprivation, contests Israeli state
 *   legitimacy, and holds the right of return as non-negotiable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.92).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.95).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony and Right of Return (Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '93e2bf16-26f6-4d32-afbc-5f69ac3f804d').
narrative_ontology:cs_kernel_codification('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', formalized).
narrative_ontology:cs_authority_grounding('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', extraction).
narrative_ontology:cs_interpretation_layer_present('93e2bf16-26f6-4d32-afbc-5f69ac3f804d').
narrative_ontology:cs_reading_relation('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', foundational, right_of_return_is_inalienable).
narrative_ontology:cs_axiom_status(right_of_return_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', right_of_return_is_inalienable, deontological).
narrative_ontology:cs_axiom('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', foundational, continuous_habitation_confers_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', continuous_habitation_confers_sovereignty, deontological).
narrative_ontology:cs_reference_frame('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', pre_1948_sovereignty_and_rights).
narrative_ontology:cs_drift_state('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('93e2bf16-26f6-4d32-afbc-5f69ac3f804d', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, displaced_palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of displacement, loss of land, and denial of self-determination. Their identity is deeply tied to the land and the right of return, making exit (abandoning claims) unthinkable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_people, payer,
    organized, generational, identity_locked, regional).

% Direct victims of the 1948 displacement and subsequent denials of return. They live in refugee camps or diaspora, maintaining their claim to ancestral lands, with their identity fused with the right of return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, displaced_palestinians, payer,
    powerless, generational, identity_locked, global).

% Enforces policies that deny Palestinian claims to continuous habitation and right of return, benefiting from territorial control and demographic advantage. Views Palestinian claims as an existential threat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from land acquisition and state protection in disputed territories. Their presence is often seen as a direct extension of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlers, beneficiary,
    powerful, biographical, mobile, local).

% Observes the conflict, issues resolutions and statements on international law, but often fails to enforce them effectively, leading to a perpetuation of the status quo.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_community, observer,
    institutional, biographical, analytical, global).

% Document violations of human rights and international law, advocate for Palestinian rights, and challenge the legitimacy of the current territorial arrangements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint actively prevents the coordination of legitimate Palestinian claims to self-determination, continuous habitation, and right of return, instead enforcing a status quo of dispossession.
% TRANSFER_FUNCTION: Transfers land, resources, sovereignty, and the right to self-determination from the Palestinian people to the Israeli state and its settlers, enforced through military, legal, and administrative means.
% ABSENT_VOICES: The voices of the Palestinian diaspora and those forcibly displaced are often marginalized or excluded from international negotiations and decision-making bodies that determine their future, despite being central to the claims.
% DISAPPEARANCE_RATIONALE: If the constraint (the denial of Palestinian legitimacy, continuous habitation, and right of return) vanished overnight, the entire geopolitical landscape of the region would fundamentally shift, requiring massive re-negotiation of borders, property rights, and citizenship, leading to a profound reorganization of the political and social order.
% FOUNDING_PROBLEM: The historical dispossession, displacement, and ongoing denial of self-determination and national rights for the Palestinian people, particularly stemming from the 1948 Nakba.
% FOUNDING_PROBLEM_CORROBORATION: International law, numerous UN resolutions, historical records, human rights reports from organizations like Amnesty International and Human Rights Watch, and independent historians consistently corroborate the ongoing nature of the founding problem, despite denials from the benefiting parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.92) as the constraint fundamentally denies the self-determination and property rights of an entire people, leading to severe deprivation. Suppression is also very high (0.95) due to the active military, legal, and administrative enforcement mechanisms used to maintain the status quo and prevent the realization of Palestinian claims. Theater ratio is low (0.10) because the constraint's function is direct and coercive, with little performative cover; the enforcement is explicit and aimed at maintaining control. Accessibility collapse is high (0.88) as alternatives to the current situation (e.g., establishing a fully sovereign Palestinian state, implementing the right of return) are systematically blocked. Resistance is high (0.85) reflecting the continuous struggle and opposition from the Palestinian people and their allies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state, the constraint might be framed as a necessary security measure or the exercise of sovereign rights. However, from the Palestinian autochthony reading, it is a clear case of ongoing extraction and suppression. The engine's classification will compute a Snare from the structural data, highlighting this divergence from any 'security' or 'natural right' claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and Israeli settlers are the primary beneficiaries, gaining territorial control and demographic advantage from the denial of Palestinian claims. The Palestinian people and displaced Palestinians are the direct targets and victims, bearing the full cost of dispossession, displacement, and denial of rights. The international community and human rights organizations act as observers, often acknowledging the injustice but lacking effective means to alter the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    international_law_interpretation_ambiguity,
    'Is the international legal framework (e.g., UN resolutions, Geneva Conventions) interpreted as supporting the Palestinian right of return and self-determination, or as primarily upholding the existing state sovereignty and security concerns?',
    'Adjudication by an internationally recognized, binding legal body with enforcement powers, or a shift in global consensus on the primacy of self-determination vs. state security in this context.',
    'If interpreted as strongly supporting Palestinian claims, the constraint''s legitimacy would further erode, increasing pressure for remedies. If interpreted as primarily upholding the status quo, the constraint''s persistence would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation and application of international law regarding Palestinian rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (military occupation, legal barriers, administrative control) or internalized (psychological impacts of trauma, learned helplessness, erosion of collective agency)?',
    'Post-occupation trajectory analysis: if suppression persists significantly after structural barriers are removed, it indicates a substantial internalized component. Conversely, rapid re-assertion of agency would indicate primarily structural suppression.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, and remedies would need to address psychological and social reconstruction, not just legal/political changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the Palestinian people.').

omega_variable(
    demographic_change_as_extraction,
    'To what extent does demographic engineering (e.g., settlement expansion, denial of family reunification) constitute an active form of extraction and suppression, beyond direct territorial control?',
    'Quantitative demographic analysis correlating policy changes with population shifts and resource allocation, alongside legal analysis of intent and impact on indigenous populations.',
    'If demographic engineering is confirmed as a primary extractive mechanism, the constraint''s extractiveness and suppression metrics would be further amplified, highlighting a deeper, systemic form of dispossession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_change_as_extraction, empirical, 'Role of demographic policies in extraction and suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.87).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2014, 0.91).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.88).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2014, 0.94).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
