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
 *   This constraint story instantiates the 'Palestinian autochthony and right
 *   of return' reading of the broader 'territorial_legitimacy_dual' kernel.
 *   From this perspective, Palestinian legitimacy is grounded in continuous
 *   habitation, the trauma of displacement, and the non-negotiable right of
 *   return. The constraint describes the ongoing denial of these claims,
 *   which results in severe extraction and suppression. The 1948 displacement
 *   (Nakba) is viewed as an ongoing injustice requiring remedy, territorial
 *   reduction as severe deprivation, and the legitimacy of the Israeli state
 *   within these contested territories is challenged.
 *
 * KEY AGENTS:
 *   - palestinians: Primary target (organized/identity_locked) — bears extraction, resists
 *   - palestinian_refugees: Primary target (powerless/identity_locked) — bears extraction, denied return
 *   - israeli_state: Primary beneficiary/agenda_setter (institutional/constrained) — benefits from and enforces the constraint
 *   - israeli_settlers: Secondary beneficiary (powerful/mobile) — benefits from territorial expansion
 *   - international_community: Analytical observer (institutional/analytical) — observes, but often constrained in enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.92).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony and Right of Return (Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '3a594193-5546-43bf-b65a-4a2226f092d2').
narrative_ontology:cs_kernel_codification('3a594193-5546-43bf-b65a-4a2226f092d2', implicit).
narrative_ontology:cs_authority_grounding('3a594193-5546-43bf-b65a-4a2226f092d2', practice).
narrative_ontology:cs_interpretation_layer_present('3a594193-5546-43bf-b65a-4a2226f092d2').
narrative_ontology:cs_reading_relation('3a594193-5546-43bf-b65a-4a2226f092d2', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('3a594193-5546-43bf-b65a-4a2226f092d2', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('3a594193-5546-43bf-b65a-4a2226f092d2', foundational, palestinian_right_of_return_absolute).
narrative_ontology:cs_axiom_status(palestinian_right_of_return_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3a594193-5546-43bf-b65a-4a2226f092d2', palestinian_right_of_return_absolute, deontological).
narrative_ontology:cs_axiom('3a594193-5546-43bf-b65a-4a2226f092d2', foundational, continuous_habitation_confers_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3a594193-5546-43bf-b65a-4a2226f092d2', continuous_habitation_confers_sovereignty, conventional).
narrative_ontology:cs_reference_frame('3a594193-5546-43bf-b65a-4a2226f092d2', pre_1948_uninterrupted_palestinian_sovereignty).
narrative_ontology:cs_drift_state('3a594193-5546-43bf-b65a-4a2226f092d2', contemporary_occupation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3a594193-5546-43bf-b65a-4a2226f092d2', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of territorial reduction, displacement, and denial of self-determination. Their identity is intrinsically linked to their continuous habitation and claim to the land, making 'exit' from this claim impossible without existential loss. They engage in continuous resistance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinians, payer,
    organized, generational, identity_locked, regional).

% Are denied the right of return to their ancestral lands, living in diaspora or refugee camps. Their claim to return is central to their collective identity, making 'exit' from this demand a profound betrayal of self and heritage. They are largely disempowered in international forums.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, payer,
    powerless, generational, identity_locked, global).

% Benefits from the territorial control and demographic status quo maintained by denying Palestinian autochthony and right of return. It actively enforces policies that prevent return and expand settlements, framing these actions as necessary for national security and self-preservation. Its legitimacy is tied to this control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from the expansion of settlements onto disputed land, often with state support and protection. Their presence reinforces the territorial claims that deny Palestinian rights. While benefiting, their long-term security is tied to the state's ability to maintain the status quo.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlers, beneficiary,
    powerful, biographical, mobile, local).

% Observes the conflict, often issuing resolutions and providing humanitarian aid, but is divided on enforcement and the interpretation of historical claims. Its actions are often constrained by geopolitical interests, leading to a de facto acceptance of the status quo despite formal recognition of Palestinian rights.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_community, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of this reading, the constraint *is* the ongoing problem of dispossession and denial of rights, rather than a coordination function. It enforces a status quo that *fails* to coordinate legitimate territorial claims and rights, instead maintaining an asymmetric power dynamic.
% TRANSFER_FUNCTION: Transfers land, resources, sovereignty, and the right to self-determination from Palestinians and Palestinian refugees to the Israeli state and Israeli settlers, enforced through military, legal, and political means.
% ABSENT_VOICES: The full, unmediated voices of Palestinian refugees and displaced persons, whose right of return is often discussed by other parties without their direct, empowered representation in negotiations or international forums.
% DISAPPEARANCE_RATIONALE: If the constraint of denied Palestinian autochthony and right of return vanished overnight, the entire demographic, political, and territorial landscape of the region would fundamentally shift. Millions of refugees would seek to return, challenging existing borders and governance structures, leading to a complete reorganization of the regional order.
% FOUNDING_PROBLEM: The historical displacement of Palestinians (the Nakba) in 1948 and the subsequent denial of their right to return and self-determination, leading to an ongoing state of statelessness and occupation.
% FOUNDING_PROBLEM_CORROBORATION: United Nations resolutions (e.g., Resolution 194 on the right of return), reports from international human rights organizations (e.g., Amnesty International, Human Rights Watch), historical archives, and extensive academic scholarship from independent historians and legal experts corroborate the ongoing nature of the displacement and denial of rights.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very high (0.92) because the constraint fundamentally denies a people's right to their land and self-determination, leading to ongoing dispossession and statelessness. Suppression is also very high (0.88) due to the active military, legal, and political enforcement mechanisms used by the Israeli state to prevent return and maintain control. The theater ratio is low (0.10) because the conflict is a deeply real and violent struggle over existential claims, not a performative maintenance of an atrophied function. Accessibility collapse is high (0.90) as alternatives for Palestinians to achieve their claims are severely limited by the existing power structures. Resistance is high (0.75) reflecting continuous Palestinian struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally diverges from the 'Zionist refuge' reading, which frames the Israeli state's existence as a necessary refuge and its territorial claims as legitimate. It also views the 'two-state coexistence' reading as potentially insufficient if it does not fully address the right of return and historical injustices. The engine's classification of this reading as a Snare highlights the structural extraction and suppression inherent in the denial of autochthony and right of return, a classification that would be strongly contested by the beneficiaries of the current arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinians and Palestinian refugees are full targets (high d) as they bear the direct and existential costs of the constraint. The Israeli state is the primary beneficiary/agenda-setter (low d) as it gains territorial control and maintains its demographic character through the constraint's operation. Israeli settlers are also beneficiaries, directly profiting from settlement expansion. The international community acts as an observer, with its directionality varying based on specific actions, but generally not directly benefiting or being targeted by this specific constraint's core operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine structural feature of reality, or one reading of a contested kernel?',
    'Recognition of alternative, structurally distinct readings (e.g., ''Zionist refuge'' or ''two-state coexistence'') that yield different ε values and classifications.',
    'If this is one reading of a kernel, its classification is relative to that reading''s premises; other readings would instantiate different constraints with different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''territorial_legitimacy_dual'' kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (external barriers, military occupation) or internalized (psychological impacts of trauma and displacement)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., psychological barriers to return or self-determination) after the external extractive mechanisms are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them even if external barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and displacement.').

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the Israeli state''s claim to exclusive sovereignty over the contested territories a ''natural law'' (e.g., divine promise, historical inevitability) or a constructed political claim maintained by force?',
    'Historical and legal analysis of the origins of the state and its territorial claims, examining the role of international law, military conquest, and demographic engineering versus claims of divine right or inherent historical connection.',
    'If a constructed claim, the constraint''s ''emerges_naturally'' property would be false, and its high extractiveness would be clearly attributable to human agency and enforcement, reinforcing its Snare classification. If genuinely ''natural'' (from a specific theological or historical perspective), it would challenge the Snare classification from that specific reading''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Ambiguity between natural law and constructed political claim for Israeli sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(terr_tr_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(terr_be_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2023, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(terr_su_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2023, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
