% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty Maximalist Reading of RBIO Norms
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty maximalist' reading of the
 *   Responsibility to Protect (R2P) and broader international norms regarding
 *   intervention (RBIO norms). In this reading, state sovereignty is
 *   considered absolute, and any external interference, including
 *   humanitarian intervention, is viewed as a pretext for regime change or an
 *   infringement on national self-determination. RBIO norms are deemed
 *   legitimate only when they reinforce state sovereignty against external
 *   pressures. This reading is a Snare, as it primarily serves to protect
 *   authoritarian regimes from accountability, at the cost of populations
 *   trapped under repressive governments.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: Primary beneficiary (institutional/arbitrage) — shielded from external accountability.
 *   - populations_under_repressive_governments: Primary victim (powerless/trapped) — denied external recourse.
 *   - states_seeking_unfettered_internal_control: Agenda setter (institutional/mobile) — actively promotes this reading.
 *   - human_rights_advocates: Victim (moderate/constrained) — efforts to protect populations are hampered.
 *   - international_humanitarian_organizations: Victim (organized/constrained) — operational capacity suppressed.
 *   - liberal_institutionalists: Excluded (institutional/analytical) — their arguments are dismissed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.92).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty Maximalist Reading of RBIO Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '25f8f306-d394-4b14-8739-bc884d1c8ed6').
narrative_ontology:cs_kernel_codification('25f8f306-d394-4b14-8739-bc884d1c8ed6', formalized).
narrative_ontology:cs_authority_grounding('25f8f306-d394-4b14-8739-bc884d1c8ed6', extraction).
narrative_ontology:cs_interpretation_layer_present('25f8f306-d394-4b14-8739-bc884d1c8ed6').
narrative_ontology:cs_reading_relation('25f8f306-d394-4b14-8739-bc884d1c8ed6', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('25f8f306-d394-4b14-8739-bc884d1c8ed6', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('25f8f306-d394-4b14-8739-bc884d1c8ed6', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('25f8f306-d394-4b14-8739-bc884d1c8ed6', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('25f8f306-d394-4b14-8739-bc884d1c8ed6', foundational, humanitarian_intervention_is_regime_change).
narrative_ontology:cs_axiom_status(humanitarian_intervention_is_regime_change, holdable).
narrative_ontology:cs_axiom_grounding('25f8f306-d394-4b14-8739-bc884d1c8ed6', humanitarian_intervention_is_regime_change, empirically_contingent).
narrative_ontology:cs_reference_frame('25f8f306-d394-4b14-8739-bc884d1c8ed6', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('25f8f306-d394-4b14-8739-bc884d1c8ed6', post_cold_war_humanitarian_interventions_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('25f8f306-d394-4b14-8739-bc884d1c8ed6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, states_seeking_unfettered_internal_control).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These regimes benefit directly from the maximalist interpretation, which shields them from external scrutiny or intervention regarding internal affairs, allowing them to maintain power without accountability to international human rights standards. They actively promote this reading in international forums.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% These populations bear the direct costs of this constraint, as it denies them any legitimate external recourse or protection against state-sponsored repression, violence, or human rights abuses. Their options are limited to internal resistance or desperate flight.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, immediate, trapped, national).

% These states actively champion and enforce the sovereignty maximalist reading, using it to legitimize their own internal policies and to resist any international efforts to impose conditionalities or humanitarian interventions. They shape the discourse in international bodies.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, states_seeking_unfettered_internal_control, agenda_setter,
    institutional, generational, mobile, global).

% These advocates find their efforts to protect vulnerable populations severely hampered by this reading, which delegitimizes their calls for intervention or accountability. They face an uphill battle against the entrenched principle of non-interference.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, payer,
    moderate, generational, constrained, global).

% These organizations struggle to deliver aid or protect civilians in conflict zones when host states invoke absolute sovereignty to deny access or reject assistance, often viewing their efforts as interference. Their operational capacity is directly suppressed.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_organizations, payer,
    organized, biographical, constrained, global).

% Proponents of a more expansive view of RBIO norms, they are structurally excluded from the maximalist framing's definition of legitimate international action. Their arguments for conditional sovereignty or humanitarian intervention are dismissed as pretexts for regime change.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutionalists, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates state behavior by establishing a clear, non-negotiable boundary around internal affairs, preventing external interference and ensuring state autonomy in a system of formally equal states.
% TRANSFER_FUNCTION: It transfers absolute authority over internal governance from international norms or external actors to the sovereign state, effectively transferring the right to self-determination from populations to their ruling regimes, and the costs of repression to trapped populations.
% ABSENT_VOICES: Populations suffering under repressive governments are the primary absent voices; they would demand external protection and accountability, but their pleas are silenced by the maximalist interpretation of sovereignty. Liberal institutionalists are also excluded from the framing's definition of legitimate discourse.
% DISAPPEARANCE_RATIONALE: If this maximalist reading vanished, the international system would undergo a profound reordering. The legitimacy of humanitarian intervention would be re-evaluated, authoritarian regimes would lose a key shield, and international human rights law would gain significantly more enforcement teeth, leading to a major shift in state-citizen relations and international accountability.
% FOUNDING_PROBLEM: The problem of external interference in the internal affairs of states, particularly by more powerful states, leading to instability, conflict, and the erosion of national self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Many post-colonial states and non-aligned movements corroborate the historical problem of external interference. However, human rights organizations and liberal institutionalists argue that while the problem of interference is live, the maximalist solution has created a greater problem of internal repression without recourse. The corroboration is thus contested by those who bear the costs.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the cost borne by populations denied protection and the unconstrained power granted to regimes. Suppression (0.92) is very high because this reading actively delegitimizes and suppresses any attempts at external intervention or accountability, effectively trapping victim populations. Theater ratio is low (0.15) as the constraint is actively and functionally enforced to protect state autonomy, with little performative maintenance. The claimed type is 'snare' because the coordination story (preventing external interference) serves as a cover for the extraction of unconstrained power by regimes from their populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this is a legitimate 'rope' that protects national self-determination and prevents neo-colonialism. From the perspective of trapped populations and human rights advocates, it is a 'snare' that enables severe human rights abuses by denying any external recourse. The engine's classification will reflect the latter due to the declared victims and high extractiveness/suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states seeking unfettered internal control are clear beneficiaries (low d) as the constraint directly serves their interests. Populations under repressive governments, human rights advocates, and humanitarian organizations are clear targets (high d) as they bear the costs of non-intervention. Liberal institutionalists are excluded, meaning their directionality is not directly computed but their arguments are actively suppressed by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling pure extraction as coordination by highlighting the beneficiaries (authoritarian regimes) and victims (trapped populations). While it claims to solve the problem of external interference, its persistence in the face of widespread internal repression reveals a mandatrophy where the original mandate (protecting self-determination) has been co-opted to serve extractive ends (protecting unaccountable power).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is the absolute sovereignty of states a higher normative priority than the universal human rights of individuals, or are these principles co-equal and mutually limiting?',
    'A global consensus shift in international law and practice, or a landmark ruling by an international court that redefines the hierarchy of these principles.',
    'If human rights are prioritized, the constraint''s legitimacy collapses, leading to reclassification as a ''snare'' or ''piton'' from all seats. If sovereignty remains absolute, the ''snare'' classification persists for victims, but the ''rope'' claim gains more internal coherence for beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'The fundamental normative conflict between state sovereignty and individual human rights.').

omega_variable(
    pretext_for_regime_change_empirical_basis,
    'To what extent are ''humanitarian interventions'' empirically pretexts for regime change, as claimed by this reading, versus genuine efforts to protect populations?',
    'Systematic, independent empirical analysis of all post-Cold War interventions labeled ''humanitarian,'' assessing their stated goals versus actual outcomes and long-term impacts on governance.',
    'Strong empirical evidence of consistent pretextual use would reinforce this reading''s ''snare'' classification and its claims of external interference. Weak or mixed evidence would challenge the reading''s factual basis, potentially weakening its suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_for_regime_change_empirical_basis, empirical, 'Empirical validity of the ''humanitarian pretext'' claim.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the RBIO norm complex, or a strategic misinterpretation designed to shield specific actors?',
    'Analysis of the historical evolution of international law and state practice, comparing this reading''s claims against foundational texts and the intent of multilateral agreements. This is a conceptual omega, but its resolution would be informed by historical-empirical work.',
    'If a strategic misinterpretation, the constraint''s legitimacy would be further undermined, reinforcing its ''snare'' classification and increasing its measured extractiveness. If a genuine, albeit contested, reading, its persistence is more understandable as a ''tangled_rope'' for some actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether this reading is a legitimate interpretation or a strategic distortion of the RBIO norm complex.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1965, 0.78).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1985, 0.82).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.86).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.93).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the RBIO practice-norm complex. It is linked to its sibling readings and other related international law constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
