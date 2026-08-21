% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)
 *   domain: Political History/Settler Colonialism/Nationalism Studies
 *
 * SUMMARY:
 *   This constraint represents the Revisionist Zionist reading of the Jewish
 *   territorial claim, specifically its maximalist demand for sovereignty
 *   over both banks of the Jordan River and the doctrine of the 'Iron Wall' –
 *   compelling Arab acceptance through military force rather than seeking
 *   consent. It is a highly coercive and extractive constraint, actively
 *   enforced to achieve and maintain territorial control and suppress
 *   resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.92).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "Political History/Settler Colonialism/Nationalism Studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '1e99742e-264a-442f-98ff-e6733fe24c84').
narrative_ontology:cs_kernel_codification('1e99742e-264a-442f-98ff-e6733fe24c84', formalized).
narrative_ontology:cs_authority_grounding('1e99742e-264a-442f-98ff-e6733fe24c84', extraction).
narrative_ontology:cs_interpretation_layer_present('1e99742e-264a-442f-98ff-e6733fe24c84').
narrative_ontology:cs_reading_relation('1e99742e-264a-442f-98ff-e6733fe24c84', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('1e99742e-264a-442f-98ff-e6733fe24c84', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('1e99742e-264a-442f-98ff-e6733fe24c84', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('1e99742e-264a-442f-98ff-e6733fe24c84', foundational, jewish_sovereignty_over_greater_israel_non_negotiable).
narrative_ontology:cs_axiom_status(jewish_sovereignty_over_greater_israel_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('1e99742e-264a-442f-98ff-e6733fe24c84', jewish_sovereignty_over_greater_israel_non_negotiable, deontological).
narrative_ontology:cs_axiom('1e99742e-264a-442f-98ff-e6733fe24c84', foundational, military_force_as_primary_means_to_compel_acceptance).
narrative_ontology:cs_axiom_status(military_force_as_primary_means_to_compel_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('1e99742e-264a-442f-98ff-e6733fe24c84', military_force_as_primary_means_to_compel_acceptance, instrumental).
narrative_ontology:cs_reference_frame('1e99742e-264a-442f-98ff-e6733fe24c84', historic_jewish_sovereignty_over_eretz_israel).
narrative_ontology:cs_drift_state('1e99742e-264a-442f-98ff-e6733fe24c84', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1e99742e-264a-442f-98ff-e6733fe24c84', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of the maximalist claim, advocating for immediate Jewish sovereignty over both banks of the Jordan River, achieved through military force and without requiring Arab consent. They actively set the political and military agenda to realize this vision.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Those who directly benefit from the expansion of Jewish control over territory, often settling in areas claimed under the maximalist vision. Their presence reinforces the claim and is protected by the military force it mandates.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers, beneficiary,
    powerful, generational, constrained, regional).

% The indigenous population whose land, sovereignty, and self-determination are directly targeted by the maximalist claim. They face dispossession, military occupation, and suppression of their national aspirations, with no recognized right to consent or resist.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% States bordering the claimed territory that face geopolitical instability, refugee flows, and military confrontation as a direct consequence of the maximalist claim and its enforcement. Their security and sovereignty are undermined.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, payer,
    organized, generational, constrained, regional).

% International bodies and states that observe, condemn, or occasionally attempt to mediate the conflict arising from the claim. Their ability to influence the constraint is limited by the proponents' rejection of external arbitration and reliance on force.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Minimal coordination function; primarily serves to unify the Revisionist Zionist movement around a clear, non-negotiable territorial and political objective, and to direct resources towards its military and settlement-based enforcement.
% TRANSFER_FUNCTION: Transfers land, sovereignty, and resources from Palestinian Arabs and potentially neighboring Arab states to Jewish control, compelled by military force and political assertion.
% ABSENT_VOICES: The voices of Palestinian Arabs are explicitly rejected as a prerequisite for Jewish sovereignty; their consent is deemed irrelevant. Any international or regional actors advocating for a negotiated settlement based on mutual recognition are also effectively excluded from the operational logic of the claim.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and its enforcement vanished overnight, the entire political, military, and settlement infrastructure built to sustain it would collapse. This would fundamentally reorder the geopolitical landscape of the Middle East, potentially leading to the emergence of a Palestinian state, a redefinition of borders, and a shift in regional power dynamics.
% FOUNDING_PROBLEM: The perceived existential threat to the Jewish people, the historical and religious claim to the land of Eretz Israel (including both banks of the Jordan), and the need for a secure, sovereign Jewish homeland free from external threats and internal dissent.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is primarily attested by the Revisionist Zionist movement itself and its adherents, who view the historical context of antisemitism and the perceived ongoing threats as validating the claim's necessity. External corroboration for the maximalist territorial scope and the 'Iron Wall' doctrine as the *only* solution is contested by most international bodies and many other Zionist factions.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the claim involves the unilateral seizure of land and sovereignty from another population. Suppression is extremely high (0.95) due to the explicit reliance on overwhelming military force to overcome any opposition and the rejection of Arab consent. Theater ratio is very low (0.05) because the military and political actions taken to enforce this claim are direct, functional, and intended to achieve concrete territorial and political objectives, not merely to perform. Accessibility collapse is high (0.90) as the goal is to eliminate any viable alternative for Arab sovereignty in the claimed territory. Resistance is high (0.88) because the claim inherently generates strong opposition from those it targets, necessitating continuous enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Revisionist Zionist movement, this claim is a necessary, historically justified, and existential imperative for Jewish survival and sovereignty. From the perspective of Palestinian Arabs, it is a brutal act of settler-colonial dispossession and military occupation. The engine's classification as a Snare reflects the latter, emphasizing the coercive and extractive nature of the constraint's operation, regardless of the proponents' justifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist Zionist movement and Jewish settlers are clear beneficiaries, gaining land, security, and political power. Palestinian Arabs and neighboring Arab states are direct victims, suffering dispossession, military control, and geopolitical destabilization. The international community acts as an observer, often condemning but largely unable to alter the fundamental dynamics of the constraint's enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_viability_of_force,
    'Can a maximalist territorial claim, enforced solely by military power and without consent, achieve stable, long-term acceptance or legitimacy?',
    'Historical analysis of similar settler-colonial projects and their ultimate outcomes, or a shift in regional power dynamics that alters the balance of force.',
    'If force proves unsustainable for long-term acceptance, the constraint''s effective suppression and extractiveness would eventually collapse, leading to a reclassification towards a Piton or even a failed constraint. If it achieves a form of ''acceptance'' through generations of sustained force, it might stabilize as a highly extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_viability_of_force, empirical, 'The sustainability of a force-based territorial claim.').

omega_variable(
    historical_vs_political_justification,
    'To what extent is the claim grounded in an immutable historical/religious right versus a contingent political strategy for national security?',
    'Analysis of internal movement documents and public discourse over time, particularly how justifications shift in response to changing geopolitical realities or internal challenges.',
    'If primarily a political strategy, it is more amenable to re-evaluation and negotiation based on changing circumstances. If framed as an immutable right, it reinforces the non-negotiable aspect, making the constraint more rigid and resistant to change, potentially increasing its perceived ''mountain-like'' quality for adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vs_political_justification, conceptual, 'The grounding of the claim in immutable right vs. political strategy.').

omega_variable(
    degree_of_arab_acceptance,
    'What constitutes ''acceptance'' by Palestinian Arabs and neighboring states, and has the ''Iron Wall'' doctrine achieved it?',
    'Empirical assessment of Palestinian political agency, public opinion, and the cessation of active resistance, as opposed to mere acquiescence under duress.',
    'If ''acceptance'' is merely the absence of effective resistance due to overwhelming force, the constraint remains a Snare. If genuine, uncoerced acceptance were to emerge, the constraint might transition towards a Tangled Rope, implying a (highly asymmetric) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_arab_acceptance, empirical, 'Defining and measuring ''Arab acceptance'' of the claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1967, 0.06).
narrative_ontology:measurement(jewi_tr_t1993, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(jewi_tr_t2024, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.88).
narrative_ontology:measurement(jewi_be_t1993, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1993, 0.9).
narrative_ontology:measurement(jewi_be_t2024, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.75).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewi_su_t1993, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1993, 0.93).
narrative_ontology:measurement(jewi_su_t2024, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_resistance_movements).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_israeli_conflict).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, international_law_on_occupation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
