% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Incoherent Shinbutsu Syncretism (State-Enforced Drift Reading)
 *   domain: religious/political/historical
 *
 * SUMMARY:
 *   This constraint instantiates the 'incoherent bundle' reading of the
 *   `shinbutsu_ontological_substrate` kernel, which posits that the
 *   historical syncretism of Shinto and Buddhism in Japan was not based on a
 *   coherent theological or ontological fusion, but rather on accumulated
 *   institutional drift and state enforcement, leading to a bundle of
 *   contradictory beliefs. This contrasts with the `syncretic_fusion_reading`
 *   (which claims ontological unity) and the `domain_partition_reading`
 *   (which claims functional separation). The constraint operates as a snare,
 *   extracting control and conformity from religious practitioners under the
 *   guise of unity, enforced by state power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.8).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.85).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Incoherent Shinbutsu Syncretism (State-Enforced Drift Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/political/historical").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'fddf71fd-de5f-402d-8388-14b4b95ad7ec').
narrative_ontology:cs_kernel_codification('fddf71fd-de5f-402d-8388-14b4b95ad7ec', implicit).
narrative_ontology:cs_authority_grounding('fddf71fd-de5f-402d-8388-14b4b95ad7ec', extraction).
narrative_ontology:cs_interpretation_layer_present('fddf71fd-de5f-402d-8388-14b4b95ad7ec').
narrative_ontology:cs_reading_relation('fddf71fd-de5f-402d-8388-14b4b95ad7ec', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('fddf71fd-de5f-402d-8388-14b4b95ad7ec', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('fddf71fd-de5f-402d-8388-14b4b95ad7ec', foundational, ontological_incoherence_is_structural).
narrative_ontology:cs_axiom_status(ontological_incoherence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('fddf71fd-de5f-402d-8388-14b4b95ad7ec', ontological_incoherence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('fddf71fd-de5f-402d-8388-14b4b95ad7ec', foundational, state_enforcement_drives_syncretism).
narrative_ontology:cs_axiom_status(state_enforcement_drives_syncretism, holdable).
narrative_ontology:cs_axiom_grounding('fddf71fd-de5f-402d-8388-14b4b95ad7ec', state_enforcement_drives_syncretism, empirically_contingent).
narrative_ontology:cs_reference_frame('fddf71fd-de5f-402d-8388-14b4b95ad7ec', pre_state_consolidation_diversity).
narrative_ontology:cs_drift_state('fddf71fd-de5f-402d-8388-14b4b95ad7ec', historical_syncretic_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fddf71fd-de5f-402d-8388-14b4b95ad7ec', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners_of_pure_shinto).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners_of_pure_buddhism).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the enforced fusion by consolidating control over religious institutions and preventing sectarian conflict or dissent that could challenge political authority. They actively enforce policies that promote or mandate syncretic practices.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% While often having to navigate the ontological incoherence, these institutions benefit from state patronage, stability, and the avoidance of direct competition or suppression that might arise from a clear separation. They often perform interpretive work to manage the contradictions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions, beneficiary,
    institutional, generational, constrained, national).

% Bear the burden of having their distinct religious identity and practices subsumed or diluted by state-enforced syncretism. They may experience internal cognitive dissonance or be forced to participate in rituals that contradict their core beliefs, with limited avenues for resistance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners_of_pure_shinto, payer,
    powerless, biographical, constrained, local).

% Similar to pure Shinto practitioners, they are compelled to accept or perform syncretic practices that may contradict their Buddhist doctrines. Their ability to practice a 'pure' form of their religion is constrained by the prevailing institutional and state-backed norms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners_of_pure_buddhism, payer,
    powerless, biographical, constrained, local).

% Experience the social and cultural costs of enforced religious incoherence. Their local traditions may be altered or suppressed, and they may face pressure to conform to state-sanctioned syncretic norms, impacting community identity and cohesion.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_communities, payer,
    moderate, generational, constrained, local).

% Analyze the historical development and structural implications of Shinbutsu syncretism, often highlighting the political and institutional drivers behind its persistence and the resulting ontological incoherence. They provide an external, critical perspective.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, scholars_of_religious_history, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a superficial religious unity across the populace, preventing overt sectarian conflict and consolidating state control over diverse local religious practices and beliefs.
% TRANSFER_FUNCTION: Transfers legitimacy and control over religious institutions and practices from diverse local traditions to state authorities, while imposing contradictory ontological frameworks on practitioners and local communities.
% ABSENT_VOICES: Advocates for distinct, un-fused Shinto or Buddhist traditions, who are marginalized or suppressed by state-enforced syncretism. Also, those who seek genuine theological coherence rather than institutional expediency, whose voices are often drowned out by the dominant narrative of unity.
% DISAPPEARANCE_RATIONALE: If state enforcement of syncretism vanished, distinct Shinto and Buddhist traditions would likely re-emerge more strongly, leading to a re-evaluation of religious sites and practices, and potentially new forms of religious organization and conflict. The state would lose a significant tool for social and political control, and religious identity would undergo a profound reorganization.
% FOUNDING_PROBLEM: The need for the state to consolidate control over diverse local religious practices and beliefs, and to prevent religious dissent or fragmentation from undermining political authority, particularly during periods of national unification or crisis.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and political analyses from outside religious institutions corroborate the state's long-standing interest in religious control. Independent historians and sociologists of religion document the political motivations behind syncretic policies, rather than purely theological ones, supporting the claim that the problem of state control over religion remains relevant.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the cost borne by practitioners forced to reconcile contradictory beliefs and practices, and the state's gain in control. Suppression (0.85) is severe due to active state enforcement and the lack of viable alternatives for expressing distinct religious identities. The theater ratio (0.6) indicates that a significant portion of the 'unity' is performative, masking underlying incoherence and institutional power dynamics rather than genuine theological synthesis. The metrics reflect a long period of state-driven religious policy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authorities, the arrangement is a necessary coordination mechanism for social order and national unity. From the perspective of practitioners, it is an imposed burden that compromises their religious integrity. The engine's classification will highlight this divergence, showing a snare for the payers and a beneficiary position for the state, despite the state's claim of providing a public good.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities are clear beneficiaries, gaining political stability and control. Established religious institutions, while navigating the incoherence, also benefit from state support and a stable, if compromised, position. Practitioners of pure Shinto and Buddhism, along with local communities, are the primary targets, bearing the costs of enforced conformity and the erosion of their distinct traditions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analytical_vs_lived_incoherence,
    'Is the ''ontological incoherence'' primarily an analytical construct of modern scholarship, or is it a lived experience of cognitive dissonance for historical practitioners?',
    'Analysis of primary historical sources (diaries, letters, local religious records) for expressions of confusion, contradiction, or resistance among practitioners, rather than relying solely on official doctrines or scholarly interpretations.',
    'If widely experienced, the constraint''s effective extractiveness and suppression on practitioners are higher than currently measured, as the burden is internalized. If primarily analytical, the constraint is more of a ''theater'' for the state, with less direct impact on individual belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_vs_lived_incoherence, empirical, 'The extent to which ontological incoherence was a lived reality for practitioners.').

omega_variable(
    state_enforcement_vs_cultural_drift,
    'What proportion of the observed syncretism is attributable to active state enforcement, versus organic cultural and religious drift over time?',
    'Comparative historical analysis of periods with strong vs. weak state control over religion, and examination of regions with varying degrees of state presence, to isolate the effect of explicit policy from broader cultural trends.',
    'If state enforcement is the dominant driver, the constraint is more clearly a snare. If organic drift is more significant, the constraint leans more towards a piton (inertial accumulation) or a tangled rope (coordination with diffuse, less intentional extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_vs_cultural_drift, empirical, 'Distinguishing state coercion from natural cultural evolution in syncretism.').

omega_variable(
    kernel_ontological_status_ambiguity,
    'Is the Shinbutsu ontological substrate genuinely incoherent (this reading), or does it possess an underlying unity (syncretic_fusion_reading) or functional partition (domain_partition_reading)?',
    'Further theological and philosophical analysis, combined with archaeological and textual discoveries, to seek evidence of a consistent underlying logic or a clear, universally accepted division of religious labor, which would challenge the ''incoherent bundle'' premise.',
    'If an underlying unity or clear partition is established, this ''incoherent bundle'' reading would be foreclosed, and the constraint would be reclassified according to the structural properties of the alternative reading (e.g., a Rope if genuinely unified coordination, or a Mountain if a natural division).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_ontological_status_ambiguity, conceptual, 'The fundamental nature of Shinbutsu relations: incoherent, unified, or partitioned.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 200, 0.6).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 250, 0.6).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 100, 0.77).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 150, 0.79).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 200, 0.8).
narrative_ontology:measurement(shin_be_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 250, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 100, 0.83).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 150, 0.85).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(shin_su_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 250, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel, focusing on the role of state enforcement and institutional drift in creating an incoherent bundle of religious practices, rather than a unified or partitioned system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
