% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Divine Legitimacy via Folk Syncretistic Ritual (Ancient Egypt)
 *   domain: religious_studies/ancient_history/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the grounding of divine legitimacy in ancient
 *   Egyptian village and household ritual practices, which pragmatically
 *   incorporate multiple deities. This 'folk syncretistic' reading emphasizes
 *   decentralized authority, resilience to top-down religious reforms, and a
 *   beneficiary structure centered on local community leaders and household
 *   heads. It stands in contrast to the more centralized, state-sponsored
 *   polytheistic (Amun) or monotheistic (Atenist) readings of divine
 *   authority. The constraint is claimed as a Mountain due to its deep
 *   cultural embedding and perceived naturalness within the folk worldview,
 *   with low extractiveness and suppression reflecting its organic,
 *   self-organizing nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.2).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, mountain).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Divine Legitimacy via Folk Syncretistic Ritual (Ancient Egypt)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious_studies/ancient_history/political_economy_of_belief_systems").

domain_priors:emerges_naturally(divine_legitimacy_substrate__folk_syncretistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '23444599-c67d-404d-a7f7-c8ec6ba8cbc8').
narrative_ontology:cs_kernel_codification('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', implicit).
narrative_ontology:cs_authority_grounding('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', practice).
narrative_ontology:cs_interpretation_layer_present('23444599-c67d-404d-a7f7-c8ec6ba8cbc8').
narrative_ontology:cs_reading_relation('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', foundational, divine_presence_is_local_and_pragmatic).
narrative_ontology:cs_axiom_status(divine_presence_is_local_and_pragmatic, holdable).
narrative_ontology:cs_axiom_grounding('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', divine_presence_is_local_and_pragmatic, conventional).
narrative_ontology:cs_axiom('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', foundational, ritual_efficacy_is_community_derived).
narrative_ontology:cs_axiom_status(ritual_efficacy_is_community_derived, holdable).
narrative_ontology:cs_axiom_grounding('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', ritual_efficacy_is_community_derived, conventional).
narrative_ontology:cs_reference_frame('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', ancestral_village_tradition).
narrative_ontology:cs_drift_state('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', pharaonic_state_cult_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('23444599-c67d-404d-a7f7-c8ec6ba8cbc8', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, common_villagers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer local rituals, interpret omens, and mediate disputes, deriving their authority from their perceived connection to the divine through established folk practices. They benefit from the social cohesion and deference this system provides.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders, agenda_setter,
    moderate, biographical, identity_locked, local).

% Perform daily household rituals, invoking various deities for pragmatic needs (fertility, harvest, protection). They benefit from the sense of order, divine favor, and community identity these practices reinforce, with minimal direct cost.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, beneficiary,
    moderate, biographical, identity_locked, local).

% Participate in communal and household rituals, contributing labor or small offerings. While they are beneficiaries of the social cohesion and perceived divine protection, they bear the diffuse costs of maintaining the ritual system and adhering to its norms. Exit is unthinkable due to social and spiritual ostracization.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, common_villagers, payer,
    powerless, immediate, identity_locked, local).

% Theoretically the supreme divine intermediary, but in this reading, his direct influence on daily village-level divine legitimacy is minimal. He is a distant, elite figure whose grand rituals are distinct from the pragmatic, syncretistic practices of the common folk. His attempts to impose top-down religious reforms are often met with passive resistance or syncretism.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, excluded,
    institutional, generational, analytical, national).

% The official interpreters of state religion, serving major temples. In this reading, their elaborate theological systems and temple rituals are largely separate from the folk practices that ground divine legitimacy in villages. They are seen as distant elites, and their attempts to standardize belief are often ignored or absorbed into existing local traditions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, priesthood, excluded,
    organized, generational, constrained, national).

% An analytical observer representing the perspective of the Amun-centric polytheistic state religion, which views folk practices as unsystematic or even heterodox, but ultimately subordinate to the grand temple cults. This observer would note the resilience of folk practices but interpret them through the lens of official theology.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, resilient framework for understanding and interacting with the divine, ensuring social cohesion and a sense of cosmic order at the local level, adapting to diverse local needs and beliefs without requiring central coordination.
% TRANSFER_FUNCTION: Transfers a sense of divine favor, protection, and social legitimacy to individuals and communities through ritual participation and adherence to local norms. It also transfers deference and social capital to village elders and household heads.
% ABSENT_VOICES: The official state priesthood and the pharaoh, who would assert a more centralized, standardized, and hierarchical model of divine legitimacy. Their voices are absent from the folk discourse, which operates largely independently of their grand theological pronouncements.
% DISAPPEARANCE_RATIONALE: If this folk system of divine legitimacy vanished, village social structures would lose their primary grounding, local authority would collapse, and communities would struggle to find meaning and order. The state religion would be unable to fill the void at the local level, leading to widespread social and spiritual disarray.
% FOUNDING_PROBLEM: How to maintain social cohesion, moral order, and a sense of divine presence in diverse, geographically dispersed communities with limited central authority, while accommodating local traditions and pragmatic needs.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence of diverse local cults, ethnographic parallels from other traditional societies, and the historical record of resistance to top-down religious reforms all corroborate that this problem was and remains live for the folk. The pharaoh and priesthood, from their elite perspective, would likely deny the problem's salience, asserting their own system as universally effective.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, ExtMetricName, E),
    domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(divine_legitimacy_substrate__folk_syncretistic_reading),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the system primarily provides social and spiritual benefits with minimal direct material cost or coercive overhead. Suppression is low (0.2) as adherence is largely voluntary and culturally ingrained, rather than enforced by a central authority. Theater ratio is low (0.1) because the rituals are genuinely believed to be efficacious and serve direct community needs. Accessibility collapse is high (0.85) and resistance low (0.05) because for the common villager, this system is the 'natural' way to interact with the divine; alternatives are culturally unthinkable or practically inaccessible. The temporal measurements show relative stability, reflecting the enduring nature of folk traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the pharaoh or state priesthood, this folk system might appear chaotic or heterodox, lacking proper centralized control. However, from the folk perspective, it is the natural, effective way to engage with the divine, providing tangible benefits at the local level. The engine's classification of this as a Mountain reflects its deep embedding and perceived naturalness within the folk worldview, despite its low formal status in the state religion.
 *
 * DIRECTIONALITY LOGIC:
 *   Village elders and household heads are beneficiaries and agenda-setters, deriving social capital and authority from their role in mediating divine legitimacy. Common villagers are diffuse payers, bearing the costs of ritual maintenance and adherence, but also primary beneficiaries of the social cohesion and perceived divine protection. The pharaoh and priesthood are largely excluded from this local system, their grand narratives having limited direct impact on daily folk practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    folk_vs_state_legitimacy_overlap,
    'To what extent did the folk syncretistic system of divine legitimacy genuinely operate independently of the state-sponsored Amun cult, or was it always implicitly subordinate to it?',
    'Further archaeological and textual analysis of local cult sites, comparing iconography and ritual practice with official temple records, and examining periods of state religious reform for evidence of active resistance vs. passive absorption.',
    'If largely independent, this reading''s Mountain classification is robust. If implicitly subordinate, the state cult''s extractiveness and suppression might be higher than currently estimated, as it would be leveraging the folk system without direct enforcement, potentially reclassifying this folk reading as a Rope or even a Tangled Rope from the state''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_vs_state_legitimacy_overlap, empirical, 'Ambiguity regarding the true autonomy of folk religious practice from state religious authority.').

omega_variable(
    beneficiary_diffusion_vs_concentration,
    'Are the benefits of this folk system truly diffuse among villagers, or do village elders and household heads capture a more concentrated, unacknowledged form of social or material extraction?',
    'Detailed micro-historical studies of resource allocation, social status, and dispute resolution within ancient Egyptian villages, looking for evidence of disproportionate gains by local leaders that are masked by the ''naturalness'' of the ritual system.',
    'If benefits are more concentrated, the extractiveness of this constraint would be higher, and the ''agenda_setter'' role of elders would carry more weight, potentially shifting the classification towards a Tangled Rope or even a Snare from the perspective of common villagers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_diffusion_vs_concentration, empirical, 'Uncertainty about the true distribution of benefits within the folk religious system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(divi_tr_t75, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(divi_be_t75, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(divi_su_t50, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(divi_su_t75, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 75, 0.22).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
