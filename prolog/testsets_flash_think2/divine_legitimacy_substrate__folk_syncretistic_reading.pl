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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Divine Legitimacy via Folk Syncretistic Practice
 *   domain: religious_studies/ancient_history/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the diffuse, pragmatic, and syncretistic
 *   religious practices at the household and village level in ancient
 *   societies, which served as a fundamental substrate for divine legitimacy.
 *   Unlike centralized state cults, this reading emphasizes the bottom-up,
 *   emergent nature of belief and ritual, incorporating multiple deities as
 *   needed for practical concerns. It is resistant to top-down revision and
 *   operates largely outside the direct control of pharaohs or official
 *   priesthoods. The claimed type is 'mountain' because it functions as an
 *   emergent, deeply ingrained cultural reality, almost a natural law of
 *   social organization at this level, despite being a human construct. The
 *   presence of beneficiaries on a mountain claim is intentional, triggering
 *   False Summit Mountain (FSM) detection to analyze the tension between its
 *   'natural' appearance and its functional benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.35).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.25).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, mountain).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Divine Legitimacy via Folk Syncretistic Practice").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious_studies/ancient_history/political_economy_of_belief_systems").

domain_priors:emerges_naturally(divine_legitimacy_substrate__folk_syncretistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, 'b00b34ef-ac68-4a9d-8458-e3e03b7b3b00').
narrative_ontology:cs_kernel_codification('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', implicit).
narrative_ontology:cs_authority_grounding('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', practice).
narrative_ontology:cs_reading_relation('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', foundational, divine_presence_is_immanent_and_local).
narrative_ontology:cs_axiom_status(divine_presence_is_immanent_and_local, holdable).
narrative_ontology:cs_axiom_grounding('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', divine_presence_is_immanent_and_local, theological).
narrative_ontology:cs_axiom('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', foundational, ritual_efficacy_is_pragmatic_and_adaptive).
narrative_ontology:cs_axiom_status(ritual_efficacy_is_pragmatic_and_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', ritual_efficacy_is_pragmatic_and_adaptive, conventional).
narrative_ontology:cs_reference_frame('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', ancestral_practice_continuity).
narrative_ontology:cs_drift_state('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', pharaonic_centralization_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b00b34ef-ac68-4a9d-8458-e3e03b7b3b00', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the social cohesion, shared meaning, and perceived divine favor derived from collective ritual practice. They also bear the diffuse costs of time and effort for these rituals. Exiting means social isolation and loss of cultural identity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities, beneficiary,
    moderate, generational, constrained, local).

% Organize and lead household rituals, ensuring the continuity of practice and the well-being of their families. They benefit from the social standing and spiritual security this provides. Their authority within the household is tied to these practices.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, beneficiary).

% As the distant, divine ruler, the pharaoh's legitimacy is often presented as flowing from a different, more centralized source. This folk practice operates largely independently of direct pharaonic control, though it contributes to the broader substrate of divine belief. The pharaoh would prefer a more centralized, controllable form of legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, excluded,
    institutional, civilizational, analytical, national).

% The established priesthood (e.g., of Amun) derives its authority from formal temples and codified rituals. They are largely bypassed by the diffuse, pragmatic folk practice, which they might view as syncretistic or impure. They would prefer a more doctrinally pure and centrally administered religious system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, priesthood, excluded,
    institutional, generational, analytical, national).

% Study the historical and sociological role of folk religion, analyzing its persistence, adaptation, and interaction with state-sponsored cults. They observe the constraint's operation without direct participation or benefit.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides social cohesion, meaning, and a pragmatic framework for addressing life's uncertainties and ensuring community well-being through shared ritual and belief, adapting to local needs and incorporating diverse deities.
% TRANSFER_FUNCTION: Transfers social capital, perceived divine favor, and community stability among participants. It also transfers the burden of ritual maintenance and belief to the household and village level, rather than central institutions.
% ABSENT_VOICES: Centralized religious authorities (like the Amun priesthood or Atenist pharaoh) would object to the diffuse, syncretistic, and pragmatic nature of this practice, arguing for doctrinal purity, centralized control, and a more exclusive focus on specific deities or the pharaoh's divine role.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the daily life, social order, and psychological well-being of village communities would be profoundly disrupted. The primary substrate of divine legitimacy and social cohesion would disappear, leading to widespread anomie, social fragmentation, and a crisis of meaning, forcing a complete reorganization of social and spiritual life.
% FOUNDING_PROBLEM: To provide a stable, accessible, and adaptable framework for understanding and interacting with the divine, ensuring community well-being, social order, and psychological security in a complex, uncertain world, particularly at the local, everyday level.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of traditional societies, historical accounts of folk religion's persistence across various cultures and eras, and the observed resilience of local religious practices even under state-imposed or centralized religious systems, all corroborate the ongoing relevance of these problems for communities.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.35 decreasing to 0.29) because the practice is largely self-organizing and provides direct benefits to participants without significant material transfer to external parties. Suppression is low (0.25 decreasing to 0.2) because its persistence relies on ingrained cultural practice rather than active enforcement, and attempts to suppress it from above are often met with passive resistance or simply ignored. Theater ratio is moderate (0.4) reflecting the inherent performative aspect of ritual, which is nonetheless deeply believed and functional for the community. Accessibility collapse is moderate (0.4) as alternatives exist but are difficult to adopt without disrupting social fabric. Resistance is moderate (0.5) reflecting its resilience against external pressures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of village communities, this practice is an essential, almost natural, part of their existence, providing stability and meaning. From the perspective of the pharaoh or priesthood, it might be seen as an unruly, uncodified, or even 'impure' form of worship that undermines their centralized authority. The engine's classification will highlight this divergence between the 'mountain' claim and the presence of beneficiaries, indicating a potential false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Village communities and household heads are beneficiaries, as they directly gain social cohesion and spiritual security from these practices. The pharaoh and priesthood are 'excluded' from direct participation and control over this specific substrate, though they benefit from the broader system of divine legitimacy it underpins. Their directionality would be analytical or even slightly targeted if they sought to impose their own, more centralized, forms of legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_emergence_vs_social_construct,
    'Is this folk syncretistic practice a truly ''natural'' emergent cultural phenomenon (a Mountain), or a socially constructed system of belief and coordination (a Rope) that benefits its participants?',
    'Comparative anthropological studies of similar emergent religious practices across diverse cultures, focusing on the degree of conscious design versus spontaneous evolution, and the presence of identifiable agents who actively shape or maintain the ''naturalness'' narrative.',
    'If resolved as a social construct, the constraint would reclassify from Mountain to Rope, highlighting the human agency and coordination involved, even if diffuse. This would shift the analysis from ''inevitable reality'' to ''chosen arrangement''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_emergence_vs_social_construct, conceptual, 'Ambiguity between emergent cultural ''naturalness'' and social construction.').

omega_variable(
    diffuse_benefit_vs_indirect_reinforcement,
    'Are the benefits of this folk practice truly diffuse among participants, or does it indirectly reinforce the legitimacy and power of distant elites (pharaoh, priesthood) by maintaining a general belief in divine authority?',
    'Historical analysis of how state-level authorities leveraged or co-opted folk religious sentiment, even without direct control, to bolster their own claims to divine mandate. This would involve tracing the flow of symbolic capital.',
    'If indirect reinforcement of elites is significant, the constraint''s effective extractiveness would be higher, and its classification might shift towards a Tangled Rope, as it would coordinate folk belief while implicitly extracting legitimacy for a separate, more centralized power structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_benefit_vs_indirect_reinforcement, empirical, 'Whether diffuse benefits mask indirect elite reinforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(divi_tr_t200, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 200, 0.39).
narrative_ontology:measurement(divi_tr_t400, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 400, 0.39).
narrative_ontology:measurement(divi_tr_t600, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 600, 0.4).
narrative_ontology:measurement(divi_tr_t800, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 800, 0.41).
narrative_ontology:measurement(divi_tr_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1000, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t200, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 200, 0.33).
narrative_ontology:measurement(divi_be_t400, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 400, 0.32).
narrative_ontology:measurement(divi_be_t600, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 600, 0.31).
narrative_ontology:measurement(divi_be_t800, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 800, 0.3).
narrative_ontology:measurement(divi_be_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1000, 0.29).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(divi_su_t200, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 200, 0.24).
narrative_ontology:measurement(divi_su_t400, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 400, 0.23).
narrative_ontology:measurement(divi_su_t600, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 600, 0.22).
narrative_ontology:measurement(divi_su_t800, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 800, 0.21).
narrative_ontology:measurement(divi_su_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1000, 0.2).


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
