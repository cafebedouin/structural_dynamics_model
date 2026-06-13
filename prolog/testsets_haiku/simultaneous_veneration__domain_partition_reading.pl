% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain Partition: Kami and Buddhas as Functionally Distinct Entities
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   In the domain-partition reading, kami and buddhas are functionally
 *   distinct entities governing separate, non-overlapping domains: kami
 *   address this-worldly prosperity (agriculture, commerce, military success,
 *   health, fertility), while buddhas and bodhisattvas address salvation and
 *   spiritual liberation. This reading frames simultaneous veneration not as
 *   metaphysical confusion but as rational specialization — practitioners
 *   petition the appropriate entity for the appropriate welfare. The
 *   constraint operates as a rope (genuine coordination, low extraction,
 *   minimal suppression) because both priesthoods benefit from clarity of
 *   domains and neither priesthood extracts from the arrangement; they
 *   cooperate by staying in separate functional spaces. This reading was
 *   operative from approximately the 9th–10th centuries (when Buddhist
 *   philosophical defenses appeared) through 1868 (when the Meiji state
 *   forcibly separated Shinto and Buddhism). The claim/metric independence is
 *   intentional: the constraint is CLAIMED as rope; the metrics show very low
 *   extractiveness (0.12 at interval end) and low theater (0.15) — authentic
 *   coordination with minimal performative overhead.
 *
 * KEY AGENTS:
 *   - practitioners_seeking_worldly_benefit — petition kami for material welfare; benefit from clear functional assignment without metaphysical contradiction
 *   - practitioners_seeking_salvation — petition buddhas for spiritual salvation; benefit from parallel clear functional assignment
 *   - shinto_priesthood — maintain kami shrines, articulate and preserve the domain-partition framework, no extraction
 *   - buddhist_priesthood — maintain temples, articulate the domain-partition as coherent framework, coordinate with kami-domain assignment
 *   - meiji_state — external observer that destroys the constraint by imposing forced institutional separation (shinbutsu bunri, 1871+)
 *   - contemporary_practitioners — inherit the constraint's practical operation post-Meiji but without its original articulated coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain Partition: Kami and Buddhas as Functionally Distinct Entities").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '5c6769f3-ceac-4178-aa0d-9ca06f6efdf8').
narrative_ontology:cs_kernel_codification('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', distributed).
narrative_ontology:cs_authority_grounding('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', practice).
narrative_ontology:cs_interpretation_layer_present('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8').
narrative_ontology:cs_reading_relation('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', foundational, functional_domain_adequacy).
narrative_ontology:cs_axiom_status(functional_domain_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', functional_domain_adequacy, instrumental).
narrative_ontology:cs_axiom('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', secondary, non_metaphysical_coherence).
narrative_ontology:cs_axiom_status(non_metaphysical_coherence, holdable).
narrative_ontology:cs_axiom_grounding('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', non_metaphysical_coherence, conventional).
narrative_ontology:cs_reference_frame('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', complementary_domain_specialization).
narrative_ontology:cs_drift_state('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', edo_period_philosophical_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5c6769f3-ceac-4178-aa0d-9ca06f6efdf8', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_worldly_benefit).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_salvation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, contemporary_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approach kami shrines for this-worldly prosperity: agricultural fertility, commercial success, health, childbirth, military protection. Benefit from a clear, functional distinction that assigns kami as the appropriate entity for petitions about material welfare. The domain partition clarifies the proper addressee for each type of request.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners_seeking_worldly_benefit, beneficiary,
    organized, biographical, mobile, national).

% Approach buddhist temples and priests for salvation, ritual purification, and afterlife welfare. Benefit from a parallel, equally clear functional distinction that assigns buddhas and bodhisattvas as the appropriate entities for spiritual liberation and post-mortem salvation. The domain partition validates the dual system without requiring metaphysical coherence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners_seeking_salvation, beneficiary,
    organized, generational, mobile, national).

% Maintain the integrity of kami shrines and the functional-domain framework distinguishing kami from buddhas. They articulate and defend the principle that kami are not bodhisattvas in disguise but rather distinct entities with distinct domains. They do not extract from this arrangement; they administer and preserve it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Maintain the integrity of buddhist temples and articulate the domain-partition reading as the coherent framework for simultaneous veneration. They present buddhas and bodhisattvas as superior in soteriological authority but coordinate with the kami-domain assignment for this-worldly matters. They do not extract from this arrangement; they preserve it alongside Shinto institutions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% From the early 1870s onward, imposed State Shinto and forced separation (shinbutsu bunri), invalidating the domain-partition reading as a live framework. Reassigned kami and buddhas to mutually exclusive institutional spaces. This historical intervention destroys the constraint by mandate.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, meiji_state, observer,
    institutional, generational, analytical, national).

% In the post-Meiji era, continue dual veneration at kami shrines and buddhist temples, often without explicit appeal to the domain-partition framework. They inherit the constraint's practical operation (visiting shrines and temples for different purposes) but operate without its original articulated coherence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, contemporary_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The domain partition solves a genuine coordination problem: practitioners need to petition entities that address different aspects of welfare (material prosperity vs. spiritual salvation). Assigning kami to this-worldly domains and buddhas to soteriological domains creates a clear, non-exclusive framework that lets practitioners direct requests to the appropriate addressee without resolving metaphysical questions about the entities' underlying nature. This-worldly prosperity and afterlife salvation are structurally distinct problems requiring structurally distinct solutions; the partition coordinates behavior by clarifying which entity handles which domain.
% TRANSFER_FUNCTION: The constraint transfers authority and functional assignment: kami receive this-worldly petitions; buddhas receive salvation petitions. The transfer is not material but jurisdictional — it routes petitions to the appropriate institutional priest (Shinto for prosperity kami, buddhist for salvation buddhas) and legitimates dual-institutional participation without requiring unified metaphysical doctrine. Practitioners transfer their trust simultaneously to two priesthoods without extractive cost to either priesthood.
% ABSENT_VOICES: Meiji state authorities (explicitly silenced after 1871 through forced institutional separation), contemporary academic philosophers who reject domain partition as incoherent (excluded from the functioning framework, present in scholarly debate), metaphysical purists who demand unified ontology (not present in early-modern folk practice; became vocal in Edo-period philosophical discourse).
% DISAPPEARANCE_RATIONALE: If the domain-partition framework vanished, practitioners would face an undecidable coherence problem: how can the same person sincerely petition two entities with incompatible ontological status? The Meiji solution was to make them institutionally exclusive; pre-Meiji, the domain partition allowed them to be venerated simultaneously without the contradiction forcing institutional separation. Without the framework, the folk practice either collapses into confusion or retreats into purely functional/pragmatic dual participation stripped of any coherence claim.
% FOUNDING_PROBLEM: Early practitioners needed to petition for both this-worldly welfare (harvest, safe childbirth, commercial profit, military victory) and soteriological salvation (liberation from cycle of rebirth, purification of karma, bodhisattva protection). Buddhism arrived in Japan (~6th century) without abolishing kami veneration; the founding problem was: how can one system of beings (kami) coexist with another (buddhas and bodhisattvas) when Buddhist doctrine claims universal salvational authority? The domain-partition reading offers a solution: they are not competitors but complementary specialists.
% FOUNDING_PROBLEM_CORROBORATION: Pre-Meiji temple and shrine records document dual veneration without requiring unified ontology (e.g., Shinto priests and Buddhist priests serving the same communities, worshippers visiting both without internal conflict accounts). Buddhist philosophical texts (from ~9th century onward, e.g., Tendai commentaries) explicitly articulated the domain-partition reading to justify simultaneous veneration. Edo-period folk practice and shrine/temple inscriptions confirm the framework was operative and widely accepted. NO corroboration exists from the Meiji state (which repudiated it) or from contemporary academic philosophy; corroboration comes from historical practice itself and from Buddhist and Shinto priesthood defenses of the arrangement in pre-Meiji texts.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because neither priesthood collects rents from the dual-system arrangement; they cooperate to administer it. Both gain institutional legitimacy from the framework's coherence, but that is not extraction — it is the ordinary benefit of a functioning institutional role. Theater is low (0.15) because the domain partition is the actual mechanism: it genuinely solves the coordination problem by clarifying appropriate addressees. The slight theater that exists reflects rhetorical emphasis on coherence over time (Edo-period texts increasingly emphasize the 'brilliance' of the system, suggesting some performative reinforcement), but the core function remains real and functional. Suppression is minimal (0.08) because the constraint requires no coercive enforcement; practitioners naturally visit both shrine and temple without resistance, and the priesthoods naturally maintain separate domains. The measurement series show modest drift: extractiveness and suppression both rise slightly from 1000–1750 (as philosophical articulations of the framework become more elaborate and institutional boundaries tighten), then stabilize at 1750–1868 as the system reaches equilibrium. The small rise in theater reflects increasing doctrinal elaboration (Edo-period Buddhist and Shinto texts spend more effort defending the framework against skeptics), not functional degradation. All metrics drop or hold steady because the constraint operates without meaningful opposition until the Meiji forcible intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioners' seats, the constraint is purely beneficial coordination: clear, functional, and low-friction. From both priesthoods' seats, the constraint is equally non-extractive: they administer separate domains and benefit from the framework's stability without extracting from practitioners. There is no perspectival gap because no seat bears costs while another collects rents; this is genuine Rope all the way through. The engine should compute the same type from every seat because the structural data (no named victims, beneficiaries only, no active enforcement, low metrics) support a non-extractive reading uniformly.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners are beneficiaries (they receive functional clarity and can petition appropriate entities without contradiction) with d near 0.3–0.4 (they benefit but do not bear costs and have exit options — they could visit only shrines or only temples). Both priesthoods are beneficiaries/agenda-setters (they maintain the framework, articulate it, and benefit from its stability) with d near 0.2–0.3 (they bear administrative costs of maintaining separate domains but do not extract from practitioners). The Meiji state is an external observer (not a stakeholder in the pre-Meiji system, only its destroyer post-1868). The directionality derivation should show all seats as near-beneficiary or symmetric because the structure is non-extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile dual veneration of kami and buddhas) is emphatically LIVE throughout the interval 1000–1868. The domain-partition reading provides a continuous, philosophically articulated answer that sustains the arrangement without calling it into question. No mandatrophy condition exists during this interval: the framework is defended by Buddhist and Shinto priesthoods, accepted by practitioners, and coherent within its own epistemic premises. Mandatrophy ARRIVES with the Meiji state's forcible separation (shinbutsu bunri, 1871), which declares the domain-partition reading incoherent by fiat and imposes institutional separation. That is an external destruction, not internal mandatrophy. The constraint's own logic never becomes a burden to itself; it fails only when external political authority repudiates it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_ontological_domains,
    'Is the domain partition (this-worldly kami vs. soteriological buddha) a functionally adequate framework for simultaneous veneration, or does it require ontological commitments (are kami and buddhas metaphysically distinct or identical)?',
    'Examination of Edo-period philosophical texts to determine whether domain-partition defenses ever invoked metaphysical identity (honji-suijaku) or remained purely functional. If purely functional, the framework does not require resolved metaphysics. If phonetically hybrid, the framework bridges functional and ontological domains.',
    'If purely functional, the domain partition is a robust coordination mechanism independent of metaphysical disputes. If ontologically hybrid, the framework depends on a specific metaphysical reading (honji-suijaku), which makes it less stable when metaphysical commitment shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_ontological_domains, conceptual, 'Whether domain partition stands independently of ontological claims about kami/buddha identity.').

omega_variable(
    priesthood_cooperation_sustainability,
    'Did Shinto and Buddhist priesthoods genuinely cooperate in maintaining the domain-partition framework, or did they tolerate it while pursuing separate interests (institutional survival, resource competition)?',
    'Analysis of joint shrine-temple administrative records, financial accounts, and correspondence between Shinto and Buddhist authorities. Evidence of joint decision-making and resource-sharing would support genuine cooperation; evidence of parallel administration with occasional friction would support tolerance rather than cooperation.',
    'If genuinely cooperative, the rope classification holds and extraction is minimal. If merely tolerant, the framework might be better modeled as two parallel constraints (kami-domain rope + buddha-domain rope) with minimal interaction, rather than a single coordinated system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priesthood_cooperation_sustainability, empirical, 'Whether priesthood cooperation was structural or merely contingent on absence of conflict.').

omega_variable(
    kernel_reading_contestation_origin,
    'When did the three readings of the simultaneous_veneration kernel (domain partition, ontological fusion, pragmatic incoherence) become philosophically explicit as competing interpretations?',
    'Textual analysis of Buddhist and Shinto philosophical works across centuries (Heian through Edo). Dating of first explicit defense of domain partition, first honji-suijaku articulation, and first skeptical challenge.',
    'If domain partition was the earliest explicit reading, it may reflect the original coherence framework. If ontological fusion or pragmatic incoherence readings predate it, the domain partition may be a later rationalization. The earliest reading is not necessarily the ''true'' reading, but it anchors the genealogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_origin, empirical, 'Genealogy of the three readings: which emerged first, which as response, which as critical challenge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 1000, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement_basis(simu_tr_t1000, projected).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.12).
narrative_ontology:measurement_basis(simu_tr_t1200, projected).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__domain_partition_reading, theater_ratio, 1400, 0.13).
narrative_ontology:measurement_basis(simu_tr_t1400, projected).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__domain_partition_reading, theater_ratio, 1600, 0.14).
narrative_ontology:measurement_basis(simu_tr_t1600, observed).
narrative_ontology:measurement(simu_tr_t1750, simultaneous_veneration__domain_partition_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement_basis(simu_tr_t1750, observed).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement_basis(simu_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement_basis(simu_be_t1000, projected).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.1).
narrative_ontology:measurement_basis(simu_be_t1200, projected).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1400, 0.11).
narrative_ontology:measurement_basis(simu_be_t1400, projected).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1600, 0.12).
narrative_ontology:measurement_basis(simu_be_t1600, observed).
narrative_ontology:measurement(simu_be_t1750, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1750, 0.13).
narrative_ontology:measurement_basis(simu_be_t1750, observed).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement_basis(simu_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.04).
narrative_ontology:measurement_basis(simu_su_t1000, projected).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement_basis(simu_su_t1200, projected).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1400, 0.06).
narrative_ontology:measurement_basis(simu_su_t1400, projected).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1600, 0.07).
narrative_ontology:measurement_basis(simu_su_t1600, observed).
narrative_ontology:measurement(simu_su_t1750, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1750, 0.08).
narrative_ontology:measurement_basis(simu_su_t1750, observed).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1868, 0.08).
narrative_ontology:measurement_basis(simu_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel permits three structurally distinct constraint readings with different ε values and classifications. This file instantiates domain_partition_reading (low extraction, rope); sibling files instantiate ontological_fusion_reading and pragmatic_incoherence_reading with different metrics and structural assumptions. All three readings share the same kernel (the fact of dual veneration) but decompose it into different constraint models based on different interpretive premises about whether the domains are functionally independent, ontologically identical, or pragmatically incoherent. The three stories are linked by the kernel and must be read together to understand the full structure of the simultaneous-veneration contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
