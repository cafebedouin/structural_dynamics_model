% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration (Pragmatic Incoherence Reading)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic incoherence' reading of
 *   simultaneous veneration in pre-Meiji Japan, where kami and buddhas were
 *   worshipped together despite underlying theological contradictions. This
 *   reading posits that the system was never truly coherent, but rather
 *   sustained by a lack of enforcement pressure that would have forced a
 *   resolution. The Meiji shinbutsu-bunri (separation of kami and buddhas) is
 *   seen not as an arbitrary rupture, but as a revelation of this latent
 *   incoherence, which had been extracting cognitive dissonance from
 *   practitioners for centuries.
 *
 * KEY AGENTS:
 *   - religious_authorities: Agenda-setter/beneficiary (maintained status quo)
 *   - practitioners: Payer/victim (experienced cognitive dissonance)
 *   - local_communities: Beneficiary (maintained traditions)
 *   - theological_scholars: Payer/victim (struggled with reconciliation)
 *   - meiji_government: Agenda-setter (imposed separation, revealing incoherence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.8).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.75).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '04afeb89-5814-47ba-9aff-f6b4d8deba61').
narrative_ontology:cs_kernel_codification('04afeb89-5814-47ba-9aff-f6b4d8deba61', implicit).
narrative_ontology:cs_authority_grounding('04afeb89-5814-47ba-9aff-f6b4d8deba61', practice).
narrative_ontology:cs_interpretation_layer_present('04afeb89-5814-47ba-9aff-f6b4d8deba61').
narrative_ontology:cs_reading_relation('04afeb89-5814-47ba-9aff-f6b4d8deba61', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('04afeb89-5814-47ba-9aff-f6b4d8deba61', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('04afeb89-5814-47ba-9aff-f6b4d8deba61', foundational, inherent_contradiction_in_simultaneous_veneration).
narrative_ontology:cs_axiom_status(inherent_contradiction_in_simultaneous_veneration, holdable).
narrative_ontology:cs_axiom_grounding('04afeb89-5814-47ba-9aff-f6b4d8deba61', inherent_contradiction_in_simultaneous_veneration, empirically_contingent).
narrative_ontology:cs_axiom('04afeb89-5814-47ba-9aff-f6b4d8deba61', foundational, lack_of_enforcement_sustained_incoherence).
narrative_ontology:cs_axiom_status(lack_of_enforcement_sustained_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('04afeb89-5814-47ba-9aff-f6b4d8deba61', lack_of_enforcement_sustained_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('04afeb89-5814-47ba-9aff-f6b4d8deba61', pre_meiji_unresolved_contradiction).
narrative_ontology:cs_drift_state('04afeb89-5814-47ba-9aff-f6b4d8deba61', meiji_shinbutsu_bunri, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('04afeb89-5814-47ba-9aff-f6b4d8deba61', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, local_communities).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, theological_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shinto_shrines).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintained the status quo of simultaneous veneration, benefiting from the lack of enforced theological consistency which allowed diverse practices to flourish under their purview without challenging their authority. They avoided difficult doctrinal choices.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Engaged in simultaneous veneration, often holding contradictory beliefs without explicit resolution. They bore the cognitive load and internal conflict of this incoherence, but were identity-locked into their religious and community practices.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, practitioners, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the social cohesion and continuity of traditional practices that simultaneous veneration allowed, avoiding disruptive theological disputes. Their identity was tied to these syncretic practices.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, local_communities, beneficiary,
    organized, generational, constrained, local).

% Struggled to reconcile the inherent contradictions within simultaneous veneration, often developing complex but ultimately unsatisfying interpretive frameworks. Their intellectual work was constrained by the need to explain an incoherent system.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, theological_scholars, payer,
    moderate, biographical, constrained, national).

% Later imposed the shinbutsu-bunri (separation of kami and buddhas), acting as an external force that revealed and broke the latent incoherence, restructuring the religious landscape for political and nationalistic aims.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Participated in simultaneous veneration, benefiting from its broad appeal but also constrained by its theological ambiguity. Post-Meiji, they lost some syncretic practices but gained a clearer, if narrower, institutional identity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions, beneficiary).

% Were central to simultaneous veneration, benefiting from the integration of kami worship. Post-Meiji, they gained a distinct national identity and state support, but lost the broader syncretic appeal.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shinto_shrines, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, shinto_shrines, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed for the pragmatic coexistence of diverse local religious practices and beliefs, integrating indigenous kami worship with imported Buddhism without requiring a definitive, coherent theological synthesis.
% TRANSFER_FUNCTION: Transferred the burden of reconciling theological contradictions onto individual practitioners and local communities, while maintaining the social and institutional stability of religious authorities and traditional practices.
% ABSENT_VOICES: Purist theologians (both Buddhist and Shinto) who would have demanded doctrinal consistency and challenged the inherent contradictions, but whose calls for resolution were implicitly suppressed by the prevailing pragmatic tolerance of incoherence.
% DISAPPEARANCE_RATIONALE: The Meiji government's forced separation (shinbutsu-bunri) demonstrated that the system of simultaneous veneration was not self-sustaining without the implicit suppression of its internal contradictions. Its disappearance led to a radical, state-mandated reorganization of religious institutions, practices, and identities across Japan.
% FOUNDING_PROBLEM: To integrate the indigenous Shinto tradition with the imported Buddhist tradition, allowing for a syncretic religious landscape that accommodated both without forcing a definitive, potentially divisive, theological synthesis.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, government decrees from the Meiji era, and subsequent scholarly analyses (e.g., by historians of religion and cultural studies) corroborate that the problem of syncretism was 'solved' by pragmatic coexistence rather than theological resolution, and that this pragmatic solution was forcibly ended by state intervention, indicating its inherent instability.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) stems from the cognitive load and internal conflict imposed on practitioners and scholars by the requirement to simultaneously hold and navigate contradictory beliefs. Suppression (0.75) was high due to the implicit social and institutional pressure against challenging the syncretic status quo, preventing any resolution of the incoherence. The 'lack of enforcement pressure' mentioned in the prompt refers to the absence of pressure *for* coherence, which effectively *enforced* the incoherent system. Theater ratio (0.5) reflects that while rituals and practices were real, the underlying theological coherence was largely performative or absent. Resistance was low (0.2) because the system's persistence relied on diffuse social norms rather than overt, contestable enforcement until the Meiji era.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this 'pragmatic incoherence' reading, the system was always extractive due to its internal contradictions. Other readings (e.g., 'ontological fusion' or 'domain partition') would perceive the same historical practices as coherent and beneficial, leading to a much lower extractiveness score and a different classification. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and local communities were beneficiaries, as the system allowed them to maintain their social roles and traditions without disruptive theological disputes. Practitioners and theological scholars were victims, bearing the cognitive and intellectual costs of the incoherence. The Meiji government, from this reading, acted as an external force that broke the existing 'snare' by imposing a new, distinct framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''pragmatic incoherence'' reading of simultaneous veneration?',
    'Comparative analysis with historical and theological texts supporting this specific interpretation, contrasting with texts supporting ''ontological fusion'' or ''domain partition'' readings.',
    'If this reading is misapplied, the classification of simultaneous veneration would shift dramatically, likely towards a ''rope'' or ''tangled_rope'' with lower extraction, reflecting a coherent or functionally distinct system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific interpretive lens applied to the simultaneous veneration kernel.').

omega_variable(
    nature_of_incoherence,
    'Was the ''incoherence'' truly fundamental and contradictory, or merely a difference in interpretive frames that could be reconciled?',
    'Deep philosophical and theological analysis of primary sources, potentially cross-cultural comparisons of syncretic systems, to determine if the contradictions were logically irreconcilable or merely culturally managed ambiguities.',
    'If reconcilable, the base extractiveness from cognitive dissonance would be lower, potentially reclassifying the constraint away from a ''snare'' towards a ''tangled_rope'' or even ''rope'' if the coordination function was dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_incoherence, conceptual, 'Examines the depth and reconcilability of the theological contradictions.').

omega_variable(
    impact_of_meiji_separation,
    'Was the Meiji shinbutsu-bunri primarily a revelation of latent incoherence (as this reading claims) or an imposed rupture on a functional, albeit syncretic, system?',
    'Historical counterfactual analysis, examining evidence of pre-Meiji attempts at theological reform or separation, and assessing the social and religious stability immediately prior to the Meiji decrees.',
    'If it was an imposed rupture on a functional system, this reading''s high extractiveness and ''snare'' classification would be less justified, as the system would have been more coherent than this reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_meiji_separation, empirical, 'Clarifies the nature of the Meiji separation''s effect on simultaneous veneration.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of contradiction structural (cultural norms, lack of institutional means for resolution) or internalized (practitioners simply accepted the ambiguity)?',
    'Sociological and anthropological studies of religious practice, examining how individuals and communities navigated the contradictions, and the mechanisms by which dissent or calls for resolution were managed or discouraged.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — practitioners carried the suppression with them. If purely structural, removing external barriers would have led to quicker resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 80, 0.5).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 60, 0.79).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 80, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(simu_su_t80, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 80, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
