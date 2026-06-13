% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism in Knowledge Legitimacy
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint, 'Legitimate knowledge arises from lived experience and
 *   community validation, with methodological standards as one tool among
 *   many,' represents the 'experiential pluralism' reading of the broader
 *   'legitimate_knowledge_boundary' kernel. It posits that knowledge derived
 *   from direct experience and validated within specific communities holds
 *   epistemic legitimacy, often on par with or exceeding that of
 *   methodologically rigorous, peer-reviewed research. Methodological
 *   standards are seen as useful tools but not as exclusive gatekeepers of
 *   truth. This reading aims to democratize knowledge production and
 *   challenge traditional hierarchies of expertise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.2).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.1).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism in Knowledge Legitimacy").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '1f482992-190b-4401-8f9d-7d9a7a751f8a').
narrative_ontology:cs_kernel_codification('1f482992-190b-4401-8f9d-7d9a7a751f8a', distributed).
narrative_ontology:cs_authority_grounding('1f482992-190b-4401-8f9d-7d9a7a751f8a', practice).
narrative_ontology:cs_interpretation_layer_present('1f482992-190b-4401-8f9d-7d9a7a751f8a').
narrative_ontology:cs_reading_relation('1f482992-190b-4401-8f9d-7d9a7a751f8a', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f482992-190b-4401-8f9d-7d9a7a751f8a', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('1f482992-190b-4401-8f9d-7d9a7a751f8a', foundational, lived_experience_is_epistemically_primary).
narrative_ontology:cs_axiom_status(lived_experience_is_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('1f482992-190b-4401-8f9d-7d9a7a751f8a', lived_experience_is_epistemically_primary, deontological).
narrative_ontology:cs_axiom('1f482992-190b-4401-8f9d-7d9a7a751f8a', foundational, community_validation_confers_legitimacy).
narrative_ontology:cs_axiom_status(community_validation_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1f482992-190b-4401-8f9d-7d9a7a751f8a', community_validation_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('1f482992-190b-4401-8f9d-7d9a7a751f8a', decentralized_epistemic_authority).
narrative_ontology:cs_drift_state('1f482992-190b-4401-8f9d-7d9a7a751f8a', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1f482992-190b-4401-8f9d-7d9a7a751f8a', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, activist_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, local_knowledge_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_academics).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their lived experiences and community-validated insights are recognized as primary sources of legitimate knowledge, empowering them in decision-making processes that affect their lives.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities, beneficiary,
    organized, generational, mobile, local).

% Advocate for and facilitate the integration of experiential knowledge into broader epistemic frameworks. They help design and implement validation processes that prioritize community input over traditional academic gatekeeping.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, activist_researchers, agenda_setter,
    moderate, biographical, constrained, regional).

% Individuals or groups whose knowledge is rooted in specific cultural, ecological, or historical contexts. This constraint validates their epistemic contributions, which might otherwise be dismissed by universalizing scientific standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, local_knowledge_holders, beneficiary,
    powerless, generational, mobile, local).

% Are challenged to broaden their definitions of expertise and methodological rigor. They may experience a perceived loss of epistemic authority as community validation gains prominence, requiring them to adapt their research and engagement practices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_academics, payer,
    institutional, generational, constrained, global).

% Benefit from a wider range of relevant knowledge inputs, leading to more context-sensitive and equitable policy outcomes. They must navigate diverse knowledge claims and integrate them into actionable strategies.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers, beneficiary,
    institutional, immediate, mobile, national).

% Are pressured to reform their peer review, funding, and publication practices to accommodate diverse forms of knowledge validation. This may involve reallocating resources and rethinking established hierarchies of knowledge production.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_institutions, payer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse knowledge claims by establishing a framework where lived experience and community validation are recognized as legitimate epistemic sources, fostering inclusive knowledge production.
% TRANSFER_FUNCTION: Transfers epistemic authority and validation power from centralized, credentialed institutions to decentralized, community-based processes and lived experience.
% ABSENT_VOICES: Those who insist on a singular, universal standard of scientific rigor as the only legitimate form of knowledge are marginalized by this framework; they would argue for the primacy of peer-reviewed, methodologically controlled research.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the epistemic landscape would revert to a more hierarchical structure, marginalizing experiential and community-validated knowledge. Decision-making would likely become less inclusive and responsive to local contexts.
% FOUNDING_PROBLEM: The historical exclusion and devaluation of knowledge from marginalized communities and non-academic sources, leading to policies and practices that ignored local realities and perpetuated inequalities.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities, activist researchers, and critical scholars consistently attest to the ongoing problem of epistemic injustice and the need for pluralistic knowledge frameworks. International development organizations and human rights groups also corroborate this, citing failures of top-down, expert-driven interventions.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).
:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it primarily functions to coordinate diverse knowledge claims and empower previously marginalized voices, with relatively low extraction and suppression. Its extractiveness (0.2) is minimal, representing the friction of integrating diverse knowledge systems and the 'cost' to traditional institutions of adapting. Suppression (0.1) is low, as it primarily opens up new avenues for validation rather than coercively shutting down existing ones. Theater ratio (0.05) is also low, indicating that its stated function of validating experiential knowledge is genuinely pursued, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and local knowledge holders, this constraint is a clear Rope, providing a pathway for their knowledge to be recognized and utilized. For traditional academics and scientific institutions, it may feel more like a Tangled Rope or even a Snare, as it challenges their established authority and requires them to cede epistemic ground, incurring costs in adapting their practices and potentially losing funding or prestige. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities, activist researchers, and local knowledge holders are clear beneficiaries, as their knowledge is legitimized and their agency in knowledge production is increased. Traditional academics and scientific institutions are payers, as they bear the costs of adapting to a more pluralistic epistemic landscape and potentially losing their exclusive epistemic authority. Policy makers are also beneficiaries, gaining access to more relevant and context-specific knowledge for decision-making.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively addresses a persistent problem of epistemic injustice, so mandatrophy is not a concern. Its mandate is to continually challenge and reconfigure the boundaries of legitimate knowledge, ensuring that diverse forms of knowing are recognized. The classification as a Rope prevents mislabeling genuine efforts at epistemic democratization as extractive, while acknowledging the 'costs' to previously dominant knowledge systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''legitimate_knowledge_boundary'' kernel, or merely a variant of the ''hybrid_coproduction_reading''?',
    'Analysis of the core axioms: if ''experiential_pluralism_reading'' consistently prioritizes lived experience over methodological rigor, even in cases of conflict, it is distinct. If it seeks to integrate them symmetrically, it leans towards ''hybrid_coproduction_reading''.',
    'If it''s a distinct reading, its classification as a Rope holds. If it''s a variant, its extractiveness and suppression might be higher due to the inherent friction of forced integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing this reading from other pluralistic knowledge frameworks.').

omega_variable(
    power_dynamics_in_validation,
    'To what extent do existing power structures influence which ''community validations'' are recognized as legitimate, potentially re-inscribing hierarchies under a new guise?',
    'Empirical study of knowledge integration processes: track whose experiential knowledge is amplified and whose remains marginalized, even within pluralistic frameworks.',
    'If power dynamics significantly distort the validation process, the constraint''s effective extractiveness and suppression would be higher, potentially reclassifying it as a Tangled Rope or Snare for certain communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_dynamics_in_validation, empirical, 'Assessing the influence of power on community validation processes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2010, 0.045).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1990, 0.07).
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. This 'experiential_pluralism_reading' emphasizes lived experience and community validation, contrasting with the 'credentialed_expertise_reading' (which prioritizes methodological rigor) and the 'hybrid_coproduction_reading' (which seeks to integrate both).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
