% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Epistemic Standard
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint story models the experiential_pluralism_reading of the
 *   legitimate_knowledge_boundary kernel — the claim that legitimate
 *   knowledge arises from lived experience and community validation, with
 *   methodological standards as one tool among many. It is one of three
 *   contested readings (alongside credentialed_expertise_reading and
 *   hybrid_coproduction_reading). The reading presents itself as a genuine
 *   coordination mechanism (rope) that solves epistemic exclusion. The
 *   authored metrics reflect a moderately extractive arrangement (ε=0.45)
 *   that redistributes epistemic authority from credentialed experts to
 *   marginalized communities, with low suppression (0.25) because it does not
 *   ban credentialed expertise but demotes it from monopoly to option.
 *   Resistance is high (0.70) from institutions invested in the credentialed
 *   model. Theater remains low (0.20) as community validation practices are
 *   substantive, though slight rise over time signals co-optation risk.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.25).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Epistemic Standard").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '14dcb95c-312a-4f07-9fa8-ae951d82b760').
narrative_ontology:cs_kernel_codification('14dcb95c-312a-4f07-9fa8-ae951d82b760', distributed).
narrative_ontology:cs_authority_grounding('14dcb95c-312a-4f07-9fa8-ae951d82b760', practice).
narrative_ontology:cs_interpretation_layer_present('14dcb95c-312a-4f07-9fa8-ae951d82b760').
narrative_ontology:cs_reading_relation('14dcb95c-312a-4f07-9fa8-ae951d82b760', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('14dcb95c-312a-4f07-9fa8-ae951d82b760', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('14dcb95c-312a-4f07-9fa8-ae951d82b760', foundational, lived_experience_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(lived_experience_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('14dcb95c-312a-4f07-9fa8-ae951d82b760', lived_experience_sufficient_for_legitimacy, deontological).
narrative_ontology:cs_axiom('14dcb95c-312a-4f07-9fa8-ae951d82b760', foundational, community_validation_epistemic_authority).
narrative_ontology:cs_axiom_status(community_validation_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('14dcb95c-312a-4f07-9fa8-ae951d82b760', community_validation_epistemic_authority, deontological).
narrative_ontology:cs_reference_frame('14dcb95c-312a-4f07-9fa8-ae951d82b760', distributed_epistemic_authority).
narrative_ontology:cs_drift_state('14dcb95c-312a-4f07-9fa8-ae951d82b760', contemporary_epistemic_justice_movement, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('14dcb95c-312a-4f07-9fa8-ae951d82b760', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, knowledge_users).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_advocates).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validators).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_requires_distributed_validation).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_constitutes_legitimate_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically excluded from credentialed knowledge production (indigenous peoples, patient groups, disabled communities, Global South knowers). Their lived experience gains direct epistemic legitimacy without requiring methodological translation. They can engage with multiple epistemic systems simultaneously and are not locked into a single validation pathway.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities, beneficiary,
    moderate, generational, mobile, global).

% Researchers and professionals whose epistemic authority derives from institutional credentials and peer review. They lose monopoly control over what counts as legitimate knowledge in their domains. Their career capital, funding access, and institutional positions are tied to the credentialed system, making exit costly. They can adapt by incorporating community validation but cannot easily abandon their credentialed identity.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Facilitators and participants in community validation processes — elders, patient advocates, community health workers, indigenous knowledge keepers. They administer the validation practices, set local standards for what counts as adequate community corroboration, and gain recognition as epistemic authorities. They can move between communities and validation roles.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validators, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validators, beneficiary).

% Policy makers, clinicians, educators, journalists, and citizens who consume knowledge. They gain access to a broader pool of validated knowledge, including experiential insights previously dismissed. They can choose which validation pathways to trust for different decisions. Their need is for reliable knowledge, not allegiance to any single epistemic system.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, knowledge_users, beneficiary,
    organized, immediate, mobile, global).

% Universities, journals, funding agencies, and professional societies that certify and reproduce credentialed expertise. They face pressure to reform peer review, recognize community-based participatory research, and fund experiential knowledge projects. Their budgets, prestige, and regulatory mandates are built on the credentialed model; structural reform is slow and contested.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_institutions, payer,
    institutional, generational, constrained, global).

% Scholars and activists in STS, feminist epistemology, decolonial theory, and disability studies who argue for epistemic pluralism. They gain institutional uptake for their frameworks and see their theoretical work translated into validation practice. They operate across academic and community spaces, not dependent on any single institution.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Theoretical position that sees the full structure: three competing readings of the legitimate_knowledge_boundary kernel, each with different beneficiary/victim profiles and coordination/extraction dynamics. Does not participate in validation struggles but models their structural properties.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes epistemic validation authority to communities with lived experience, solving the problem of excluded knowers and narrow methodological gatekeeping by making community corroboration a legitimate pathway to knowledge status.
% TRANSFER_FUNCTION: Moves epistemic authority and validation power from credentialed institutions to communities of lived experience, with methodological standards repositioned as optional tools rather than mandatory gatekeepers.
% ABSENT_VOICES: Traditional knowledge holders in oral cultures who may not fit formal 'community validation' frameworks; individuals with rare or stigmatized experiences lacking a community; credentialed experts from the Global South who view hard-won credentials as decolonial achievements rather than exclusionary tools; quantitative methodologists who see methodological rigor as protection against bias, not gatekeeping.
% DISAPPEARANCE_RATIONALE: Without distributed community validation as a legitimate pathway, credentialed peer review would reassert its monopoly over knowledge legitimacy, systematically excluding lived experience and re-centering methodological rigor as the sole criterion.
% FOUNDING_PROBLEM: Credentialed peer review systematically excludes knowledge from marginalized communities, indigenous peoples, and those without institutional access, treating methodological rigor as the sole legitimacy criterion and dismissing experiential knowledge as anecdote or bias.
% FOUNDING_PROBLEM_CORROBORATION: Documented by STS scholars (Harding on standpoint theory, Fricker on epistemic injustice), indigenous epistemologists (Kovach, Smith), patient advocacy movements (HIV/AIDS activism, rare disease communities), and decolonial theorists (Mignolo, Santos) — sources outside the direct beneficiary set of this reading.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness reflects the transfer of epistemic rent: credentialed experts lose exclusive validation power (a form of capital), communities gain it. Suppression is low because the constraint does not forbid credentialed validation — it only removes its monopoly. Accessibility collapse is low because alternative validation pathways (credentialed, hybrid) remain open. Resistance is high because scientific institutions actively contest the redistribution through funding, hiring, and publishing gatekeeping. The claim/metric independence is maintained: claimed_type='rope' (the reading's self-presentation as coordination) while metrics describe a constraint with measurable extraction and resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (credentialed_experts, scientific_institutions) experience this constraint as extraction of their epistemic authority; the beneficiary seats experience it as liberation from exclusion. The agenda_setter seat (community_validators) experiences it as new administrative burden and recognition. The engine computes per-seat types from these structural asymmetries; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and knowledge_users are beneficiaries (d near 0.0) — they gain validation access without surrendering exit options. Credentialed_experts and scientific_institutions are payers (d near 1.0) — they bear the cost of lost monopoly, with constrained exit (credentials are identity-locked to career). Community_validators are agenda_setters with mobile exit (they facilitate but don't own the arrangement). Epistemic_justice_advocates are beneficiaries with mobile exit. The analytical_observer sits at d=0.5 (symmetric). Directionality derives from beneficiary/victim declarations plus exit structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic exclusion) remains live. The constraint is not a degraded remnant of a solved problem — it addresses an ongoing injustice. However, mandatrophy risk exists if community validation becomes ritualized without substantive engagement, or if 'lived experience' becomes a credential of its own. The theater_ratio trajectory monitors this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_experiential_pluralism,
    'Is the experiential pluralism reading a genuine coordination mechanism or does it create new extraction dynamics (e.g., new community gatekeepers, capture of validation processes)?',
    'Empirical study of communities adopting this standard: track whether validation power concentrates in new intermediaries, whether ''community validation'' becomes a performative checkbox, and whether error rates change.',
    'If capture occurs, reclassifies from rope to tangled_rope or snare; if genuine coordination persists, supports rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_experiential_pluralism, empirical, 'Whether this reading instantiates genuine coordination or masked extraction.').

omega_variable(
    methodological_standard_role_ambiguity,
    'When methodological standards are ''one tool among many,'' does epistemic quality control degrade, adapt, or bifurcate (rigor for some domains, pluralism for others)?',
    'Longitudinal comparison of error rates, replication success, and policy outcomes in domains adopting pluralist vs. credentialist validation standards.',
    'If quality degrades substantially in high-stakes domains (medicine, engineering), the coordination function is compromised; if bifurcation emerges, the constraint may be domain-specific rather than universal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_standard_role_ambiguity, empirical, 'Epistemic quality consequences of demoting methodological standards from gatekeeper to tool.').

omega_variable(
    suppression_mechanism_credentialed_institutions,
    'Is resistance from credentialed institutions primarily structural (funding, hiring, publishing gatekeeping) or internalized (epistemic inferiority complexes in marginalized communities that persist after formal barriers fall)?',
    'Post-policy-change tracking: if exclusion persists after formal recognition of community validation, internalized component confirmed. Compare communities with/without internalized epistemic marginalization.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_credentialed_institutions, empirical, 'Structural vs. internalized suppression of experiential knowledge claims.').

omega_variable(
    credentialed_expertise_global_south_ambiguity,
    'For credentialed experts from the Global South, are credentials experienced as exclusionary tools (to be dismantled) or as hard-won decolonial achievements (to be defended)?',
    'Qualitative study of Global South scientific communities'' positions on epistemic pluralism vs. credential defense; track institutional alignments.',
    'If credentials are defended as decolonial achievements, the victim/payer classification for credentialed_experts fractures — some become beneficiaries of pluralism, others remain payers. This changes the extraction map.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialed_expertise_global_south_ambiguity, conceptual, 'Whether the credentialed_experts stakeholder fractures along Global North/South lines.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_ep_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lkb_ep_tr_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lkb_ep_tr_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(lkb_ep_tr_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(lkb_ep_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(lkb_ep_tr_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(lkb_ep_tr_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(lkb_ep_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lkb_ep_be_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(lkb_ep_be_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(lkb_ep_be_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(lkb_ep_be_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(lkb_ep_be_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(lkb_ep_be_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lkb_ep_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(lkb_ep_su_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(lkb_ep_su_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(lkb_ep_su_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(lkb_ep_su_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(lkb_ep_su_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 25, 0.25).
narrative_ontology:measurement(lkb_ep_su_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.08).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the experiential_pluralism_reading of the legitimate_knowledge_boundary kernel. It differs from the credentialed_expertise_reading (which centers methodological rigor and peer review as sole validators) and the hybrid_coproduction_reading (which requires integration of both through co-production). The ε values differ: this reading claims low-extraction coordination (rope, ε=0.45) while credentialed_expertise_reading likely shows high extraction from excluded knowers (snare/tangled_rope). The hybrid reading likely shows intermediate extraction (tangled_rope). All three are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, powerful, 0.85).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional, 0.9).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, moderate, 0.2).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
