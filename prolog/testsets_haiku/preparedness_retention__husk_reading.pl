% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance: Ritual Without Competence
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This is the 'husk reading' of the preparedness_retention kernel: the
 *   reading that declares preparedness has become ceremonial
 *   performance—drills and inspections are rituals that satisfy visibility
 *   and compliance metrics while tacit skill, community capacity, and actual
 *   operational readiness decay. The constraint extracts institutional
 *   legitimacy from field responders and communities by converting their time
 *   and attention toward visible compliance instead of toward competence
 *   building. It is claimed as a Piton (atrophied function, maintained by
 *   theater) because the founding problem—post-1953 legitimacy crisis—is now
 *   dead; the arrangement persists through administrative inertia and the
 *   identity-fusion of compliance officers. The measurement series shows
 *   rising theater_ratio (ceremoniality increases) and rising extraction as
 *   the gap between visible preparation and actual capacity widens. This
 *   reading sits in structural opposition to the 'competence_reading' (where
 *   drills ARE competence-preserving) and in partial coexistence with the
 *   'hybrid_reading' (which asserts specialized institutions retain
 *   competence while broader society becomes ceremonial). The ε-invariance
 *   test: if this constraint is measured by 'drills completed' it appears
 *   high-function; measured by 'response capacity under chaos' it appears as
 *   pure ceremony. The referent for this reading's ε is the standing
 *   arrangement (ceremonial preparedness) assessed by the husk reading's own
 *   lights (it is memorial performance lacking competence), never by the
 *   competence reading's lights.
 *
 * KEY AGENTS:
 *   - Compliance Administration: institutional seat that designs drills, schedules inspections, evaluates compliance. Identity-locked to the equation 'visible preparation = adequate preparation.'
 *   - Field Responders: moderate power, constrained exit. Bear the cost of drills consuming time better spent on actual skill development. Discover mismatch during real events.
 *   - Distributed Communities: powerless, trapped. Excluded from drill design, bear consequences of preparedness failure. Local competence is neither recognized nor tested.
 *   - Tacit Skill Community: moderate power, identity-locked. Holders of embodied disaster-response knowledge. Apprenticeship chains break as drills become the institutional metric.
 *   - Post-Disaster Analysis Bodies: external observers. Invariably document that drills did not match chaos; recommendations absorbed into next cycle.
 *   - Institutional Legitimacy Apparatus: non-agent beneficiary. The narrative claim 'we are prepared' serves government legitimacy and bureaucratic continuity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.71).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance: Ritual Without Competence").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'c9017a16-a367-4a5e-8d72-b7c6780fdd82').
narrative_ontology:cs_kernel_codification('c9017a16-a367-4a5e-8d72-b7c6780fdd82', fixed_text).
narrative_ontology:cs_authority_grounding('c9017a16-a367-4a5e-8d72-b7c6780fdd82', extraction).
narrative_ontology:cs_interpretation_layer_present('c9017a16-a367-4a5e-8d72-b7c6780fdd82').
narrative_ontology:cs_reading_relation('c9017a16-a367-4a5e-8d72-b7c6780fdd82', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('c9017a16-a367-4a5e-8d72-b7c6780fdd82', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('c9017a16-a367-4a5e-8d72-b7c6780fdd82', foundational, ceremonial_preparation_is_functionally_decoupled_from_competence).
narrative_ontology:cs_axiom_status(ceremonial_preparation_is_functionally_decoupled_from_competence, holdable).
narrative_ontology:cs_axiom_grounding('c9017a16-a367-4a5e-8d72-b7c6780fdd82', ceremonial_preparation_is_functionally_decoupled_from_competence, empirically_contingent).
narrative_ontology:cs_axiom('c9017a16-a367-4a5e-8d72-b7c6780fdd82', secondary, institutional_legitimacy_requires_visible_performance_over_real_capacity).
narrative_ontology:cs_axiom_status(institutional_legitimacy_requires_visible_performance_over_real_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c9017a16-a367-4a5e-8d72-b7c6780fdd82', institutional_legitimacy_requires_visible_performance_over_real_capacity, conventional).
narrative_ontology:cs_reference_frame('c9017a16-a367-4a5e-8d72-b7c6780fdd82', post_1953_legitimacy_restoration).
narrative_ontology:cs_drift_state('c9017a16-a367-4a5e-8d72-b7c6780fdd82', contemporary_post_disaster_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9017a16-a367-4a5e-8d72-b7c6780fdd82', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, compliance_administration).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, tacit_skill_retention).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, distributed_community_competence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, elected_government_officials).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, field_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, distributed_communities).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, tacit_skill_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and evaluates drills and inspections. Their career advancement and departmental budget hinge on measurable compliance metrics—drills conducted, checklists signed, inspection reports filed. They have internalized the equation: visible preparation equals adequate preparation. Changing this equation would require redefining their professional identity and the success metrics by which they are judged.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, compliance_administration, agenda_setter,
    institutional, biographical, identity_locked, national).

% Benefit from the appearance of preparedness without bearing the cost of developing genuine competence. Drills and inspections produce visible artifacts—reports, photographs, checkbox completions—that satisfy public anxiety and legislative oversight. A disaster that occurs despite drills is more defensible than a disaster preceded by evidence of negligence.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, elected_government_officials, beneficiary,
    institutional, biographical, constrained, national).

% Are required to participate in drills while their tacit skill-building time is consumed by compliance activities. They experience the gap between ceremonial participation and real readiness but lack authority to redirect resources toward actual competence. During actual disasters, they discover that drill-based training maps poorly to chaos.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, field_responders, payer,
    moderate, biographical, constrained, national).

% Bear the consequences of preparedness failure while excluded from the ritual performance. Their actual capacity to self-organize and respond (local knowledge, neighbor networks, informal coordination) is neither tested by drills nor officially recognized. When disaster strikes, they discover that the preparation performed for them does not extend to their actual circumstances.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, distributed_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, distributed_communities, excluded).

% Practitioners who hold embodied, experiential knowledge of disaster response built through apprenticeship, repeated minor incidents, and intergenerational transmission. Their knowledge is not scoreable on compliance rubrics. As drills become the metric that consumes their time and attention, the apprenticeship chain breaks and this knowledge atrophies. The institutional shift toward ceremonial preparation makes their expertise invisible and unnecessary.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, tacit_skill_community, payer,
    moderate, generational, identity_locked, regional).

% Conduct investigations after major events and invariably report: 'Drills did not match the actual scenario. Real-world chaos revealed gaps that simulations had not exposed.' They serve as the external accountability seat that repeatedly documents the mismatch between ceremony and competence but whose recommendations are absorbed into a next cycle of drills.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, post_disaster_analysis_bodies, observer,
    institutional, generational, analytical, national).

% The non-agent beneficiary: the narrative and institutional claim that 'we have prepared' serves government legitimacy, bureaucratic continuity, and the narrative of state capacity. This apparatus requires the visible preparation rituals to persist; their absence would expose the legitimacy claim as unsupported.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, institutional_legitimacy_apparatus, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Drills coordinate the appearance of readiness across distributed government agencies, creating a unified performance of preparedness that assures the public and legislature without requiring actual competence coordination.
% TRANSFER_FUNCTION: Moves time, attention, and budget allocation away from tacit skill retention and community competence toward visible compliance artifacts—reports, photographs, drill schedules—that concentrate legitimacy benefit in institutional administration.
% ABSENT_VOICES: Communities with local disaster experience and tacit-knowledge holders who understand what actual readiness requires are excluded from the drill design and evaluation process. Their objections ('this drill bears no resemblance to what happened') are treated as outlier feedback, not structural evidence of mismatch.
% DISAPPEARANCE_RATIONALE: If the ceremonial preparedness constraint disappeared—if drills ceased to be mandatory, compliance checklists were abandoned, and resources flowed instead toward tacit skill development and community capacity—the institutions administering compliance would lose their legitimacy function. Government would face the unsettling transparency that preparedness cannot be performed, only built. Communities and field responders would reorganize around actual competence-building practices.
% FOUNDING_PROBLEM: After the 1953 North Sea flood and subsequent major disasters, the state faced public anxiety and legitimacy pressure to demonstrate that preparation had improved. Drills, inspections, and standardized procedures were established to make preparedness visible and measurable—to answer the public's demand: 'How do we know you are ready?'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—providing evidence of readiness after catastrophic failure—was live in the 1950s–1970s. Post-disaster analysis bodies (independent investigative commissions) have repeatedly documented since the 1980s that the drills produced did not correlate with actual response capacity during events. Field responder testimony and academic studies of organizational resilience (sources outside the compliance apparatus) converge on the assessment that ceremonial preparation became decoupled from functional readiness. The founding problem is acknowledged by all parties except the compliance administration itself, which remains invested in the performance.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater-ratio is high and rising (0.82 at interval end) because the constraint's primary function is not competence-building but visibility-production—drills are designed to be photographed, inspections to generate signed reports. Extractiveness is substantial (0.68) because the arrangement moves time and budget from those who could build real capacity (field responders, communities, tacit practitioners) toward those who benefit from the legitimacy appearance (administrators, elected officials, institutional apparatus). Suppression is maintained (0.71) by the identity-lock of compliance officers and the structural exclusion of communities from drill design—alternative preparedness frameworks cannot surface when the decision-maker is incentivized by drill metrics. Accessibility-collapse is moderate (0.45) rather than high because field responders and communities continually experience the gap between ceremony and chaos; alternatives are partially visible but structurally blocked. Resistance is moderate (0.58) because external bodies and responders mount documentation and advocacy, but the compliance apparatus absorbs criticism into procedural refinement rather than substantive change. The measurement trajectory shows a piton signature: theater rising as extraction plateaus (the gap between visible and real widens; the arrangement survives by performing harder, not by building competence).
 *
 * PERSPECTIVAL GAP:
 *   From the compliance administration seat, this constraint is a genuine coordination problem solved—drills coordinate readiness narratives across agencies, producing unified appearance to the public. From the field responder and community seats, the same constraint is pure extraction dressed as coordination—their time is moved toward visible compliance and away from actual competence-building. The gap widens over the interval as theater rises: the administration must perform more ceremonially to sustain the legitimacy claim while the gap between performance and capacity grows. The competence_reading would frame this as a tragic failure of institutional design; the husk_reading frames it as a structural benefit to administrators and a structural cost to responders.
 *
 * DIRECTIONALITY LOGIC:
 *   Compliance administration is the beneficiary seat (derives legitimacy, career advancement, departmental budget from compliance metrics—d near 0.0, low extraction). Field responders and tacit practitioners are target seats (lose time to compliance, pay via atrophied skills—d near 1.0, high extraction). Distributed communities are the most trapped target seat (powerless, identity-locked by place, excluded from the mechanism itself—d approaches 1.0). Elected officials are beneficiary seats (collect legitimacy without competence cost—d near 0.2, moderate indirect benefit). Post-disaster bodies are observers (analytical seat, no d). The structural asymmetry is acute: the seats that benefit from ceremony are those that set the metric; the seats that bear the cost are those excluded from defining success.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classical mandatrophy: the founding mandate (make preparedness visible after 1953 legitimacy crisis) has been solved. Post-disaster bodies and responders attest that drills now exist for their own sake, not because they solve the visibility problem (visibility is assured; the legitimacy apparatus runs on autopilot). Yet the constraint persists because the compliance apparatus has no incentive to dissolve it and communities have no power to force dissolution. The Piton classification captures this: low beneficiary concentration (legitimacy is diffuse, not captured), but persistence through administrative inertia and identity-lock. The constraint cannot be removed without destabilizing the careers and self-concepts of those administering it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_competence_decoupling_mechanism,
    'What is the causal mechanism by which ceremonial compliance produces the gap between visible preparation and actual response capacity? Is it measurement selection (drills measure what is easy to score, not what matters), resource capture (compliance administration captures budget that could build competence), or identity-lock (administrators internalize the drill-as-preparation equation)?',
    'Comparative analysis of institutions that decoupled drill metrics from competence training versus those that integrated them; interviews with responders about what competence-building activities were displaced by drills; budget tracking showing resource flows between compliance and tacit-skill development.',
    'If measurement selection is primary, modest procedural reform (redesigning drills) could close the gap. If resource capture is primary, budget reallocation is necessary. If identity-lock is primary, the gap will persist until professional identity and career advancement are decoupled from compliance metrics—a much harder intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_decoupling_mechanism, empirical, 'Causal mechanism of ceremony-competence decoupling').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of alternative preparedness frameworks (community-based, tacit-skill-centered, localized) structural (budget rules, authorization structures, legally enforced) or internalized (field responders and communities have accepted that compliance is the metric, drills are legitimate)?',
    'Test case: authorizing alternative preparedness frameworks without institutional suppression (local resilience budgets, community-design drills, skill apprenticeship formal recognition) and observing whether they grow or atrophy. Post-exit trajectories of responders who leave the institutional system for community-based preparedness.',
    'If structural, policy reform addressing budget authorization and decision-authority could open alternatives. If internalized, the suppression travels with the responder even if they exit; the constraint''s effective suppression is higher than the structural measure suggests, and reform requires cognitive reframing (what counts as ''real'' preparation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression of alternative preparedness frameworks').

omega_variable(
    distributed_community_competence_asymmetry,
    'Do communities actually hold tacit, distributed competence for disaster response (local knowledge, neighbor networks, informal coordination)—and if so, why is it not formally tested, recognized, or integrated into institutional preparedness?',
    'Ethnographic study of actual community response during minor disaster events; comparison of community-coordinated response versus institutionally-coordinated response on outcomes and speed; documentation of what local knowledge responders draw on when institutional drills fail to map to chaos.',
    'If distributed competence is real and substantial, the constraint''s victim set is correct: it is extracting and suppressing an alternative capacity that could improve actual preparedness. If distributed competence is marginal or unreliable, the constraint''s focus on institutional training is justified, and the husk reading weakens (ceremony may not be optimal but it is the best scalable solution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_community_competence_asymmetry, empirical, 'Existence and institutional recognition of distributed community disaster-response competence').

omega_variable(
    kernel_reading_choice,
    'Which reading of the preparedness_retention kernel is structurally true: competence (drills preserve real capacity), husk (drills are ceremony), or hybrid (specialized competence + ceremonial broader society)?',
    'Large-scale comparative analysis of disaster outcomes across jurisdictions that prioritize drills versus those that prioritize skill apprenticeship, community involvement, and tacit knowledge. Longitudinal study of the same institution tracking whether theater-ratio rise predicts poor outcomes in subsequent events.',
    'The husk reading is only true if drills demonstrably do not preserve actual response capacity relative to alternative preparedness investments. The competence reading is true if drills do preserve capacity. The hybrid reading is true if institutional stratification explains observed outcomes (specialized agencies do well, broader responders do poorly). Each reading carries a different victim structure and a different mandatrophy verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice, empirical, 'Which reading of the preparedness kernel (competence, husk, hybrid) matches the structural relationship between drills and actual response capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__husk_reading, theater_ratio, 5, 0.69).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__husk_reading, theater_ratio, 10, 0.74).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__husk_reading, theater_ratio, 15, 0.77).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__husk_reading, theater_ratio, 25, 0.81).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.82).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__husk_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__husk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__husk_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__husk_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__husk_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__husk_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__husk_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__husk_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel admits three structurally distinct readings. The husk_reading (this constraint) asserts that institutional preparedness has become ceremonial and that drills are poor proxies for actual response capacity. The competence_reading asserts that drills do preserve real operational readiness. The hybrid_reading asserts that competence is retained in specialized institutions while broader societal memory becomes ceremonial. These three readings are not different observations of a single constraint; they are different ε-valued constraints (husk: high extraction via ceremony; competence: low extraction via genuine skill transfer; hybrid: stratified extraction via institutional differentiation). Each reading has its own beneficiary/victim structure and its own Piton/Rope/Snare classification. They are linked here because they share a contested kernel (the institutional commitment to post-1953 preparedness procedures) and because understanding one requires understanding the alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
