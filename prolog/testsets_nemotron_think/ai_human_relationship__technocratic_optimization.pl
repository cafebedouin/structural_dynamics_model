% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__technocratic_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__technocratic_optimization, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_human_relationship__technocratic_optimization
 *   human_readable: Technocratic Optimization Reading of AI-Human Relationship
 *   domain: technology_ethics/political_theology/catholic_social_teaching
 *
 * SUMMARY:
 *   This constraint story captures the technocratic_optimization reading of
 *   the ai_human_relationship kernel — the frame that presents AI as an
 *   instrument of efficiency maximization and measures human value by
 *   productivity and optimization potential. It is one of three contested
 *   readings; the others are instrumental_subsidiarity (AI as neutral tool to
 *   be regulated) and incarnational_humanism (AI must serve integral human
 *   development, human person as imago Dei irreducible to optimization). The
 *   claimed type is tangled_rope: a genuine coordination function (scalable
 *   allocation at speed) coexists with asymmetric extraction (concentration
 *   of power in algorithmic gatekeepers, exclusion of 'inefficient'
 *   populations, subordination of work to machine pace). The metrics describe
 *   the constraint's operation as assessed from this reading's own lights:
 *   high and rising extractiveness, high suppression, moderate theater,
 *   substantial accessibility collapse. The measurement series (2010–2025)
 *   tracks the acceleration of algorithmic governance across labor, finance,
 *   healthcare, and public administration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, 0.82).
domain_priors:suppression_score(ai_human_relationship__technocratic_optimization, 0.78).
domain_priors:theater_ratio(ai_human_relationship__technocratic_optimization, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, extractiveness, 0.82).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__technocratic_optimization, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__technocratic_optimization, "Technocratic Optimization Reading of AI-Human Relationship").
narrative_ontology:topic_domain(ai_human_relationship__technocratic_optimization, "technology_ethics/political_theology/catholic_social_teaching").

domain_priors:requires_active_enforcement(ai_human_relationship__technocratic_optimization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__technocratic_optimization, '5353737d-1975-4d0d-b795-07d0589c2421').
narrative_ontology:cs_kernel_codification('5353737d-1975-4d0d-b795-07d0589c2421', distributed).
narrative_ontology:cs_authority_grounding('5353737d-1975-4d0d-b795-07d0589c2421', distributed).
narrative_ontology:cs_reading_relation('5353737d-1975-4d0d-b795-07d0589c2421', ai_human_relationship__instrumental_subsidiarity, coexists_with).
narrative_ontology:cs_reading_relation('5353737d-1975-4d0d-b795-07d0589c2421', ai_human_relationship__incarnational_humanism, influences).
narrative_ontology:cs_axiom('5353737d-1975-4d0d-b795-07d0589c2421', foundational, efficiency_maximization_primary_value).
narrative_ontology:cs_axiom_status(efficiency_maximization_primary_value, holdable).
narrative_ontology:cs_axiom_grounding('5353737d-1975-4d0d-b795-07d0589c2421', efficiency_maximization_primary_value, instrumental).
narrative_ontology:cs_axiom('5353737d-1975-4d0d-b795-07d0589c2421', foundational, human_productivity_optimization_metric).
narrative_ontology:cs_axiom_status(human_productivity_optimization_metric, holdable).
narrative_ontology:cs_axiom_grounding('5353737d-1975-4d0d-b795-07d0589c2421', human_productivity_optimization_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('5353737d-1975-4d0d-b795-07d0589c2421', technocratic_efficiency_paradigm).
narrative_ontology:cs_drift_state('5353737d-1975-4d0d-b795-07d0589c2421', contemporary_ai_acceleration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5353737d-1975-4d0d-b795-07d0589c2421', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__technocratic_optimization, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, algorithmic_gatekeepers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, efficiency_obsessed_institutions).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, vulnerable_populations).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, workers_subordinated).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, data_profiled_persons).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, technocratic_efficiency_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, optimization_imperative).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, productivity_as_human_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, deploy, and govern the algorithmic systems that allocate work, credit, attention, and opportunity. They set the optimization objectives, define efficiency metrics, and control the infrastructure that makes the technocratic frame operational. They capture the surplus from optimization while externalizing the costs of exclusion and subordination.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmic_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Corporations, government agencies, and NGOs that adopt algorithmic optimization to reduce costs, increase throughput, and meet performance targets. They benefit from the coordination function — standardized, scalable decision-making — but are also locked into the frame by competitive pressure and institutional path dependence. Their exit is constrained by the risk of falling behind peers who optimize more aggressively.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, efficiency_obsessed_institutions, beneficiary,
    organized, biographical, constrained, global).

% Populations deemed 'inefficient' by optimization metrics: the elderly, disabled, precarious workers, Global South communities, and those with non-legible forms of contribution. They bear the costs of exclusion — denied credit, healthcare, employment, and social recognition — because their value does not compute in the optimization function. Exit is effectively closed; the system that excludes them also controls the gates to inclusion.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, vulnerable_populations, payer,
    powerless, biographical, trapped, global).

% Workers whose labor is paced, monitored, and evaluated by algorithmic management systems — gig drivers, warehouse workers, call center agents, knowledge workers under productivity surveillance. They experience work subordinated to machine pace, with autonomy eroded by real-time optimization. Exit is constrained by labor market conditions and the pervasiveness of algorithmic management across sectors.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, workers_subordinated, payer,
    moderate, biographical, constrained, global).

% All persons reduced to data profiles — behavioral traces, predictive scores, risk assessments — that become the primary interface between them and institutional decisions. The profile precedes and shapes the person; resistance requires rejecting the digital infrastructure that mediates modern life. Identity lock operates through the fusion of self-concept with the quantified self, making opt-out existentially costly.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, data_profiled_persons, payer,
    powerless, biographical, identity_locked, global).

% Catholic Social Teaching scholars who analyze the technocratic frame through the lens of human dignity, common good, solidarity, and preferential option for the poor. They articulate the incarnational_humanism reading as a counter-reading of the same kernel. Their seat is analytical — they do not collect from nor pay into the constraint, but their witness structures the contest over the kernel.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, cst_theologians, observer,
    analytical, generational, analytical, universal).

% Academics, civil society researchers, and policy analysts who critique algorithmic governance, advocate for human-centered AI, and document the harms of optimization-first frameworks. They occupy the instrumental_subsidiarity and adjacent critical positions. Like CST theologians, they are analytical observers of the constraint's operation.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, technology_ethicists, observer,
    analytical, generational, analytical, global).

% Communities and movements advocating for relational AI, data sovereignty, algorithmic justice, and post-growth economics — voices that would object to the technocratic frame if included in governance forums. They are structurally excluded from the rooms where optimization objectives are set, their epistemologies treated as 'non-technical' or 'ideological' rather than legitimate contestation of the kernel.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, excluded_alternative_voices, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, standardized framework for allocating resources, labor, and attention across complex systems — replacing negotiated, context-sensitive human judgment with computable optimization objectives that can be executed at speed and scale.
% TRANSFER_FUNCTION: Moves decision-making authority, economic surplus, and social recognition from situated human agents to algorithmic systems and their operators; extracts dignity, autonomy, and material security from populations whose value does not register in the optimization function, concentrating gains in gatekeeping institutions.
% ABSENT_VOICES: The excluded_alternative_voices stakeholder names the communities and movements kept out of AI governance: Global South data justice advocates, disability rights organizers, labor movements resisting algorithmic management, indigenous data sovereignty movements, and post-growth economists. They are absent from the tables where optimization objectives are codified into infrastructure.
% DISAPPEARANCE_RATIONALE: If the technocratic optimization frame vanished overnight, the algorithmic infrastructure would remain but its legitimating logic would collapse. Institutions would face a legitimacy crisis in their allocation decisions; workers would reclaim pace-setting authority; vulnerable populations would lose the 'inefficient' label but gain no immediate alternative allocation mechanism. The world would rearrange through contested reconstruction of the AI-human kernel — not return to a pre-technocratic state.
% FOUNDING_PROBLEM: The coordination problem of governing complex, high-velocity systems (markets, logistics, administration, defense) at scale without relying on slow, biased, or corruptible human discretion. The technocratic frame promised to solve this by substituting computation for judgment.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the algorithmic_gatekeepers and efficiency_obsessed_institutions as still live (complexity has only increased). CST theologians, technology ethicists, and excluded_alternative_voices attest it is substantially a category error — the problem was never 'how to optimize at scale' but 'how to govern justly at scale,' and the substitution of optimization for justice is the extraction. Independent corroboration comes from the history of cybernetics and operations research (e.g., Pickering, The Cybernetic Brain; Scott, Seeing Like a State) showing the founding problem was always framed by power, not discovered by necessity.
narrative_ontology:disappearance_verdict(ai_human_relationship__technocratic_optimization, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__technocratic_optimization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__technocratic_optimization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__technocratic_optimization, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__technocratic_optimization, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__technocratic_optimization_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__technocratic_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82 at interval end) reflects the scale of value transfer: the optimization frame extracts dignity, autonomy, and material security from vulnerable populations while concentrating economic surplus and decision authority in gatekeeping institutions. Suppression (0.78) is high because the frame's persistence depends on actively excluding alternative epistemologies (relational, justice-oriented, post-growth) from governance — not merely on participant preference. Theater ratio (0.45) captures the real but diminishing coordination function: the efficiency gains are genuine but increasingly decoupled from human flourishing. Accessibility collapse (0.75) is high because once the optimization frame is accepted as 'technical necessity,' alternatives appear irrational or obsolete. Resistance (0.55) is moderate: CST, labor, and justice movements resist but the constraint's infrastructure is deeply embedded.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (algorithmic_gatekeepers) experiences the constraint as genuine coordination they built and maintain — a rope from their position. The payer seats (vulnerable_populations, workers_subordinated, data_profiled_persons) experience it as enforced extraction with no meaningful exit — a snare from their positions. The beneficiary seat (efficiency_obsessed_institutions) experiences a tangled rope: real coordination benefit, real extraction cost, constrained exit. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the structural reality that both coordination and extraction are present, not a single-seat perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithmic_gatekeepers are structural beneficiaries (d near 0.0) — they set the optimization objectives and capture the surplus. Efficiency_obsessed_institutions are near-symmetric beneficiaries (d ~ 0.3) — they gain coordination but are locked in by competitive pressure. Vulnerable_populations are full targets (d near 1.0) — trapped, identity-locked in their exclusion, bearing the full cost of the frame's operation. Workers_subordinated are high targets (d ~ 0.8) — constrained exit, subordinated pace. Data_profiled_persons are high targets with identity lock (d ~ 0.85) — the profile precedes the person, opt-out is existentially costly. Observers (CST theologians, technology ethicists) sit at d = 0.5 (analytical). Excluded voices are trapped targets (d ~ 0.9) — their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing complex systems at scale) was real but the technocratic frame's solution — substituting optimization for judgment — has outlived its justification. The mandate has atrophied: the coordination function persists but serves extraction more than human flourishing. The constraint is not a piton (the function has not fully atrophied; active enforcement continues) but a tangled_rope where the coordination story increasingly covers extraction. The mandatrophy is unresolved: the frame persists because no alternative coordination mechanism has been institutionalized at scale, and the gatekeepers actively suppress alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the technocratic_optimization frame a genuine reading of the ai_human_relationship kernel, or a distortion that forecloses the kernel''s irreducible tension?',
    'Genealogical analysis of the kernel''s formation: does the optimization frame emerge from within the kernel''s conceptual space, or is it an external imposition (market/state power) that captures the kernel''s terminology?',
    'If genuine reading, the kernel itself is contested terrain and all three readings are structurally symmetric. If distortion, the technocratic frame is a snare masquerading as a reading — its coordination function is a cover for extraction, and the kernel''s true structure is incarnational_humanism with instrumental_subsidiarity as a transitional scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the optimization frame is internal or external to the kernel''s conceptual structure.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (scalable allocation) be separated from the asymmetric extraction (gatekeeper capture, population exclusion), or are they structurally fused in the optimization frame?',
    'Natural experiments from jurisdictions implementing algorithmic transparency, worker participation in optimization objectives, and non-extractive AI governance (e.g., EU AI Act implementation, Chilean algorithmic registry, union-negotiated algorithmic management). If coordination persists without extraction, they are separable.',
    'If separable, the constraint is a tangled_rope where extraction is removable overhead. If fused, the optimization frame itself is extractive by nature — a snare with a coordination cover story. This determines whether reform (tangled_rope → rope) or abolition (snare → mountain of human dignity) is the structurally honest path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and extraction are structurally separable in algorithmic governance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (algorithmic infrastructure, legal frameworks, market discipline) or internalized (the quantified self, meritocratic internalization, identity fusion with productivity metrics)?',
    'Post-exit suppression trajectory studies: track persons who leave algorithmic management (retire, change sectors, resist) — does the optimization frame''s grip persist in self-assessment, worth-valuation, and relational patterns?',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal exit. This would push classification toward snare (internalized suppression = deeper extraction) and imply that structural reform alone is insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the technocratic optimization frame.').

omega_variable(
    reading_relation_diagnosis,
    'Does the technocratic_optimization reading structurally foreclose the incarnational_humanism reading, or merely create downstream pressure?',
    'Institutional analysis: can a single governance framework (e.g., a hospital system, a municipal government) simultaneously hold optimization objectives for resource allocation AND incarnational commitments to the irreducible dignity of each patient/citizen? Or does the optimization logic colonize the incarnational space?',
    'If forecloses, the kernel is a zero-sum contest — one reading must displace the other. If influences, the readings coexist in tension, and the task is to structure institutional firewalls. This determines the reading_relations classification and the strategic posture of CST engagement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_diagnosis, conceptual, 'Whether technocratic optimization logically forecloses incarnational humanism within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__technocratic_optimization, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t2010, ai_human_relationship__technocratic_optimization, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(ai_h_tr_t2013, ai_human_relationship__technocratic_optimization, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(ai_h_tr_t2016, ai_human_relationship__technocratic_optimization, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(ai_h_tr_t2019, ai_human_relationship__technocratic_optimization, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(ai_h_tr_t2022, ai_human_relationship__technocratic_optimization, theater_ratio, 2022, 0.42).
narrative_ontology:measurement(ai_h_tr_t2025, ai_human_relationship__technocratic_optimization, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2010, ai_human_relationship__technocratic_optimization, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(ai_h_be_t2013, ai_human_relationship__technocratic_optimization, base_extractiveness, 2013, 0.52).
narrative_ontology:measurement(ai_h_be_t2016, ai_human_relationship__technocratic_optimization, base_extractiveness, 2016, 0.61).
narrative_ontology:measurement(ai_h_be_t2019, ai_human_relationship__technocratic_optimization, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(ai_h_be_t2022, ai_human_relationship__technocratic_optimization, base_extractiveness, 2022, 0.77).
narrative_ontology:measurement(ai_h_be_t2025, ai_human_relationship__technocratic_optimization, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2010, ai_human_relationship__technocratic_optimization, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(ai_h_su_t2013, ai_human_relationship__technocratic_optimization, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(ai_h_su_t2016, ai_human_relationship__technocratic_optimization, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement(ai_h_su_t2019, ai_human_relationship__technocratic_optimization, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(ai_h_su_t2022, ai_human_relationship__technocratic_optimization, suppression_requirement, 2022, 0.73).
narrative_ontology:measurement(ai_h_su_t2025, ai_human_relationship__technocratic_optimization, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__technocratic_optimization, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__technocratic_optimization, 0.12).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, algorithmic_labor_management).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, predictive_policing_allocation).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, credit_scoring_exclusion).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, healthcare_triage_optimization).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ai_human_relationship constraint family. The kernel decomposes into three readings: technocratic_optimization (this story), instrumental_subsidiarity, and incarnational_humanism. The technocratic frame upstream-influences the other two by setting the infrastructural and epistemic conditions they must contest. The ε values differ substantially: technocratic_optimization ε ≈ 0.82 (high extraction), instrumental_subsidiarity ε ≈ 0.35 (moderate, regulation-dependent), incarnational_humanism ε ≈ 0.1 (negligible extraction, Mountain-like from the analytical seat).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__technocratic_optimization, institutional, 0.1).
constraint_indexing:directionality_override(ai_human_relationship__technocratic_optimization, organized, 0.3).
constraint_indexing:directionality_override(ai_human_relationship__technocratic_optimization, powerless, 0.95).
constraint_indexing:directionality_override(ai_human_relationship__technocratic_optimization, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
