% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint operates in disaster-preparedness systems where drills,
 *   inspections, and compliance documentation have become primary evidence of
 *   readiness, detached from continuous operational competence. The husk
 *   reading models the situation where preparedness has become ceremonial:
 *   bureaucrats schedule exercises on paper, responders participate in them,
 *   and the public and oversight bodies treat compliance as proof of
 *   capacity. The actual coordination knowledge, tacit skill, and real-time
 *   judgment required to execute under pressure have atrophied because the
 *   institutional system allocates resources toward visible evidence
 *   production rather than live skill retention. The measurement series shows
 *   rising theater_ratio (compliance growing increasingly decoupled from
 *   competence), stable-to-rising extractiveness (institutional legitimacy
 *   persists even as actual response capacity declines), and modest
 *   suppression (the gap between drills and reality is not explicitly denied,
 *   but is not acted upon). This is a piton: the constraint persists not
 *   because anyone benefits enough to defend it or suffers enough to remove
 *   it, but because the administrative apparatus has become dependent on the
 *   compliance narrative and changing it would require admitting past
 *   failures.
 *
 * KEY AGENTS:
 *   - Disaster response bureaucrats: designers and schedulers of exercises; career incentive is compliance reporting, not competence verification
 *   - Frontline responders: firefighters, medics, water managers; participate in drills but find them decoupled from real judgment and coordination demands
 *   - Disaster-affected populations: powerless, trapped; depend on preparedness claims being true; bear the full cost when competence fails
 *   - Specialized technical institutions (e.g., Rijkswaterstaat): excluded from the husk-reading narrative because their continuous operational practice sustains competence; the split between ceremonial and operational systems is itself evidence of the constraint
 *   - Oversight bodies: operate under information asymmetry; see compliance metrics, not latent capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.52).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'a31f5759-6cd0-404e-a23d-d9fc91dc58ce').
narrative_ontology:cs_kernel_codification('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', implicit).
narrative_ontology:cs_authority_grounding('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', extraction).
narrative_ontology:cs_interpretation_layer_present('a31f5759-6cd0-404e-a23d-d9fc91dc58ce').
narrative_ontology:cs_reading_relation('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', foundational, ceremony_substitutes_for_competence).
narrative_ontology:cs_axiom_status(ceremony_substitutes_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', ceremony_substitutes_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', secondary, compliance_visibility_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(compliance_visibility_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', compliance_visibility_sufficient_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', preparedness_as_auditable_certification).
narrative_ontology:cs_drift_state('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', contemporary_disaster_failure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a31f5759-6cd0-404e-a23d-d9fc91dc58ce', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, administrative_legitimacy).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, disaster_affected_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, political_leadership).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, bureaucratic_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional claim that preparedness systems are maintained and functional. This non-agent entity is what benefits: the regime's legitimacy persists because it can point to drills, inspections, compliance checklists, and formal readiness declarations as evidence of state capacity, even when the actual competence to execute under pressure has atrophied.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, administrative_legitimacy, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_retention__husk_reading, administrative_legitimacy).

% Design, schedule, and conduct preparedness drills and inspections. They define what 'readiness' looks like on paper, allocate budgets toward visible compliance (exercises, documentation), and report completion rates upward. They are under pressure to produce evidence of readiness for audit and oversight purposes. Their career advancement depends on being able to demonstrate compliance, not on whether the trained capacity would actually function in a genuine crisis.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_response_bureaucrats, agenda_setter,
    institutional, biographical, constrained, national).

% Firefighters, emergency medical personnel, water managers, and local coordinators who actually execute response during disasters. They participate in drills and inspections as required, but find the exercises rarely test the judgment, coordination, or improvisation skills needed during real events. They bear the cost of time spent in ceremonial exercises and the burden of discovering during actual disasters that the chain of command, communication protocols, and resource stockpiles are not what the drills suggested.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, regional).

% Communities that suffer injury, displacement, and property loss when disaster strikes. They depend on the state's preparedness claims to be true. When the response system fails or operates below the capacity the drills and inspections promised, they pay the full cost: death, displacement, economic ruin. They have no voice in the design of the preparedness system and cannot exit from the dependence on it.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_affected_populations, payer,
    powerless, immediate, trapped, local).

% Government audit offices, parliamentary committees, and international disaster-relief assessment bodies that review preparedness metrics. They operate under the same information asymmetry as the public: they see drill schedules, inspection checklists, and compliance reports, not the tacit competence or decomposition occurring between exercises. Their oversight role is constrained by the difficulty of measuring latent capability.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, oversight_bodies, observer,
    institutional, generational, analytical, national).

% Organizations like Rijkswaterstaat (Dutch water authority) that retain continuous operational competence through ongoing engineering practice and live infrastructure management. Their exclusion from the core preparedness narrative reflects a split: where continuous technical operation sustains competence, the husk reading does not apply; where preparedness is detached from operational life (civic disaster response, evacuation coordination), the reading applies fully. The separation itself is a governance choice.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, specialized_technical_institutions, excluded,
    institutional, generational, constrained, national).

% Elected officials and senior appointed leadership who benefit from being able to declare the state is prepared. Preparedness drills are photo opportunities; inspection compliance is legislative testimony material. Leadership rarely bears personal consequences from preparedness failures unless the failure becomes a national scandal; their career risk is political (public perception of competence), not operational (actual disaster response responsibility).
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, political_leadership, beneficiary,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, administrative_legitimacy).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint purports to solve the coordination problem of maintaining disaster response competence across dispersed, episodic institutions. Drills and inspections are meant to sustain a shared understanding of procedures, keep equipment stocks verified, and ensure chains of command remain clear.
% TRANSFER_FUNCTION: Moves institutional legitimacy from demonstrable competence to documented compliance. Resources flow from operational capacity (training people for real judgment) toward ceremonial capacity (scheduling exercises, filing inspection reports, producing readiness declarations). Time, attention, and budget allocation are extracted from skill development and invested in evidence production.
% ABSENT_VOICES: Communities living in flood-risk or earthquake-prone zones would object that the drills and inspections do not test the actual coordination failures that kill people in disasters—crossed lines of command, missing equipment when it matters, communication systems that collapse under load, evacuation routes that are not actually passable. They are not present in the design of the preparedness system. Technical specialists in continuing institutions (water boards, port authorities) would testify that competence is preserved through operational practice, not ceremonial exercise, and that the shift toward memorial performance erodes the conditions for that practice.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if drills and inspections stopped being allocated resources and bureaucratic legitimacy shifted from compliance proof to demonstrated competence—the institutional structure would reorganize. Budget would flow toward continuous training and operational experience; disaster response would be staffed and equipped as an ongoing function rather than a mobilizable capacity; communities would demand competence audits rather than compliance certificates. The absence of the constraint would expose the gap between reported readiness and actual capacity, forcing choices about which to invest in.
% FOUNDING_PROBLEM: After World War II and into the Cold War, states built civil-defense preparedness systems to respond to nuclear threat and major disasters. Keeping large populations trained and coordinated for low-probability events required institutional mechanisms: regular exercises, inspection schedules, command structures, and mutual-aid agreements. The challenge was maintaining competence when the real event might not occur for decades.
% FOUNDING_PROBLEM_CORROBORATION: Disaster-response practitioners and emergency-management researchers testify that the founding problem (maintaining readiness for infrequent events) is still nominally live, but the means have shifted. Where operational life sustains practice (water boards managing rivers continuously), competence persists. Where preparedness is purely ceremonial (municipal evacuation protocols, regional coordination), the founding problem has become decoupled from the solution. International disaster assessment bodies and post-disaster review commissions document repeatedly that drills did not predict actual failure modes or response incapacity.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Theater ratio of 0.71 at interval end is high—the constraint is primarily performative. Extractiveness of 0.68 is substantial but not maximal because the beneficiary (institutional legitimacy) is a non-actor entity that collects no concrete resource; the extraction is of institutional attention and budget from operational skill into evidence production. Suppression is modest (0.52) because the gap between drills and reality is not hidden by force—it is hidden by complexity. Frontline responders know the drills do not match real events, but they lack authority to change the system; disaster-affected populations have no voice in the design. Accessibility of alternatives is moderate (0.45 collapse) because exiting the preparedness system is impossible for the responders and populations who depend on it, but the system itself could be reorganized toward continuous operation rather than ceremonial exercise. Resistance is moderate (0.58): responders resist, post-disaster commissions document failures, but the institutional structure absorbs the criticism without changing the core constraint. The coercion grid shows stakes_inflation highest at class level (disaster-affected populations face the catastrophic cost of failure) and organizational level suppression highest (bureaucratic apparatus actively maintains the certification fiction). Individual resistance is sustained because practitioners know the difference between drills and real execution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (disaster-response bureaucrats) and the payer seats (frontline responders, affected populations) should compute radically differently. From the bureaucratic seat, the constraint is a sensible coordination mechanism: it produces auditable evidence, maintains institutional continuity, and distributes preparation across the year. From the responder and population seats, the same structure is an extraction mechanism: it consumes time and resources in exercises that do not build the judgment and muscle-memory needed during actual events, and it permits the state to claim preparedness while actual capacity degrades. The engine should compute rope or tangled-rope from the bureaucratic seat and snare from the responder and population seats. Suppression is moderate rather than high because it is not coerced silence—it is diffuse institutional inertia and information asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative legitimacy (the non-agent beneficiary) benefits from the constraint: it can point to drills and inspections as evidence of state capacity. Bureaucrats benefit indirectly by having a defensible preparedness narrative. Political leadership benefits by being able to declare readiness. Frontline responders are trapped: they must participate in the drills (constrained exit), but the drills do not build the competence they need, so they bear the cost of time invested without the corresponding skill gain. Disaster-affected populations are trapped (no exit) and bear the catastrophic cost when the system fails. The beneficiary is institutional, the victims are human (and diffuse at population level). Directionality for the responder and population seats should be near the target end (high extraction); for bureaucrats, it should be near symmetric (coordinated to participate, minor indirect benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a piton, not a snare, because the constraint persists by inertia and institutional dependence rather than by concentrated extraction that someone actively defends. No bureaucrat becomes rich from the preparedness system; no institution captures the rents. The system persists because changing it requires admitting that past drills and inspections did not deliver the promised competence—a reputational cost the administrative apparatus avoids by maintaining the fiction. The founding problem (maintaining competence for infrequent events) is dead: where continuous operational practice exists (water boards, engineering institutions), competence is preserved; where the preparedness system is purely ceremonial, competence has atrophied. The constraint persists because no single actor has the authority and interest to reorganize it, not because anyone benefits enough to fight for it. The theater_ratio rising from 0.55 to 0.71 is the smoking gun of piton dynamics: the ceremonial aspect is growing while the functional aspect shrinks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_vs_competence_observability,
    'Can the distinction between ceremonial compliance and live competence be reliably measured and made visible to oversight bodies without creating perverse incentives for even more theater?',
    'Post-disaster competence audits conducted by independent technical experts (not the preparedness bureaucracy itself), comparing drill performance to actual event response. Longitudinal tracking of the same responders across drills and real events to measure skill transfer.',
    'If the distinction can be made visible, the constraint would face pressure toward reorganization. If measurement itself produces theater (responders perform better in audited drills because they know they are being measured for competence rather than mere compliance), the constraint persists and feedback loops deepen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_vs_competence_observability, empirical, 'Whether the competence-compliance gap is measurable without inducing Goodhart drift.').

omega_variable(
    dispersed_victim_coalition_formation,
    'Can disaster-affected populations, frontline responders, and technical specialists coordinate to demand competence-based preparedness rather than compliance-based, even though they are geographically dispersed and lack formal representation in the preparedness system?',
    'Post-disaster political mobilization: after a major disaster where drills failed to predict actual response failures, do survivors and responders have sufficient collective voice to redefine preparedness criteria?',
    'Coalition formation among victims could transform the constraint from piton (no one invested in changing it) to snare (the constraint is defended against coherent pressure). If victims cannot coordinate, the piton persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispersed_victim_coalition_formation, empirical, 'Whether dispersed payees can organize to challenge the constraint.').

omega_variable(
    institutional_identity_fusion,
    'To what degree is the administrative preparedness bureaucracy''s institutional identity fused with the certification-compliance model such that adopting a competence-based model would require dismantling the bureaucracy or fundamentally redefining its role?',
    'Comparative case studies of countries/regions that attempted to shift from certification to competence-based preparedness. What structures were dismantled? What career paths ended? What resistance emerged from within the preparedness apparatus?',
    'If identity fusion is high, the constraint is structurally inertial and change would require external shock or political intervention. If low, the constraint could be reorganized through internal reform. Identity_locked exit for bureaucrats (career path dependence on compliance metrics) is the suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_fusion, conceptual, 'Whether the bureaucratic apparatus is identity-locked to the compliance model.').

omega_variable(
    kernel_reading_observability,
    'Can an observer determine whether a given preparedness system instantiates the husk_reading (memorial performance), the competence_reading (live exercised knowledge), or the hybrid_reading (stratified competence) without access to the internal organizational structure and decision-making?',
    'Structured interviews with responders across different preparedness domains (civic evacuation vs. water management); comparison of drill schedules to operational practices; longitudinal skill-retention measurements across personnel.',
    'If the readings are observationally distinct, the kernel can be empirically evaluated. If readings are inherently interpretation-dependent (observer-relative), the kernel is a matter of framing, not structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_observability, conceptual, 'Whether the husk_reading vs. competence_reading vs. hybrid_reading distinction is empirically grounded or frame-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.67).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.72).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__husk_reading, theater_ratio, 32, 0.7).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.71).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__husk_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__husk_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(prep_su_t8, observed).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__husk_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement_basis(prep_su_t16, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__husk_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(prep_su_t32, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(prep_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(prep_grid_01, preparedness_retention__husk_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(prep_grid_02, preparedness_retention__husk_reading, accessibility_collapse(class), 40, 0.4).
narrative_ontology:measurement(prep_grid_03, preparedness_retention__husk_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement(prep_grid_04, preparedness_retention__husk_reading, accessibility_collapse(individual), 40, 0.35).
narrative_ontology:measurement(prep_grid_05, preparedness_retention__husk_reading, accessibility_collapse(organizational), 0, 0.5).
narrative_ontology:measurement(prep_grid_06, preparedness_retention__husk_reading, accessibility_collapse(organizational), 40, 0.55).
narrative_ontology:measurement(prep_grid_07, preparedness_retention__husk_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(prep_grid_08, preparedness_retention__husk_reading, accessibility_collapse(structural), 40, 0.6).
narrative_ontology:measurement(prep_grid_09, preparedness_retention__husk_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(prep_grid_10, preparedness_retention__husk_reading, resistance(class), 40, 0.55).
narrative_ontology:measurement(prep_grid_11, preparedness_retention__husk_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(prep_grid_12, preparedness_retention__husk_reading, resistance(individual), 40, 0.6).
narrative_ontology:measurement(prep_grid_13, preparedness_retention__husk_reading, resistance(organizational), 0, 0.48).
narrative_ontology:measurement(prep_grid_14, preparedness_retention__husk_reading, resistance(organizational), 40, 0.45).
narrative_ontology:measurement(prep_grid_15, preparedness_retention__husk_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(prep_grid_16, preparedness_retention__husk_reading, resistance(structural), 40, 0.4).
narrative_ontology:measurement(prep_grid_17, preparedness_retention__husk_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(prep_grid_18, preparedness_retention__husk_reading, stakes_inflation(class), 40, 0.75).
narrative_ontology:measurement(prep_grid_19, preparedness_retention__husk_reading, stakes_inflation(individual), 0, 0.65).
narrative_ontology:measurement(prep_grid_20, preparedness_retention__husk_reading, stakes_inflation(individual), 40, 0.7).
narrative_ontology:measurement(prep_grid_21, preparedness_retention__husk_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(prep_grid_22, preparedness_retention__husk_reading, stakes_inflation(organizational), 40, 0.58).
narrative_ontology:measurement(prep_grid_23, preparedness_retention__husk_reading, stakes_inflation(structural), 0, 0.48).
narrative_ontology:measurement(prep_grid_24, preparedness_retention__husk_reading, stakes_inflation(structural), 40, 0.5).
narrative_ontology:measurement(prep_grid_25, preparedness_retention__husk_reading, suppression(class), 0, 0.55).
narrative_ontology:measurement(prep_grid_26, preparedness_retention__husk_reading, suppression(class), 40, 0.58).
narrative_ontology:measurement(prep_grid_27, preparedness_retention__husk_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(prep_grid_28, preparedness_retention__husk_reading, suppression(individual), 40, 0.42).
narrative_ontology:measurement(prep_grid_29, preparedness_retention__husk_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(prep_grid_30, preparedness_retention__husk_reading, suppression(organizational), 40, 0.52).
narrative_ontology:measurement(prep_grid_31, preparedness_retention__husk_reading, suppression(structural), 0, 0.35).
narrative_ontology:measurement(prep_grid_32, preparedness_retention__husk_reading, suppression(structural), 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel preparedness_retention. The husk_reading models preparedness as ceremonial performance (high theater_ratio, competence decoupled from compliance). The competence_reading models preparedness as live operational knowledge (low theater_ratio, competence demonstrated through practice). The hybrid_reading recognizes that both readings are structurally true in different domains: technical institutions (water boards, engineering) instantiate competence_reading; civic preparedness systems (evacuation, mutual aid) instantiate husk_reading. All three are sibling readings of the same kernel. Epsilon values differ substantially across readings: husk_reading has high extractiveness (institutional legitimacy extracted from operational capacity); competence_reading has lower extractiveness (coordination function is real and serves beneficiaries and payers symmetrically); hybrid_reading has moderate extractiveness (extraction present where ceremonial systems operate, minimal where technical systems operate). The three stories are linked by network.affects_constraints to enable downstream analysis of kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
