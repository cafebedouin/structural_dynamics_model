% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission: Infrastructure Competence vs. Coordination Knowledge Decay
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   Civil preparedness systems transmit knowledge unevenly across their
 *   layers. Physical infrastructure competence (dams, levees, pumps, power
 *   systems) remains high because engineering knowledge is continuous,
 *   practice-tested, and failure is visible. Civilian coordination knowledge
 *   (evacuation sequencing, multi-agency response, shelter operations,
 *   communication protocols) has decayed because these skills are tested
 *   rarely, personnel rotate unpredictably, and exercises remain tabletop
 *   rather than integrated. This constraint instantiates the HYBRID READING
 *   of the preparedness_transmission kernel: the system works when it is
 *   asked to do only what engineering can provide, and fails catastrophically
 *   when it must do what coordination can no longer execute. The constraint
 *   is a tangled_rope because it coordinates infrastructure stability while
 *   extracting from coordination networks and dependent populations. Theater
 *   is high (0.61) because preparedness is performed through drills and
 *   inspections that validate engineering but do not exercise coordination
 *   failures.
 *
 * KEY AGENTS:
 *   - engineering_cohorts: continuous knowledge transmission, career security, visible competence — beneficiary
 *   - infrastructure_maintenance_orgs: set standards, control budgets, extract benefit from stable infrastructure — agenda-setter/beneficiary
 *   - civilian_coordination_networks: responsible for translation layer, bear costs of knowledge decay, constrained resources — payer
 *   - evacuation_dependent_populations: lack voice, rely on coordination layer, trapped in geographic dependence — payer/excluded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission: Infrastructure Competence vs. Coordination Knowledge Decay").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'c43ed57a-2ea0-451a-aef9-b8fefe330947').
narrative_ontology:cs_kernel_codification('c43ed57a-2ea0-451a-aef9-b8fefe330947', distributed).
narrative_ontology:cs_authority_grounding('c43ed57a-2ea0-451a-aef9-b8fefe330947', practice).
narrative_ontology:cs_interpretation_layer_present('c43ed57a-2ea0-451a-aef9-b8fefe330947').
narrative_ontology:cs_reading_relation('c43ed57a-2ea0-451a-aef9-b8fefe330947', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c43ed57a-2ea0-451a-aef9-b8fefe330947', preparedness_transmission__husk_reading, influences).
narrative_ontology:cs_axiom('c43ed57a-2ea0-451a-aef9-b8fefe330947', foundational, infrastructure_competence_visible_coordination_invisible).
narrative_ontology:cs_axiom_status(infrastructure_competence_visible_coordination_invisible, holdable).
narrative_ontology:cs_axiom_grounding('c43ed57a-2ea0-451a-aef9-b8fefe330947', infrastructure_competence_visible_coordination_invisible, empirically_contingent).
narrative_ontology:cs_axiom('c43ed57a-2ea0-451a-aef9-b8fefe330947', secondary, dual_layer_transmission_requires_equal_exercise).
narrative_ontology:cs_axiom_status(dual_layer_transmission_requires_equal_exercise, holdable).
narrative_ontology:cs_axiom_grounding('c43ed57a-2ea0-451a-aef9-b8fefe330947', dual_layer_transmission_requires_equal_exercise, instrumental).
narrative_ontology:cs_reference_frame('c43ed57a-2ea0-451a-aef9-b8fefe330947', dual_layer_active_transmission).
narrative_ontology:cs_drift_state('c43ed57a-2ea0-451a-aef9-b8fefe330947', contemporary_unexercised_coordination, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c43ed57a-2ea0-451a-aef9-b8fefe330947', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_cohorts).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_maintenance_orgs).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_coordination_networks).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, evacuation_dependent_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Technical specialists in structural engineering, hydraulics, power systems, and critical infrastructure. Their knowledge transmission is continuous and practice-tested through regular maintenance, inspection cycles, and equipment upgrades. They work in a domain where failure is immediately visible and costly, so competence standards remain high. They benefit from the constraint because it creates stable career pathways, funding, and professional validation through visible infrastructure performance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_cohorts, beneficiary,
    organized, generational, mobile, national).

% Government agencies and private contractors responsible for dams, levees, pumps, power distribution, water treatment, and transport networks. They set the inspection schedule, enforce standards, allocate maintenance budgets, and control which competencies are transmitted to successors. Their budgets and mandates are tied to infrastructure performance metrics. They benefit because functional infrastructure is visible, measurable, and politically defensible.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_maintenance_orgs, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, infrastructure_maintenance_orgs, beneficiary).

% Emergency management officials at local and regional levels, community liaisons, shelter coordinators, voluntary organization networks. They are responsible for translating infrastructure performance into actual evacuation, sheltering, medical response, and community reassurance. Their knowledge is transmitted through rotating personnel, unpredictable crises, and institutional memory that decays when key individuals leave. They bear the cost when infrastructure works but coordination fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_coordination_networks, payer,
    moderate, biographical, constrained, regional).

% Residents in flood zones, near critical infrastructure, in areas dependent on evacuation routes and shelter systems. They depend on the coordination layer to translate infrastructure capability into actual safety outcomes. They receive no advance notice of which coordinators have left, which procedures have been forgotten, or which communication systems are being tested versus activated. Their options are migration (economically infeasible for most) or accepting evacuation-dependent residency.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, evacuation_dependent_populations, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, evacuation_dependent_populations, excluded).

% Researchers, archivists, and institutional historians who document disaster response systems. They can observe the stratification directly: engineering manuals are archived, maintained, taught; coordination protocols exist on paper but lack refresher training, lack cross-agency exercises, lack documented handoff procedures for key personnel.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, knowledge_preservationists, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, infrastructure_maintenance_orgs).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a layered system for hazard response: physical infrastructure (dams, pumps, levees, power lines) detects and mitigates hazards; coordination systems (emergency management, evacuation routes, shelter allocation, communication) translate infrastructure capability into population-level safety outcomes. Both layers must function for preparedness to succeed.
% TRANSFER_FUNCTION: Resources (training time, inspection budgets, knowledge transfer, operational attention) flow disproportionately to the infrastructure layer, which is visible and tested continuously. The coordination layer receives nominal training and budgets but actual transmission of operational knowledge decays as personnel rotate, and exercises remain tabletop rather than integrated.
% ABSENT_VOICES: Evacuees and powerless populations dependent on coordination success are not in the room where competence stratification is debated. Engineers and infrastructure organizations testify to their competence; coordination networks testify to resource constraints but lack power to demand equal investment in their knowledge transmission. Communities that have not experienced catastrophic coordination failure have no voice in the debate.
% DISAPPEARANCE_RATIONALE: If the constraint—the asymmetric transmission that keeps engineering competence high while coordination knowledge decays—were removed and resources were redistricted to match population-safety criticality, evacuation success would improve sharply, preparedness would become genuinely layered rather than front-loaded in infrastructure, and insurance/risk models would shift. The current arrangement exists only because infrastructure visibility sustains its funding and coordination invisibility permits its decay.
% FOUNDING_PROBLEM: Post-WWII civil defense required both physical hardening (infrastructure) and organizational coordination (evacuation, sheltering, recovery). Initial training was intensive and integrated. Over decades of non-events and routine maintenance, physical infrastructure stayed current through continuous engineering cycles, but coordination knowledge (how to orchestrate multi-agency response under stress, how to sequence evacuations when routes are partially blocked, how to operate shelters when normal supply chains break) was never refreshed because no event forced the test.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—teaching both layers simultaneously—is dead in the infrastructure layer but persists unacknowledged in the coordination layer. Engineering organizations attest infrastructure competence is live and continuously exercised. Disaster researchers, after-action reviews from recent events (2005 Hurricane Katrina, 2011 Fukushima), and institutional historians from outside the agencies attest that coordination knowledge has visibly decayed: personnel turnover, loss of handoff documentation, absence of integrated full-scale exercises, degraded inter-agency communication protocols. No organization currently responsible for preparedness attests the founding problem is dead; the attestation comes from observers with no institutional stake in the asymmetry.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the constraint sustains asymmetric transmission: engineering gets continuous investment and testing, coordination gets nominal budgets and exercises that do not expose knowledge gaps. Suppression (0.42) is moderate because the asymmetry is not maintained by overt coercion but by structural invisibility: coordination failures only emerge during rare catastrophic events, so the decay is not disputed until it is too late. Theater (0.61) is the dominant metric because preparedness is performed through regular drills and inspections that visibly validate infrastructure and superficially validate systems (the drills run, people respond), but do not exercise the coordination layer under stress conditions (staffing changes, communication overload, multi-hazard simultaneous occurrence). The measurement series show steady increase in extractiveness and theater as personnel turnover accelerates and exercises drift further from integrated operations.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure/maintenance seat, preparedness looks robust and improving—infrastructure is newer, more redundant, better monitored. From the coordination seat, the constraint is becoming riskier—personnel turnover has accelerated, knowledge transfer is degraded, and the last major integrated exercise revealed gaps in communication protocols that have never been fixed. From the dependent population seat, the system is opaque: they perform evacuation drills that work (because routes are physically clear) without knowing that coordinators could not orchestrate a multi-zone simultaneous evacuation or manage shelter allocation under supply-chain stress. The engine should compute these seats as radically different directionalities: infrastructure as beneficiary (d near 0.2), coordinators as partial targets (d near 0.65), dependent populations as full targets (d near 0.9). The authored claim (tangled_rope) predicts this gap; the metrics describe the extraction from the coordination side.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering cohorts and infrastructure organizations are beneficiaries (d low, ~0.15–0.3): they receive continuous training, professional validation, career stability, and visible evidence of competence. Resources flow to them, and their knowledge transmission is rewarded. Civilian coordination networks are partial targets (d ~0.60–0.70): they are coordinated (part of the system) but also pay: they receive less training, less integration testing, and are responsible for failures that originate in resource starvation, not in their competence. Evacuation-dependent populations are full targets (d ~0.85–0.95): they bear the cost (geographic trap, evacuation dependence) without any voice in how the system is designed or maintained. The constraint extracts from the coordination layer and the dependent layer to benefit the infrastructure layer. This is asymmetric coordination, not pure extraction, because the infrastructure layer's competence does provide genuine safety—just not enough to compensate for the coordination layer's decay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was: how to maintain dual-layer preparedness (physical and coordination) through long periods without events. The problem is dead in the infrastructure layer—engineering cycles force continuous updating and testing—but persists unresolved in the coordination layer. The constraint persists because infrastructure organizations have succeeded in the visible (engineering) and made success look like system success. The mandate to maintain both layers has been fulfilled in name (drills run, exercises happen) but not in function (coordination knowledge has decayed). This is a textbook case of mandatrophy resolution via theater and invisibility: the arrangement persists because the measurement domain (infrastructure) is alive and visible, while the vulnerable domain (coordination) is unobserved until catastrophe. The constraint should be classified as tangled_rope (coordination function + asymmetric extraction from coordination networks) with rising theater, indicating that the actual function (both layers must work) is increasingly performed as ritual while the infrastructure layer carries the appearance of readiness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_decay_observability,
    'Is the decay of coordination knowledge observable before a catastrophic event occurs, or does it only become visible in the failure of evacuation under extreme stress?',
    'Integrated full-scale exercises (not tabletop) that test multi-agency coordination under realistic constraints (time pressure, partial communication loss, competing demands). Post-exercise audits that document gaps, training needs, and knowledge loss relative to prior performance.',
    'If decay is observable through exercises before catastrophic failure, the constraint can be corrected before extraction reaches critical levels. If decay is only visible in actual catastrophe, the constraint persists until a rare event forces recognition—increasing risk and the severity of the eventual failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_decay_observability, empirical, 'Whether coordination knowledge gaps are discoverable through testing or only through failure.').

omega_variable(
    infrastructure_competence_depth,
    'Does engineering competence include tacit knowledge about how infrastructure behaves under multi-hazard scenarios (e.g., what happens to power distribution when water treatment fails), or is each infrastructure domain maintained in isolation?',
    'Cross-domain incident simulations; interviews with senior engineers about system-level failure modes; after-action reviews from events where multiple infrastructure systems failed simultaneously.',
    'If infrastructure competence is isolated (each domain expert knows their system but not how systems interact), then the infrastructure layer''s apparent competence is partly illusory—it works in isolation but may fail under compound stress. If competence is integrated, the infrastructure layer is genuinely more robust than the coordination layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_competence_depth, empirical, 'Whether infrastructure competence is domain-specific or system-integrated.').

omega_variable(
    reading_foreclosure_structure,
    'Can the hybrid reading (stratified competence, infrastructure up/coordination down) coexist with the competence reading (both layers alive) in the same institutional framework, or do they logically foreclose each other?',
    'Policy experiment: redesign preparedness transmission to require integrated exercises that test both layers equally. If coordination knowledge decays despite equal investment, the hybrid reading is true and the competence reading is foreclosed (the founding problem is not solvable through exercises alone). If coordination knowledge recovers with equal investment, the readings coexist (decay was choice, not necessity).',
    'Foreclosure would mean the competence reading is scientifically refuted in this domain—both layers cannot be kept alive simultaneously without fundamental changes. Coexistence would mean decay is a governance choice, not structural inevitability, and both readings remain live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether the hybrid reading logically rules out the competence reading or both can be held simultaneously.').

omega_variable(
    kernel_reading_sibling_alignment,
    'This constraint is the hybrid reading of the preparedness_transmission kernel. Does the authored ε (0.58) accurately represent the standing arrangement under contest (the current stratified state: infrastructure competent, coordination decayed), or should ε be adjusted to represent the competence reading''s endorsed alternative (both layers alive)?',
    'Clarify the referent: what is the standing arrangement THIS reading is about? The current state (stratified, infrastructure up) or the alternative the competence reading would put in place (both layers exercised equally)? The ε-invariance principle requires the referent to be fixed and the ε to be about the standing arrangement under contest, not the alternative.',
    'If ε refers to the current standing arrangement (stratified state: 0.58 is reasonable—extraction from coordination layer is moderate because infrastructure competence provides genuine benefit). If ε is meant to anticipate the competence reading''s correction, ε should be much lower (~0.15–0.25 in the ideal state). The authored choice affects downstream classification and directionality computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_alignment, conceptual, 'Kernel reading ε referent: standing arrangement (current) vs. alternative arrangement (endorsed).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__hybrid_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__hybrid_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__hybrid_reading, theater_ratio, 32, 0.59).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.61).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__hybrid_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__hybrid_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__hybrid_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__hybrid_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(prep_su_t8, observed).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__hybrid_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement_basis(prep_su_t16, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__hybrid_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(prep_su_t32, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel exhibits three structural readings: competence_reading (both layers alive through continuous exercise), husk_reading (both layers perform drills as memorial ritual, knowledge hollow), and hybrid_reading (infrastructure competent, coordination decayed). Each reading has distinct ε, distinct beneficiary/victim structure, and distinct type. The hybrid reading describes the actual observed stratification—infrastructure engineering remains current through continuous cycles, while coordination knowledge decays due to infrequent testing and personnel turnover. The three readings are linked via affects_constraints; they are sibling constraint stories of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__hybrid_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
