% ============================================================================
% CONSTRAINT STORY: boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_maintenance_reading, []).

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
 *   constraint_id: boundary_maintenance_reading
 *   human_readable: Catastrophe Memory as Boundary Enforcement (Boundary Maintenance Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story models ONE READING of the catastrophe memory
 *   kernel: the boundary-maintenance reading, which frames collective
 *   mourning-practice as a mechanism for enforcing group boundaries through
 *   shared ritual participation. The same kernel (a catastrophic historical
 *   event requiring collective memory transmission) can be read as
 *   symbol-continuity (maintaining semantic stability of commemorative
 *   symbols), survival-competence (transmitting practical knowledge for group
 *   persistence), or trauma-encoding (preserving psychological injury across
 *   generations). This reading focuses on the boundary-enforcement function:
 *   participation in mourning-practice signals group membership,
 *   non-participation triggers exclusion, and the ritual coordinates in-group
 *   solidarity by extracting conformity from members and excluding out-group
 *   relations. The constraint is a tangled rope because it combines genuine
 *   coordination (collective memory, mutual support, identity continuity
 *   under persecution) with asymmetric extraction (conformity costs,
 *   suppression of dissenting interpretations, exclusion mechanisms). The
 *   measurements show extraction rising from t=0 (immediate post-catastrophe,
 *   high solidarity) to t=75 (boundary-enforcement institutionalized), then
 *   stabilizing as reform coalitions gain traction. Suppression declines over
 *   the interval as voluntary participation norms spread. Theater ratio rises
 *   modestly as the ritual's boundary-enforcement function becomes more
 *   performative (participation as membership signal rather than grief
 *   expression).
 *
 * KEY AGENTS:
 *   - Dissenting Members: Primary victims (powerless/identity_locked) — cannot exit without abandoning community identity; experience ritual as coercive boundary enforcement
 *   - Ambivalent Participants: Secondary victims (moderate/constrained) — bear conformity costs but also benefit from solidarity; mixed coordination-extraction experience
 *   - Boundary Enforcement Authority: Primary beneficiaries (institutional/arbitrage) — religious leadership whose authority is legitimized by administering the practice
 *   - In-Group Cohesion: Abstract beneficiary (the collective good of group solidarity) — benefits from boundary clarity and shared identity
 *   - Out-Group Relations: Secondary victims (excluded from participation) — boundary-maintenance extracts from potential coalition-building across group lines
 *   - Reform Coalition: Organized agents (organized/mobile) — see boundary-enforcement as transitional; advocate for voluntary participation with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both coordination and extraction; measures the cost of boundary-maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_maintenance_reading, 0.48).
domain_priors:suppression_score(boundary_maintenance_reading, 0.62).
domain_priors:theater_ratio(boundary_maintenance_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_maintenance_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(boundary_maintenance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(boundary_maintenance_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(boundary_maintenance_reading, "Catastrophe Memory as Boundary Enforcement (Boundary Maintenance Reading)").
narrative_ontology:topic_domain(boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(boundary_maintenance_reading, '6e8111e1-5dc6-4477-aeb2-3550b2dde3a2').
narrative_ontology:cs_kernel_codification('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', fixed_text).
narrative_ontology:cs_authority_grounding('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', lineage).
narrative_ontology:cs_interpretation_layer_present('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2').
narrative_ontology:cs_reading_relation('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', boundary_maintenance_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', boundary_maintenance_reading__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', boundary_maintenance_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', foundational, group_boundary_requires_ritual_enforcement).
narrative_ontology:cs_axiom_status(group_boundary_requires_ritual_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', group_boundary_requires_ritual_enforcement, conventional).
narrative_ontology:cs_axiom('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', secondary, conformity_cost_justified_by_cohesion).
narrative_ontology:cs_axiom_status(conformity_cost_justified_by_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', conformity_cost_justified_by_cohesion, instrumental).
narrative_ontology:cs_reference_frame('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', post_catastrophe_solidarity_norm).
narrative_ontology:cs_drift_state('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', contemporary_voluntary_participation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e8111e1-5dc6-4477-aeb2-3550b2dde3a2', '').
narrative_ontology:cs_kernel_id(boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, boundary_enforcement_authority).
narrative_ontology:constraint_victim(boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(boundary_maintenance_reading, out_group_relations).
narrative_ontology:constraint_victim(boundary_maintenance_reading, dissenting_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING MEMBER (SNARE) — Identity-locked within the community; exit would require abandoning family ties, social network, and self-concept as member. Experiences mourning-practice as coercive boundary enforcement: participation is mandatory for maintaining standing, non-participation triggers exclusion. Maximum extraction — the ritual extracts conformity and suppresses individual interpretation of the catastrophe's meaning.
constraint_indexing:constraint_classification(boundary_maintenance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: AMBIVALENT PARTICIPANT (TANGLED ROPE) — Constrained by social costs of exit but not identity-fused. Benefits from community solidarity and mutual support during mourning; also bears cost of conformity pressure and boundary policing. Mixed experience: the ritual coordinates genuine collective grief AND enforces who belongs through participation requirements.
constraint_indexing:constraint_classification(boundary_maintenance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BOUNDARY ENFORCEMENT AUTHORITY (ROPE) — Religious leadership or community elders who administer the mourning-practice. Experience the constraint as coordination: the ritual solves the real problem of maintaining group identity across generations in diaspora or under external pressure. Net beneficiary — the practice legitimizes their authority and concentrates interpretive control.
constraint_indexing:constraint_classification(boundary_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized members advocating for voluntary participation and pluralistic interpretation. See the boundary-enforcement function as temporary: as the community secures its survival and integration, coercive participation requirements can sunset into voluntary commemoration. The coordination function (collective memory) persists; the extraction mechanism (mandatory conformity) is transitional.
constraint_indexing:constraint_classification(boundary_maintenance_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both genuine coordination (collective memory transmission, mutual support, identity continuity under persecution) and asymmetric extraction (conformity costs, exclusion of dissenters, suppression of alternative interpretations). The boundary-maintenance function is real but not cost-free: the ritual coordinates in-group solidarity by extracting from those who would interpret the catastrophe differently or exit the practice.
constraint_indexing:constraint_classification(boundary_maintenance_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_maintenance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The boundary-maintenance function extracts conformity from members (participation is mandatory for standing) and excludes out-group relations (the ritual is closed to non-members). But the extraction is not as severe as a pure snare because the coordination function is real: the ritual does transmit collective memory, provide mutual support, and maintain identity continuity under external pressure. The value reflects that boundary-maintenance has genuine coordination benefits alongside its conformity costs. Suppression (0.62): Moderate-high. Significant barriers to exit include identity-lock (self-concept as member), social costs (family ties, community network), and interpretive closure (alternative readings of the catastrophe are delegitimized). But suppression is not total: reform coalitions exist, voluntary participation norms are spreading, and some members do exit. Theater ratio (0.35): Low-moderate. The mourning-practice has some performative content (participation as membership signal) but is not primarily theatrical. Most participants experience genuine grief, solidarity, and memory transmission. The theater has increased modestly over the interval as the ritual's boundary-enforcement function has become more institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The dissenting member sees a snare: coercive boundary enforcement with no exit. The ambivalent participant sees tangled rope: real solidarity benefits mixed with conformity costs. The boundary enforcement authority sees rope: the ritual solves the legitimate problem of maintaining group identity. The reform coalition sees scaffold: the boundary-enforcement function is transitional and can sunset into voluntary commemoration. The analytical observer sees tangled rope: both coordination and extraction are structurally real. The gap reveals that the same ritual structure produces radically different experiences depending on the agent's power, exit options, and relationship to the boundary being enforced. The constraint is not 'really' any single type — it is a presheaf over the observation site, and each perspective is a legitimate local section.
 *
 * DIRECTIONALITY LOGIC:
 *   Dissenting members are identity-locked victims with no exit — they experience maximum extraction because their identity is constituted through group membership, and the ritual enforces conformity as the price of belonging. Ambivalent participants are constrained victims who also appear in the beneficiary set (in-group cohesion) — they experience moderate extraction because they bear conformity costs but also benefit from solidarity. The boundary enforcement authority is an institutional beneficiary with arbitrage exit — they experience low or negative extraction because the ritual legitimizes their interpretive control. Out-group relations are excluded victims with no standing in the practice — they bear extraction through exclusion from potential coalition-building. The reform coalition is organized with mobile exit — they experience low extraction because they have agency to advocate for change and can exit if reform fails. The analytical observer recognizes the mixed structure: genuine coordination (memory transmission, mutual support) layered with asymmetric extraction (conformity costs, exclusion mechanisms).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that boundary-maintenance is neither pure coordination nor pure extraction. The coordination function is real: the ritual does transmit collective memory, maintain identity continuity, and provide mutual support. The extraction is also real: the ritual enforces conformity, suppresses dissenting interpretations, and excludes out-group relations. The tangled rope classification captures both: the constraint coordinates in-group solidarity by extracting from those who would interpret the catastrophe differently or exit the practice. The reform coalition's scaffold perspective shows that the extraction mechanism (mandatory participation) can be separated from the coordination function (collective memory) — as the community secures its survival, coercive boundary-enforcement can sunset into voluntary commemoration. The analytical observer's task is not to choose between coordination and extraction but to measure both and map the perspectival gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is catastrophe memory ritual primarily boundary-enforcement (this reading), symbol-continuity (sibling reading), survival-competence (sibling reading), or trauma-encoding (sibling reading)?',
    'Cross-reading structural comparison: which reading''s beneficiary/victim structure best predicts observed participation patterns, exit costs, and enforcement mechanisms across multiple communities with the same kernel ritual?',
    'If boundary-maintenance: extraction flows to in-group cohesion at cost of individual autonomy. If symbol-continuity: extraction flows to interpretive authority at cost of semantic drift. If survival-competence: coordination dominates, extraction minimal. If trauma-encoding: extraction flows to memory-keepers at cost of healing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the catastrophe memory kernel best captures the structural relationship').

omega_variable(
    voluntary_participation_threshold,
    'At what level of voluntary participation does the boundary-enforcement function collapse into pure coordination?',
    'Longitudinal study of communities that transitioned from mandatory to voluntary mourning-practice: does group cohesion persist, does boundary clarity degrade, do alternative interpretations proliferate?',
    'If cohesion persists with voluntary participation: the extraction mechanism (coerced conformity) was not load-bearing, and the constraint is closer to rope than tangled_rope. If cohesion degrades: the boundary-enforcement extraction was structural, confirming tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_threshold, empirical, 'Whether voluntary participation preserves boundary-maintenance function').

omega_variable(
    out_group_exclusion_necessity,
    'Is exclusion of out-group members from the mourning-practice necessary for in-group cohesion, or is it extractive overhead?',
    'Comparison of communities with inclusive vs exclusive mourning-practices: does inclusive participation dilute group identity or does it strengthen solidarity through broader coalition?',
    'If exclusion is necessary: the victim (out-group relations) is a structural cost of coordination. If exclusion is overhead: the constraint is more extractive than the base metrics suggest, and the boundary-maintenance reading naturalizes what is actually gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(out_group_exclusion_necessity, empirical, 'Whether out-group exclusion is coordination cost or extractive overhead').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the catastrophe event itself, or the interpretive tradition that frames the event as requiring ritual response?',
    'Historical analysis: do communities with the same catastrophe but different interpretive traditions produce different ritual structures? If yes, the kernel is the interpretive tradition (lineage authority), not the event.',
    'If kernel is the event: authority_grounding should be practice (the event itself demands response). If kernel is the interpretive tradition: authority_grounding is correctly lineage (the tradition mediates the event''s meaning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the catastrophe event or the interpretive tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boundary_maint_theater_t0, boundary_maintenance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(boundary_maint_theater_t25, boundary_maintenance_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(boundary_maint_theater_t50, boundary_maintenance_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(boundary_maint_theater_t75, boundary_maintenance_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(boundary_maint_theater_t100, boundary_maintenance_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(boundary_maint_extract_t0, boundary_maintenance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(boundary_maint_extract_t25, boundary_maintenance_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(boundary_maint_extract_t50, boundary_maintenance_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(boundary_maint_extract_t75, boundary_maintenance_reading, base_extractiveness, 75, 0.51).
narrative_ontology:measurement(boundary_maint_extract_t100, boundary_maintenance_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(boundary_maint_suppress_t0, boundary_maintenance_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(boundary_maint_suppress_t25, boundary_maintenance_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(boundary_maint_suppress_t50, boundary_maintenance_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(boundary_maint_suppress_t75, boundary_maintenance_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement(boundary_maint_suppress_t100, boundary_maintenance_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(boundary_maintenance_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, survival_competence_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The boundary-maintenance reading is one of four structurally distinct constraints derived from the catastrophe memory kernel. Each reading has its own ε value reflecting different extraction mechanisms: boundary-maintenance extracts conformity for in-group cohesion; symbol-continuity extracts interpretive control for semantic stability; survival-competence coordinates practical knowledge transmission with minimal extraction; trauma-encoding extracts healing capacity for memory preservation. The readings are not measurement-dependent views of one constraint — they are different constraints that happen to share a historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
