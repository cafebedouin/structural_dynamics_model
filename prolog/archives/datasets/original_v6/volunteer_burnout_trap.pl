% ============================================================================
% CONSTRAINT STORY: volunteer_burnout_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_volunteer_burnout_trap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: volunteer_burnout_trap
 *   human_readable: Volunteer Burnout Trap in Community Organizations
 *   domain: social/organizational/labor
 *
 * SUMMARY:
 *   The volunteer burnout trap in community organizations creates a
 *   structural tension between organizational survival imperatives and
 *   volunteer health. Organizations depend on volunteer labor to maintain
 *   service delivery within constrained budgets. Individual volunteers
 *   develop identity fusion with their helping role and organizational
 *   membership. The constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic exemplar for how identity-lock and
 *   internalized suppression function in extractive constraints. From the
 *   volunteer's perspective, the constraint appears as a snare: high
 *   suppression (organizational dependency, guilt narratives, identity
 *   fusion), high extraction (unpaid labor, emotional labor, personal time
 *   sacrifice), and identity-locked exit options (cannot leave without
 *   becoming a different person). From the organization's perspective, the
 *   constraint appears as pure coordination (Rope) — volunteers enable
 *   service delivery and mission accomplishment. From the advocacy
 *   coalition's perspective, the constraint is temporary (Scaffold) —
 *   volunteer management standards, workload limits, and peer support create
 *   a sunset pathway. From the nonprofit sector's civilizational perspective,
 *   volunteer reliance appears as a natural law of resource scarcity
 *   (Mountain), but the structural data reveals this as a false summit:
 *   organizations choose volunteer-dependency because it is cheaper than
 *   professional staffing, not because it is inevitable. The theater ratio
 *   progression (0.32 → 0.48) reflects increasing performative content in
 *   volunteer narratives as exploitation becomes more visible: mission
 *   framing and passion appeals intensify as working conditions deteriorate.
 *
 * KEY AGENTS:
 *   - Committed Volunteer: Primary victim (powerless/identity_locked) — identity constituted through volunteer role and organizational belonging; maximum extraction through unpaid labor and emotional absorption
 *   - New Volunteer Cohort: Secondary victim (moderate/constrained) — face social and emotional barriers to exit; higher geographic/career mobility than established volunteers
 *   - Organization Leadership: Primary beneficiary (institutional/arbitrage) — captures service delivery capacity and mission accomplishment; arbitrage options (can hire staff, seek grants, reduce scope) but conflate organizational survival with volunteer wellbeing
 *   - Service Recipients: Secondary beneficiary (powerless/trapped) — benefit from volunteer service but have no control over sustainability; stake in volunteer availability but no influence on working conditions
 *   - Volunteer Advocacy Coalition: Organized actors (organized/mobile) — volunteer resource centers, labor advocates, professional associations building alternative pathways with workload limits, role clarity, and transition protocols
 *   - Nonprofit Sector Structure: Institutional inertia (institutional/arbitrage) — maintains volunteer-dependent model through cultural expectation and underfunding, despite emerging evidence of professionalization benefits
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent funding model as immutable law of resource scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(volunteer_burnout_trap, 0.58).
domain_priors:suppression_score(volunteer_burnout_trap, 0.62).
domain_priors:theater_ratio(volunteer_burnout_trap, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(volunteer_burnout_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(volunteer_burnout_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(volunteer_burnout_trap, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(volunteer_burnout_trap, tangled_rope).
narrative_ontology:human_readable(volunteer_burnout_trap, "Volunteer Burnout Trap in Community Organizations").
narrative_ontology:topic_domain(volunteer_burnout_trap, "social/organizational/labor").

domain_priors:requires_active_enforcement(volunteer_burnout_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(volunteer_burnout_trap, organization_survival).
narrative_ontology:constraint_beneficiary(volunteer_burnout_trap, service_recipients).
narrative_ontology:constraint_victim(volunteer_burnout_trap, volunteer_health_and_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMMITTED VOLUNTEER (SNARE) — Structurally mobile (could leave) but identity-fused with volunteer role. Self-concept is constituted through organizational membership and helping identity. Exit would require abandoning identity as 'the reliable one,' 'the helper,' 'the person the organization depends on.' High suppression from internalized obligation and identity lock. Experiences maximum extraction: provides labor during personal crisis, subordinates health to organizational needs, absorbs emotional labor without compensation.
constraint_indexing:constraint_classification(volunteer_burnout_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: NEW VOLUNTEER COHORT (TANGLED ROPE) — Face constrained exit: leaving carries social penalty (disappointing the organization), loss of community belonging, and guilt about service gap. However, younger volunteers have slightly higher geographic and career mobility than established volunteers. Genuine coordination function exists (volunteers do enable organizational survival and service delivery), but asymmetric extraction occurs as organizational culture normalizes overwork and boundary violation. Extraction is not total because newer volunteers have ongoing exit deliberation and some agency.
constraint_indexing:constraint_classification(volunteer_burnout_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATION LEADERSHIP (ROPE) — Experiences the constraint as pure coordination. Volunteers enable the organization's mission and survival. Leadership sees itself as solving a collective action problem: how to maintain service delivery on limited budget. The extraction mechanism is invisible from this perspective because leadership conflates organizational survival with volunteer wellbeing — 'the organization needs this' becomes 'the volunteer should provide this.' Beneficiary position with arbitrage options (can hire staff, can seek grants, can reduce scope) creates negative effective extraction from their perspective.
constraint_indexing:constraint_classification(volunteer_burnout_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: VOLUNTEER ADVOCACY COALITION (SCAFFOLD) — Organized actors (volunteer resource centers, labor advocacy groups, professional associations) see burnout as a solvable coordination problem with sunset logic. Platforms like HandUp, Better Impact, and volunteer management standards create alternative pathways: role clarity, workload limits, peer support, and transition protocols. High suppression tolerance because the coalition perceives a 10-15 year transition pathway to professionalized volunteer management or strategic service reduction. Mobile exit options (can implement systemic alternatives) drive scaffold classification.
constraint_indexing:constraint_classification(volunteer_burnout_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: NONPROFIT SECTOR STRUCTURE (PITON) — The reliance on volunteer labor is largely theatrical at civilizational scale. Nonprofits depend on volunteers because of structural underfunding, not because volunteers are the optimal service delivery model. The sector maintains volunteer programs through inertia: 'this is how nonprofits work' persists despite emerging evidence that professionalized staffing + selective volunteering produces better outcomes. Theater ratio (0.48) reflects that some genuine volunteer coordination still occurs, but the performative element (the 'heart' narrative, the 'passion' framing) carries increasing weight as exploitation becomes visible.
constraint_indexing:constraint_classification(volunteer_burnout_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, volunteer labor extraction appears as a natural law of resource scarcity: 'nonprofits will always depend on volunteers because they can't afford staff.' This perspective naturalizes the extraction mechanism as an unavoidable consequence of limited funding. However, the structural data contradicts mountain classification — the constraint is contingent on funding models, leadership choice, and labor supply inelasticity, not on laws of nature. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(volunteer_burnout_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(volunteer_burnout_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(volunteer_burnout_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(volunteer_burnout_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(volunteer_burnout_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(volunteer_burnout_trap, TR),
    TR >= 0.70.

:- end_tests(volunteer_burnout_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial value (0.35) reflects genuine coordination function: volunteers do enable service delivery and community benefit. Middle value (0.48) reflects emergence of burnout signaling and identity lock as organizational dependency crystallizes. Final value (0.58) reflects intensifying extraction as organizations normalize overwork and suppress turnover narratives. Suppression (0.62): Moderate-high. Barriers to exit include internalized obligation (identity fusion), organizational dependency narrative ('the organization needs you'), social penalty (disappointing community, guilt), and lack of alternative service pathways. However, suppression is not total because some volunteers do exit, geographic mobility exists, and legal barriers are absent. Theater ratio (0.48): Moderate, reflecting mixed genuine coordination and performative narrative. Initial ratio (0.32) is lower because early volunteer relationships have authentic coordination: people genuinely help and organizations genuinely benefit. As burnout sets in, 'passion' and 'mission' narratives intensify as performative substitutes for improving working conditions. Final ratio (0.48) reflects bifurcation: some genuine coordination persists (volunteers do accomplish work), but increasing theater around mission, calling, and fulfillment as material conditions deteriorate.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the committed volunteer's snare classification and the organization's rope classification is the engine's diagnostic signal that identity-lock mechanisms are active. The volunteer appears to have mobile exit options (could leave, could change roles) but classifies as trapped; the organization appears to be extracting labor. The gap reveals that the suppression mechanism is partly internalized: the volunteer carries the organization's dependency narrative as their own identity narrative. Identity-lock emerges as a distinct exit modulation precisely because it explains why structurally mobile agents remain in extractive constraints. The scaffold perspective's sunset logic is real — volunteer management standards and professional hybrid models are emerging — but depends on whether organizations choose implementation. The piton perspective flags that the nonprofit sector is increasingly maintaining volunteer reliance through mission narrative performativity rather than genuine functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from structural position: beneficiary status + exit options → low d (negative effective extraction); victim status + identity-locked exit → high d (high effective extraction). The committed volunteer's directionality is high (d ≈ 0.88) derived from victim status + identity_locked exit: structurally mobile (could leave) but functionally trapped by identity fusion. Organization leadership's directionality is low (d ≈ 0.12) derived from beneficiary status + arbitrage exit: captures service benefits with options to exit volunteerism via staffing changes. New volunteer cohort's directionality is moderate (d ≈ 0.62) derived from mixed victim status (bear labor costs) + constrained exit (social/emotional penalties to leaving). The volunteer advocacy coalition's directionality is moderate (d ≈ 0.45) derived from organized victim status (advocating for other volunteers) + mobile exit (can implement alternative systems). The piton perspective derives from theater ratio ≥ 0.70 gate, not from high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   IDENTITY-LOCK EXEMPLAR: This constraint demonstrates how identity-locked exit options create snare classification despite material mobility. The committed volunteer could leave (no physical confinement, no legal prohibition, no economic dependency blocking all exit routes). Yet they experience maximum extraction because their identity as 'the reliable helper,' 'the person the organization depends on,' 'the mission-driven volunteer' is constituted through the constraint. Exit would require identity death — becoming a person who walks away from people in need, who abandons their helping identity, who no longer sees themselves as fundamentally good. The identity lock is not a metaphor; it is the binding mechanism. The mandatrophy resolves by showing that the committed volunteer's snare classification is structurally distinct from simple trapped classification: the trap is cognitive/identity-based, not material. This distinction has implications for intervention: material barrier removal (raising volunteer wages, offering exit incentives, providing staff) will not resolve the constraint if the binding mechanism is identity fusion. Identity restructuring work, peer support for identity transition, and explicit permission narratives ('it's okay to step back,' 'you are not abandoning people by taking care of yourself') are required alongside structural change. The organization's rope classification reveals the extraction mechanism's invisibility from the beneficiary perspective: leadership genuinely experiences the constraint as solving a coordination problem, not as exploiting volunteers. This is why organizational culture change is difficult — the extraction is structurally invisible to extractors. The analytical observer's false summit (mountain classification) reveals how resource scarcity narratives naturalize contingent organizational choices. The nonprofit sector is not forced by laws of physics to depend on volunteers; it chooses this model because it is cheaper than professional staffing. Alternative funding models, hybrid professional-volunteer staffing, and service scope reduction all exist as viable paths. The 'natural law' framing obscures these choices and makes extraction seem inevitable rather than chosen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained,
    'Is volunteer retention driven primarily by internalized identity fusion or by material/social constraints to exit?',
    'Longitudinal tracking of exit timing and articulated reasons; comparison of retention rates when exit costs are systematically reduced (grant funding for staff replacement, public recognition for transition); post-exit contact to assess whether volunteers report identity dissolution or relief',
    'If identity-locked: exit requires identity restructuring, not barrier removal. Raising wages or offering exit incentives will not address the binding mechanism. If constrained: material barrier reduction will increase exit, validating the taxonomy. If both: split the constraint into separate stories (identity binding + material constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained, empirical, 'Whether burnout trap is driven by identity fusion or material constraints').

omega_variable(
    organizational_dependency_circularity,
    'Do organizations become dependent on specific volunteers because of volunteer commitment, or do volunteers become committed because organizations become dependent on them (circular reinforcement)?',
    'Structural analysis of volunteer recruitment patterns in organizations that enforce rotation policies vs those that do not; comparison of burnout onset timing with organizational dependency patterns; case studies of organizations that successfully reduced volunteer load without service collapse',
    'If volunteers drive dependency: the extraction mechanism is primarily volunteers'' identity lock and organizational exploitation of it. If organizational dependency drives volunteer commitment: the constraint is a coordination failure (organizations have options but don''t exercise them). If circular: the constraint is a feedback loop that requires dual intervention (both identity work and organizational restructuring).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_dependency_circularity, empirical, 'Whether dependency is driven by volunteer commitment or organization-first choices').

omega_variable(
    suppression_mechanism_internalization,
    'Is volunteer suppression (barriers to exit) structural (legal liability, economic need, geographic isolation) or internalized (guilt, identity fusion, obligation narrative)?',
    'Post-exit interviews: do exited volunteers report that suppression barriers persisted after exit, or did suppression dissolve? Measurement of reengagement rates when organizations explicitly remove exit barriers (grants for staff replacement, public celebration of transitions, role-limit policies)',
    'If structural: reducing material barriers (funding, job replacement, transportation) will enable exit. If internalized: the volunteer carries suppression with them after exit; exit requires identity work, not barrier removal. If both: constraint family decomposition is appropriate (separate stories for material suppression and identity suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in volunteer burnout').

omega_variable(
    service_continuity_necessity,
    'Do organizations genuinely require volunteer labor to maintain service continuity, or is volunteer labor preferred because it is cheaper and more flexible?',
    'Case analysis of organizations that transitioned to fully professional staffing without service reduction; cost-benefit analysis including hidden costs of burnout (volunteer turnover, quality loss, organizational churn); comparison with sectors that do not rely on volunteering (healthcare, education)',
    'If genuinely required: the constraint is a coordination necessity (Rope/Tangled Rope). If preferred (cheaper/flexible): the constraint is organizational extraction (Snare). If sector-dependent: some types of service (emergency response, specialized expertise) may have different volunteer/staff ratios than others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(service_continuity_necessity, empirical, 'Whether volunteer labor is necessary or preferred for service continuity').

omega_variable(
    mission_identity_fusion_depth,
    'How deeply is volunteer identity fused with organizational mission, and how permanent is that fusion?',
    'Narrative analysis of how volunteers describe their role (mission framing vs task framing); assessment of post-exit identity reconstruction; measurement of reengagement rates if organization pivots mission or dissolves',
    'If shallow/temporary: volunteers see burnout as solvable problem with role changes or workload limits. If deep/permanent: identity restructuring required; burnout is a sign of identity-lock constraint, not merely work overload. Distinction informs whether scaffold (temporary solution) or snare (requires identity work) is correct classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_identity_fusion_depth, empirical, 'Depth and permanence of mission-identity fusion in volunteer self-concept').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(volunteer_burnout_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vburn_tr_t0, volunteer_burnout_trap, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vburn_tr_t5, volunteer_burnout_trap, theater_ratio, 5, 0.4).
narrative_ontology:measurement(vburn_tr_t10, volunteer_burnout_trap, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(vburn_be_t0, volunteer_burnout_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vburn_be_t5, volunteer_burnout_trap, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(vburn_be_t10, volunteer_burnout_trap, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(volunteer_burnout_trap, attachment_coordination).
narrative_ontology:boltzmann_floor_override(volunteer_burnout_trap, 0.12).
narrative_ontology:affects_constraint(volunteer_burnout_trap, nonprofit_sustainability_model).
narrative_ontology:affects_constraint(volunteer_burnout_trap, caregiving_labor_extraction).
narrative_ontology:affects_constraint(volunteer_burnout_trap, mission_capture).

% DUAL FORMULATION NOTE:
% Volunteer burnout comprises multiple structurally distinct constraints: (1) organizational labor extraction (ε≈0.58, tangled_rope, this story); (2) identity-locked attachment dynamics (ε≈0.72, snare, separate story with attachment_coordination type); (3) nonprofit sector underfunding model (ε≈0.45, piton, separate story). Each has different ε values, different suppression mechanisms, and different intervention points. Link via network.affects_constraints: burnout story → identity_lock story → sector_underfunding story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(volunteer_burnout_trap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
