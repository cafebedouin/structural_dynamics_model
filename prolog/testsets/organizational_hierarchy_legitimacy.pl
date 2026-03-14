% ============================================================================
% CONSTRAINT STORY: organizational_hierarchy_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_hierarchy_legitimacy, []).

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
 *   constraint_id: organizational_hierarchy_legitimacy
 *   human_readable: Organizational Hierarchy Legitimacy
 *   domain: organizational_theory/governance
 *
 * SUMMARY:
 *   Organizational hierarchy legitimacy is a foundational constraint that
 *   enables scale-based coordination while simultaneously extracting
 *   deference, information asymmetry, and decision-making authority toward
 *   leadership. The constraint exhibits the full spectrum of DR
 *   classification depending on structural position: rank-and-file workers
 *   trapped in the system experience it as a snare; mid-level managers
 *   benefit from advancement opportunity while bearing excluded voice
 *   (tangled rope); senior leaders experience pure coordination and arbitrage
 *   (rope); technical specialists experience mixed coordination and
 *   suppression (tangled rope); organized reform movements see a temporary
 *   problem with a sunset (scaffold); institutional bureaucracy maintains
 *   hierarchy through ritual despite declining functional necessity (piton);
 *   the analytical observer sees the hybrid nature across all scales (tangled
 *   rope). Theater ratio has increased from 0.35 to 0.68 over the interval as
 *   remote work, collaborative tools, and flat-structure alternatives have
 *   made hierarchy's coordination function less essential while its
 *   legitimacy maintenance has become more performative. The constraint is
 *   neither purely functional nor purely extractive — it is a structural
 *   hybrid that solves real coordination problems while distributing benefits
 *   asymmetrically.
 *
 * KEY AGENTS:
 *   - Rank-and-File Workers: Primary victims (powerless/trapped) — bear suppression through limited voice, surveillance, normalized deference; economic dependency creates exit barrier
 *   - Senior Leadership: Primary beneficiaries (institutional/arbitrage) — capture asymmetric decision authority, information control, status premium; arbitrage exit means can transfer authority across organizations
 *   - Mid-Level Managers: Secondary victims and partial beneficiaries (moderate/constrained) — benefit from career advancement through hierarchy, bear suppression through forced alignment and selective information access
 *   - Technical Specialists: Secondary victims with mobility (powerful/mobile) — benefit from task coordination, bear salary suppression justified by hierarchy, can exit but project lock-in constrains mobility
 *   - Organized Reform Movements: Organized agents (organized/constrained) — see hierarchy legitimacy as solvable problem; building alternative legitimacy models (peer governance, flat structures, consensus decision-making)
 *   - Bureaucratic Institutions: Institutional actors (institutional/arbitrage) — maintain hierarchy through ritual despite declining functional necessity; theater ratio measures growing gap between performance and function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and real asymmetric extraction; mandatrophy resolved by accepting hybrid nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_hierarchy_legitimacy, 0.55).
domain_priors:suppression_score(organizational_hierarchy_legitimacy, 0.62).
domain_priors:theater_ratio(organizational_hierarchy_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_hierarchy_legitimacy, extractiveness, 0.55).
narrative_ontology:constraint_metric(organizational_hierarchy_legitimacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(organizational_hierarchy_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_hierarchy_legitimacy, tangled_rope).
narrative_ontology:human_readable(organizational_hierarchy_legitimacy, "Organizational Hierarchy Legitimacy").
narrative_ontology:topic_domain(organizational_hierarchy_legitimacy, "organizational_theory/governance").

domain_priors:requires_active_enforcement(organizational_hierarchy_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_hierarchy_legitimacy, senior_leadership).
narrative_ontology:constraint_beneficiary(organizational_hierarchy_legitimacy, information_controllers).
narrative_ontology:constraint_victim(organizational_hierarchy_legitimacy, rank_and_file_workers).
narrative_ontology:constraint_victim(organizational_hierarchy_legitimacy, organizational_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE WORKER (SNARE) — Trapped within hierarchy by economic dependency, lack of alternative employment in their local context, and normalized power asymmetry. Hierarchy legitimacy constraint extracts compliance and deference; worker bears suppression through limited voice, surveillance, and conformity demands. No exit without severe cost (relocation, retraining, income loss). Maximum experienced extraction — the worker's structural position offers no arbitrage or mobility within the constraint.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by career path dependence and skill specialization within the organization, but benefits from hierarchy as a coordination mechanism that enables delegated authority and advancement opportunity. Genuine coordination function (clear reporting lines, task distribution) coexists with asymmetric extraction (forced alignment with upper management, selective information access, limited autonomous decision-making). Moderate power and constrained exit produce perspectival gap between snare and rope.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LEADERSHIP (ROPE) — Experiences hierarchy legitimacy as a pure coordination mechanism that enables scale, delegation, and organizational function. Leadership benefits from the extracted deference and information asymmetry (arbitrage option: can move to peer organizations with transferred authority). Perceives hierarchy as rational coordination necessity, not extraction. Net beneficiary with high exit optionality transforms the constraint into genuine rope from this perspective.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNICAL SPECIALIST (TANGLED ROPE) — Mobile exit option (can transfer specialized skills across organizations) but constrained by organizational knowledge lock-in and project continuity. Benefits from hierarchy's task coordination (clear reporting for specialized work) but bears extraction through salary suppression (hierarchy justifies lower pay for non-management roles) and excluded decision-making on projects they shape. Powerful within narrow domain but hierarchy limits domain expansion. Mobile exit reduces experienced extraction relative to rank-and-file.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYEES ORGANIZED FOR REFORM (SCAFFOLD) — Organized agents (unions, worker committees, peer review boards) see hierarchy legitimacy as a temporary coordination problem with a sunset. Flat-structure experiments, consensus governance, and peer evaluation protocols represent alternative legitimacy pathways. Extraction is constrained by the organization's capacity to resist these alternatives — the constraint weakens as organizational norms shift toward distributed authority. Theater remains high (performance of 'consultation' replaces actual voice), but sunset logic applies if structural change occurs within organizational lifespan (10-30 years).
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: BUREAUCRATIC LEGITIMACY RITUAL (PITON) — Traditional hierarchy legitimacy persists through institutional inertia despite declining functional necessity (remote work, collaborative tools, flat-structure startups demonstrate alternatives). The legitimacy ritual is substantially performative: org charts, chain-of-command protocols, and deference ceremonies persist because 'this is how organizations work' rather than because alternatives have been tried and failed. Theater ratio (0.68) reflects the growing gap between ritual maintenance and actual coordination function. The constraint has become a vestigial institutional form.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, hierarchy legitimacy solves a genuine coordination problem (how to organize humans for collective action at scale) while simultaneously extracting asymmetric benefits toward leadership. The constraint is neither a mountain (immutable natural law — flat organizations exist and function) nor a rope (pure coordination — extraction is clearly asymmetric). The analytical view identifies this as a hybrid: hierarchy coordinates large-scale work AND extracts deference, compliance, and decision-making authority. Mandatrophy is resolved by accepting both functions as structural.
constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_hierarchy_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_hierarchy_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_hierarchy_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_hierarchy_legitimacy, TR),
    TR >= 0.70.

:- end_tests(organizational_hierarchy_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. Hierarchy does extract asymmetric benefits toward leadership (information control, decision authority, status premium), but the extraction is not maximized — some reward for leadership is legitimate coordination incentive. The value reflects that senior roles genuinely require broader information access and authority for organizational function. The increase from 0.42 to 0.55 over the 40-year interval indicates that traditional coordination functions (task distribution, reporting clarity) have become more substitutable (remote work, collaborative tools, project management systems), exposing the extraction component more visibly. Suppression (0.62): Moderate-high. Workers face real barriers to voice (retaliation risk, normalization of deference, information asymmetry), but suppression is not totalizing — dissent is possible and occurs (unions, worker committees, resignations). The suppression reflects both structural barriers (firing risk) and internalized legitimacy beliefs (the constraint's own propaganda). Theater ratio (0.68): High and rising. As technical alternatives to hierarchy have emerged (flat organizations function, remote teams coordinate without chain-of-command), the maintenance of hierarchy has become increasingly ritualistic. Org charts, deference ceremonies, chain-of-command protocols persist because 'this is how organizations are structured' rather than because alternatives have failed. The 33-point increase in theater over the interval reflects this growing gap.
 *
 * PERSPECTIVAL GAP:
 *   Why does the analytical observer classify this as tangled rope rather than accepting the mountain classification that might emerge from naturalizing hierarchy as inevitable? The mountain perspective would argue: 'Organizations above a certain size require hierarchy; this is a law of organization theory.' But this is a false summit. Flat organizations exist and function (GitHub, Zappos, open-source projects). The mountain classification naturalizes what is actually a contingent institutional arrangement. The tangled rope classification correctly identifies that hierarchy solves real coordination problems (genuine rope function) while extracting asymmetric benefits (genuine snare function). The analytical observer's role is to reject naturalizing framings and identify the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The senior leadership's beneficiary status derives from their control of information and decision-making authority. They capture asymmetric rewards (salary premium, status, autonomy) justified partly as coordination incentive but partly as pure extraction. The rank-and-file worker's victim status derives from suppressed voice and limited autonomy — they bear the compliance costs of hierarchy while bearing none of the coordination benefits (they do not decide on tasks, only execute them). Mid-level managers are both: they benefit from career advancement pathway (partial beneficiary) but bear suppressed voice and forced alignment (partial victim). The mid-level status creates the perspectival gap where they see tangled rope (both coordination and extraction) while workers see pure snare (extraction only). Directionality overrides are not needed because the structural derivation captures the relationships: beneficiaries have lower d (less extracted), victims have higher d (more extracted), and the classification follows from the base properties (extractiveness 0.55, suppression 0.62, coordination function present, asymmetric extraction present = tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that organizational hierarchy legitimacy genuinely performs both coordination and extraction functions simultaneously. It is not 'should we classify this as rope or snare?' but 'hierarchy is both.' The constraint coordinates work at scale (leadership cannot function without delegation; workers cannot coordinate without task assignment) and extracts asymmetric benefits (authority, information, status flow to leadership disproportionately). The false summit (mountain perspective naturalizing hierarchy as inevitable) is rejected by evidence of viable alternatives (flat organizations, consensus governance, peer-based authority). The classification as tangled rope is mandatroph-resolved: the constraint is neither a hidden snare mislabeled as coordination nor hidden coordination mislabeled as extraction. It is transparently both. The increase in theater ratio from 0.35 to 0.68 reveals that as technical alternatives have become viable, the coordination function has become more substitute-able while the extraction function has become more exposed and defended ritualistically. Organizations are choosing to maintain hierarchy less for functional necessity and more for legitimacy maintenance — this is the measurable drift from rope toward piton over civilizational timescales. At the current 40-year mark, the constraint is properly classified as tangled rope with strong piton signals emerging (theater at 0.68, rising).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_internalization_depth,
    'Is suppression of voice and dissent structural (external barriers like firing/ostracism) or internalized (workers believe hierarchy is legitimate/necessary)?',
    'Post-hierarchy organization studies: workers in flat-structure orgs compared to hierarchical orgs on dissent behavior, voice-use confidence, and reported legitimacy beliefs. Exit behavior analysis: do workers who leave hierarchical orgs maintain suppression patterns in non-hierarchical contexts?',
    'If primarily structural: suppression score reflects external coercion (current 0.62 is accurate). If primarily internalized: actual suppression is higher (workers carry the constraint internally), and identity_locked exit option becomes more appropriate for many workers. Classification may shift toward snare if internalization is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_internalization_depth, empirical, 'Degree to which hierarchy suppression is externalized coercion vs. internalized legitimacy belief').

omega_variable(
    coordination_function_necessity,
    'How much of the hierarchy''s coordination function is essential to organizational function vs. performative status maintenance?',
    'Comparative analysis of flat-structure, matrix, and peer-governed organizations; correlation between hierarchy depth and operational efficiency; measurement of information flow and decision quality in hierarchical vs alternative structures.',
    'If hierarchy is 70%+ functionally necessary: classification shifts toward rope (coordination dominates). If hierarchy is 40% or less functionally necessary: classification shifts toward piton (theater dominates). Theater ratio (0.68) suggests 32% functional, 68% performative — if analysis confirms, the constraint is properly classified as tangled_rope with strong piton signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Proportion of hierarchy''s coordination function that is essential vs. performative').

omega_variable(
    alternative_legitimacy_credibility,
    'Are emerging legitimacy models (meritocratic peer review, distributed authority, consensus governance) genuinely functional alternatives or aspirational myths?',
    'Longitudinal study of organizations that adopted alternative models; measurement of burnout, retention, innovation, and decision quality; correlation between legitimacy model and organizational lifespan.',
    'If alternatives are genuinely credible: scaffold perspective is accurate, and extractiveness may decline over time as organizations shift. If alternatives are myths: scaffold perspective overstates the sunset, and hierarchy legitimacy may be structurally stable. Organizational history shows both patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_credibility, empirical, 'Whether emerging organizational legitimacy models are functionally viable alternatives').

omega_variable(
    identity_fusion_in_hierarchy,
    'How many workers experience their professional identity as fused with their hierarchical role, making exit unthinkable even when materially possible?',
    'Qualitative analysis of identity narratives in exit interviews and career-change studies. Measurement of identity shift: do workers who exit hierarchical organizations report identity reconstitution or identity loss? Comparison of identity fusion across professional domains (academics in universities vs contractors in tech).',
    'If identity fusion is prevalent: many trapped agents are actually identity_locked (structurally mobile but cognitively captured). This elevates the measured suppression score and suggests the rope classification from leadership perspective conceals extraction that depends on identity lock-in for workers who might otherwise exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_hierarchy, empirical, 'Extent of professional identity fusion with hierarchical role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_hierarchy_legitimacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgh_tr_t0, organizational_hierarchy_legitimacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(orgh_tr_t20, organizational_hierarchy_legitimacy, theater_ratio, 20, 0.52).
narrative_ontology:measurement(orgh_tr_t40, organizational_hierarchy_legitimacy, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(orgh_be_t0, organizational_hierarchy_legitimacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(orgh_be_t20, organizational_hierarchy_legitimacy, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(orgh_be_t40, organizational_hierarchy_legitimacy, base_extractiveness, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_hierarchy_legitimacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(organizational_hierarchy_legitimacy, 0.18).
narrative_ontology:affects_constraint(organizational_hierarchy_legitimacy, information_asymmetry_in_organizations).
narrative_ontology:affects_constraint(organizational_hierarchy_legitimacy, career_path_dependency).
narrative_ontology:affects_constraint(organizational_hierarchy_legitimacy, distributed_decision_making).

% DUAL FORMULATION NOTE:
% Organizational hierarchy legitimacy is a parent constraint affecting three downstream constraints: information asymmetry (the hierarchy's asymmetric information access), career path dependency (the hierarchy's advancement pathway mechanism), and distributed decision-making (alternative legitimacy models attempting to bypass the hierarchy). Each downstream constraint has its own ε value reflecting different measurement domains. Hierarchy legitimacy is upstream and more general.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_hierarchy_legitimacy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
